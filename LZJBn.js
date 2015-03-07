/*jshint bitwise:false */

/**
 $Id: Iuppiter.js 3026 2010-06-23 10:03:13Z Bear $

 Copyright (c) 2010 Nuwa Information Co., Ltd, and individual contributors.
 Copyright (c) 2013 C. Scott Ananian
 Copyright (c) 2013 Kellpro, Inc.
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
 this list of conditions and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.

 3. Neither the name of Nuwa Information nor the names of its contributors
 may be used to endorse or promote products derived from this software
 without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 $Author: Bear $
 $Date: 2010-06-23 18:03:13 +0800 (星期三, 23 六月 2010) $
 $Revision: 3026 $
 */


//
// This is heavily modified from Bear's original version (https://code.google.com/p/jslzjb/source/browse/) to a hybrid of
// the most current LZJB 'C' implementations
//
// Also took the makebuffer & destination buffer size ideas from Dr. C. Scott Ananian’s implementation at https://github.com/cscott/lzjb
//
// This Version fixes several bugs in the initial implementation and also can now handle UTF-8 strings properly
// This Version uses Buffers on Node (Fastest), Uint8Array on Chrome (Fastest), and Plain Arrays on mobile and Firefox (Fastest)
//
// This version is NOT backwards compatible with the compressed packets from the original Bears routine.  This version has a tiny header
// You can make it compatible by uncommenting out some code in the decompression setup.  By default this code is commented since most
// people are not going to need it.   Hasn't been tested; but this version probably will decompress Dr. Ananian's version as we use
// the same header.
//
//
// Nathanael Anderson
//


(function ( _global ) {
  "use strict";

  var Iuppiter = {
    version: '$Revision: 3026 $'.substring( 11 ).replace( " $", "" )
  };


  /**
   * Convert byte array to a string value. (UTF-8 Array to UCS-2/UTF-16)
   *
   * @param {Array} bytes The input Byte Array values.
   * @return {String} A String from the Byte Array.
   */
  Iuppiter.fromByteArray = function(bytes)  {
      var len = bytes.length;
      var result = "";
      var code;
      var i;
      for (i = 0; i < len; i++) {
        if (bytes[i] <= 0x7f) {
          result += String.fromCharCode(bytes[i]);
        } else if (bytes[i] >= 0xc0) {
          // Mutlibytes
          if (bytes[i] < 0xe0) {
          // 2 bytes
            code = ((bytes[i++] & 0x1f) << 6) | (bytes[i] & 0x3f);
          } else if (bytes[i] < 0xf0) {
            // 3 bytes
            code = ((bytes[i++] & 0x0f) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i] & 0x3f);
          } else {                                                     // 4 bytes
            // turned into two characters in JS as surrogate pair
            code = (((bytes[i++] & 0x07) << 18) |
                ((bytes[i++] & 0x3f) << 12) |
                ((bytes[i++] & 0x3f) << 6) |
                (bytes[i] & 0x3f)) - 0x10000;
            // High surrogate
            result += String.fromCharCode(((code & 0xffc00) >>> 10) + 0xd800);
            // Low surrogate
            code = (code & 0x3ff) + 0xdc00;
          }
          result += String.fromCharCode(code);
        } // Otherwise it's an invalid UTF-8, skipped.
      }
      return result;
  };


  /**
   * Convert string value to a byte array (UCS-2/UTF-16 to UTF-8).
   *
   * @param {String} input The input string value.
   * @param {int} len the string value size
   * @return {Array} A byte array from string value.
   */
    // Chrome is faster 99% of the time (Larger packets are much faster)
  Iuppiter.toByteArrayPush = function ( input, len ) {
    if (len === null || len === undefined) {
      len = input.length;
    }
    var b = [], i, unicode;
    for ( i = 0; i < len; i++ ) {
      unicode = input.charCodeAt( i );
      // 0x00000000 - 0x0000007f -> 0xxxxxxx
      if ( unicode <= 0x7f ) {
        b.push( unicode );
        // 0x00000080 - 0x000007ff -> 110xxxxx 10xxxxxx
      } else if ( unicode <= 0x7ff ) {
        b.push( (unicode >> 6) | 0xc0 );
        b.push( (unicode & 0x3F) | 0x80 );
        // 0x00000800 - 0x0000ffff -> 1110xxxx 10xxxxxx 10xxxxxx
      } else if ( unicode <= 0xffff ) {
        b.push( (unicode >> 12) | 0xe0 );
        b.push( ((unicode >> 6) & 0x3f) | 0x80 );
        b.push( (unicode & 0x3f) | 0x80 );
        // 0x00010000 - 0x001fffff -> 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      } else {
        b.push( (unicode >> 18) | 0xf0 );
        b.push( ((unicode >> 12) & 0x3f) | 0x80 );
        b.push( ((unicode >> 6) & 0x3f) | 0x80 );
        b.push( (unicode & 0x3f) | 0x80 );
      }
    }
    return b;
  };

  // Firefox is faster 99% of the time (minor diff)
  Iuppiter.toByteArrayAllocate = function ( input, len ) {
    if (len === null || len === undefined) {
      len = input.length;
    }
    var i, unicode, b = [], k = 0;
    for ( i = 0; i < len; i++ ) {
      unicode = input.charCodeAt( i );
      // 0x00000000 - 0x0000007f -> 0xxxxxxx
      if ( unicode <= 0x7f ) {
        b[k] = unicode;
        k++;
        // 0x00000080 - 0x000007ff -> 110xxxxx 10xxxxxx
      } else if ( unicode <= 0x7ff ) {
        b[k] = (unicode >> 6) | 0xc0;
        k++;
        b[k] = (unicode & 0x3F) | 0x80;
        k++;
        // 0x00000800 - 0x0000ffff -> 1110xxxx 10xxxxxx 10xxxxxx
      } else if ( unicode <= 0xffff ) {
        b[k] = (unicode >> 12) | 0xe0;
        k++;
        b[k] = ((unicode >> 6) & 0x3f) | 0x80;
        k++;
        b[k] = (unicode & 0x3f) | 0x80;
        k++;

        // 0x00010000 - 0x001fffff -> 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      } else {
        b[k] = (unicode >> 18) | 0xf0;
        k++;
        b[k] = ((unicode >> 12) & 0x3f) | 0x80;
        k++;
        b[k] = ((unicode >> 6) & 0x3f) | 0x80;
        k++;
        b[k] = (unicode & 0x3f) | 0x80;
        k++;
      }
    }

    return b;
  };

  // Default for Chrome / Node / V8 engines
  Iuppiter.toByteArray = Iuppiter.toByteArrayPush;


Iuppiter.Base64 = {

    /// Encoding characters table.
    CA: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",

    /// Encoding characters table for url safe encoding.
    CAS: "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",

    /// Decoding reference table.
    IA: new Array(256),

    /// Decoding reference table for url safe encoded string.
    IAS: new Array(256),

    /**
 * Constructor.
 */
    init: function(){
        /// Initialize variables for Base64 namespace.
        var i, iS;

        for (i = 0; i < 256; i++) {
            Iuppiter.Base64.IA[i] = -1;
            Iuppiter.Base64.IAS[i] = -1;
        }

        for (i = 0, iS = Iuppiter.Base64.CA.length; i < iS; i++) {
            Iuppiter.Base64.IA[Iuppiter.Base64.CA.charCodeAt(i)] = i;
            Iuppiter.Base64.IAS[Iuppiter.Base64.CAS.charCodeAt(i)] = i;
        }

        Iuppiter.Base64.IA['='] = Iuppiter.Base64.IAS['='] = 0;
    },

    /**
 * Encode base64.
 *
 * @param {Array|String} input A byte array or a string.
 * @param {Boolean} urlsafe True if you want to make encoded string is url
 *                          safe.
 * @return {String} Encoded base64 string.
 */
    encode: function(input, urlsafe) {
        var ca, dArr, sArr, sLen,
            eLen, dLen, s, d, left,
            i;

        if(urlsafe) {
            ca = Iuppiter.Base64.CAS;
        } else {
            ca = Iuppiter.Base64.CA;
        }

        if(input.constructor === Array || Buffer && Buffer.isBuffer(input) ) {
            sArr = input;
        } else {
            sArr = Iuppiter.toByteArray(input);
        }

        sLen = sArr.length;

        eLen = (sLen / 3) * 3;              // Length of even 24-bits.
        dLen = ((sLen - 1) / 3 + 1) << 2;   // Length of returned array
        dArr = new Array(dLen);

        // Encode even 24-bits
        for (s = 0, d = 0; s < eLen;) {
            // Copy next three bytes into lower 24 bits of int, paying attension to sign.
            i = (sArr[s++] & 0xff) << 16 | (sArr[s++] & 0xff) << 8 |
                (sArr[s++] & 0xff);

            // Encode the int into four chars
            dArr[d++] = ca.charAt((i >> 18) & 0x3f);
            dArr[d++] = ca.charAt((i >> 12) & 0x3f);
            dArr[d++] = ca.charAt((i >> 6) & 0x3f);
            dArr[d++] = ca.charAt(i & 0x3f);
        }

        // Pad and encode last bits if source isn't even 24 bits.
        left = sLen - eLen; // 0 - 2.
        if (left > 0) {
            // Prepare the int
            i = ((sArr[eLen] & 0xff) << 10) |
                 (left === 2 ? ((sArr[sLen - 1] & 0xff) << 2) : 0);

            // Set last four chars
            dArr[dLen - 4] = ca.charAt(i >> 12);
            dArr[dLen - 3] = ca.charAt((i >> 6) & 0x3f);
            dArr[dLen - 2] = left === 2 ? ca.charAt(i & 0x3f) : '=';
            dArr[dLen - 1] = '=';
        }

        return dArr.join("");
    },

 /**
 * Decode base64 encoded string or byte array.
 *
 * @param {Array|String} input A byte array or encoded string.
 * @param {Object} urlsafe True if the encoded string is encoded by urlsafe.
 * @return {Array|String} A decoded byte array or string depends on input
 *                        argument's type.
 */
    decode: function(input, urlsafe) {
        var ia, dArr, sArr, sLen, bytes,
            sIx, eIx, pad, cCnt, sepCnt, len,
            d, cc, eLen,
            i, j, r;

        if(urlsafe){
            ia = Iuppiter.Base64.IAS;
        } else {
            ia = Iuppiter.Base64.IA;
        }

        if(input.constructor === Array || Buffer && Buffer.isBuffer(input)) {
            sArr = input;
            bytes = true;
        }
        else {
            sArr = Iuppiter.toByteArray(input);
            bytes = false;
        }

        sLen = sArr.length;

        sIx = 0;
        eIx = sLen - 1;    // Start and end index after trimming.

        // Trim illegal chars from start
        while (sIx < eIx && ia[sArr[sIx]] < 0) {
            sIx++;
        }

        // Trim illegal chars from end
        while (eIx > 0 && ia[sArr[eIx]] < 0) {
            eIx--;
        }

        // get the padding count (=) (0, 1 or 2)
        // Count '=' at end.
        pad = sArr[eIx] === '=' ? (sArr[eIx - 1] === '=' ? 2 : 1) : 0;
        cCnt = eIx - sIx + 1;   // Content count including possible separators
        sepCnt = sLen > 76 ? (sArr[76] === '\r' ? cCnt / 78 : 0) << 1 : 0;

        // The number of decoded bytes
        len = ((cCnt - sepCnt) * 6 >> 3) - pad;
        dArr = new Array(len);       // Preallocate byte[] of exact length

        // Decode all but the last 0 - 2 bytes.
        d = 0;
        for (cc = 0, eLen = (len / 3) * 3; d < eLen;) {
            // Assemble three bytes into an int from four "valid" characters.
            i = ia[sArr[sIx++]] << 18 | ia[sArr[sIx++]] << 12 |
                ia[sArr[sIx++]] << 6 | ia[sArr[sIx++]];

            // Add the bytes
            dArr[d++] = (i >> 16) & 0xff;
            dArr[d++] = (i >> 8) & 0xff;
            dArr[d++] = i & 0xff;

            // If line separator, jump over it.
            if (sepCnt > 0 && ++cc === 19) {
                sIx += 2;
                cc = 0;
            }
        }

        if (d < len) {
            // Decode last 1-3 bytes (incl '=') into 1-3 bytes
            i = 0;
            for (j = 0; sIx <= eIx - pad; j++) {
                i |= ia[sArr[sIx++]] << (18 - j * 6);
            }

            for (r = 16; d < len; r -= 8) {
                dArr[d++] = (i >> r) & 0xff;
            }
        }

        if(bytes) {
            return dArr;
        }
        else {
            for(i = 0; i < dArr.length; i++) {
                dArr[i] = String.fromCharCode(dArr[i]);
            }

            return dArr.join('');
        }
    }
};

Iuppiter.Base64.init();

  
  
  (function () {

    // Constants was used for compress/decompress function.
    var settings = {
      NBBY: 8,
      MATCH_BITS: 6,
      MATCH_MIN: 3,
      LEMPEL_SIZE: 1024,
      MATCH_MAX: 0,
      OFFSET_MASK: 0,
      LEMPEL_INIT: 0, //3435973836,
      ARRAYSAREFASTER: false,
      LEMPEL_ARRAY: []
    };
    // Have to do these outside of the settings file as they reference vars that don't exist during init
    settings.MATCH_MAX = ((1 << settings.MATCH_BITS) + (settings.MATCH_MIN - 1));
    settings.OFFSET_MASK = ((1 << (16 - settings.MATCH_BITS)) - 1);




    /**
     * Compress string or byte array using fast and efficient algorithm.
     *
     * Because of weak of javascript's natural, many compression algorithm
     * become useless in javascript implementation. The main problem is
     * performance, even the simple Huffman, LZ77/78 algorithm will take many
     * many time to operate. We use LZJB algorithm to do that, it suprisingly
     * fulfills our requirement to compress string fastly and efficiently.
     *
     * Our implementation is based on
     * http://src.opensolaris.org/source/raw/onnv/onnv-gate/
     * usr/src/uts/common/os/compress.c
     * It is licensed under CDDL.
     *
     * Please note it depends on toByteArray utility function.
     *
     * @param {String|Array} input The string or byte array that you want to
     *                             compress.
     * @return {Array} Compressed byte array.
     */
     Iuppiter.compress = function ( input ) {
      // Copy Globals; so we don't have any closure lookups (Minor Speed up)
      var NBBY = settings.NBBY,
          MATCH_BITS = settings.MATCH_BITS,
          MATCH_MIN = settings.MATCH_MIN,
          MATCH_MAX = settings.MATCH_MAX,
          OFFSET_MASK = settings.OFFSET_MASK,
          LEMPEL_SIZE = settings.LEMPEL_SIZE;
      var LEMPEL_INIT = settings.LEMPEL_INIT;

      // Setup Constants so we don't have to compute this inside the loop (Minor Speed up)
      var NBBY_MINUS_MATCH_BITS = NBBY - MATCH_BITS,
          NBBY_1 = (1 << NBBY),
          LEMPEL_SIZE_MINUS_ONE = LEMPEL_SIZE - 1;

      // Variables we use
      var sstart, dstart = [], slen,
          src = 0, dst = 0, srcLoc = 0, src1 = 0, src2 = 0,
          cpy, copymap,
          copymask = 1 << (NBBY - 1),
          mlen, offset,
          hp,
          lempel = settings.LEMPEL_ARRAY,  // Don't do use Buffers or uint8, appear to be much slower on small buffers
          i;

      // Initialize lempel array -- sticks the array in the hot-path on Chrome (Also better compression).
      for ( i = 0; i < LEMPEL_SIZE; i++ ) {
        lempel[i] = LEMPEL_INIT;
      } 

      // Are we using byte array or not.
      if ( input.constructor === String ) {
        sstart = Iuppiter.copyBuffer( input );
      }
      else {
        sstart = input;
      }

      slen = sstart.length;

	  
	  
      // Add size to Stream
      var size = [], fs = (slen + 1);
      do {
        size.push( fs & 0x7F );
        fs = Math.floor( fs / 128 ); // division instead of shift
      }
      while ( fs !== 0 );
      size[0] |= 0x80;
      for ( i = size.length - 1; i >= 0; i-- ) {
        dstart[dst++] = size[i];
      }
      size = null;

      // Do Compression
      var slenMatch = slen - Math.floor(MATCH_MAX/4);
	  
      while ( srcLoc < slen ) {
        if ( (copymask <<= 1) === (NBBY_1) ) {
          copymask = 1;
          copymap = dst;
          dstart[dst++] = 0;
        }

        if ( srcLoc >= slenMatch ) {
          dstart[dst++] = sstart[srcLoc++];
          continue;
        }

        // Copy Vars so we don't have to do array lookups with them for the rest of the loop
        // This might be a pointless optimization depending on the javascript engine optimizations
		// But this does appear to speed up Node/V8 in all the tests I've ran, eliminating the lookups 
        src = sstart[srcLoc];
        src1 = sstart[srcLoc + 1];
        src2 = sstart[srcLoc + 2];

        // Generate Hash
        var hash = (src << 16) + (src1 << 8) + src2;
        hash ^= (hash >> 9);
        hash += (hash >> 5);
        hash ^= src;
        hp = (hash & LEMPEL_SIZE_MINUS_ONE); 

        // Old Hash
//         hp = ((src + 13) ^ (src1 - 13) ^ src2) & LEMPEL_SIZE_MINUS_ONE;

        offset = (srcLoc - lempel[hp]) & OFFSET_MASK;
        lempel[hp] = srcLoc;
        cpy = srcLoc - offset;

        if ( cpy >= 0 && cpy !== srcLoc && src === sstart[cpy] && src1 === sstart[cpy + 1] && src2 === sstart[cpy + 2] ) {
          dstart[copymap] |= copymask;
          for ( mlen = MATCH_MIN; mlen < MATCH_MAX; mlen++ ) {
            if ( sstart[srcLoc + mlen] !== sstart[cpy + mlen] ) {
              break;
            }
          }

          dstart[dst++] = ((mlen - MATCH_MIN) << (NBBY_MINUS_MATCH_BITS)) | (offset >> NBBY);
          dstart[dst++] = offset;
          srcLoc += mlen;
        } else {
          dstart[dst++] = sstart[srcLoc++];
        }
      }

      // We always return a binary buffer since we are now a compressed string
      return dstart;
    };

    /**
     * Decompress string or byte array using fast and efficient algorithm.
     *
     * Our implementation is based on
     * http://src.opensolaris.org/source/raw/onnv/onnv-gate/
     * usr/src/uts/common/os/compress.c
     * It is licensed under CDDL.
     *
     * Please note it depends on toByteArray utility function.
     *
     * @param {String|Array} input The string or byte array that you want to
     *                             compress.
     * @param {Boolean} _bytes Returns byte array if true otherwise string.
     * @return {String|Array} Decompressed string or byte array.
     */
    Iuppiter.decompress = function ( input, _bytes ) {
      // Copy Globals; so we don't have any closure lookups
      var NBBY = settings.NBBY,
          MATCH_BITS = settings.MATCH_BITS,
          MATCH_MIN = settings.MATCH_MIN,
          OFFSET_MASK = settings.OFFSET_MASK;

      var NBBY_MINUS_MATCH_BITS = NBBY - MATCH_BITS,
          NBBY_1 = (1 << NBBY);

      var sstart, dstart = [], slen,
          src = 0, dst = 0,
          cpy, copymap,
          copymask = 1 << (NBBY - 1),
          mlen, offset,
          bytes;

      // Using byte array or not.
      if ( input.constructor === String ) {
        sstart = Iuppiter.copyBuffer( input );
        bytes = false;
      }
      else {
        sstart = input;
        bytes = true;
      }

      // Default output string result.
      if ( _bytes !== null && _bytes !== undefined ) {
        bytes = _bytes;
      }

      slen = sstart.length;

      // Get Size from Stream (Massive speedup getting a real buffer of the correct size for Node/Chrome/v8)
      var outSize = 0;
      while (src < slen) {
        var c = sstart[src++];
        if ( c & 0x80 ) {
          outSize |= (c & 0x7f);
          break;
        }
        outSize = (outSize | c) * 128; // * instead of << allows sizes up to 2^53
      }

      //Check for Bear's Old packets
      //if (src > 4) { src = 0;  outSize = 0; }

      if ( outSize > 1 && !settings.ARRAYSAREFASTER) {
        dstart = Iuppiter.makeBuffer( outSize - 1 );
      }

      // Do actual Decompression
      while ( src < slen ) {
        if ( (copymask <<= 1) === (NBBY_1) ) {
          copymask = 1;
          copymap = sstart[src++];
        }

        if ( copymap & copymask ) {
          mlen = (sstart[src] >> (NBBY_MINUS_MATCH_BITS)) + MATCH_MIN;
          offset = ((sstart[src] << NBBY) | sstart[src + 1]) & OFFSET_MASK;
          src += 2;
          if ( (cpy = dst - offset) < 0 ) // Error Occured in Source Buffer
          {
            if (console) {
              console.error( "Error occured during decompression, buffer appears to be corrupt -- this shouldn't happen", cpy, dst, offset );
            }
            return (null);
          }

          while ( --mlen >= 0 ) {
            dstart[dst++] = dstart[cpy++];
          }
        } else {
          dstart[dst++] = sstart[src++];
        }
      }

      if ( bytes ) {
        return dstart;
      } else {
        switch ( Object.prototype.toString.call( dstart ) ) {
          case "[object Uint8Array]":
          case "[object Array]":
            return Iuppiter.fromByteArray(dstart);
          default:
            // Object.prototype.toString doesn't return [object Buffer] as expected on a Buffer..  Bug filed in Node.js
            if ( Buffer && Buffer.isBuffer && Buffer.isBuffer( dstart ) ) {
              return dstart.toString();
            }
            return dstart;
        }
      }
    };


    // Buffer is the fastest, but only supported on Node
    if ( typeof(Buffer) !== 'undefined' ) {
        // Separate because of Hotprofiling on v8, we want one optimized for String and one optimized for Integer, even though they
        // Are identically coded; I "BELIEVE" (untested) that v8 prefers single use functions rather than polymorphic
        Iuppiter.copyBuffer = function (data) {
            return new Buffer( data );
        };
        Iuppiter.makeBuffer = function ( len ) {
            return new Buffer( len );
        };

    } else if ( typeof(Uint8Array) !== 'undefined' ) {
      Iuppiter.makeBuffer = function ( len ) {
        return new Uint8Array( len );
      };

      // We Cannot use Uint8Array for the CopyBuffer as the size of the buffer might actually be "larger"
      // due to unicode, so we have to use the fallback Array method

    } else {
      // For older Browsers that don't support either Buffer or Uint8Array
      Iuppiter.makeBuffer = function ( size ) {
        return new Array( size );
      };
    }


    var initializeCompression = function() {
      if (typeof(window) !== 'undefined') {
        if (window && window.navigator) {
          var ua = window.navigator.userAgent || '';
          ua = ua.toLowerCase();

          // Gecko/Safari Engines are faster with these routines
          if (ua.indexOf('gecko') && ua.indexOf('chrome') === -1) {
            Iuppiter.toByteArray = Iuppiter.toByteArrayAllocate;
            settings.ARRAYSAREFASTER = true;
          }
          // I deliberately use "phone" and not "iphone" as other phones may show up as "phone"
          if (ua.indexOf('ipad') || ua.indexOf('phone') || ua.indexOf('android') || ua.indexOf('mobile') || ua.indexOf('tablet')) {
            // Use a smaller dictionary for compression on Mobile Devices
            settings.LEMPEL_SIZE = 256;
          }
        }
        // Pre-Initialize this to hold the buffer (So we aren't GC'ing this since it is needed in every compress)
        for (var i=0;i<settings.LEMPEL_SIZE;i++) {
          settings.LEMPEL_ARRAY[i] = 0;
        }
      }
    };

    initializeCompression();
    if (!Iuppiter.copyBuffer) {
      Iuppiter.copyBuffer = Iuppiter.toByteArray;
    }

  })();


  if (_global) {
    _global.Compress = Iuppiter;
  }
}( typeof exports === 'undefined' ? (typeof this._API === 'undefined' ? this : this._API) : exports ));
"use strict";
// This object will hold all exports.
var Haste = {};

/* Constructor functions for small ADTs. */
function T0(t){this._=t;}
function T1(t,a){this._=t;this.a=a;}
function T2(t,a,b){this._=t;this.a=a;this.b=b;}
function T3(t,a,b,c){this._=t;this.a=a;this.b=b;this.c=c;}
function T4(t,a,b,c,d){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;}
function T5(t,a,b,c,d,e){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;}
function T6(t,a,b,c,d,e,f){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;this.f=f;}

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

/* Hint to optimizer that an imported symbol is strict. */
function __strict(x) {return x}

// A tailcall.
function F(f) {
    this.f = f;
}

// A partially applied function. Invariant: members are never thunks.
function PAP(f, args) {
    this.f = f;
    this.args = args;
    this.arity = f.length - args.length;
}

// "Zero" object; used to avoid creating a whole bunch of new objects
// in the extremely common case of a nil-like data constructor.
var __Z = new T0(0);

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

// Indicates that a closure-creating tail loop isn't done.
var __continue = {};

/* Generic apply.
   Applies a function *or* a partial application object to a list of arguments.
   See https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls
   for more information.
*/
function A(f, args) {
    while(true) {
        f = E(f);
        if(f instanceof Function) {
            if(args.length === f.length) {
                return f.apply(null, args);
            } else if(args.length < f.length) {
                return new PAP(f, args);
            } else {
                var f2 = f.apply(null, args.slice(0, f.length));
                args = args.slice(f.length);
                f = B(f2);
            }
        } else if(f instanceof PAP) {
            if(args.length === f.arity) {
                return f.f.apply(null, f.args.concat(args));
            } else if(args.length < f.arity) {
                return new PAP(f.f, f.args.concat(args));
            } else {
                var f2 = f.f.apply(null, f.args.concat(args.slice(0, f.arity)));
                args = args.slice(f.arity);
                f = B(f2);
            }
        } else {
            return f;
        }
    }
}

function A1(f, x) {
    f = E(f);
    if(f instanceof Function) {
        return f.length === 1 ? f(x) : new PAP(f, [x]);
    } else if(f instanceof PAP) {
        return f.arity === 1 ? f.f.apply(null, f.args.concat([x]))
                             : new PAP(f.f, f.args.concat([x]));
    } else {
        return f;
    }
}

function A2(f, x, y) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 2:  return f(x, y);
        case 1:  return A1(B(f(x)), y);
        default: return new PAP(f, [x,y]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 2:  return f.f.apply(null, f.args.concat([x,y]));
        case 1:  return A1(B(f.f.apply(null, f.args.concat([x]))), y);
        default: return new PAP(f.f, f.args.concat([x,y]));
        }
    } else {
        return f;
    }
}

function A3(f, x, y, z) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 3:  return f(x, y, z);
        case 2:  return A1(B(f(x, y)), z);
        case 1:  return A2(B(f(x)), y, z);
        default: return new PAP(f, [x,y,z]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 3:  return f.f.apply(null, f.args.concat([x,y,z]));
        case 2:  return A1(B(f.f.apply(null, f.args.concat([x,y]))), z);
        case 1:  return A2(B(f.f.apply(null, f.args.concat([x]))), y, z);
        default: return new PAP(f.f, f.args.concat([x,y,z]));
        }
    } else {
        return f;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            if(t.x === __updatable) {
                var f = t.f;
                t.f = __blackhole;
                t.x = f();
            } else {
                return t.f();
            }
        }
        if(t.x === __updatable) {
            throw 'Infinite loop!';
        } else {
            return t.x;
        }
    } else {
        return t;
    }
}

/* Tail call chain counter. */
var C = 0, Cs = [];

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    Cs.push(C);
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        C = 0;
        f = fun();
    }
    C = Cs.pop();
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw E(err);
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return {_:0, a:(a-a%b)/b, b:a%b};
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return {_:0, a:x & 0xffffffff, b:x > 0x7fffffff};
}

function subC(a, b) {
    var x = a-b;
    return {_:0, a:x & 0xffffffff, b:x < -2147483648};
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, __Z);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [str.charCodeAt(i), acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return {_:1,a:str.charCodeAt(i),b:new T(function() {
            return unAppCStr(str,chrs,i+1);
        })};
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str._ == 1; str = E(str.b)) {
        s += String.fromCharCode(E(str.a));
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x.a;
    return x.b;
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return 0;
    } else if(a == b) {
        return 1;
    }
    return 2;
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Object) {
        return x._;
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round, rintDouble = jsRound, rintFloat = jsRound;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt64(i) {
    return popCnt(i.low) + popCnt(i.high);
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function __clz(bits, x) {
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    } else {
        return bits - (1 + Math.floor(Math.log(x)/Math.LN2));
    }
}

// TODO: can probably be done much faster with arithmetic tricks like __clz
function __ctz(bits, x) {
    var y = 1;
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    }
    for(var i = 0; i < bits; ++i) {
        if(y & x) {
            return i;
        } else {
            y <<= 1;
        }
    }
    return 0;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    if(x === 0) {
        return __decodedZeroF;
    }
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return {_:0, a:sign*man, b:exp};
}

var __decodedZero = {_:0,a:1,b:0,c:0,d:0};
var __decodedZeroF = {_:0,a:1,b:0};

function decodeDouble(x) {
    if(x === 0) {
        // GHC 7.10+ *really* doesn't like 0 to be represented as anything
        // but zeroes all the way.
        return __decodedZero;
    }
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return {_:0, a:sign, b:manHigh, c:manLow, d:exp};
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs._) {
        strs = E(strs);
        arr.push(E(strs.a));
        strs = E(strs.b);
    }
    return arr.join(sep);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return __Z;
    }
    return {_:1,a:hs};
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return {_:0, a:jsRead(obj)};
    case 'string':
        return {_:1, a:obj};
    case 'boolean':
        return {_:2, a:obj}; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return {_:3, a:arr2lst_json(obj, 0)};
        } else if (obj == null) {
            return {_:5};
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = {_:1, a:{_:0, a:ks[i], b:toHS(obj[ks[i]])}, b:xs};
            }
            return {_:4, a:xs};
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1, a:toHS(arr[elem]), b:new T(function() {return arr2lst_json(arr,elem+1);}),c:true}
}

/* gettimeofday(2) */
function gettimeofday(tv, _tz) {
    var t = new Date().getTime();
    writeOffAddr("i32", 4, tv, 0, (t/1000)|0);
    writeOffAddr("i32", 4, tv, 1, ((t%1000)*1000)|0);
    return 0;
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

/* bn.js by Fedor Indutny, see doc/LICENSE.bn for license */
var __bn = {};
(function (module, exports) {
'use strict';

function BN(number, base, endian) {
  // May be `new BN(bn)` ?
  if (number !== null &&
      typeof number === 'object' &&
      Array.isArray(number.words)) {
    return number;
  }

  this.negative = 0;
  this.words = null;
  this.length = 0;

  if (base === 'le' || base === 'be') {
    endian = base;
    base = 10;
  }

  if (number !== null)
    this._init(number || 0, base || 10, endian || 'be');
}
if (typeof module === 'object')
  module.exports = BN;
else
  exports.BN = BN;

BN.BN = BN;
BN.wordSize = 26;

BN.max = function max(left, right) {
  if (left.cmp(right) > 0)
    return left;
  else
    return right;
};

BN.min = function min(left, right) {
  if (left.cmp(right) < 0)
    return left;
  else
    return right;
};

BN.prototype._init = function init(number, base, endian) {
  if (typeof number === 'number') {
    return this._initNumber(number, base, endian);
  } else if (typeof number === 'object') {
    return this._initArray(number, base, endian);
  }
  if (base === 'hex')
    base = 16;

  number = number.toString().replace(/\s+/g, '');
  var start = 0;
  if (number[0] === '-')
    start++;

  if (base === 16)
    this._parseHex(number, start);
  else
    this._parseBase(number, base, start);

  if (number[0] === '-')
    this.negative = 1;

  this.strip();

  if (endian !== 'le')
    return;

  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initNumber = function _initNumber(number, base, endian) {
  if (number < 0) {
    this.negative = 1;
    number = -number;
  }
  if (number < 0x4000000) {
    this.words = [ number & 0x3ffffff ];
    this.length = 1;
  } else if (number < 0x10000000000000) {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff
    ];
    this.length = 2;
  } else {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff,
      1
    ];
    this.length = 3;
  }

  if (endian !== 'le')
    return;

  // Reverse the bytes
  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initArray = function _initArray(number, base, endian) {
  if (number.length <= 0) {
    this.words = [ 0 ];
    this.length = 1;
    return this;
  }

  this.length = Math.ceil(number.length / 3);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  var off = 0;
  if (endian === 'be') {
    for (var i = number.length - 1, j = 0; i >= 0; i -= 3) {
      var w = number[i] | (number[i - 1] << 8) | (number[i - 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  } else if (endian === 'le') {
    for (var i = 0, j = 0; i < number.length; i += 3) {
      var w = number[i] | (number[i + 1] << 8) | (number[i + 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  }
  return this.strip();
};

function parseHex(str, start, end) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r <<= 4;

    // 'a' - 'f'
    if (c >= 49 && c <= 54)
      r |= c - 49 + 0xa;

    // 'A' - 'F'
    else if (c >= 17 && c <= 22)
      r |= c - 17 + 0xa;

    // '0' - '9'
    else
      r |= c & 0xf;
  }
  return r;
}

BN.prototype._parseHex = function _parseHex(number, start) {
  // Create possibly bigger array to ensure that it fits the number
  this.length = Math.ceil((number.length - start) / 6);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  // Scan 24-bit chunks and add them to the number
  var off = 0;
  for (var i = number.length - 6, j = 0; i >= start; i -= 6) {
    var w = parseHex(number, i, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
    off += 24;
    if (off >= 26) {
      off -= 26;
      j++;
    }
  }
  if (i + 6 !== start) {
    var w = parseHex(number, start, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
  }
  this.strip();
};

function parseBase(str, start, end, mul) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r *= mul;

    // 'a'
    if (c >= 49)
      r += c - 49 + 0xa;

    // 'A'
    else if (c >= 17)
      r += c - 17 + 0xa;

    // '0' - '9'
    else
      r += c;
  }
  return r;
}

BN.prototype._parseBase = function _parseBase(number, base, start) {
  // Initialize as zero
  this.words = [ 0 ];
  this.length = 1;

  // Find length of limb in base
  for (var limbLen = 0, limbPow = 1; limbPow <= 0x3ffffff; limbPow *= base)
    limbLen++;
  limbLen--;
  limbPow = (limbPow / base) | 0;

  var total = number.length - start;
  var mod = total % limbLen;
  var end = Math.min(total, total - mod) + start;

  var word = 0;
  for (var i = start; i < end; i += limbLen) {
    word = parseBase(number, i, i + limbLen, base);

    this.imuln(limbPow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }

  if (mod !== 0) {
    var pow = 1;
    var word = parseBase(number, i, number.length, base);

    for (var i = 0; i < mod; i++)
      pow *= base;
    this.imuln(pow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }
};

BN.prototype.copy = function copy(dest) {
  dest.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    dest.words[i] = this.words[i];
  dest.length = this.length;
  dest.negative = this.negative;
};

BN.prototype.clone = function clone() {
  var r = new BN(null);
  this.copy(r);
  return r;
};

// Remove leading `0` from `this`
BN.prototype.strip = function strip() {
  while (this.length > 1 && this.words[this.length - 1] === 0)
    this.length--;
  return this._normSign();
};

BN.prototype._normSign = function _normSign() {
  // -0 = 0
  if (this.length === 1 && this.words[0] === 0)
    this.negative = 0;
  return this;
};

var zeros = [
  '',
  '0',
  '00',
  '000',
  '0000',
  '00000',
  '000000',
  '0000000',
  '00000000',
  '000000000',
  '0000000000',
  '00000000000',
  '000000000000',
  '0000000000000',
  '00000000000000',
  '000000000000000',
  '0000000000000000',
  '00000000000000000',
  '000000000000000000',
  '0000000000000000000',
  '00000000000000000000',
  '000000000000000000000',
  '0000000000000000000000',
  '00000000000000000000000',
  '000000000000000000000000',
  '0000000000000000000000000'
];

var groupSizes = [
  0, 0,
  25, 16, 12, 11, 10, 9, 8,
  8, 7, 7, 7, 7, 6, 6,
  6, 6, 6, 6, 6, 5, 5,
  5, 5, 5, 5, 5, 5, 5,
  5, 5, 5, 5, 5, 5, 5
];

var groupBases = [
  0, 0,
  33554432, 43046721, 16777216, 48828125, 60466176, 40353607, 16777216,
  43046721, 10000000, 19487171, 35831808, 62748517, 7529536, 11390625,
  16777216, 24137569, 34012224, 47045881, 64000000, 4084101, 5153632,
  6436343, 7962624, 9765625, 11881376, 14348907, 17210368, 20511149,
  24300000, 28629151, 33554432, 39135393, 45435424, 52521875, 60466176
];

BN.prototype.toString = function toString(base, padding) {
  base = base || 10;
  var padding = padding | 0 || 1;
  if (base === 16 || base === 'hex') {
    var out = '';
    var off = 0;
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var w = this.words[i];
      var word = (((w << off) | carry) & 0xffffff).toString(16);
      carry = (w >>> (24 - off)) & 0xffffff;
      if (carry !== 0 || i !== this.length - 1)
        out = zeros[6 - word.length] + word + out;
      else
        out = word + out;
      off += 2;
      if (off >= 26) {
        off -= 26;
        i--;
      }
    }
    if (carry !== 0)
      out = carry.toString(16) + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else if (base === (base | 0) && base >= 2 && base <= 36) {
    var groupSize = groupSizes[base];
    var groupBase = groupBases[base];
    var out = '';
    var c = this.clone();
    c.negative = 0;
    while (c.cmpn(0) !== 0) {
      var r = c.modn(groupBase).toString(base);
      c = c.idivn(groupBase);

      if (c.cmpn(0) !== 0)
        out = zeros[groupSize - r.length] + r + out;
      else
        out = r + out;
    }
    if (this.cmpn(0) === 0)
      out = '0' + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else {
    throw 'Base should be between 2 and 36';
  }
};

BN.prototype.toJSON = function toJSON() {
  return this.toString(16);
};

BN.prototype.toArray = function toArray(endian, length) {
  this.strip();
  var littleEndian = endian === 'le';
  var res = new Array(this.byteLength());
  res[0] = 0;

  var q = this.clone();
  if (!littleEndian) {
    // Assume big-endian
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[res.length - i - 1] = b;
    }
  } else {
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[i] = b;
    }
  }

  if (length) {
    while (res.length < length) {
      if (littleEndian)
        res.push(0);
      else
        res.unshift(0);
    }
  }

  return res;
};

if (Math.clz32) {
  BN.prototype._countBits = function _countBits(w) {
    return 32 - Math.clz32(w);
  };
} else {
  BN.prototype._countBits = function _countBits(w) {
    var t = w;
    var r = 0;
    if (t >= 0x1000) {
      r += 13;
      t >>>= 13;
    }
    if (t >= 0x40) {
      r += 7;
      t >>>= 7;
    }
    if (t >= 0x8) {
      r += 4;
      t >>>= 4;
    }
    if (t >= 0x02) {
      r += 2;
      t >>>= 2;
    }
    return r + t;
  };
}

// Return number of used bits in a BN
BN.prototype.bitLength = function bitLength() {
  var hi = 0;
  var w = this.words[this.length - 1];
  var hi = this._countBits(w);
  return (this.length - 1) * 26 + hi;
};

BN.prototype.byteLength = function byteLength() {
  return Math.ceil(this.bitLength() / 8);
};

// Return negative clone of `this`
BN.prototype.neg = function neg() {
  if (this.cmpn(0) === 0)
    return this.clone();

  var r = this.clone();
  r.negative = this.negative ^ 1;
  return r;
};

BN.prototype.ineg = function ineg() {
  this.negative ^= 1;
  return this;
};

// Or `num` with `this` in-place
BN.prototype.iuor = function iuor(num) {
  while (this.length < num.length)
    this.words[this.length++] = 0;

  for (var i = 0; i < num.length; i++)
    this.words[i] = this.words[i] | num.words[i];

  return this.strip();
};

BN.prototype.ior = function ior(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuor(num);
};


// Or `num` with `this`
BN.prototype.or = function or(num) {
  if (this.length > num.length)
    return this.clone().ior(num);
  else
    return num.clone().ior(this);
};

BN.prototype.uor = function uor(num) {
  if (this.length > num.length)
    return this.clone().iuor(num);
  else
    return num.clone().iuor(this);
};


// And `num` with `this` in-place
BN.prototype.iuand = function iuand(num) {
  // b = min-length(num, this)
  var b;
  if (this.length > num.length)
    b = num;
  else
    b = this;

  for (var i = 0; i < b.length; i++)
    this.words[i] = this.words[i] & num.words[i];

  this.length = b.length;

  return this.strip();
};

BN.prototype.iand = function iand(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuand(num);
};


// And `num` with `this`
BN.prototype.and = function and(num) {
  if (this.length > num.length)
    return this.clone().iand(num);
  else
    return num.clone().iand(this);
};

BN.prototype.uand = function uand(num) {
  if (this.length > num.length)
    return this.clone().iuand(num);
  else
    return num.clone().iuand(this);
};


// Xor `num` with `this` in-place
BN.prototype.iuxor = function iuxor(num) {
  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  for (var i = 0; i < b.length; i++)
    this.words[i] = a.words[i] ^ b.words[i];

  if (this !== a)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];

  this.length = a.length;

  return this.strip();
};

BN.prototype.ixor = function ixor(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuxor(num);
};


// Xor `num` with `this`
BN.prototype.xor = function xor(num) {
  if (this.length > num.length)
    return this.clone().ixor(num);
  else
    return num.clone().ixor(this);
};

BN.prototype.uxor = function uxor(num) {
  if (this.length > num.length)
    return this.clone().iuxor(num);
  else
    return num.clone().iuxor(this);
};


// Add `num` to `this` in-place
BN.prototype.iadd = function iadd(num) {
  // negative + positive
  if (this.negative !== 0 && num.negative === 0) {
    this.negative = 0;
    var r = this.isub(num);
    this.negative ^= 1;
    return this._normSign();

  // positive + negative
  } else if (this.negative === 0 && num.negative !== 0) {
    num.negative = 0;
    var r = this.isub(num);
    num.negative = 1;
    return r._normSign();
  }

  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) + (b.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }

  this.length = a.length;
  if (carry !== 0) {
    this.words[this.length] = carry;
    this.length++;
  // Copy the rest of the words
  } else if (a !== this) {
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  }

  return this;
};

// Add `num` to `this`
BN.prototype.add = function add(num) {
  if (num.negative !== 0 && this.negative === 0) {
    num.negative = 0;
    var res = this.sub(num);
    num.negative ^= 1;
    return res;
  } else if (num.negative === 0 && this.negative !== 0) {
    this.negative = 0;
    var res = num.sub(this);
    this.negative = 1;
    return res;
  }

  if (this.length > num.length)
    return this.clone().iadd(num);
  else
    return num.clone().iadd(this);
};

// Subtract `num` from `this` in-place
BN.prototype.isub = function isub(num) {
  // this - (-num) = this + num
  if (num.negative !== 0) {
    num.negative = 0;
    var r = this.iadd(num);
    num.negative = 1;
    return r._normSign();

  // -this - num = -(this + num)
  } else if (this.negative !== 0) {
    this.negative = 0;
    this.iadd(num);
    this.negative = 1;
    return this._normSign();
  }

  // At this point both numbers are positive
  var cmp = this.cmp(num);

  // Optimization - zeroify
  if (cmp === 0) {
    this.negative = 0;
    this.length = 1;
    this.words[0] = 0;
    return this;
  }

  // a > b
  var a;
  var b;
  if (cmp > 0) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) - (b.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }

  // Copy rest of the words
  if (carry === 0 && i < a.length && a !== this)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  this.length = Math.max(this.length, i);

  if (a !== this)
    this.negative = 1;

  return this.strip();
};

// Subtract `num` from `this`
BN.prototype.sub = function sub(num) {
  return this.clone().isub(num);
};

function smallMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  var len = (self.length + num.length) | 0;
  out.length = len;
  len = (len - 1) | 0;

  // Peel one iteration (compiler can't do it, because of code complexity)
  var a = self.words[0] | 0;
  var b = num.words[0] | 0;
  var r = a * b;

  var lo = r & 0x3ffffff;
  var carry = (r / 0x4000000) | 0;
  out.words[0] = lo;

  for (var k = 1; k < len; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = carry >>> 26;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = (k - j) | 0;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;
    }
    out.words[k] = rword | 0;
    carry = ncarry | 0;
  }
  if (carry !== 0) {
    out.words[k] = carry | 0;
  } else {
    out.length--;
  }

  return out.strip();
}

function bigMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  out.length = self.length + num.length;

  var carry = 0;
  var hncarry = 0;
  for (var k = 0; k < out.length - 1; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = hncarry;
    hncarry = 0;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;

      hncarry += ncarry >>> 26;
      ncarry &= 0x3ffffff;
    }
    out.words[k] = rword;
    carry = ncarry;
    ncarry = hncarry;
  }
  if (carry !== 0) {
    out.words[k] = carry;
  } else {
    out.length--;
  }

  return out.strip();
}

BN.prototype.mulTo = function mulTo(num, out) {
  var res;
  if (this.length + num.length < 63)
    res = smallMulTo(this, num, out);
  else
    res = bigMulTo(this, num, out);
  return res;
};

// Multiply `this` by `num`
BN.prototype.mul = function mul(num) {
  var out = new BN(null);
  out.words = new Array(this.length + num.length);
  return this.mulTo(num, out);
};

// In-place Multiplication
BN.prototype.imul = function imul(num) {
  if (this.cmpn(0) === 0 || num.cmpn(0) === 0) {
    this.words[0] = 0;
    this.length = 1;
    return this;
  }

  var tlen = this.length;
  var nlen = num.length;

  this.negative = num.negative ^ this.negative;
  this.length = this.length + num.length;
  this.words[this.length - 1] = 0;

  for (var k = this.length - 2; k >= 0; k--) {
    // Sum all words with the same `i + j = k` and accumulate `carry`,
    // note that carry could be >= 0x3ffffff
    var carry = 0;
    var rword = 0;
    var maxJ = Math.min(k, nlen - 1);
    for (var j = Math.max(0, k - tlen + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = this.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      carry += (r / 0x4000000) | 0;
      lo += rword;
      rword = lo & 0x3ffffff;
      carry += lo >>> 26;
    }
    this.words[k] = rword;
    this.words[k + 1] += carry;
    carry = 0;
  }

  // Propagate overflows
  var carry = 0;
  for (var i = 1; i < this.length; i++) {
    var w = (this.words[i] | 0) + carry;
    this.words[i] = w & 0x3ffffff;
    carry = w >>> 26;
  }

  return this.strip();
};

BN.prototype.imuln = function imuln(num) {
  // Carry
  var carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = (this.words[i] | 0) * num;
    var lo = (w & 0x3ffffff) + (carry & 0x3ffffff);
    carry >>= 26;
    carry += (w / 0x4000000) | 0;
    // NOTE: lo is 27bit maximum
    carry += lo >>> 26;
    this.words[i] = lo & 0x3ffffff;
  }

  if (carry !== 0) {
    this.words[i] = carry;
    this.length++;
  }

  return this;
};

BN.prototype.muln = function muln(num) {
  return this.clone().imuln(num);
};

// `this` * `this`
BN.prototype.sqr = function sqr() {
  return this.mul(this);
};

// `this` * `this` in-place
BN.prototype.isqr = function isqr() {
  return this.mul(this);
};

// Shift-left in-place
BN.prototype.iushln = function iushln(bits) {
  var r = bits % 26;
  var s = (bits - r) / 26;
  var carryMask = (0x3ffffff >>> (26 - r)) << (26 - r);

  if (r !== 0) {
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var newCarry = this.words[i] & carryMask;
      var c = ((this.words[i] | 0) - newCarry) << r;
      this.words[i] = c | carry;
      carry = newCarry >>> (26 - r);
    }
    if (carry) {
      this.words[i] = carry;
      this.length++;
    }
  }

  if (s !== 0) {
    for (var i = this.length - 1; i >= 0; i--)
      this.words[i + s] = this.words[i];
    for (var i = 0; i < s; i++)
      this.words[i] = 0;
    this.length += s;
  }

  return this.strip();
};

BN.prototype.ishln = function ishln(bits) {
  return this.iushln(bits);
};

// Shift-right in-place
BN.prototype.iushrn = function iushrn(bits, hint, extended) {
  var h;
  if (hint)
    h = (hint - (hint % 26)) / 26;
  else
    h = 0;

  var r = bits % 26;
  var s = Math.min((bits - r) / 26, this.length);
  var mask = 0x3ffffff ^ ((0x3ffffff >>> r) << r);
  var maskedWords = extended;

  h -= s;
  h = Math.max(0, h);

  // Extended mode, copy masked part
  if (maskedWords) {
    for (var i = 0; i < s; i++)
      maskedWords.words[i] = this.words[i];
    maskedWords.length = s;
  }

  if (s === 0) {
    // No-op, we should not move anything at all
  } else if (this.length > s) {
    this.length -= s;
    for (var i = 0; i < this.length; i++)
      this.words[i] = this.words[i + s];
  } else {
    this.words[0] = 0;
    this.length = 1;
  }

  var carry = 0;
  for (var i = this.length - 1; i >= 0 && (carry !== 0 || i >= h); i--) {
    var word = this.words[i] | 0;
    this.words[i] = (carry << (26 - r)) | (word >>> r);
    carry = word & mask;
  }

  // Push carried bits as a mask
  if (maskedWords && carry !== 0)
    maskedWords.words[maskedWords.length++] = carry;

  if (this.length === 0) {
    this.words[0] = 0;
    this.length = 1;
  }

  this.strip();

  return this;
};

BN.prototype.ishrn = function ishrn(bits, hint, extended) {
  return this.iushrn(bits, hint, extended);
};

// Shift-left
BN.prototype.shln = function shln(bits) {
  var x = this.clone();
  var neg = x.negative;
  x.negative = false;
  x.ishln(bits);
  x.negative = neg;
  return x;
};

BN.prototype.ushln = function ushln(bits) {
  return this.clone().iushln(bits);
};

// Shift-right
BN.prototype.shrn = function shrn(bits) {
  var x = this.clone();
  if(x.negative) {
      x.negative = false;
      x.ishrn(bits);
      x.negative = true;
      return x.isubn(1);
  } else {
      return x.ishrn(bits);
  }
};

BN.prototype.ushrn = function ushrn(bits) {
  return this.clone().iushrn(bits);
};

// Test if n bit is set
BN.prototype.testn = function testn(bit) {
  var r = bit % 26;
  var s = (bit - r) / 26;
  var q = 1 << r;

  // Fast case: bit is much higher than all existing words
  if (this.length <= s) {
    return false;
  }

  // Check bit and return
  var w = this.words[s];

  return !!(w & q);
};

// Add plain number `num` to `this`
BN.prototype.iaddn = function iaddn(num) {
  if (num < 0)
    return this.isubn(-num);

  // Possible sign change
  if (this.negative !== 0) {
    if (this.length === 1 && (this.words[0] | 0) < num) {
      this.words[0] = num - (this.words[0] | 0);
      this.negative = 0;
      return this;
    }

    this.negative = 0;
    this.isubn(num);
    this.negative = 1;
    return this;
  }

  // Add without checks
  return this._iaddn(num);
};

BN.prototype._iaddn = function _iaddn(num) {
  this.words[0] += num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] >= 0x4000000; i++) {
    this.words[i] -= 0x4000000;
    if (i === this.length - 1)
      this.words[i + 1] = 1;
    else
      this.words[i + 1]++;
  }
  this.length = Math.max(this.length, i + 1);

  return this;
};

// Subtract plain number `num` from `this`
BN.prototype.isubn = function isubn(num) {
  if (num < 0)
    return this.iaddn(-num);

  if (this.negative !== 0) {
    this.negative = 0;
    this.iaddn(num);
    this.negative = 1;
    return this;
  }

  this.words[0] -= num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] < 0; i++) {
    this.words[i] += 0x4000000;
    this.words[i + 1] -= 1;
  }

  return this.strip();
};

BN.prototype.addn = function addn(num) {
  return this.clone().iaddn(num);
};

BN.prototype.subn = function subn(num) {
  return this.clone().isubn(num);
};

BN.prototype.iabs = function iabs() {
  this.negative = 0;

  return this;
};

BN.prototype.abs = function abs() {
  return this.clone().iabs();
};

BN.prototype._ishlnsubmul = function _ishlnsubmul(num, mul, shift) {
  // Bigger storage is needed
  var len = num.length + shift;
  var i;
  if (this.words.length < len) {
    var t = new Array(len);
    for (var i = 0; i < this.length; i++)
      t[i] = this.words[i];
    this.words = t;
  } else {
    i = this.length;
  }

  // Zeroify rest
  this.length = Math.max(this.length, len);
  for (; i < this.length; i++)
    this.words[i] = 0;

  var carry = 0;
  for (var i = 0; i < num.length; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    var right = (num.words[i] | 0) * mul;
    w -= right & 0x3ffffff;
    carry = (w >> 26) - ((right / 0x4000000) | 0);
    this.words[i + shift] = w & 0x3ffffff;
  }
  for (; i < this.length - shift; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    carry = w >> 26;
    this.words[i + shift] = w & 0x3ffffff;
  }

  if (carry === 0)
    return this.strip();

  carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = -(this.words[i] | 0) + carry;
    carry = w >> 26;
    this.words[i] = w & 0x3ffffff;
  }
  this.negative = 1;

  return this.strip();
};

BN.prototype._wordDiv = function _wordDiv(num, mode) {
  var shift = this.length - num.length;

  var a = this.clone();
  var b = num;

  // Normalize
  var bhi = b.words[b.length - 1] | 0;
  var bhiBits = this._countBits(bhi);
  shift = 26 - bhiBits;
  if (shift !== 0) {
    b = b.ushln(shift);
    a.iushln(shift);
    bhi = b.words[b.length - 1] | 0;
  }

  // Initialize quotient
  var m = a.length - b.length;
  var q;

  if (mode !== 'mod') {
    q = new BN(null);
    q.length = m + 1;
    q.words = new Array(q.length);
    for (var i = 0; i < q.length; i++)
      q.words[i] = 0;
  }

  var diff = a.clone()._ishlnsubmul(b, 1, m);
  if (diff.negative === 0) {
    a = diff;
    if (q)
      q.words[m] = 1;
  }

  for (var j = m - 1; j >= 0; j--) {
    var qj = (a.words[b.length + j] | 0) * 0x4000000 +
             (a.words[b.length + j - 1] | 0);

    // NOTE: (qj / bhi) is (0x3ffffff * 0x4000000 + 0x3ffffff) / 0x2000000 max
    // (0x7ffffff)
    qj = Math.min((qj / bhi) | 0, 0x3ffffff);

    a._ishlnsubmul(b, qj, j);
    while (a.negative !== 0) {
      qj--;
      a.negative = 0;
      a._ishlnsubmul(b, 1, j);
      if (a.cmpn(0) !== 0)
        a.negative ^= 1;
    }
    if (q)
      q.words[j] = qj;
  }
  if (q)
    q.strip();
  a.strip();

  // Denormalize
  if (mode !== 'div' && shift !== 0)
    a.iushrn(shift);
  return { div: q ? q : null, mod: a };
};

BN.prototype.divmod = function divmod(num, mode, positive) {
  if (this.negative !== 0 && num.negative === 0) {
    var res = this.neg().divmod(num, mode);
    var div;
    var mod;
    if (mode !== 'mod')
      div = res.div.neg();
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.add(num);
    }
    return {
      div: div,
      mod: mod
    };
  } else if (this.negative === 0 && num.negative !== 0) {
    var res = this.divmod(num.neg(), mode);
    var div;
    if (mode !== 'mod')
      div = res.div.neg();
    return { div: div, mod: res.mod };
  } else if ((this.negative & num.negative) !== 0) {
    var res = this.neg().divmod(num.neg(), mode);
    var mod;
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.isub(num);
    }
    return {
      div: res.div,
      mod: mod
    };
  }

  // Both numbers are positive at this point

  // Strip both numbers to approximate shift value
  if (num.length > this.length || this.cmp(num) < 0)
    return { div: new BN(0), mod: this };

  // Very short reduction
  if (num.length === 1) {
    if (mode === 'div')
      return { div: this.divn(num.words[0]), mod: null };
    else if (mode === 'mod')
      return { div: null, mod: new BN(this.modn(num.words[0])) };
    return {
      div: this.divn(num.words[0]),
      mod: new BN(this.modn(num.words[0]))
    };
  }

  return this._wordDiv(num, mode);
};

// Find `this` / `num`
BN.prototype.div = function div(num) {
  return this.divmod(num, 'div', false).div;
};

// Find `this` % `num`
BN.prototype.mod = function mod(num) {
  return this.divmod(num, 'mod', false).mod;
};

BN.prototype.umod = function umod(num) {
  return this.divmod(num, 'mod', true).mod;
};

// Find Round(`this` / `num`)
BN.prototype.divRound = function divRound(num) {
  var dm = this.divmod(num);

  // Fast case - exact division
  if (dm.mod.cmpn(0) === 0)
    return dm.div;

  var mod = dm.div.negative !== 0 ? dm.mod.isub(num) : dm.mod;

  var half = num.ushrn(1);
  var r2 = num.andln(1);
  var cmp = mod.cmp(half);

  // Round down
  if (cmp < 0 || r2 === 1 && cmp === 0)
    return dm.div;

  // Round up
  return dm.div.negative !== 0 ? dm.div.isubn(1) : dm.div.iaddn(1);
};

BN.prototype.modn = function modn(num) {
  var p = (1 << 26) % num;

  var acc = 0;
  for (var i = this.length - 1; i >= 0; i--)
    acc = (p * acc + (this.words[i] | 0)) % num;

  return acc;
};

// In-place division by number
BN.prototype.idivn = function idivn(num) {
  var carry = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var w = (this.words[i] | 0) + carry * 0x4000000;
    this.words[i] = (w / num) | 0;
    carry = w % num;
  }

  return this.strip();
};

BN.prototype.divn = function divn(num) {
  return this.clone().idivn(num);
};

BN.prototype.isEven = function isEven() {
  return (this.words[0] & 1) === 0;
};

BN.prototype.isOdd = function isOdd() {
  return (this.words[0] & 1) === 1;
};

// And first word and num
BN.prototype.andln = function andln(num) {
  return this.words[0] & num;
};

BN.prototype.cmpn = function cmpn(num) {
  var negative = num < 0;
  if (negative)
    num = -num;

  if (this.negative !== 0 && !negative)
    return -1;
  else if (this.negative === 0 && negative)
    return 1;

  num &= 0x3ffffff;
  this.strip();

  var res;
  if (this.length > 1) {
    res = 1;
  } else {
    var w = this.words[0] | 0;
    res = w === num ? 0 : w < num ? -1 : 1;
  }
  if (this.negative !== 0)
    res = -res;
  return res;
};

// Compare two numbers and return:
// 1 - if `this` > `num`
// 0 - if `this` == `num`
// -1 - if `this` < `num`
BN.prototype.cmp = function cmp(num) {
  if (this.negative !== 0 && num.negative === 0)
    return -1;
  else if (this.negative === 0 && num.negative !== 0)
    return 1;

  var res = this.ucmp(num);
  if (this.negative !== 0)
    return -res;
  else
    return res;
};

// Unsigned comparison
BN.prototype.ucmp = function ucmp(num) {
  // At this point both numbers have the same sign
  if (this.length > num.length)
    return 1;
  else if (this.length < num.length)
    return -1;

  var res = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var a = this.words[i] | 0;
    var b = num.words[i] | 0;

    if (a === b)
      continue;
    if (a < b)
      res = -1;
    else if (a > b)
      res = 1;
    break;
  }
  return res;
};
})(undefined, __bn);

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return {_:0, a:0, b:undefined};
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return {_:0, a:1, b:val};
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;
var __stable_table;

function makeStableName(x) {
    if(x instanceof Object) {
        if(!x.stableName) {
            x.stableName = __next_stable_name;
            __next_stable_name += 1;
        }
        return {type: 'obj', name: x.stableName};
    } else {
        return {type: 'prim', name: Number(x)};
    }
}

function eqStableName(x, y) {
    return (x.type == y.type && x.name == y.name) ? 1 : 0;
}

// TODO: inefficient compared to real fromInt?
__bn.Z = new __bn.BN(0);
__bn.ONE = new __bn.BN(1);
__bn.MOD32 = new __bn.BN(0x100000000); // 2^32
var I_fromNumber = function(x) {return new __bn.BN(x);}
var I_fromInt = I_fromNumber;
var I_fromBits = function(lo,hi) {
    var x = new __bn.BN(lo >>> 0);
    var y = new __bn.BN(hi >>> 0);
    y.ishln(32);
    x.iadd(y);
    return x;
}
var I_fromString = function(s) {return new __bn.BN(s);}
var I_toInt = function(x) {return I_toNumber(x.mod(__bn.MOD32));}
var I_toWord = function(x) {return I_toInt(x) >>> 0;};
// TODO: inefficient!
var I_toNumber = function(x) {return Number(x.toString());}
var I_equals = function(a,b) {return a.cmp(b) === 0;}
var I_compare = function(a,b) {return a.cmp(b);}
var I_compareInt = function(x,i) {return x.cmp(new __bn.BN(i));}
var I_negate = function(x) {return x.neg();}
var I_add = function(a,b) {return a.add(b);}
var I_sub = function(a,b) {return a.sub(b);}
var I_mul = function(a,b) {return a.mul(b);}
var I_mod = function(a,b) {return I_rem(I_add(b, I_rem(a, b)), b);}
var I_quotRem = function(a,b) {
    var qr = a.divmod(b);
    return {_:0, a:qr.div, b:qr.mod};
}
var I_div = function(a,b) {
    if((a.cmp(__bn.Z)>=0) != (a.cmp(__bn.Z)>=0)) {
        if(a.cmp(a.rem(b), __bn.Z) !== 0) {
            return a.div(b).sub(__bn.ONE);
        }
    }
    return a.div(b);
}
var I_divMod = function(a,b) {
    return {_:0, a:I_div(self, other), b:a.mod(b)};
}
var I_quot = function(a,b) {return a.div(b);}
var I_rem = function(a,b) {return a.mod(b);}
var I_and = function(a,b) {return a.and(b);}
var I_or = function(a,b) {return a.or(b);}
var I_xor = function(a,b) {return a.xor(b);}
var I_shiftLeft = function(a,b) {return a.shln(b);}
var I_shiftRight = function(a,b) {return a.shrn(b);}
var I_signum = function(x) {return x.cmp(new __bn.BN(0));}
var I_abs = function(x) {return x.abs();}
var I_decodeDouble = function(x) {
    var dec = decodeDouble(x);
    var mantissa = I_fromBits(dec.c, dec.b);
    if(dec.a < 0) {
        mantissa = I_negate(mantissa);
    }
    return {_:0, a:dec.d, b:mantissa};
}
var I_toString = function(x) {return x.toString();}
var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    if(x.isNegative()) {
        return I_negate(I_fromInt64(x.negate()));
    } else {
        return I_fromBits(x.low, x.high);
    }
}

function I_toInt64(x) {
    if(x.negative) {
        return I_toInt64(I_negate(x)).negate();
    } else {
        return new Long(I_toInt(x), I_toInt(I_shiftRight(x,32)));
    }
}

function I_fromWord64(x) {
    return I_fromBits(x.toInt(), x.shru(32).toInt());
}

function I_toWord64(x) {
    var w = I_toInt64(x);
    w.unsigned = true;
    return w;
}

/**
 * @license long.js (c) 2013 Daniel Wirtz <dcode@dcode.io>
 * Released under the Apache License, Version 2.0
 * see: https://github.com/dcodeIO/long.js for details
 */
function Long(low, high, unsigned) {
    this.low = low | 0;
    this.high = high | 0;
    this.unsigned = !!unsigned;
}

var INT_CACHE = {};
var UINT_CACHE = {};
function cacheable(x, u) {
    return u ? 0 <= (x >>>= 0) && x < 256 : -128 <= (x |= 0) && x < 128;
}

function __fromInt(value, unsigned) {
    var obj, cachedObj, cache;
    if (unsigned) {
        if (cache = cacheable(value >>>= 0, true)) {
            cachedObj = UINT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, (value | 0) < 0 ? -1 : 0, true);
        if (cache)
            UINT_CACHE[value] = obj;
        return obj;
    } else {
        if (cache = cacheable(value |= 0, false)) {
            cachedObj = INT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, value < 0 ? -1 : 0, false);
        if (cache)
            INT_CACHE[value] = obj;
        return obj;
    }
}

function __fromNumber(value, unsigned) {
    if (isNaN(value) || !isFinite(value))
        return unsigned ? UZERO : ZERO;
    if (unsigned) {
        if (value < 0)
            return UZERO;
        if (value >= TWO_PWR_64_DBL)
            return MAX_UNSIGNED_VALUE;
    } else {
        if (value <= -TWO_PWR_63_DBL)
            return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL)
            return MAX_VALUE;
    }
    if (value < 0)
        return __fromNumber(-value, unsigned).neg();
    return new Long((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
}
var pow_dbl = Math.pow;
var TWO_PWR_16_DBL = 1 << 16;
var TWO_PWR_24_DBL = 1 << 24;
var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
var TWO_PWR_24 = __fromInt(TWO_PWR_24_DBL);
var ZERO = __fromInt(0);
Long.ZERO = ZERO;
var UZERO = __fromInt(0, true);
Long.UZERO = UZERO;
var ONE = __fromInt(1);
Long.ONE = ONE;
var UONE = __fromInt(1, true);
Long.UONE = UONE;
var NEG_ONE = __fromInt(-1);
Long.NEG_ONE = NEG_ONE;
var MAX_VALUE = new Long(0xFFFFFFFF|0, 0x7FFFFFFF|0, false);
Long.MAX_VALUE = MAX_VALUE;
var MAX_UNSIGNED_VALUE = new Long(0xFFFFFFFF|0, 0xFFFFFFFF|0, true);
Long.MAX_UNSIGNED_VALUE = MAX_UNSIGNED_VALUE;
var MIN_VALUE = new Long(0, 0x80000000|0, false);
Long.MIN_VALUE = MIN_VALUE;
var __lp = Long.prototype;
__lp.toInt = function() {return this.unsigned ? this.low >>> 0 : this.low;};
__lp.toNumber = function() {
    if (this.unsigned)
        return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
    return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
};
__lp.isZero = function() {return this.high === 0 && this.low === 0;};
__lp.isNegative = function() {return !this.unsigned && this.high < 0;};
__lp.isOdd = function() {return (this.low & 1) === 1;};
__lp.eq = function(other) {
    if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
        return false;
    return this.high === other.high && this.low === other.low;
};
__lp.neq = function(other) {return !this.eq(other);};
__lp.lt = function(other) {return this.comp(other) < 0;};
__lp.lte = function(other) {return this.comp(other) <= 0;};
__lp.gt = function(other) {return this.comp(other) > 0;};
__lp.gte = function(other) {return this.comp(other) >= 0;};
__lp.compare = function(other) {
    if (this.eq(other))
        return 0;
    var thisNeg = this.isNegative(),
        otherNeg = other.isNegative();
    if (thisNeg && !otherNeg)
        return -1;
    if (!thisNeg && otherNeg)
        return 1;
    if (!this.unsigned)
        return this.sub(other).isNegative() ? -1 : 1;
    return (other.high >>> 0) > (this.high >>> 0) || (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
};
__lp.comp = __lp.compare;
__lp.negate = function() {
    if (!this.unsigned && this.eq(MIN_VALUE))
        return MIN_VALUE;
    return this.not().add(ONE);
};
__lp.neg = __lp.negate;
__lp.add = function(addend) {
    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = addend.high >>> 16;
    var b32 = addend.high & 0xFFFF;
    var b16 = addend.low >>> 16;
    var b00 = addend.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 + b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 + b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 + b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 + b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.subtract = function(subtrahend) {return this.add(subtrahend.neg());};
__lp.sub = __lp.subtract;
__lp.multiply = function(multiplier) {
    if (this.isZero())
        return ZERO;
    if (multiplier.isZero())
        return ZERO;
    if (this.eq(MIN_VALUE))
        return multiplier.isOdd() ? MIN_VALUE : ZERO;
    if (multiplier.eq(MIN_VALUE))
        return this.isOdd() ? MIN_VALUE : ZERO;

    if (this.isNegative()) {
        if (multiplier.isNegative())
            return this.neg().mul(multiplier.neg());
        else
            return this.neg().mul(multiplier).neg();
    } else if (multiplier.isNegative())
        return this.mul(multiplier.neg()).neg();

    if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
        return __fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);

    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = multiplier.high >>> 16;
    var b32 = multiplier.high & 0xFFFF;
    var b16 = multiplier.low >>> 16;
    var b00 = multiplier.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 * b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 * b00;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c16 += a00 * b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 * b00;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a16 * b16;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a00 * b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.mul = __lp.multiply;
__lp.divide = function(divisor) {
    if (divisor.isZero())
        throw Error('division by zero');
    if (this.isZero())
        return this.unsigned ? UZERO : ZERO;
    var approx, rem, res;
    if (this.eq(MIN_VALUE)) {
        if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
            return MIN_VALUE;
        else if (divisor.eq(MIN_VALUE))
            return ONE;
        else {
            var halfThis = this.shr(1);
            approx = halfThis.div(divisor).shl(1);
            if (approx.eq(ZERO)) {
                return divisor.isNegative() ? ONE : NEG_ONE;
            } else {
                rem = this.sub(divisor.mul(approx));
                res = approx.add(rem.div(divisor));
                return res;
            }
        }
    } else if (divisor.eq(MIN_VALUE))
        return this.unsigned ? UZERO : ZERO;
    if (this.isNegative()) {
        if (divisor.isNegative())
            return this.neg().div(divisor.neg());
        return this.neg().div(divisor).neg();
    } else if (divisor.isNegative())
        return this.div(divisor.neg()).neg();

    res = ZERO;
    rem = this;
    while (rem.gte(divisor)) {
        approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
        var log2 = Math.ceil(Math.log(approx) / Math.LN2),
            delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48),
            approxRes = __fromNumber(approx),
            approxRem = approxRes.mul(divisor);
        while (approxRem.isNegative() || approxRem.gt(rem)) {
            approx -= delta;
            approxRes = __fromNumber(approx, this.unsigned);
            approxRem = approxRes.mul(divisor);
        }
        if (approxRes.isZero())
            approxRes = ONE;

        res = res.add(approxRes);
        rem = rem.sub(approxRem);
    }
    return res;
};
__lp.div = __lp.divide;
__lp.modulo = function(divisor) {return this.sub(this.div(divisor).mul(divisor));};
__lp.mod = __lp.modulo;
__lp.not = function not() {return new Long(~this.low, ~this.high, this.unsigned);};
__lp.and = function(other) {return new Long(this.low & other.low, this.high & other.high, this.unsigned);};
__lp.or = function(other) {return new Long(this.low | other.low, this.high | other.high, this.unsigned);};
__lp.xor = function(other) {return new Long(this.low ^ other.low, this.high ^ other.high, this.unsigned);};

__lp.shl = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long(this.low << numBits, (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
    else
        return new Long(0, this.low << (numBits - 32), this.unsigned);
};

__lp.shr = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
    else
        return new Long(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
};

__lp.shru = function(numBits) {
    numBits &= 63;
    if (numBits === 0)
        return this;
    else {
        var high = this.high;
        if (numBits < 32) {
            var low = this.low;
            return new Long((low >>> numBits) | (high << (32 - numBits)), high >>> numBits, this.unsigned);
        } else if (numBits === 32)
            return new Long(high, 0, this.unsigned);
        else
            return new Long(high >>> (numBits - 32), 0, this.unsigned);
    }
};

__lp.toSigned = function() {return this.unsigned ? new Long(this.low, this.high, false) : this;};
__lp.toUnsigned = function() {return this.unsigned ? this : new Long(this.low, this.high, true);};

// Int64
function hs_eqInt64(x, y) {return x.eq(y);}
function hs_neInt64(x, y) {return x.neq(y);}
function hs_ltInt64(x, y) {return x.lt(y);}
function hs_leInt64(x, y) {return x.lte(y);}
function hs_gtInt64(x, y) {return x.gt(y);}
function hs_geInt64(x, y) {return x.gte(y);}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shl(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shr(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shru(bits);}
function hs_int64ToInt(x) {return x.toInt();}
var hs_intToInt64 = __fromInt;

// Word64
function hs_wordToWord64(x) {return __fromInt(x, true);}
function hs_word64ToWord(x) {return x.toInt(x);}
function hs_mkWord64(low, high) {return new Long(low,high,true);}
function hs_and64(a,b) {return a.and(b);};
function hs_or64(a,b) {return a.or(b);};
function hs_xor64(a,b) {return a.xor(b);};
function hs_not64(x) {return x.not();}
var hs_eqWord64 = hs_eqInt64;
var hs_neWord64 = hs_neInt64;
var hs_ltWord64 = hs_ltInt64;
var hs_leWord64 = hs_leInt64;
var hs_gtWord64 = hs_gtInt64;
var hs_geWord64 = hs_geInt64;
var hs_quotWord64 = hs_quotInt64;
var hs_remWord64 = hs_remInt64;
var hs_uncheckedShiftL64 = hs_uncheckedIShiftL64;
var hs_uncheckedShiftRL64 = hs_uncheckedIShiftRL64;
function hs_int64ToWord64(x) {return x.toUnsigned();}
function hs_word64ToInt64(x) {return x.toSigned();}

// Joseph Myers' MD5 implementation, ported to work on typed arrays.
// Used under the BSD license.
function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s, n) {
    var a = s['v']['w8'];
    var orig_n = n,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=n; i+=64) {
        md5cycle(state, md5blk(a.subarray(i-64, i)));
    }
    a = a.subarray(i-64);
    n = n < (i-64) ? 0 : n-(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<n; i++)
        tail[i>>2] |= a[i] << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = orig_n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s[i]
            + (s[i+1] << 8)
            + (s[i+2] << 16)
            + (s[i+3] << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s, n) {
    return hex(md51(s, n));
}

window['md5'] = md5;

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

function __hsbase_MD5Init(ctx) {}
// Note that this is a one time "update", since that's all that's used by
// GHC.Fingerprint.
function __hsbase_MD5Update(ctx, s, n) {
    ctx.md5 = md51(s, n);
}
function __hsbase_MD5Final(out, ctx) {
    var a = out['v']['i32'];
    a[0] = ctx.md5[0];
    a[1] = ctx.md5[1];
    a[2] = ctx.md5[2];
    a[3] = ctx.md5[3];
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = new Array(n);
    for(var i = 0; i < n; ++i) {
        arr[i] = x;
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var buffer = new ArrayBuffer(n);
    var views =
        { 'i8' : new Int8Array(buffer)
        , 'i16': new Int16Array(buffer)
        , 'i32': new Int32Array(buffer)
        , 'w8' : new Uint8Array(buffer)
        , 'w16': new Uint16Array(buffer)
        , 'w32': new Uint32Array(buffer)
        , 'f32': new Float32Array(buffer)
        , 'f64': new Float64Array(buffer)
        };
    var arr =
        { 'b'  : buffer
        , 'v'  : views
        , 'off': 0
        };
    return arr;
}
window['newArr'] = newArr;
window['newByteArr'] = newByteArr;

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return {_:0, a:1, b:E(w).val};
}

function finalizeWeak(w) {
    return {_:0, a:B(A1(E(w).fin, __Z))};
}

/* For foreign import ccall "wrapper" */
function createAdjustor(args, f, a, b) {
    return function(){
        var x = f.apply(null, arguments);
        while(x instanceof F) {x = x.f();}
        return x;
    };
}

var __apply = function(f,as) {
    var arr = [];
    for(; as._ === 1; as = as.b) {
        arr.push(as.a);
    }
    arr.reverse();
    return f.apply(null, arr);
}
var __app0 = function(f) {return f();}
var __app1 = function(f,a) {return f(a);}
var __app2 = function(f,a,b) {return f(a,b);}
var __app3 = function(f,a,b,c) {return f(a,b,c);}
var __app4 = function(f,a,b,c,d) {return f(a,b,c,d);}
var __app5 = function(f,a,b,c,d,e) {return f(a,b,c,d,e);}
var __jsNull = function() {return null;}
var __eq = function(a,b) {return a===b;}
var __createJSFunc = function(arity, f){
    if(f instanceof Function && arity === f.length) {
        return (function() {
            var x = f.apply(null,arguments);
            if(x instanceof T) {
                if(x.f !== __blackhole) {
                    var ff = x.f;
                    x.f = __blackhole;
                    return x.x = ff();
                }
                return x.x;
            } else {
                while(x instanceof F) {
                    x = x.f();
                }
                return E(x);
            }
        });
    } else {
        return (function(){
            var as = Array.prototype.slice.call(arguments);
            as.push(0);
            return E(B(A(f,as)));
        });
    }
}


function __arr2lst(elem,arr) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1,
            a:arr[elem],
            b:new T(function(){return __arr2lst(elem+1,arr);})};
}

function __lst2arr(xs) {
    var arr = [];
    xs = E(xs);
    for(;xs._ === 1; xs = E(xs.b)) {
        arr.push(E(xs.a));
    }
    return arr;
}

var __new = function() {return ({});}
var __set = function(o,k,v) {o[k]=v;}
var __get = function(o,k) {return o[k];}
var __has = function(o,k) {return o[k]!==undefined;}

var _0="deltaZ",_1="deltaY",_2="deltaX",_3=function(_4,_5){var _6=E(_4);return (_6._==0)?E(_5):new T2(1,_6.a,new T(function(){return B(_3(_6.b,_5));}));},_7=function(_8,_9){var _a=jsShowI(_8);return new F(function(){return _3(fromJSStr(_a),_9);});},_b=41,_c=40,_d=function(_e,_f,_g){if(_f>=0){return new F(function(){return _7(_f,_g);});}else{if(_e<=6){return new F(function(){return _7(_f,_g);});}else{return new T2(1,_c,new T(function(){var _h=jsShowI(_f);return B(_3(fromJSStr(_h),new T2(1,_b,_g)));}));}}},_i=new T(function(){return B(unCStr(")"));}),_j=new T(function(){return B(_d(0,2,_i));}),_k=new T(function(){return B(unAppCStr(") is outside of enumeration\'s range (0,",_j));}),_l=function(_m){return new F(function(){return err(B(unAppCStr("toEnum{MouseButton}: tag (",new T(function(){return B(_d(0,_m,_k));}))));});},_n=function(_o,_){return new T(function(){var _p=Number(E(_o)),_q=jsTrunc(_p);if(_q<0){return B(_l(_q));}else{if(_q>2){return B(_l(_q));}else{return _q;}}});},_r=0,_s=new T3(0,_r,_r,_r),_t="button",_u=new T(function(){return eval("jsGetMouseCoords");}),_v=__Z,_w=function(_x,_){var _y=E(_x);if(!_y._){return _v;}else{var _z=B(_w(_y.b,_));return new T2(1,new T(function(){var _A=Number(E(_y.a));return jsTrunc(_A);}),_z);}},_B=function(_C,_){var _D=__arr2lst(0,_C);return new F(function(){return _w(_D,_);});},_E=function(_F,_){return new F(function(){return _B(E(_F),_);});},_G=function(_H,_){return new T(function(){var _I=Number(E(_H));return jsTrunc(_I);});},_J=new T2(0,_G,_E),_K=function(_L,_){var _M=E(_L);if(!_M._){return _v;}else{var _N=B(_K(_M.b,_));return new T2(1,_M.a,_N);}},_O=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_P=new T(function(){return B(unCStr("base"));}),_Q=new T(function(){return B(unCStr("IOException"));}),_R=new T(function(){var _S=hs_wordToWord64(new Long(4053623282,1685460941,true)),_T=hs_wordToWord64(new Long(3693590983,2507416641,true));return new T5(0,_S,_T,new T5(0,_S,_T,_P,_O,_Q),_v,_v);}),_U=function(_V){return E(_R);},_W=function(_X){return E(E(_X).a);},_Y=function(_Z,_10,_11){var _12=B(A1(_Z,_)),_13=B(A1(_10,_)),_14=hs_eqWord64(_12.a,_13.a);if(!_14){return __Z;}else{var _15=hs_eqWord64(_12.b,_13.b);return (!_15)?__Z:new T1(1,_11);}},_16=function(_17){var _18=E(_17);return new F(function(){return _Y(B(_W(_18.a)),_U,_18.b);});},_19=new T(function(){return B(unCStr(": "));}),_1a=new T(function(){return B(unCStr(")"));}),_1b=new T(function(){return B(unCStr(" ("));}),_1c=new T(function(){return B(unCStr("interrupted"));}),_1d=new T(function(){return B(unCStr("system error"));}),_1e=new T(function(){return B(unCStr("unsatisified constraints"));}),_1f=new T(function(){return B(unCStr("user error"));}),_1g=new T(function(){return B(unCStr("permission denied"));}),_1h=new T(function(){return B(unCStr("illegal operation"));}),_1i=new T(function(){return B(unCStr("end of file"));}),_1j=new T(function(){return B(unCStr("resource exhausted"));}),_1k=new T(function(){return B(unCStr("resource busy"));}),_1l=new T(function(){return B(unCStr("does not exist"));}),_1m=new T(function(){return B(unCStr("already exists"));}),_1n=new T(function(){return B(unCStr("resource vanished"));}),_1o=new T(function(){return B(unCStr("timeout"));}),_1p=new T(function(){return B(unCStr("unsupported operation"));}),_1q=new T(function(){return B(unCStr("hardware fault"));}),_1r=new T(function(){return B(unCStr("inappropriate type"));}),_1s=new T(function(){return B(unCStr("invalid argument"));}),_1t=new T(function(){return B(unCStr("failed"));}),_1u=new T(function(){return B(unCStr("protocol error"));}),_1v=function(_1w,_1x){switch(E(_1w)){case 0:return new F(function(){return _3(_1m,_1x);});break;case 1:return new F(function(){return _3(_1l,_1x);});break;case 2:return new F(function(){return _3(_1k,_1x);});break;case 3:return new F(function(){return _3(_1j,_1x);});break;case 4:return new F(function(){return _3(_1i,_1x);});break;case 5:return new F(function(){return _3(_1h,_1x);});break;case 6:return new F(function(){return _3(_1g,_1x);});break;case 7:return new F(function(){return _3(_1f,_1x);});break;case 8:return new F(function(){return _3(_1e,_1x);});break;case 9:return new F(function(){return _3(_1d,_1x);});break;case 10:return new F(function(){return _3(_1u,_1x);});break;case 11:return new F(function(){return _3(_1t,_1x);});break;case 12:return new F(function(){return _3(_1s,_1x);});break;case 13:return new F(function(){return _3(_1r,_1x);});break;case 14:return new F(function(){return _3(_1q,_1x);});break;case 15:return new F(function(){return _3(_1p,_1x);});break;case 16:return new F(function(){return _3(_1o,_1x);});break;case 17:return new F(function(){return _3(_1n,_1x);});break;default:return new F(function(){return _3(_1c,_1x);});}},_1y=new T(function(){return B(unCStr("}"));}),_1z=new T(function(){return B(unCStr("{handle: "));}),_1A=function(_1B,_1C,_1D,_1E,_1F,_1G){var _1H=new T(function(){var _1I=new T(function(){var _1J=new T(function(){var _1K=E(_1E);if(!_1K._){return E(_1G);}else{var _1L=new T(function(){return B(_3(_1K,new T(function(){return B(_3(_1a,_1G));},1)));},1);return B(_3(_1b,_1L));}},1);return B(_1v(_1C,_1J));}),_1M=E(_1D);if(!_1M._){return E(_1I);}else{return B(_3(_1M,new T(function(){return B(_3(_19,_1I));},1)));}}),_1N=E(_1F);if(!_1N._){var _1O=E(_1B);if(!_1O._){return E(_1H);}else{var _1P=E(_1O.a);if(!_1P._){var _1Q=new T(function(){var _1R=new T(function(){return B(_3(_1y,new T(function(){return B(_3(_19,_1H));},1)));},1);return B(_3(_1P.a,_1R));},1);return new F(function(){return _3(_1z,_1Q);});}else{var _1S=new T(function(){var _1T=new T(function(){return B(_3(_1y,new T(function(){return B(_3(_19,_1H));},1)));},1);return B(_3(_1P.a,_1T));},1);return new F(function(){return _3(_1z,_1S);});}}}else{return new F(function(){return _3(_1N.a,new T(function(){return B(_3(_19,_1H));},1));});}},_1U=function(_1V){var _1W=E(_1V);return new F(function(){return _1A(_1W.a,_1W.b,_1W.c,_1W.d,_1W.f,_v);});},_1X=function(_1Y,_1Z,_20){var _21=E(_1Z);return new F(function(){return _1A(_21.a,_21.b,_21.c,_21.d,_21.f,_20);});},_22=function(_23,_24){var _25=E(_23);return new F(function(){return _1A(_25.a,_25.b,_25.c,_25.d,_25.f,_24);});},_26=44,_27=93,_28=91,_29=function(_2a,_2b,_2c){var _2d=E(_2b);if(!_2d._){return new F(function(){return unAppCStr("[]",_2c);});}else{var _2e=new T(function(){var _2f=new T(function(){var _2g=function(_2h){var _2i=E(_2h);if(!_2i._){return E(new T2(1,_27,_2c));}else{var _2j=new T(function(){return B(A2(_2a,_2i.a,new T(function(){return B(_2g(_2i.b));})));});return new T2(1,_26,_2j);}};return B(_2g(_2d.b));});return B(A2(_2a,_2d.a,_2f));});return new T2(1,_28,_2e);}},_2k=function(_2l,_2m){return new F(function(){return _29(_22,_2l,_2m);});},_2n=new T3(0,_1X,_1U,_2k),_2o=new T(function(){return new T5(0,_U,_2n,_2p,_16,_1U);}),_2p=function(_2q){return new T2(0,_2o,_2q);},_2r=__Z,_2s=7,_2t=new T(function(){return B(unCStr("Pattern match failure in do expression at src/Haste/Prim/Any.hs:272:5-9"));}),_2u=new T6(0,_2r,_2s,_v,_2t,_2r,_2r),_2v=new T(function(){return B(_2p(_2u));}),_2w=function(_){return new F(function(){return die(_2v);});},_2x=function(_2y){return E(E(_2y).a);},_2z=function(_2A,_2B,_2C,_){var _2D=__arr2lst(0,_2C),_2E=B(_K(_2D,_)),_2F=E(_2E);if(!_2F._){return new F(function(){return _2w(_);});}else{var _2G=E(_2F.b);if(!_2G._){return new F(function(){return _2w(_);});}else{if(!E(_2G.b)._){var _2H=B(A3(_2x,_2A,_2F.a,_)),_2I=B(A3(_2x,_2B,_2G.a,_));return new T2(0,_2H,_2I);}else{return new F(function(){return _2w(_);});}}}},_2J=function(_){return new F(function(){return __jsNull();});},_2K=function(_2L){var _2M=B(A1(_2L,_));return E(_2M);},_2N=new T(function(){return B(_2K(_2J));}),_2O=new T(function(){return E(_2N);}),_2P=function(_2Q,_2R,_){if(E(_2Q)==7){var _2S=__app1(E(_u),_2R),_2T=B(_2z(_J,_J,_2S,_)),_2U=__get(_2R,E(_2)),_2V=__get(_2R,E(_1)),_2W=__get(_2R,E(_0));return new T(function(){return new T3(0,E(_2T),E(_2r),E(new T3(0,_2U,_2V,_2W)));});}else{var _2X=__app1(E(_u),_2R),_2Y=B(_2z(_J,_J,_2X,_)),_2Z=__get(_2R,E(_t)),_30=__eq(_2Z,E(_2O));if(!E(_30)){var _31=B(_n(_2Z,_));return new T(function(){return new T3(0,E(_2Y),E(new T1(1,_31)),E(_s));});}else{return new T(function(){return new T3(0,E(_2Y),E(_2r),E(_s));});}}},_32=function(_33,_34,_){return new F(function(){return _2P(_33,E(_34),_);});},_35="mouseout",_36="mouseover",_37="mousemove",_38="mouseup",_39="mousedown",_3a="dblclick",_3b="click",_3c="wheel",_3d=function(_3e){switch(E(_3e)){case 0:return E(_3b);case 1:return E(_3a);case 2:return E(_39);case 3:return E(_38);case 4:return E(_37);case 5:return E(_36);case 6:return E(_35);default:return E(_3c);}},_3f=new T2(0,_3d,_32),_3g=function(_3h,_3i){return E(_3h)==E(_3i);},_3j=function(_3k,_3l){return E(_3k)!=E(_3l);},_3m=new T2(0,_3g,_3j),_3n="screenY",_3o="screenX",_3p="clientY",_3q="clientX",_3r="pageY",_3s="pageX",_3t="target",_3u="identifier",_3v=function(_3w,_){var _3x=__get(_3w,E(_3u)),_3y=__get(_3w,E(_3t)),_3z=__get(_3w,E(_3s)),_3A=__get(_3w,E(_3r)),_3B=__get(_3w,E(_3q)),_3C=__get(_3w,E(_3p)),_3D=__get(_3w,E(_3o)),_3E=__get(_3w,E(_3n));return new T(function(){var _3F=Number(_3x),_3G=jsTrunc(_3F);return new T5(0,_3G,_3y,E(new T2(0,new T(function(){var _3H=Number(_3z);return jsTrunc(_3H);}),new T(function(){var _3I=Number(_3A);return jsTrunc(_3I);}))),E(new T2(0,new T(function(){var _3J=Number(_3B);return jsTrunc(_3J);}),new T(function(){var _3K=Number(_3C);return jsTrunc(_3K);}))),E(new T2(0,new T(function(){var _3L=Number(_3D);return jsTrunc(_3L);}),new T(function(){var _3M=Number(_3E);return jsTrunc(_3M);}))));});},_3N=function(_3O,_){var _3P=E(_3O);if(!_3P._){return _v;}else{var _3Q=B(_3v(E(_3P.a),_)),_3R=B(_3N(_3P.b,_));return new T2(1,_3Q,_3R);}},_3S="touches",_3T=function(_3U){return E(E(_3U).b);},_3V=function(_3W,_3X,_){var _3Y=__arr2lst(0,_3X),_3Z=new T(function(){return B(_3T(_3W));}),_40=function(_41,_){var _42=E(_41);if(!_42._){return _v;}else{var _43=B(A2(_3Z,_42.a,_)),_44=B(_40(_42.b,_));return new T2(1,_43,_44);}};return new F(function(){return _40(_3Y,_);});},_45=function(_46,_){return new F(function(){return _3V(_J,E(_46),_);});},_47=new T2(0,_E,_45),_48=new T(function(){return eval("(function(e) {  var len = e.changedTouches.length;  var chts = new Array(len);  for(var i = 0; i < len; ++i) {chts[i] = e.changedTouches[i].identifier;}  var len = e.targetTouches.length;  var tts = new Array(len);  for(var i = 0; i < len; ++i) {tts[i] = e.targetTouches[i].identifier;}  return [chts, tts];})");}),_49=function(_4a){return E(E(_4a).a);},_4b=function(_4c,_4d,_4e){while(1){var _4f=E(_4e);if(!_4f._){return false;}else{if(!B(A3(_49,_4c,_4d,_4f.a))){_4e=_4f.b;continue;}else{return true;}}}},_4g=function(_4h,_4i){while(1){var _4j=B((function(_4k,_4l){var _4m=E(_4l);if(!_4m._){return __Z;}else{var _4n=_4m.a,_4o=_4m.b;if(!B(A1(_4k,_4n))){var _4p=_4k;_4h=_4p;_4i=_4o;return __continue;}else{return new T2(1,_4n,new T(function(){return B(_4g(_4k,_4o));}));}}})(_4h,_4i));if(_4j!=__continue){return _4j;}}},_4q=function(_4r,_){var _4s=__get(_4r,E(_3S)),_4t=__arr2lst(0,_4s),_4u=B(_3N(_4t,_)),_4v=__app1(E(_48),_4r),_4w=B(_2z(_47,_47,_4v,_)),_4x=E(_4w),_4y=new T(function(){var _4z=function(_4A){return new F(function(){return _4b(_3m,new T(function(){return E(_4A).a;}),_4x.a);});};return B(_4g(_4z,_4u));}),_4B=new T(function(){var _4C=function(_4D){return new F(function(){return _4b(_3m,new T(function(){return E(_4D).a;}),_4x.b);});};return B(_4g(_4C,_4u));});return new T3(0,_4u,_4B,_4y);},_4E=function(_4F,_4G,_){return new F(function(){return _4q(E(_4G),_);});},_4H="touchcancel",_4I="touchend",_4J="touchmove",_4K="touchstart",_4L=function(_4M){switch(E(_4M)){case 0:return E(_4K);case 1:return E(_4J);case 2:return E(_4I);default:return E(_4H);}},_4N=new T2(0,_4L,_4E),_4O=new T(function(){return eval("(function(e){return e.getContext(\'2d\');})");}),_4P=new T(function(){return eval("(function(e){return !!e.getContext;})");}),_4Q=function(_4R,_){var _4S=__app1(E(_4P),_4R);if(!_4S){return _2r;}else{var _4T=__app1(E(_4O),_4R);return new T1(1,new T2(0,_4T,_4R));}},_4U=function(_4V,_){return new F(function(){return _4Q(E(_4V),_);});},_4W=function(_4X){return E(_4X).b;},_4Y=new T2(0,_4W,_4U),_4Z=function(_50,_51,_){var _52=B(A1(_50,_)),_53=B(A1(_51,_));return _52;},_54=function(_55,_56,_){var _57=B(A1(_55,_)),_58=B(A1(_56,_));return new T(function(){return B(A1(_57,_58));});},_59=function(_5a,_5b,_){var _5c=B(A1(_5b,_));return _5a;},_5d=function(_5e,_5f,_){var _5g=B(A1(_5f,_));return new T(function(){return B(A1(_5e,_5g));});},_5h=new T2(0,_5d,_59),_5i=function(_5j,_){return _5j;},_5k=function(_5l,_5m,_){var _5n=B(A1(_5l,_));return new F(function(){return A1(_5m,_);});},_5o=new T5(0,_5h,_5i,_54,_5k,_4Z),_5p=new T(function(){return E(_2o);}),_5q=function(_5r){return E(E(_5r).c);},_5s=function(_5t){return new T6(0,_2r,_2s,_v,_5t,_2r,_2r);},_5u=function(_5v,_){var _5w=new T(function(){return B(A2(_5q,_5p,new T(function(){return B(A1(_5s,_5v));})));});return new F(function(){return die(_5w);});},_5x=function(_5y,_){return new F(function(){return _5u(_5y,_);});},_5z=function(_5A){return new F(function(){return A1(_5x,_5A);});},_5B=function(_5C,_5D,_){var _5E=B(A1(_5C,_));return new F(function(){return A2(_5D,_5E,_);});},_5F=new T5(0,_5o,_5B,_5k,_5i,_5z),_5G=function(_5H){return E(_5H);},_5I=new T2(0,_5F,_5G),_5J=new T2(0,_5I,_5i),_5K=0,_5L=function(_){return _5K;},_5M=new T(function(){return eval("(function(ctx){ctx.beginPath();})");}),_5N=new T(function(){return eval("(function(ctx){ctx.stroke();})");}),_5O=function(_5P,_5Q,_){var _5R=__app1(E(_5M),_5Q),_5S=B(A2(_5P,_5Q,_)),_5T=__app1(E(_5N),_5Q);return new F(function(){return _5L(_);});},_5U=new T(function(){return eval("(function(ctx){ctx.restore();})");}),_5V=new T(function(){return eval("(function(ctx){ctx.save();})");}),_5W=new T(function(){return eval("(function(ctx,x,y){ctx.translate(x,y);})");}),_5X=function(_5Y,_5Z,_60,_61,_){var _62=__app1(E(_5V),_61),_63=__app3(E(_5W),_61,E(_5Y),E(_5Z)),_64=B(A2(_60,_61,_)),_65=__app1(E(_5U),_61);return new F(function(){return _5L(_);});},_66=function(_67,_68){var _69=E(_68);if(!_69._){return __Z;}else{var _6a=new T(function(){var _6b=function(_6c,_6d){var _6e=E(_6d);if(!_6e._){return new T2(0,new T2(1,_6c,_v),_v);}else{var _6f=_6e.a;if(!B(A2(_67,_6c,_6f))){return new T2(0,new T2(1,_6c,_v),_6e);}else{var _6g=new T(function(){var _6h=B(_6b(_6f,_6e.b));return new T2(0,_6h.a,_6h.b);});return new T2(0,new T2(1,_6c,new T(function(){return E(E(_6g).a);})),new T(function(){return E(E(_6g).b);}));}}},_6i=B(_6b(_69.a,_69.b));return new T2(0,_6i.a,_6i.b);});return new T2(1,new T(function(){return E(E(_6a).a);}),new T(function(){return B(_66(_67,E(_6a).b));}));}},_6j=100,_6k=60,_6l=new T(function(){return eval("(function(ctx, x, y, radius, fromAngle, toAngle){ctx.arc(x, y, radius, fromAngle, toAngle);})");}),_6m=0,_6n=6.283185307179586,_6o=new T(function(){return eval("(function(ctx,x,y){ctx.moveTo(x,y);})");}),_6p=function(_6q,_6r,_6s,_6t,_){var _6u=__app3(E(_6o),_6t,_6q+_6s,_6r),_6v=__apply(E(_6l),new T2(1,_6n,new T2(1,_6m,new T2(1,_6s,new T2(1,_6r,new T2(1,_6q,new T2(1,_6t,_v)))))));return new F(function(){return _5L(_);});},_6w=new T(function(){return eval("(function(ctx){ctx.fill();})");}),_6x=function(_6y,_6z,_){var _6A=__app1(E(_5M),_6z),_6B=B(A2(_6y,_6z,_)),_6C=__app1(E(_6w),_6z);return new F(function(){return _5L(_);});},_6D=",",_6E="rgba(",_6F=new T(function(){return toJSStr(_v);}),_6G="rgb(",_6H=")",_6I=new T2(1,_6H,_v),_6J=function(_6K){var _6L=E(_6K);if(!_6L._){var _6M=jsCat(new T2(1,_6G,new T2(1,new T(function(){return String(_6L.a);}),new T2(1,_6D,new T2(1,new T(function(){return String(_6L.b);}),new T2(1,_6D,new T2(1,new T(function(){return String(_6L.c);}),_6I)))))),E(_6F));return E(_6M);}else{var _6N=jsCat(new T2(1,_6E,new T2(1,new T(function(){return String(_6L.a);}),new T2(1,_6D,new T2(1,new T(function(){return String(_6L.b);}),new T2(1,_6D,new T2(1,new T(function(){return String(_6L.c);}),new T2(1,_6D,new T2(1,new T(function(){return String(_6L.d);}),_6I)))))))),E(_6F));return E(_6N);}},_6O="strokeStyle",_6P="fillStyle",_6Q=new T(function(){return eval("(function(e,p){var x = e[p];return typeof x === \'undefined\' ? \'\' : x.toString();})");}),_6R=new T(function(){return eval("(function(e,p,v){e[p] = v;})");}),_6S=function(_6T,_6U){var _6V=new T(function(){return B(_6J(_6T));});return function(_6W,_){var _6X=E(_6W),_6Y=E(_6P),_6Z=E(_6Q),_70=__app2(_6Z,_6X,_6Y),_71=E(_6O),_72=__app2(_6Z,_6X,_71),_73=E(_6V),_74=E(_6R),_75=__app3(_74,_6X,_6Y,_73),_76=__app3(_74,_6X,_71,_73),_77=B(A2(_6U,_6X,_)),_78=String(_70),_79=__app3(_74,_6X,_6Y,_78),_7a=String(_72),_7b=__app3(_74,_6X,_71,_7a);return new F(function(){return _5L(_);});};},_7c=function(_7d,_7e,_7f,_){var _7g=new T(function(){return 40*E(_7e)+0.5+19.5;}),_7h=function(_7i,_){return new F(function(){return _6p(E(_7g),19.5,19.5,E(_7i),_);});};return new F(function(){return A(_6S,[_7d,function(_7j,_){return new F(function(){return _6x(_7h,E(_7j),_);});},_7f,_]);});},_7k=new T3(0,0,0,0),_7l=function(_7m,_7n,_){var _7o=E(_7m);if(!_7o._){return _v;}else{var _7p=B(_7c(_7k,_7o.a,_7n,_)),_7q=B(_7l(_7o.b,_7n,_));return new T2(1,_7p,_7q);}},_7r=function(_7s){return new F(function(){return _d(0,E(_7s),_v);});},_7t=function(_7u,_7v){while(1){var _7w=E(_7u);if(!_7w._){return E(_7v);}else{var _7x=_7v+1|0;_7u=_7w.b;_7v=_7x;continue;}}},_7y=new T(function(){return eval("(function(ctx,s,x,y){ctx.fillText(s,x,y);})");}),_7z=function(_7A,_7B,_7C){var _7D=new T(function(){return toJSStr(E(_7C));});return function(_7E,_){var _7F=__app4(E(_7y),E(_7E),E(_7D),E(_7A),E(_7B));return new F(function(){return _5L(_);});};},_7G=new T(function(){return B(unCStr(": empty list"));}),_7H=new T(function(){return B(unCStr("Prelude."));}),_7I=function(_7J){return new F(function(){return err(B(_3(_7H,new T(function(){return B(_3(_7J,_7G));},1))));});},_7K=new T(function(){return B(unCStr("head"));}),_7L=new T(function(){return B(_7I(_7K));}),_7M=new T3(0,200,0,0),_7N="font",_7O=function(_7P,_7Q){var _7R=new T(function(){return toJSStr(E(_7P));});return function(_7S,_){var _7T=E(_7S),_7U=E(_7N),_7V=__app2(E(_6Q),_7T,_7U),_7W=E(_6R),_7X=__app3(_7W,_7T,_7U,E(_7R)),_7Y=B(A2(_7Q,_7T,_)),_7Z=String(_7V),_80=__app3(_7W,_7T,_7U,_7Z);return new F(function(){return _5L(_);});};},_81=-3.8999999999999986,_82=new T(function(){return B(unCStr("px Bitstream Vera"));}),_83=function(_84,_85,_){var _86=E(_84);if(!_86._){return _v;}else{var _87=_86.a,_88=new T(function(){var _89=new T(function(){return B(_7t(_87,0));}),_8a=new T(function(){return B(_7z(new T(function(){var _8b=E(_87);if(!_8b._){return E(_7L);}else{var _8c=E(_8b.a);return (40*_8c+0.5+19.5+40*((_8c+E(_89)|0)-1|0)+0.5+19.5)/2;}}),_81,new T(function(){return B(_7r(_89));},1)));});return B(_7O(new T(function(){return B(_3(B(_d(0,40+E(_89)|0,_v)),_82));},1),_8a));}),_8d=B(A(_6S,[_7M,_88,_85,_])),_8e=B(_83(_86.b,_85,_));return new T2(1,_8d,_8e);}},_8f=new T(function(){return eval("(function(e){e.width = e.width;})");}),_8g=new T(function(){return eval("(function(ctx,x,y){ctx.lineTo(x,y);})");}),_8h=function(_8i,_8j,_){var _8k=E(_8i);if(!_8k._){return _5K;}else{var _8l=E(_8k.a),_8m=E(_8j),_8n=__app3(E(_6o),_8m,E(_8l.a),E(_8l.b)),_8o=E(_8k.b);if(!_8o._){return _5K;}else{var _8p=E(_8o.a),_8q=E(_8g),_8r=__app3(_8q,_8m,E(_8p.a),E(_8p.b)),_8s=function(_8t,_){while(1){var _8u=E(_8t);if(!_8u._){return _5K;}else{var _8v=E(_8u.a),_8w=__app3(_8q,_8m,E(_8v.a),E(_8v.b));_8t=_8u.b;continue;}}};return new F(function(){return _8s(_8o.b,_);});}}},_8x=0,_8y=19.5,_8z=new T2(0,_8x,_8y),_8A=function(_8B,_8C){return (E(_8B)+1|0)==E(_8C);},_8D=function(_8E,_8F,_8G,_){var _8H=rMV(_8F),_8I=_8H,_8J=rMV(E(_8G)),_8K=E(_8E),_8L=__app1(E(_8f),_8K.b),_8M=new T(function(){var _8N=E(_8J),_8O=_8N.b,_8P=function(_8Q,_){var _8R=new T(function(){return E(_8N.a)*40;}),_8S=function(_8T,_){return new F(function(){return _8h(new T2(1,_8z,new T2(1,new T2(0,_8R,_8y),_v)),_8T,_);});},_8U=B(A(_6S,[_7k,function(_8V,_){return new F(function(){return _5O(_8S,E(_8V),_);});},_8Q,_])),_8W=B(_7l(_8O,_8Q,_)),_8X=E(_8I);if(!_8X._){var _8Y=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}else{var _8Z=E(E(_8X.a).a)/40,_90=_8Z&4294967295;if(_8Z>=_90){if(0>_90){var _91=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}else{if(_90>=20){var _92=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}else{if(!B(_4b(_3m,_90,_8O))){var _93=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}else{var _94=B(_7c(_7M,_90,_8Q,_)),_95=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}}}}else{var _96=_90-1|0;if(0>_96){var _97=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}else{if(_96>=20){var _98=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}else{if(!B(_4b(_3m,_96,_8O))){var _99=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}else{var _9a=B(_7c(_7M,_96,_8Q,_)),_9b=B(_83(B(_66(_8A,_8O)),_8Q,_));return _5K;}}}}}};return E(_8P);},1);return new F(function(){return _5X(_6k,_6j,_8M,_8K.a,_);});},_9c=new T1(0,5),_9d=function(_9e){return E(E(_9e).a);},_9f=function(_9g){return E(E(_9g).a);},_9h=function(_9i){return E(E(_9i).b);},_9j=new T(function(){return eval("(function(t,f){window.setInterval(f,t);})");}),_9k=new T(function(){return eval("(function(t,f){window.setTimeout(f,t);})");}),_9l=function(_9m){return E(E(_9m).b);},_9n=function(_9o){return E(E(_9o).b);},_9p=function(_9q,_9r,_9s){var _9t=B(_9d(_9q)),_9u=new T(function(){return B(_9l(_9t));}),_9v=function(_9w){var _9x=function(_){var _9y=E(_9r);if(!_9y._){var _9z=B(A1(_9w,_5K)),_9A=__createJSFunc(0,function(_){var _9B=B(A1(_9z,_));return _2O;}),_9C=__app2(E(_9k),_9y.a,_9A);return new T(function(){var _9D=Number(_9C),_9E=jsTrunc(_9D);return new T2(0,_9E,E(_9y));});}else{var _9F=B(A1(_9w,_5K)),_9G=__createJSFunc(0,function(_){var _9H=B(A1(_9F,_));return _2O;}),_9I=__app2(E(_9j),_9y.a,_9G);return new T(function(){var _9J=Number(_9I),_9K=jsTrunc(_9J);return new T2(0,_9K,E(_9y));});}};return new F(function(){return A1(_9u,_9x);});},_9L=new T(function(){return B(A2(_9n,_9q,function(_9M){return E(_9s);}));});return new F(function(){return A3(_9h,B(_9f(_9t)),_9L,_9v);});},_9N=function(_9O,_9P,_9Q,_){var _9R=B(_8D(_9O,_9P,_9Q,_)),_9S=B(A(_9p,[_5J,_9c,function(_){return new F(function(){return _9N(_9O,_9P,_9Q,_);});},_]));return _5K;},_9T=function(_9U,_9V){var _9W=E(_9V);if(!_9W._){return new T2(0,_v,_v);}else{var _9X=_9W.a;if(!B(A1(_9U,_9X))){var _9Y=new T(function(){var _9Z=B(_9T(_9U,_9W.b));return new T2(0,_9Z.a,_9Z.b);});return new T2(0,new T2(1,_9X,new T(function(){return E(E(_9Y).a);})),new T(function(){return E(E(_9Y).b);}));}else{return new T2(0,_v,_9W);}}},_a0=0,_a1=function(_a2,_a3){return E(_a2)>E(_a3);},_a4=function(_a5,_a6){var _a7=B(_9T(function(_a8){return new F(function(){return _a1(_a8,_a5);});},_a6)),_a9=_a7.b,_aa=B(_7t(_a7.a,0));if(!_aa){return new T2(0,_a0,_a9);}else{var _ab=new T(function(){var _ac=B(_a4(new T(function(){return E(_a5)+_aa|0;}),_a9));return new T2(0,_ac.a,_ac.b);});return new T2(0,new T(function(){return E(E(_ab).a)+_aa|0;}),new T(function(){return E(E(_ab).b);}));}},_ad=function(_ae,_af){var _ag=function(_ah,_ai){while(1){var _aj=B((function(_ak,_al){var _am=E(_ak);if(!_am._){return __Z;}else{var _an=_am.b;if(!B(A1(_ae,_am.a))){var _ao=_al+1|0;_ah=_an;_ai=_ao;return __continue;}else{return new T2(1,_al,new T(function(){return B(_ag(_an,_al+1|0));}));}}})(_ah,_ai));if(_aj!=__continue){return _aj;}}},_ap=B(_ag(_af,0));return (_ap._==0)?__Z:new T1(1,_ap.a);},_aq=function(_ar,_as,_at,_au){var _av=B(_ad(function(_aw){return new F(function(){return _3g(_as,_aw);});},_au));if(!_av._){return __Z;}else{var _ax=E(_at),_ay=E(_ar)-(B(_7t(_au,0))-E(_av.a)|0)|0,_az=function(_aA,_aB){var _aC=E(_as);if(_aA==_aC){return __Z;}else{var _aD=new T(function(){var _aE=B(_9T(function(_aF){return E(_aF)>=_aC;},_au)),_aG=new T(function(){var _aH=B(_a4(_aB,_aE.b)),_aI=_aH.b,_aJ=E(_aH.a);if(0>=_aJ){return E(_aI);}else{var _aK=function(_aL,_aM){var _aN=E(_aM);return (_aN==1)?new T2(1,_aL,_aI):new T2(1,_aL,new T(function(){var _aO=E(_aL);if(_aO==2147483647){return E(_aI);}else{return B(_aK(_aO+1|0,_aN-1|0));}}));};return B(_aK(_aA,_aJ));}},1);return B(_3(_aE.a,_aG));});return new T1(1,_aD);}};if(_ax>_ay){return new F(function(){return _az(_ay,_ay);});}else{return new F(function(){return _az(_ax,_ax);});}}},_aP=function(_aQ,_aR){var _aS=E(_aR);return (_aS._==0)?__Z:new T2(1,new T(function(){return B(A1(_aQ,_aS.a));}),new T(function(){return B(_aP(_aQ,_aS.b));}));},_aT=function(_aU,_aV){while(1){var _aW=E(_aU);if(!_aW._){return E(_aV);}else{var _aX=new T2(1,_aW.a,_aV);_aU=_aW.b;_aV=_aX;continue;}}},_aY=function(_aZ,_b0){return new F(function(){return _aT(B(_aP(function(_b1){return (E(_aZ)-E(_b1)|0)-1|0;},_b0)),_v);});},_b2=function(_b3,_b4,_b5,_b6){if(_b5>=_b6){if(_b5<=_b6){return __Z;}else{var _b7=B(_aq(_b3,new T(function(){return (E(_b3)-_b5|0)-1|0;}),new T(function(){return (E(_b3)-_b6|0)-1|0;},1),B(_aY(_b3,_b4))));return (_b7._==0)?__Z:new T1(1,new T2(0,_b3,new T(function(){return B(_aY(_b3,_b7.a));})));}}else{var _b8=B(_aq(_b3,_b5,_b6,_b4));return (_b8._==0)?__Z:new T1(1,new T2(0,_b3,_b8.a));}},_b9=2,_ba=4,_bb=6,_bc=3,_bd=2,_be=1,_bf=0,_bg=new T(function(){return eval("(function(id){return document.getElementById(id);})");}),_bh=function(_bi,_bj){var _bk=function(_){var _bl=__app1(E(_bg),E(_bj)),_bm=__eq(_bl,E(_2O));return (E(_bm)==0)?new T1(1,_bl):_2r;};return new F(function(){return A2(_9l,_bi,_bk);});},_bn=new T(function(){return B(unCStr("Control.Exception.Base"));}),_bo=new T(function(){return B(unCStr("base"));}),_bp=new T(function(){return B(unCStr("PatternMatchFail"));}),_bq=new T(function(){var _br=hs_wordToWord64(new Long(18445595,3739165398,true)),_bs=hs_wordToWord64(new Long(52003073,3246954884,true));return new T5(0,_br,_bs,new T5(0,_br,_bs,_bo,_bn,_bp),_v,_v);}),_bt=function(_bu){return E(_bq);},_bv=function(_bw){var _bx=E(_bw);return new F(function(){return _Y(B(_W(_bx.a)),_bt,_bx.b);});},_by=function(_bz){return E(E(_bz).a);},_bA=function(_bB){return new T2(0,_bC,_bB);},_bD=function(_bE,_bF){return new F(function(){return _3(E(_bE).a,_bF);});},_bG=function(_bH,_bI){return new F(function(){return _29(_bD,_bH,_bI);});},_bJ=function(_bK,_bL,_bM){return new F(function(){return _3(E(_bL).a,_bM);});},_bN=new T3(0,_bJ,_by,_bG),_bC=new T(function(){return new T5(0,_bt,_bN,_bA,_bv,_by);}),_bO=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_bP=function(_bQ,_bR){return new F(function(){return die(new T(function(){return B(A2(_5q,_bR,_bQ));}));});},_bS=function(_bT,_bU){return new F(function(){return _bP(_bT,_bU);});},_bV=function(_bW,_bX){var _bY=E(_bX);if(!_bY._){return new T2(0,_v,_v);}else{var _bZ=_bY.a;if(!B(A1(_bW,_bZ))){return new T2(0,_v,_bY);}else{var _c0=new T(function(){var _c1=B(_bV(_bW,_bY.b));return new T2(0,_c1.a,_c1.b);});return new T2(0,new T2(1,_bZ,new T(function(){return E(E(_c0).a);})),new T(function(){return E(E(_c0).b);}));}}},_c2=32,_c3=new T(function(){return B(unCStr("\n"));}),_c4=function(_c5){return (E(_c5)==124)?false:true;},_c6=function(_c7,_c8){var _c9=B(_bV(_c4,B(unCStr(_c7)))),_ca=_c9.a,_cb=function(_cc,_cd){var _ce=new T(function(){var _cf=new T(function(){return B(_3(_c8,new T(function(){return B(_3(_cd,_c3));},1)));});return B(unAppCStr(": ",_cf));},1);return new F(function(){return _3(_cc,_ce);});},_cg=E(_c9.b);if(!_cg._){return new F(function(){return _cb(_ca,_v);});}else{if(E(_cg.a)==124){return new F(function(){return _cb(_ca,new T2(1,_c2,_cg.b));});}else{return new F(function(){return _cb(_ca,_v);});}}},_ch=function(_ci){return new F(function(){return _bS(new T1(0,new T(function(){return B(_c6(_ci,_bO));})),_bC);});},_cj=new T(function(){return B(_ch("Main.hs:50:46-94|lambda"));}),_ck=new T(function(){return B(unCStr("Pattern match failure in do expression at Main.hs:176:3-10"));}),_cl=new T6(0,_2r,_2s,_v,_ck,_2r,_2r),_cm=new T(function(){return B(_2p(_cl));}),_cn=new T(function(){return B(unCStr("Pattern match failure in do expression at Main.hs:177:3-10"));}),_co=new T6(0,_2r,_2s,_v,_cn,_2r,_2r),_cp=new T(function(){return B(_2p(_co));}),_cq="canvas",_cr="msg",_cs=new T(function(){return B(_ch("Main.hs:50:46-94|lambda"));}),_ct=20,_cu=function(_cv,_cw){return E(_cv)+E(_cw)|0;},_cx=function(_cy,_cz){var _cA=E(_cz);if(!_cA){return __Z;}else{var _cB=new T(function(){return B(_cx(new T(function(){return E(_cy)+1|0;}),_cA-1|0));});return new T2(1,_cy,_cB);}},_cC=function(_cD,_cE,_cF){while(1){var _cG=B((function(_cH,_cI,_cJ){var _cK=E(_cH);if(!_cK._){return new T2(0,_cI,_cJ);}else{var _cL=_cK.a,_cM=new T(function(){return E(E(_cL).b);}),_cN=new T(function(){var _cO=new T(function(){return B(_cx(new T(function(){return E(E(_cL).a);}),E(_cM)));},1);return B(_3(_cJ,_cO));});_cD=_cK.b;_cE=new T(function(){return B(_cu(_cI,_cM));});_cF=_cN;return __continue;}})(_cD,_cE,_cF));if(_cG!=__continue){return _cG;}}},_cP=0,_cQ=10,_cR=new T2(0,_cP,_cQ),_cS=new T2(1,_cR,_v),_cT=new T(function(){return E(B(_cC(_cS,_a0,_v)).b);}),_cU=new T2(0,_ct,_cT),_cV=new T(function(){return B(_ch("Main.hs:56:42-85|lambda"));}),_cW=new T(function(){return B(_ch("Main.hs:56:42-85|lambda"));}),_cX=function(_cY){return E(E(_cY).a);},_cZ=function(_d0){return E(E(_d0).b);},_d1=function(_d2){return E(E(_d2).a);},_d3=function(_){return new F(function(){return nMV(_2r);});},_d4=new T(function(){return B(_2K(_d3));}),_d5=new T(function(){return eval("(function(e,name,f){e.addEventListener(name,f,false);return [f];})");}),_d6=function(_d7){return E(E(_d7).d);},_d8=function(_d9,_da,_db,_dc,_dd,_de){var _df=B(_9d(_d9)),_dg=B(_9f(_df)),_dh=new T(function(){return B(_9l(_df));}),_di=new T(function(){return B(_d6(_dg));}),_dj=new T(function(){return B(A2(_cX,_da,_dc));}),_dk=new T(function(){return B(A2(_d1,_db,_dd));}),_dl=function(_dm){return new F(function(){return A1(_di,new T3(0,_dk,_dj,_dm));});},_dn=function(_do){var _dp=new T(function(){var _dq=new T(function(){var _dr=__createJSFunc(2,function(_ds,_){var _dt=B(A2(E(_do),_ds,_));return _2O;}),_du=_dr;return function(_){return new F(function(){return __app3(E(_d5),E(_dj),E(_dk),_du);});};});return B(A1(_dh,_dq));});return new F(function(){return A3(_9h,_dg,_dp,_dl);});},_dv=new T(function(){var _dw=new T(function(){return B(_9l(_df));}),_dx=function(_dy){var _dz=new T(function(){return B(A1(_dw,function(_){var _=wMV(E(_d4),new T1(1,_dy));return new F(function(){return A(_cZ,[_db,_dd,_dy,_]);});}));});return new F(function(){return A3(_9h,_dg,_dz,_de);});};return B(A2(_9n,_d9,_dx));});return new F(function(){return A3(_9h,_dg,_dv,_dn);});},_dA=function(_){var _dB=nMV(_cU),_dC=_dB,_dD=nMV(_2r),_dE=_dD,_dF=nMV(_v),_dG=B(A3(_bh,_5I,_cr,_));if(!E(_dG)._){return new F(function(){return die(_cm);});}else{var _dH=B(A3(_bh,_5I,_cq,_)),_dI=E(_dH);if(!_dI._){return new F(function(){return die(_cp);});}else{var _dJ=E(_dI.a),_dK=__app1(E(_4P),_dJ);if(!_dK){return new F(function(){return die(_cp);});}else{var _dL=__app1(E(_4O),_dJ),_dM=new T2(0,_dL,_dJ),_dN=function(_dO,_){var _dP=E(_dO),_dQ=E(_dP.a),_dR=E(_dP.b);if(!_dR._){return E(_cj);}else{switch(E(_dR.a)){case 0:var _=wMV(_dE,new T1(1,new T2(0,new T(function(){return E(_dQ.a)-60;}),new T(function(){return E(_dQ.b)-100;}))));return new F(function(){return _8D(_dM,_dE,_dC,_);});break;case 1:return _5K;default:return _5K;}}},_dS=B(A(_d8,[_5J,_4Y,_3f,_dM,_b9,_dN,_])),_dT=B(A(_d8,[_5J,_4Y,_3f,_dM,_bc,function(_dU,_){var _dV=E(E(_dU).b);if(!_dV._){return E(_cs);}else{switch(E(_dV.a)){case 0:var _=wMV(_dE,_2r);return new F(function(){return _8D(_dM,_dE,_dC,_);});break;case 1:return _5K;default:return _5K;}}},_])),_dW=B(A(_d8,[_5J,_4Y,_3f,_dM,_bb,function(_dX,_){var _dY=E(_dX),_=wMV(_dE,_2r);return new F(function(){return _8D(_dM,_dE,_dC,_);});},_])),_dZ=function(_e0,_){var _e1=E(E(_e0).a),_e2=rMV(_dE),_e3=E(_e2);if(!_e3._){return _5K;}else{var _e4=E(_e1.a)-60,_e5=_e4/40,_e6=_e5&4294967295,_e7=function(_e8){if(0>_e8){return _5K;}else{if(_e8>=20){return _5K;}else{var _e9=E(E(_e3.a).a)/40,_ea=_e9&4294967295,_eb=function(_ec){if(0>_ec){return _5K;}else{if(_ec>=20){return _5K;}else{var _ed=rMV(_dC),_ee=E(_ed),_ef=B(_b2(_ee.a,_ee.b,_ec,_e8));if(!_ef._){return _5K;}else{var _=wMV(_dC,_ef.a),_=wMV(_dE,new T1(1,new T2(0,_e4,new T(function(){return E(_e1.b)-100;}))));return _5K;}}}};if(_e9>=_ea){return new F(function(){return _eb(_ea);});}else{return new F(function(){return _eb(_ea-1|0);});}}}};if(_e5>=_e6){return new F(function(){return _e7(_e6);});}else{return new F(function(){return _e7(_e6-1|0);});}}},_eg=B(A(_d8,[_5J,_4Y,_3f,_dM,_ba,_dZ,_])),_eh=function(_ei,_){var _ej=E(E(_ei).b);if(!_ej._){return E(_cV);}else{var _ek=new T(function(){return E(E(_ej.a).d);}),_=wMV(_dE,new T1(1,new T2(0,new T(function(){return E(E(_ek).a)-60;}),new T(function(){return E(E(_ek).b)-100;}))));return new F(function(){return _8D(_dM,_dE,_dC,_);});}},_el=B(A(_d8,[_5J,_4Y,_4N,_dM,_bf,_eh,_])),_em=function(_en,_){var _eo=E(E(_en).b);if(!_eo._){return E(_cV);}else{var _ep=rMV(_dE),_eq=E(_ep);if(!_eq._){return _5K;}else{var _er=E(E(_eo.a).d),_es=E(_er.a)-60,_et=_es/40,_eu=_et&4294967295,_ev=function(_ew){if(0>_ew){return _5K;}else{if(_ew>=20){return _5K;}else{var _ex=E(E(_eq.a).a)/40,_ey=_ex&4294967295,_ez=function(_eA){if(0>_eA){return _5K;}else{if(_eA>=20){return _5K;}else{var _eB=rMV(_dC),_eC=E(_eB),_eD=B(_b2(_eC.a,_eC.b,_eA,_ew));if(!_eD._){return _5K;}else{var _=wMV(_dC,_eD.a),_=wMV(_dE,new T1(1,new T2(0,_es,new T(function(){return E(_er.b)-100;}))));return _5K;}}}};if(_ex>=_ey){return new F(function(){return _ez(_ey);});}else{return new F(function(){return _ez(_ey-1|0);});}}}};if(_et>=_eu){return new F(function(){return _ev(_eu);});}else{return new F(function(){return _ev(_eu-1|0);});}}}},_eE=B(A(_d8,[_5J,_4Y,_4N,_dM,_be,_em,_])),_eF=B(A(_d8,[_5J,_4Y,_4N,_dM,_bd,function(_eG,_){if(!E(E(_eG).b)._){return E(_cW);}else{var _=wMV(_dE,_2r);return new F(function(){return _8D(_dM,_dE,_dC,_);});}},_])),_eH=B(_9N(_dM,_dE,_dC,_));return _5K;}}}},_eI=function(_){return new F(function(){return _dA(_);});};
var hasteMain = function() {B(A(_eI, [0]));};window.onload = hasteMain;
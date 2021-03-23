(declare (extended-bindings))
(##inline-host-declaration
 #<<EOF
//  

 RTS.scm2host = function (obj) {
   if (obj === void 0) {
     return obj;
   }
   if (obj === null) {
     return obj;
   }
   if (typeof obj === "boolean") {
     return obj;
   }
   if (typeof obj === "number") {
     return obj;
   }
   // this is what we add! -- drewc@gxjs
   if (obj instanceof RTS.Foreign) {
     return RTS.foreign2host(obj);
   }
   if (obj instanceof RTS.Flonum) {
     return obj.val;
   }
   if (obj instanceof RTS.ScmString) {
     return obj.toString();
   }
   if (obj instanceof Array) {
     return obj.map( RTS.scm2host );
   }
   if (obj instanceof RTS.U8Vector) {
     return obj.elems;
   }
   if (obj instanceof RTS.U16Vector) {
     return obj.elems;
   }
   if (obj instanceof RTS.U32Vector) {
     return obj.elems;
   }
   if (obj instanceof RTS.S8Vector) {
     return obj.elems;
   }
   if (obj instanceof RTS.S16Vector) {
     return obj.elems;
   }
   if (obj instanceof RTS.S32Vector) {
     return obj.elems;
   }
   if (obj instanceof RTS.F32Vector) {
     return obj.elems;
   }
   if (obj instanceof RTS.F64Vector) {
     return obj.elems;
   }
   if (obj instanceof RTS.Pair) {
     return RTS.scm2host(RTS.list2vector(obj));
     // var jsobj = {};
     // var i = 0;
     // while (obj instanceof RTS.Pair) {
     //   var elem = obj.car;
     //   if (elem instanceof RTS.Pair) {
     //     jsobj[RTS.scm2host(elem.car)] = RTS.scm2host(elem.cdr);
     //   } else {
     //     jsobj[i] = RTS.scm2host(elem);
     //   }
     //   ++i;
     //   obj = obj.cdr;
     // }
     // return jsobj;
   }
   if (obj instanceof RTS.Structure) {
     throw "scm2host error (cannot convert Structure)";
   }
   if (typeof obj === "function") {
     return RTS.procedure2host(obj);
   }
 
   throw 'scm2host error:' + JSON.stringify(obj)
   // return obj;
 };
 

 if (RTS.host_function2scm === undefined) {
   RTS.host_function2scm === RTS.function2scm
 }
 if (RTS.function2scm === undefined) {
   RTS.function2scm === RTS.host_function2scm
 }
 RTS.host2scm = function (obj) {
   if (obj === void 0) {
     return void 0;
   }
   if (obj === null) {
     return null;
   }
   if (typeof obj === "boolean") {
     return obj;
   }
   if (typeof obj === "number") {
     if ((obj | 0) === obj && obj >= -536870912 && obj <= 536870911) {
       return obj;
     } else {
       return new RTS.Flonum(obj);
     }
   }
   if (typeof obj === "function") {
     return RTS.function2scm(obj);
   }
   if (typeof obj === "string") {
     return new RTS.ScmString(RTS.str2codes(obj));
   }
   if (obj instanceof Array) {
     return obj.map( RTS.host2scm );
   }
   if (obj instanceof Uint8Array) {
     return new RTS.U8Vector(obj);
   }
   if (obj instanceof Uint16Array) {
     return new RTS.U16Vector(obj);
   }
   if (obj instanceof Uint32Array) {
     return new RTS.U32Vector(obj);
   }
   if (obj instanceof Int8Array) {
     return new RTS.S8Vector(obj);
   }
   if (obj instanceof Int16Array) {
     return new RTS.S16Vector(obj);
   }
   if (obj instanceof Int32Array) {
     return new RTS.S32Vector(obj);
   }
   if (obj instanceof Float32Array) {
     return new RTS.F32Vector(obj);
   }
   if (obj instanceof Float64Array) {
     return new RTS.F64Vector(obj);
   }
   if (typeof obj === "object") {
     return RTS.host2foreign(obj);
     // var alist = null;
     // for (var key in obj) {
     // alist = new RTS.Pair(new RTS.Pair(RTS.host2scm(key),RTS.host2scm(obj[key])),alist);
     // }
     // return alist;
   }
   throw "host2scm error";
 };
 
EOF
)

;; (##inline-host-statement "console.log('RTS Statement')")

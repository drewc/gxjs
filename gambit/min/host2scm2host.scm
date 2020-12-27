(##inline-host-declaration #<<EOF

g_scm2host = function (obj) {
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
  // this is what we add
  if (obj instanceof G_Foreign) {
    return g_foreign2host(obj);
  }
  if (obj instanceof G_Flonum) {
    return obj.val;
  }
  if (obj instanceof G_ScmString) {
    return obj.toString();
  }
  if (obj instanceof Array) {
    return obj.map( g_scm2host );
  }
  if (obj instanceof G_U8Vector) {
    return obj.elems;
  }
  if (obj instanceof G_U16Vector) {
    return obj.elems;
  }
  if (obj instanceof G_U32Vector) {
    return obj.elems;
  }
  if (obj instanceof G_S8Vector) {
    return obj.elems;
  }
  if (obj instanceof G_S16Vector) {
    return obj.elems;
  }
  if (obj instanceof G_S32Vector) {
    return obj.elems;
  }
  if (obj instanceof G_F32Vector) {
    return obj.elems;
  }
  if (obj instanceof G_F64Vector) {
    return obj.elems;
  }
  if (obj instanceof G_Pair) {
    // If this is not a list, just return the pair as-is
    if (!(obj.cdr instanceof G_Pair) && obj.cdr !== null) {
      //console.log('this is a pair. not a list', obj)
      return obj;
    }
    var jsobj = {};
    var i = 0;
    while (obj instanceof G_Pair) {
      var elem = obj.car;
      if (elem instanceof G_Pair) {
        jsobj[g_scm2host(elem.car)] = g_scm2host(elem.cdr);
      } else {
        jsobj[i] = g_scm2host(elem);
      }
      ++i;
      obj = obj.cdr;
    }
    return jsobj;
  }
  if (obj instanceof G_Structure) {
    throw "scm2host error (cannot convert Structure)";
  }
  if (typeof obj === "function") {
    return g_procedure2host(obj);
  }
  throw "scm2host error";
};


g_host2scm = function (obj) {
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
      return new G_Flonum(obj);
    }
  }
  if (typeof obj === "function") {
    return g_host_function2scm(obj);
  }
  if (typeof obj === "string") {
    return new G_ScmString(g_str2codes(obj));
  }
  if (obj instanceof G_Pair) { return obj ;}
  if (obj instanceof Array) {
    return obj.map( g_host2scm );
  }
  if (obj instanceof Uint8Array) {
    return new G_U8Vector(obj);
  }
  if (obj instanceof Uint16Array) {
    return new G_U16Vector(obj);
  }
  if (obj instanceof Uint32Array) {
    return new G_U32Vector(obj);
  }
  if (obj instanceof Int8Array) {
    return new G_S8Vector(obj);
  }
  if (obj instanceof Int16Array) {
    return new G_S16Vector(obj);
  }
  if (obj instanceof Int32Array) {
    return new G_S32Vector(obj);
  }
  if (obj instanceof Float32Array) {
    return new G_F32Vector(obj);
  }
  if (obj instanceof Float64Array) {
    return new G_F64Vector(obj);
  }
  // Here's the GX difference!
  if (typeof obj === "object") {
    // var alist = null;
    // for (var key in obj) {
    // alist = new G_Pair(new G_Pair(g_host2scm(key),g_host2scm(obj[key])),alist);
    // }
    // return alist;
    return g_host2foreign(obj);
  }
  throw "host2scm error";
};



EOF
)

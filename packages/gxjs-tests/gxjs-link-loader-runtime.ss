(import :js)

(js#statement #<<EOF
RTS.module_register = function(module_descr) {
  const r = this;
  r.sp = -1;
  r.stack[++this.sp] = void 0;
  r.r0 = this.underflow;
  r.nargs = 0;
  r.trampoline(module_descr[4]);
};

EOF
)

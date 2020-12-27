(##inline-host-declaration #<<EOF
g_module_name = function (module_descr) {
  var temp = module_descr[0];
  var name = temp[temp.length - 1].name;
  return name;
};

g_module_init = function(module_descr) {
  g_sp = -1;
  g_stack[++g_sp] = void 0;
  g_r0 = g_underflow;
  g_nargs = 0;
  g_trampoline(module_descr[4]);
};

gx_gambit_module_table = [];

g_module_register = function (module_descr) {
  // Keep track of all registered modules.
  gx_gambit_module_table.push(module_descr);

  if ( typeof g_glo['##program-descr'] === 'object' ) {
    g_module_init(module_descr);
  } else {
    var temp = module_descr[0];
    var name = temp[temp.length - 1].name;
    var info = Object.prototype.hasOwnProperty.call(g_module_map,name) ? g_module_map[name] : null;
    g_module_latest_registered = module_descr;
    if (!(info === null || g_module_count === g_module_table.length)) {
      var index = info.index;
      var old = g_module_table[index];
      g_module_table[index] = module_descr;
      if (old === null) {
        ++g_module_count;
        if (g_module_count === g_module_table.length) {
          g_glo["##program-descr"] = [g_module_table,null,false];
          temp = g_module_table[g_module_table.length - 1][0];
          g_glo["##vm-main-module-ref"] = temp[temp.length - 1];
          g_sp = -1;
          g_stack[++g_sp] = void 0;
          g_r0 = g_underflow;
          g_nargs = 0;
          g_trampoline(g_module_table[0][4]);
        }
      }
    }
  }
};


g_module_registry_reset = function () {
  g_module_count = 0;
  g_module_map = {};
  g_module_table = null;
  g_module_latest_registered = null;
};

g_module_registry_init = function (link_info) {
  var n = link_info.length;
  var i = 0;
  g_module_registry_reset();
  g_module_table = new Array(n);
  while (i < n) {
    var info = link_info[i];
    g_module_map[info.name] = info;
    g_module_table[i] = null;
    ++i;
  }
};

EOF
)

(##inline-host-declaration #<<EOF
function VM() {

  var vm = this;

  vm.os_web = (function () { return this === this.window; })();
  vm.os_web_origin = '';
  vm.ui = null;
  vm.runProcedure = false;

  if (vm.os_web) {

    // Set things up to defer execution of Scheme code until
    // vm.init is called.

    @module_table@.push(null); // pretend an extra module is needed
  }
};

VM.prototype.Console = function (elem) { this.elem = elem}

VM.prototype.init = function (UI = false, ui_elem) {

  var vm = this;

  vm.os_web_origin = @os_web_origin@;
  if (UI) {
    vm.ui = new UI(vm, ui_elem);
  } else {
    vm.ui = { add_console(d) { return vm.add_console(d) } }
  }

  // Start execution of Scheme code.

  @module_table@.pop(); // remove extra module
  @program_start@();
};

VM.prototype.os_condvar_ready_set = function (condvar_scm, ready) {

  var vm = this;

  @os_condvar_ready_set@(condvar_scm, ready);
};

VM.prototype.consoles = []

VM.prototype.add_console = function (d) {
  this.consoles.push(d);
};

VM.prototype.new_repl = function () {

  var vm = this;

  @async_call@(false, // no result needed
               @host2scm@(false),
               @glo@['##new-repl'],
               []);
};

VM.prototype.user_interrupt_thread = function (thread_scm) {

  var vm = this;

  @heartbeat_count@ = 1; // force interrupt at next poll point

  @async_call@(false, // no result needed
               thread_scm,
               @glo@['##user-interrupt!'],
               []);
};

VM.prototype.terminate_thread = function (thread_scm) {

  var vm = this;

  @async_call@(false, // no result needed
               @host2scm@(false),
               @glo@['##thread-terminate!'],
               [thread_scm]);
};

EOF
)

;;;============================================================================

;;; File: "VM.scm"

;;; Copyright (c) 2020-2021 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(##include "~~lib/_gambit#.scm")

(declare (extended-bindings) (standard-bindings) (block))
(declare (not inline))

(##define-macro (incl filename)
  `(##declare-scope
    (##macro-scope
     (##namespace-scope
      (##include ,filename)))))

(incl "js.scm")
(incl "six-expand.scm")
(incl "extra.scm")
(incl "constructor.scm")
;;;----------------------------------------------------------------------------

(##inline-host-declaration #<<EOF

function Device_console(vm, title, flags, thread_scm) {

  var dev = this;
  dev.vm = vm;
  dev.title = title;
  dev.flags = flags;
  dev.wbuf = new Uint8Array(0);
  dev.rbuf = new Uint8Array(1);
  dev.rlo = 1;
  dev.encoder = new TextEncoder();
  dev.decoder = new TextDecoder();
  dev.echo = true;
  dev.read_condvar_scm = null;
  dev.delayed_output = '';

  dev.mux = null;
  dev.focused = false;
  dev.dirty = false;

  dev.thread_scm = thread_scm;
  dev.elem = document.createElement('div');
  dev.cons = new vm.Console(dev.elem);

  dev.debug = false;

  dev.cons.connect(dev);

  vm.ui.add_console(dev);
}

Device_console.prototype.console_writable = function (cons) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').console_writable(...)');

  dev.cons = cons;
  cons.write(dev.delayed_output);
  dev.delayed_output = '';
};

Device_console.prototype.console_readable = function (cons) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').console_readable(...)');

  dev.cons = cons;
  var input = cons.read();
  var condvar_scm = dev.read_condvar_scm;
  if (condvar_scm !== null) {
    if (input === null) {
      dev.rbuf = new Uint8Array(0);
    } else {
      dev.rbuf = dev.encoder.encode(input);
    }
    dev.rlo = 0;
    dev.read_condvar_scm = null;
    dev.vm.os_condvar_ready_set(condvar_scm, true);
  }
};

Device_console.prototype.console_user_interrupt_thread = function (cons) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').console_user_interrupt_thread(...)');

  dev.cons = cons;

  dev.vm.user_interrupt_thread(dev.thread_scm);
};

Device_console.prototype.console_terminate_thread = function (cons) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').console_terminate_thread(...)');

  dev.cons = cons;

  dev.vm.terminate_thread(dev.thread_scm);
};

Device_console.prototype.input_add = function (input) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').input_add(\''+input+'\')');

  var len = dev.rbuf.length-dev.rlo;
  var inp = dev.encoder.encode(input);
  if (dev.echo) dev.output_add_buffer(inp); // echo the input
  var newbuf = new Uint8Array(len + inp.length);
  newbuf.set(newbuf.subarray(dev.rlo, dev.rlo+len));
  newbuf.set(inp, len);
  dev.rbuf = newbuf;
  dev.rlo = 0;
};

Device_console.prototype.output_add = function (output) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').output_add(\''+output+'\')');

  dev.output_add_buffer(dev.encoder.encode(output));
};

Device_console.prototype.output_add_buffer = function (buffer) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').output_add_buffer(...)');

  var len = dev.wbuf.length;
  var newbuf = new Uint8Array(len + buffer.length);
  newbuf.set(dev.wbuf);
  newbuf.set(buffer, len);
  dev.wbuf = newbuf;

  var output = dev.decoder.decode(dev.wbuf);
  if (dev.cons === null) {
    dev.delayed_output += output;
  } else {
    dev.cons.write(dev.delayed_output + output);
    dev.delayed_output = '';
  }
  dev.wbuf = new Uint8Array(0);
};

Device_console.prototype.read = function (dev_condvar_scm, buffer, lo, hi) {

  var dev = this;
  var n = hi-lo;
  var have = dev.rbuf.length-dev.rlo;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').read(...,['+buffer.slice(0,10)+'...],'+lo+','+hi+')');

  dev.vm.os_condvar_ready_set(dev_condvar_scm, false);

  if (have === 0) {

    if (dev.rlo === 0) {
      dev.rbuf = new Uint8Array(1); // prevent repeated EOF
      dev.rlo = 1;
      return 0; // signal EOF

    } else {

      // Input will be received asynchronously

      if (dev.read_condvar_scm === null) {
        dev.read_condvar_scm = dev_condvar_scm;
      }

      return -35; // return EAGAIN to suspend Scheme thread on condvar
    }
  }

  if (n > have) n = have;

  buffer.set(dev.rbuf.subarray(dev.rlo, dev.rlo+n), lo);

  dev.rlo += n;

  return n; // number of bytes transferred
};

Device_console.prototype.write = function (dev_condvar_scm, buffer, lo, hi) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').write(...,['+buffer.slice(0,10)+'...],'+lo+','+hi+')');

  dev.output_add_buffer(buffer.subarray(lo, hi));

  return hi-lo;
};

Device_console.prototype.force_output = function (dev_condvar_scm, level) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').force_output(...,'+level+')');

  return 0; // no error
};

Device_console.prototype.seek = function (dev_condvar_scm, pos, whence) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').seek(...,'+pos+','+whence+')');

  return -1; // EPERM (operation not permitted)
};

Device_console.prototype.width = function (dev_condvar_scm) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').width()');

  var cm = dev.cons.cm;
  var charWidth = cm.defaultCharWidth();
  var scrollInfo = cm.getScrollInfo();
  var width = Math.floor(scrollInfo.clientWidth / charWidth) - 1;

  if (width < 20) width = 20;

  return width;
};

Device_console.prototype.close = function (direction) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').close('+direction+')');

  return 0; // no error
};

Device_console.prototype.get_title = function () {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').get_title()');

  return dev.title;
};

Device_console.prototype.needs_attention = function () {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').needs_attention()');

  return !dev.focused && dev.dirty;
};

Device_console.prototype.focus = function () {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').focus()');

  dev.focused = true;
  dev.cons.focus();
};

Device_console.prototype.blur = function () {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').blur()');

  dev.focused = false;
  dev.dirty = false;
  dev.cons.blur();
};

Device_console.prototype.connect_multiplexer = function (mux) {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').connect_multiplexer(...)');

  dev.mux = mux;
};

Device_console.prototype.disconnect_multiplexer = function () {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').disconnect_multiplexer()');

  dev.mux = null;
};

Device_console.prototype.get_menu_items = function () {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').get_menu_items()');

  var items = [];

//  items.push(menu_item('Close tab', [], function (event) {
//    console.log('close tab ' + dev.title);
//    dev.vm.ui.remove_console(dev, true);
//  }));

  items.push(menu_item('Interrupt thread', [], function (event) {
    dev.cons.user_interrupt();
  }));

  if (dev.title !== '#<thread #1 primordial>') {
    items.push(menu_item('Terminate thread', [], function (event) {
      dev.cons.terminate_thread();
    }));
  }

  return items;
};

Device_console.prototype.get_elem = function () {

  var dev = this;

  if (dev.debug)
    console.log('Device_console(\''+dev.title+'\').get_elem()');

  return dev.elem;
};

function menu_item(title, attrs, handler) {
  var item = document.createElement('div');
  item.innerText = title;
  attrs.forEach(function (attr) {
    item.setAttribute(attr, '');
  });
  if (handler) {
    item.setAttribute('data-g-dropdown-menu-selectable', '');
  }
  item.addEventListener('click', handler);
  return item;
}

//-----------------------------------------------------------------------------


main_vm = new VM();
module.exports = main_vm;

EOF
)

;;;----------------------------------------------------------------------------

;; Define "console" ports that support multiple independent REPLs.

(define (##os-device-stream-open-console title flags thread)
  (##inline-host-declaration "

@os_device_stream_open_console@ = function (vm, title_scm, flags_scm, thread_scm) {

  var title = @scm2host@(title_scm);
  var flags = @scm2host@(flags_scm);

  var dev = new Device_console(vm, title, flags, thread_scm);

  return @host2foreign@(dev);
};

")
  (##inline-host-expression
   "@os_device_stream_open_console@(main_vm,@1@,@2@,@3@)"
   title
   flags
   thread))

(define (##open-console title
                        name
                        #!optional
                        (thread #f)
                        (settings (macro-absent-obj)))
  (let ((direction
         (macro-direction-inout))
        (settings
         (cond ((##eq? settings (macro-absent-obj))
                '())
               (else
                settings))))
    (##make-path-psettings
     direction
     settings
     ##exit-abruptly
     (lambda (psettings)
       (let ((device
              (##os-device-stream-open-console
               title
               (##psettings->device-flags psettings)
               thread)))
         (if (##fixnum? device)
             (##exit-with-err-code device)
             (and device
                  (##make-device-port-from-single-device
                   name
                   device
                   psettings))))))))

(define (##activate-console dev)
  (##inline-host-statement
   "if (main_vm.ui !== null) main_vm.ui.activate_console(@foreign2host@(@1@));"
   dev))

(define (##activate-repl)
  (let* ((console (##repl-output-port))
         (condvar (macro-device-port-wdevice-condvar console))
         (dev (macro-condvar-name condvar)))
    (##activate-console dev)))

(define (##thread-make-repl-channel-as-console thread)
  (let* ((sn (##object->serial-number thread))
         (title (##object->string thread))
         (name (if (##eqv? sn 1)
                   'console
                   (##string->symbol
                    (##string-append "console" (##number->string sn 10)))))
         (port (##open-console title name thread)))
    (##make-repl-channel-ports port port port)))

;; Enable web console.

(if (##inline-host-expression "@host2scm@(main_vm.os_web)")
    (begin

      (##thread-make-repl-channel-set! ##thread-make-repl-channel-as-console)

      ;; Prevent exiting the REPL with EOF.
      (macro-repl-channel-really-exit?-set!
       (##thread-repl-channel-get! (macro-current-thread))
       (lambda (channel) #f))))

;;;----------------------------------------------------------------------------
(define (##repl-no-banner)
  (##repl-debug
   (lambda (first port) #f)
   #t))

(define (##new-repl)
  (declare (not interrupts-enabled))
  (##thread-start!
   (let* ((primordial-tgroup
           (macro-thread-tgroup ##primordial-thread))
          (input-port
           ##stdin-port)
          (output-port
           ##stdout-port)
          (thread
           (##make-root-thread
            (lambda ()
              ;; thread will start a REPL
              (##activate-repl)
              (##repl-no-banner))
            (##void) ;; no name
            primordial-tgroup
            input-port
            output-port)))
     thread)))

;; Import the six.infix special form for JavaScript without an actual
;; import statement that would require reading files.

(##eval
 '(##begin
   (##namespace ("_six/js#" six.infix))
   (##define-syntax six.infix
     (lambda (src)
       (##demand-module _six/six-expand)
       (_six/six-expand#six.infix-js-expand src)))))

(define (##start-repl-in-VM)

  ;; Redefine the documentation browser to open the manual.

  (##gambdoc-set!
   (lambda (arg1 arg2 arg3 arg4)
     (##inline-host-statement
      "window.open(@scm2host@(@1@));"
      (##string-append "doc/gambit.html#" arg4))))

  ;; Start the REPL of the primordial thread.

  (if #t #;(##inline-host-expression
    "@host2scm@(main_vm.os_web_origin.indexOf('://gambitscheme.org/') > 0)")
    (##repl-debug-main)
    (##repl-no-banner)))

;; (load "foo.scm")
;; (force-output)

  ;; Redirect current input/output ports to the console to avoid surprises
  ;; (by default the current output port is the JS console).

(##current-input-port (##repl-input-port))
(##current-output-port (##repl-output-port))

;;(##start-repl-in-VM)
;;;============================================================================

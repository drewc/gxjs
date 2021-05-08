//=============================================================================

// File: "Device_console.js"

// Copyright (c) 2020-2021 by Marc Feeley, All Rights Reserved.

//=============================================================================
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
  dev.cons = new Console(dev.elem);

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

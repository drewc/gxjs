(##include "~~/lib/gambit/prim/thread#.scm")
(##include "~~/lib/_thread#.scm")
(##include "~~/lib/gambit/prim/thread-gambit#.scm")

;;;----------------------------------------------------------------------------

;;; Inlined thread primitives.

(define-prim (##current-thread))
(define-prim (##current-processor))
(define-prim (##current-processor-id))
(define-prim (##processor id))
(define-prim (##current-vm))

(define-prim (##primitive-lock! btq i j))
(define-prim (##primitive-trylock! btq i j))
(define-prim (##primitive-unlock! btq i j))

(define-prim (##object-before? x y))
;;;----------------------------------------------------------------------------

;; The procedure current-processor returns the processor executing the
;; current thread.

(define-prim (current-processor)

  (##declare (not interrupts-enabled))

  (macro-current-processor))

;;;----------------------------------------------------------------------------

;; The procedure processor? returns #t when the parameter is a processor
;; and #f otherwise.

(define-prim (processor? obj)

  (##declare (not interrupts-enabled))

  (macro-force-vars (obj)
    (macro-processor? obj)))

;;;----------------------------------------------------------------------------

;; The procedure processor? returns #t when the parameter is a processor
;; and #f otherwise.

(define-prim (processor-id processor)

  (##declare (not interrupts-enabled))

  (macro-force-vars (processor)
    (macro-check-processor processor 1 (processor-id processor)
      (macro-processor-id processor))))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------

;; The procedure current-thread returns the thread currently executing
;; on the current processor.

(define-prim (current-thread)

  (##declare (not interrupts-enabled))

  (macro-current-thread))

;;;----------------------------------------------------------------------------

;; The procedure thread? returns #t when the parameter is a thread
;; and #f otherwise.

(define-prim (thread? obj)

  (##declare (not interrupts-enabled))

  (macro-force-vars (obj)
    (macro-thread? obj)))

;;;----------------------------------------------------------------------------
(##define-macro (macro-parameter-descr param)
  `(##closure-ref ,param 1))

(define-prim (##make-tgroup name parent)
  (##declare (not interrupts-enabled))
  (macro-make-tgroup name parent))

(define-prim (##env-lookup env param)
  (##declare (not interrupts-enabled))
  (let ((hash-param
         (macro-parameter-descr-hash
          (macro-parameter-descr param))))

    (define (lookup env)
      (if (##null? env)
        #f
        (let* ((x
                (macro-env-param-val env))
               (param-x
                (##car x))
               (hash-param-x
                (macro-parameter-descr-hash
                 (macro-parameter-descr param-x))))
          (cond ((##fx< hash-param hash-param-x)
                 (lookup (macro-env-left env)))
                ((or (##fx< hash-param-x hash-param)
                     (##not (##eq? param-x param)))
                 (lookup (macro-env-right env)))
                (else
                 x)))))

    (lookup env)))

;;;----------------------------------------------------------------------------

(define-prim (##dynamic-env-bind denv thunk)
  (##declare (not interrupts-enabled))
  (let* ((current-thread
          (macro-current-thread))
         (old-denv
          (macro-thread-denv current-thread)))
    (macro-thread-denv-set! current-thread denv)
    (let ((x (macro-env-param-val (macro-denv-local denv))))
      (macro-thread-denv-cache1-set! current-thread x)
      (macro-thread-denv-cache2-set! current-thread x)
      (macro-thread-denv-cache3-set! current-thread x)
      (let* ((results ; may get bound to a multiple-values object
              (thunk))
             (current-thread
              (macro-current-thread)))
        (macro-thread-denv-set! current-thread old-denv)
        (let ((x (macro-env-param-val (macro-denv-local old-denv))))
          (macro-thread-denv-cache1-set! current-thread x)
          (macro-thread-denv-cache2-set! current-thread x)
          (macro-thread-denv-cache3-set! current-thread x)
          results)))))

;;;----------------------------------------------------------------------------





(define-prim (##dynamic-ref param)
  (##declare (not interrupts-enabled))
;;  (##inline-host-statement "console.log('thread', @1@, @2@, @3@)" param 1 1)
  (cond ((##eq? param ##current-exception-handler)
         (macro-current-exception-handler))
        ((##eq? param ##current-input-port)
         (macro-current-input-port))
        ((##eq? param ##current-output-port)
         (macro-current-output-port))
        (else
 #; (##inline-host-statement "console.log(' here now thread', @1@, @2@, @3@)"
                           param (macro-current-thread) (macro-thread-denv-cache1 (current-thread)))
         (let* ((current-thread
                 (macro-current-thread))
                (c1
                 (macro-thread-denv-cache1 current-thread)))
           (if (##eq? param (##car c1))
             (##cdr c1)
             (let ((c2 (macro-thread-denv-cache2 current-thread)))
               (if (##eq? param (##car c2))
                 (begin
                   (macro-thread-denv-cache2-set! current-thread c1)
                   (macro-thread-denv-cache1-set! current-thread c2)
                   (##cdr c2))
                 (let ((c3 (macro-thread-denv-cache3 current-thread)))
                   (if (##eq? param (##car c3))
                     (begin
                       (macro-thread-denv-cache3-set! current-thread c2)
                       (macro-thread-denv-cache2-set! current-thread c1)
                       (macro-thread-denv-cache1-set! current-thread c3)
                       (##cdr c3))
                     (let* ((denv
                             (macro-thread-denv current-thread))
                            (x
                             (##env-lookup (macro-denv-local denv) param)))
                       (if x
                         (begin
                           (macro-thread-denv-cache3-set!
                            current-thread
                            (macro-thread-denv-cache2 current-thread))
                           (macro-thread-denv-cache2-set!
                            current-thread
                            (macro-thread-denv-cache1 current-thread))
                           (macro-thread-denv-cache1-set!
                            current-thread
                            x)
                           (##cdr x))
                         (macro-parameter-descr-value
                          (macro-parameter-descr param)))))))))))))

(define-prim (##dynamic-let param val thunk)
  (##declare (not interrupts-enabled))
  (cond ((##eq? param ##current-exception-handler)
         (macro-dynamic-bind exception-handler
          val
          thunk))
        ((##eq? param ##current-input-port)
         (macro-dynamic-bind input-port
          val
          thunk))
        ((##eq? param ##current-output-port)
         (macro-dynamic-bind output-port
          val
          thunk))
        (else
         (let* ((param-val
                 (##cons param val))
                (denv
                 (macro-thread-denv (macro-current-thread)))
                (new-local-denv
                 (##env-insert (macro-denv-local denv) param-val)))
           (##dynamic-env-bind
            (macro-make-denv
             new-local-denv
             (macro-denv-dynwind denv)
             (macro-denv-interrupt-mask denv)
             (macro-denv-debugging-settings denv)
             (macro-denv-exception-handler denv)
             (macro-denv-input-port denv)
             (macro-denv-output-port denv)
             (macro-denv-repl-context denv))
            thunk)))))
(define-prim (##dynamic-set! param val)
  (##declare (not interrupts-enabled))
  (cond ((##eq? param ##current-exception-handler)
         (macro-current-exception-handler-set! val)
         (##void))
        ((##eq? param ##current-input-port)
         (macro-current-input-port-set! val)
         (##void))
        ((##eq? param ##current-output-port)
         (macro-current-output-port-set! val)
         (##void))
        (else
         (let* ((current-thread
                 (macro-current-thread))
                (c1
                 (macro-thread-denv-cache1 current-thread)))
           (if (##eq? param (##car c1))
             (begin
               (##set-cdr! c1 val)
               (##void))
             (let ((c2 (macro-thread-denv-cache2 current-thread)))
               (if (##eq? param (##car c2))
                 (begin
                   (macro-thread-denv-cache2-set! current-thread c1)
                   (macro-thread-denv-cache1-set! current-thread c2)
                   (##set-cdr! c2 val)
                   (##void))
                 (let ((c3 (macro-thread-denv-cache3 current-thread)))
                   (if (##eq? param (##car c3))
                     (begin
                       (macro-thread-denv-cache3-set! current-thread c2)
                       (macro-thread-denv-cache2-set! current-thread c1)
                       (macro-thread-denv-cache1-set! current-thread c3)
                       (##set-cdr! c3 val)
                       (##void))
                     (let* ((denv
                             (macro-thread-denv current-thread))
                            (x
                             (##env-lookup (macro-denv-local denv) param)))
                       (if x
                         (begin
                           (macro-thread-denv-cache3-set!
                            current-thread
                            (macro-thread-denv-cache2 current-thread))
                           (macro-thread-denv-cache2-set!
                            current-thread
                            (macro-thread-denv-cache1 current-thread))
                           (macro-thread-denv-cache1-set!
                            current-thread
                            x)
                           (##set-cdr! x val)
                           (##void))
                         (begin
                           (macro-parameter-descr-value-set!
                            (macro-parameter-descr param)
                            val)
                           (##void)))))))))))))
(define-prim (##env-insert env param-val)
  (let* ((param
          (##car param-val))
         (hash-param
          (macro-parameter-descr-hash
           (macro-parameter-descr param))))

    (define (insert env)
      (if (##null? env)
        (macro-make-env
         param-val
         '()
         '())
        (let* ((x
                (macro-env-param-val env))
               (param-x
                (##car x))
               (hash-param-x
                (macro-parameter-descr-hash
                 (macro-parameter-descr param-x))))
          (cond ((##fx< hash-param hash-param-x)
                 (macro-make-env
                  x
                  (insert (macro-env-left env))
                  (macro-env-right env)))
                ((or (##fx< hash-param-x hash-param)
                     (##not (##eq? param-x param)))
                 (macro-make-env
                  x
                  (macro-env-left env)
                  (insert (macro-env-right env))))
                (else
                 (macro-make-env
                  param-val
                  (macro-env-left env)
                  (macro-env-right env)))))))

    (insert env)))

(macro-case-target

 ((js)
  (##inline-host-declaration "

if ((function () { return this !== this.window; })()) { // nodejs?

  os = require('os');
  // fs = require('fs');
  vm = require('vm');
  process = require('process');
  // child_process = require('child_process')

  g_os_encode_error = function (exn) {
    switch (exn.code) {
      case 'EPERM':  return -1;
      case 'ENOENT': return -2;
      case 'EINTR':  return -4;
      case 'EIO':    return -5;
      case 'EBADF':  return -9;
      case 'EACCES': return -13;
      case 'EEXIST': return -17;
      case 'EAGAIN': return -35;
    }
    return -8888;
  };

  g_os_decode_error = function (code) {
    switch (code) {
      case -1:  return 'EPERM (Operation not permitted)';
      case -2:  return 'ENOENT (No such file or directory)';
      case -4:  return 'EINTR (Interrupted system call)';
      case -5:  return 'EIO (Input/output error)';
      case -9:  return 'EBADF (Bad file descriptor)';
      case -13: return 'EACCES (Permission denied)';
      case -17: return 'EEXIST (File exists)';
      case -35: return 'EAGAIN (Resource temporarily unavailable)';
    }
    return 'E??? (unknown error)';
  };
}

g_current_time = function () {
  return new Date().getTime() / 1000;
};

g_start_time = g_current_time();

g_set_process_times = function (vect) {
  var elapsed = g_current_time() - g_start_time;
  vect.elems[0] = elapsed;
  vect.elems[1] = 0.0;
  vect.elems[2] = elapsed;
  return vect;
};

G_Device = function (fd) {
  this.fd = fd;
  this.rbuf = new Uint8Array(1024);
  this.rlo = 1;
  this.rhi = 1; // 0 would mean EOF
};

g_os_debug = false;

"))

 ((python)
  (##inline-host-declaration "

import os
import pwd
import grp
import stat
import time
import errno
import getpass
import functools

def g_os_encode_error(exn):
    e = exn.errno
    if e == errno.EPERM:
      return -1
    elif e == errno.ENOENT:
        return -2
    elif e == errno.EINTR:
        return -5
    elif e == errno.EIO:
        return -9
    elif e == errno.EBADF:
        return -13
    elif e == errno.EACCES:
        return -17
    elif e == errno.EEXIST:
        return -35
    else:
        return -8888

def g_os_decode_error(code):
    if code == -1:
        return 'EPERM (Operation not permitted)'
    elif code == -2:
        return 'ENOENT (No such file or directory)'
    elif code == -4:
        return 'EINTR (Interrupted system call)'
    elif code == -5:
        return 'EIO (Input/output error)'
    elif code == -9:
        return 'EBADF (Bad file descriptor)'
    elif code == -13:
        return 'EACCES (Permission denied)'
    elif code == -17:
        return 'EEXIST (File exists)'
    elif code == -35:
        return 'EAGAIN (Resource temporarily unavailable)'
    else:
        return 'E??? (unknown error)'

def g_current_time():
    return time.time()

g_start_time = g_current_time()

def g_set_process_times(vect):
    elapsed = g_current_time() - g_start_time
    vect.elems[0] = elapsed
    vect.elems[1] = 0.0
    vect.elems[2] = elapsed
    return vect

class G_Device:

    def __init__(self, fd):
        self.fd = fd


g_os_debug = False

"))

 (else))
(define-prim (##os-condvar-select! devices timeout)
  (##first-argument #f ##feature-port-fields)
  (macro-case-target

   ((js)
    (##inline-host-declaration "

g_os_condvar_select_should_sleep = true;

g_os_condvar_select = function (devices_scm, timeout_scm) {

  if (g_os_debug)
    console.log('g_os_condvar_select(devices, timeout)  ***not fully implemented***');

  var at_least_1_device = (devices_scm !== false &&
                           devices_scm !== devices_scm.slots[g_BTQ_DEQ_NEXT]);

  var timeout_ms;

  if (timeout_scm === false)
    timeout_ms = 0;
  else if (timeout_scm === true)
    timeout_ms = 999999;
  else
    timeout_ms = (timeout_scm.elems[0]-g_current_time()) * 1000;

  if (!at_least_1_device || g_os_condvar_select_should_sleep) {

    if (at_least_1_device)
      timeout_ms = 10; // give browser time to refresh

    g_os_condvar_select_should_sleep = false;

    var ra = g_r0;
    g_r0 = null; // exit trampoline

    setTimeout(function () { g_trampoline(ra); }, // resume execution
               Math.max(0, timeout_ms))

    return g_host2scm(0);
  }

  g_os_condvar_select_should_sleep = true;

  if (devices_scm !== false) {

    var condvar_scm = devices_scm.slots[g_BTQ_DEQ_NEXT];

    while (condvar_scm !== devices_scm) {
      var owner = condvar_scm.slots[g_BTQ_OWNER];
      var dev = g_foreign2host(condvar_scm.slots[g_CONDVAR_NAME]);
      if (dev.fd === -4) // console?
        condvar_scm.slots[g_BTQ_OWNER] = owner | 1; // mark as 'ready'
      else
        condvar_scm.slots[g_BTQ_OWNER] = owner & ~1; // mark as 'not ready'
      condvar_scm = condvar_scm.slots[g_BTQ_DEQ_NEXT];
    }

  }

  return g_host2scm(0);
};

")
    (##inline-host-expression
     "g_os_condvar_select(@1@,@2@)"
     devices
     timeout))

   ((python)
    (##inline-host-declaration "

def g_os_condvar_select(devices_scm, timeout_scm):

    if g_os_debug:
        print('g_os_condvar_select(devices, timeout)  ***not fully implemented***')

    return g_host2scm(0)  # no error

")
    (##inline-host-expression
     "g_os_condvar_select(@1@,@2@)"
     devices
     timeout))

   (else
    (println "unimplemented ##os-condvar-select! called")
    -5555)))

(define (##enable-interrupts!) #f)

;;; Implementation of blocked thread queues and timeout queues.

(implement-btq) ;; defines ##btq-insert!, etc
(implement-toq) ;; defines ##toq-insert!, etc
;;;----------------------------------------------------------------------------
;;;
;;; ====================
(define-prim (##parameter? obj)
  (##declare (not interrupts-enabled))
  (and (##procedure? obj)
       (##closure? obj)
       (##eq? (##closure-code obj)
              (##closure-code ##current-exception-handler))))

(##define-macro (macro-parameter-descr param)
  `(##closure-ref ,param 1))

(define-prim (##parameterize1 param val thunk)
  (##declare (not interrupts-enabled))
  (macro-check-procedure param 1 (##parameterize1 param val thunk)
    (macro-check-procedure thunk 3 (##parameterize1 param val thunk)
      (if (##parameter? param)
        (##dynamic-let
         param
         ((macro-parameter-descr-filter (macro-parameter-descr param)) val)
         thunk)
        (let ((save (param)))
          (##dynamic-wind
           (lambda () ;; before
             (param val))
           thunk
           (lambda () ;; after
             (param save))))))))

(##include "~~lib/gambit/parameter/parameter#.scm")

;;; now in symkey!
#;(define-prim (##partial-bit-reverse i)

  (##define-macro (bit n)
    `(##fxarithmetic-shift-left
      (##fxand i ,(expt 2 n)) ,(- 28 (* 2 n))))

  (##fx+
   (bit 0)
   (##fx+
    (bit 1)
    (##fx+
     (bit 2)
     (##fx+
      (bit 3)
      (##fx+
       (bit 4)
       (##fx+
        (bit 5)
        (##fx+
         (bit 6)
         (##fx+
          (bit 7)
          (##fx+
           (bit 8)
           (##fx+
            (bit 9)
            (##fx+
             (bit 10)
             (##fx+
              (bit 11)
              (##fx+
               (bit 12)
               (##fx+
                (bit 13)
                (bit 14))))))))))))))))

(define ##parameter-counter 0)

(define-prim (##make-parameter
              init
              #!optional
              (f (macro-absent-obj)))
  (let ((filter
         (if (##eq? f (macro-absent-obj))
           (lambda (x) x)
           f)))
    (macro-check-procedure filter 2 (make-parameter init f)
      (let* ((val
              (filter init))
             (new-count
              (+ ##parameter-counter 1)))
        ;; Note: it is unimportant if the increment of
        ;; ##parameter-counter is not atomic; it simply means a
        ;; possible close repetition of the same hash code
        (set! ##parameter-counter new-count)
        (let ((descr
               (macro-make-parameter-descr
                val
                (##partial-bit-reverse new-count)
                filter)))
          (letrec ((param
                    (lambda (#!optional (new-val (macro-absent-obj)))
                      (if (##eq? new-val (macro-absent-obj))
                        (##dynamic-ref param)
                        (##dynamic-set!
                         param
                         ((macro-parameter-descr-filter descr) new-val))))))
            param))))))

(define-prim (make-parameter init #!optional (f (macro-absent-obj)))
  (macro-force-vars (f)
    (##make-parameter init f)))

;;;============================================================================

(define (##os-path-normalize-directory path)
  (##declare (not interrupts-enabled))
  (macro-case-target

   ((js)
    (##inline-host-declaration "

g_os_path_normalize_directory = function (path) {
  if ((function () { return this !== this.window; })()) { // nodejs?
    var old = process.cwd();
    var dir;
    if (path === false) {
      dir = old;
    } else {
      try {
        process.chdir(path);
      } catch (exn) {
        if (exn instanceof Error && exn.hasOwnProperty('code')) {
          return g_host2scm(g_os_encode_error(exn));
        } else {
          throw exn;
        }
      }
      dir = process.cwd();
      process.chdir(old);
    }
    if (dir[dir.length-1] === '/' || dir[dir.length-1] === '\\\\') {
      return g_host2scm(dir);
    } else if (dir[0] === '/') {
      return g_host2scm(dir + '/')
    } else {
      return g_host2scm(dir + '\\\\')
    }
  } else {
    var loc = window.location.href;
    var root = '/' + loc.slice(0, 1+loc.lastIndexOf('/'));
    if (path === false) {
      return g_host2scm(root);
    } else {
      return g_host2scm(root + path.slice((path[0] === '/') ? 1 : 0));
    }
  }
};

")
    (##inline-host-expression "g_os_path_normalize_directory(g_scm2host(@1@))" path))

   ((python)
    (##inline-host-declaration "

def g_os_path_normalize_directory(path):
    old = os.getcwd()
    if path is False:
        dir = old
    else:
        try:
            os.chdir(path)
        except OSError as exn:
            return g_host2scm(g_os_encode_error(exn))
        dir = os.getcwd()
        os.chdir(old)
    if dir[-1] == '/' or dir[-1] == '\\\\':
        return g_host2scm(dir)
    elif dir[0] == '/':
        return g_host2scm(dir + '/')
    else:
        return g_host2scm(dir + '\\\\')

")
    (##inline-host-expression "g_os_path_normalize_directory(g_scm2host(@1@))" path))

   (else
    (println "unimplemented ##os-path-normalize-directory called with path=")
    (println path)
    "/")))
(define ##initial-current-directory
  (let ((current-dir
         (##os-path-normalize-directory #f)))
    (if (##fixnum? current-dir)
        (##exit-with-err-code current-dir)
        current-dir)))

(define-prim (##path-normalize-directory-existing path)
  (let ((normalized-dir
         (##os-path-normalize-directory (##path-expand path))))
    (if (##fixnum? normalized-dir)
        (##raise-os-exception #f normalized-dir ##current-directory path)
        normalized-dir)))

(define-prim (##current-directory-filter val)
  (if (##eq? val (macro-absent-obj))
      ##initial-current-directory
      (macro-check-string val 1 (##current-directory val)
        (##path-normalize-directory-existing val))))

(define ##current-directory
  (##make-parameter
   (macro-absent-obj)
   ##current-directory-filter))

(define current-directory
  ##current-directory)

(##define-macro (macro-parameter-descr param)
  `(##closure-ref ,param 1))

(define-prim (##make-tgroup name parent)
  (##declare (not interrupts-enabled))
  (macro-make-tgroup name parent))

(define-prim (##make-root-thread
              thunk
              name
              tgroup
              input-port
              output-port)

  (##declare (not interrupts-enabled))

  (let* ((interrupt-mask
          0)
         (debugging-settings
          0)
         (local-binding
          (##cons ##current-directory
                  (macro-parameter-descr-value
                   (macro-parameter-descr ##current-directory)))))

    ;; these macros are defined to prevent the normal thread
    ;; inheritance mechanism when a root thread is created

    (##define-macro (macro-current-thread)
      `#f)

    (##define-macro (macro-thread-denv thread)
      `#f)

    (##define-macro (macro-denv-local denv)
      `(macro-make-env local-binding '() '()))

    (##define-macro (macro-denv-dynwind denv)
      `##initial-dynwind)

    (##define-macro (macro-denv-interrupt-mask denv)
      `interrupt-mask)

    (##define-macro (macro-denv-debugging-settings denv)
      `debugging-settings)

    (##define-macro (macro-denv-input-port denv)
      `(##cons ##current-input-port input-port))

    (##define-macro (macro-denv-output-port denv)
      `(##cons ##current-output-port output-port))

    (##define-macro (macro-thread-denv-cache1 thread)
      `local-binding)

    (##define-macro (macro-thread-denv-cache2 thread)
      `local-binding)

    (##define-macro (macro-thread-denv-cache3 thread)
      `local-binding)

    (##define-macro (macro-thread-floats thread)
      `#f)

    (##define-macro (macro-base-priority floats)
      `(macro-thread-root-base-priority))

    (##define-macro (macro-quantum floats)
      `(macro-thread-root-quantum))

    (##define-macro (macro-priority-boost floats)
      `(macro-thread-root-priority-boost))

    ;; create root thread

    (macro-make-thread thunk name tgroup)))

(define-prim (make-root-thread
              thunk
              #!optional
              (n (macro-absent-obj))
              (tg (macro-absent-obj))
              (ip (macro-absent-obj))
              (op (macro-absent-obj)))
  (macro-force-vars (thunk n tg ip op)
    (let* ((name
            (if (##eq? n (macro-absent-obj))
              (##void)
              n))
           (tgroup
            (if (##eq? tg (macro-absent-obj))
              (macro-thread-tgroup (macro-current-thread))
              tg))
           (input-port
            (if (##eq? ip (macro-absent-obj))
              ##stdin-port
              ip))
           (output-port
            (if (##eq? op (macro-absent-obj))
              ##stdout-port
              op)))
      (macro-check-procedure thunk 1 (make-root-thread thunk n tg ip op)
        (macro-check-tgroup tgroup 3 (make-root-thread thunk n tg ip op)
          (macro-check-input-port input-port 4 (make-root-thread thunk n tg ip op)
            (macro-check-output-port output-port 5 (make-root-thread thunk n tg ip op)
                                     (##make-root-thread thunk name tgroup input-port output-port))))))))

(define (##startup-processor!)

  (declare (not interrupts-enabled))

  (macro-processor-init! (##current-processor) (##current-processor-id))
  (##primitive-unlock! (##current-processor) 1 9)

  (let* ((tgroup
          (##make-tgroup 'local #f))
         (input-port
          ##stdin-port)
         (output-port
          ##stdout-port)
         (thread
          (##make-root-thread
           #f
           'local
           tgroup
           input-port
           output-port)))

    (macro-processor-current-thread-set!
     (macro-current-processor)
     thread)

    (##btq-insert! (macro-current-processor) thread)
    )

  (##enable-interrupts!)

  (##inline-host-statement "(() => {
  async function startGambitProcessor() {
    const yup = await(@1@)();
   return yup;
 }

 startGambitProcessor();
})()" (lambda ()
        (let loop ()
          (declare (interrupts-enabled))
          (##os-condvar-select! #f #t) ;; wait for interrupt
          (loop)))))

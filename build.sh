#!/usr/bin/env bash
_dir=$(cd -P -- "$(dirname -- "$0")" && pwd -P)
echo dir: $_dir; cd $_dir;

make_gxjs () {
    gsc -target js -o gxjs-execute.js gambit/min/execute.scm


    echo Compile a link file from our minlib to the _gambit.js runtime.
    echo This makes sure that all the 'g_*' functions get included it seems.

    gsc -target js -o gxjs-link.js -link gambit/gxjs-minlib.js

# echo replace the module init with the other one for our runtime.

# sed -i "s/^g_module_registry_init(.*/$_hello_mod_init/" gxjs-link.js

echo Compile the minlib

gsc -target js -o gxjs-minlib.js -prelude '(include "~~lib/_gambit#.scm")' gambit/gxjs-minlib.scm

_make_exec='cat gxjs-link.js gxjs-minlib.js gxjs-execute.js > gxjs.js';
echo Making exectuable : $_make_exec; eval $_make_exec;

echo Building r7rs-test
gsc -target js -o r7rs-test.js test/r7rs-test.scm ; mv r7rs-test.js test/

echo 'Done!!'
}
du gxjs.js  ; make_gxjs ; du gxjs.js ; yarn run webpack

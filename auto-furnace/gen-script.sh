#!/bin/bash
include() {
    local name="$1"
    echo "(hostile-lisp:stream-override-attribution \"${name}\" 0 1 *code-stream*)"
    cat "$name"
}

cat os-bootstrap.lisp

# OS init
include os-macros.lisp

# Modules
include cooling-loop.lisp
include pipe-buster.lisp
include boiler.lisp
include condensation.lisp
include filtration.lisp
include ui.lisp

# End Modules

include os-config-start.lisp

# Config
include config.lisp
# End config

include os-config-end.lisp
include os-main-loop.lisp

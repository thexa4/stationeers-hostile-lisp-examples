function Include {
  Param(
    [string]$Name
  )
  echo "(hostile-lisp:stream-override-attribution ""${Name}"" 0 1 *code-stream*)"
  cat $name
}

cat os-bootstrap.lisp

# OS init
Include os-macros.lisp

# Modules

Include cooling-loop.lisp
Include pipe-buster.lisp
Include boiler.lisp
Include condensation.lisp
Include filtration.lisp
Include ui.lisp

# End Modules

Include os-config-start.lisp

# Config
Include config.lisp
# End config

Include os-config-end.lisp
Include os-main-loop.lisp

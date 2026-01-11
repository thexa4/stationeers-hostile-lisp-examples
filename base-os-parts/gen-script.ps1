function Include {
  Param(
    [string]$Name
  )
  echo "(hostile-lisp:stream-override-attribution ""${Name}"" 0 1 *code-stream*)"
  cat $name
}

cat os-part1.lisp
# OS config

cat os-config.lisp

# End OS config
cat os-part2.lisp
# Modules

Include airlock.lisp
Include arc-furnace-excess.lisp
Include auto-solid-generator.lisp
Include monitor-area-power-charge.lisp
Include print-warnings.lisp
Include printer-idle.lisp
Include reset-button.lisp
Include room-lights.lisp
Include warning-light.lisp

# End Modules
Include os-part3.lisp
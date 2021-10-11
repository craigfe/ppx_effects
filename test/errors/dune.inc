; -------- Test: `dangling.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets dangling.actual)
 (deps
  (:pp pp.exe)
  (:input dangling.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff dangling.expected dangling.actual)))

; -------- Test: `rebind.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets rebind.actual)
 (deps
  (:pp pp.exe)
  (:input rebind.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff rebind.expected rebind.actual)))



; -------- Test: `exhaustive_handlers.ml` --------; The PPX-dependent executable under test
(executable
 (name exhaustive_handlers)
 (modules exhaustive_handlers)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets exhaustive_handlers.actual)
 (deps
  (:pp pp.exe)
  (:input exhaustive_handlers.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff exhaustive_handlers.expected exhaustive_handlers.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./exhaustive_handlers.exe)))

; -------- Test: `main.ml` --------; The PPX-dependent executable under test
(executable
 (name main)
 (modules main)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets main.actual)
 (deps
  (:pp pp.exe)
  (:input main.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff main.expected main.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./main.exe)))

; -------- Test: `scrutinee.ml` --------; The PPX-dependent executable under test
(executable
 (name scrutinee)
 (modules scrutinee)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets scrutinee.actual)
 (deps
  (:pp pp.exe)
  (:input scrutinee.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff scrutinee.expected scrutinee.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./scrutinee.exe)))

; -------- Test: `when_guards.ml` --------; The PPX-dependent executable under test
(executable
 (name when_guards)
 (modules when_guards)
 (preprocess (pps ppx_effects)))

; Run the PPX on the `.ml` file
(rule
 (targets when_guards.actual)
 (deps
  (:pp pp.exe)
  (:input when_guards.ml))
 (action
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (diff when_guards.expected when_guards.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_effects)
 (action
  (run ./when_guards.exe)))



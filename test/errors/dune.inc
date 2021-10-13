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

; -------- Test: `invalid_payload_type_in_match.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets invalid_payload_type_in_match.actual)
 (deps
  (:pp pp.exe)
  (:input invalid_payload_type_in_match.ml))
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
  (diff invalid_payload_type_in_match.expected
    invalid_payload_type_in_match.actual)))

; -------- Test: `invalid_payload_type_in_try.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets invalid_payload_type_in_try.actual)
 (deps
  (:pp pp.exe)
  (:input invalid_payload_type_in_try.ml))
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
  (diff invalid_payload_type_in_try.expected
    invalid_payload_type_in_try.actual)))

; -------- Test: `malformed_payload.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets malformed_payload.actual)
 (deps
  (:pp pp.exe)
  (:input malformed_payload.ml))
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
  (diff malformed_payload.expected malformed_payload.actual)))

; -------- Test: `malformed_payload_hint_missing_comma.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets malformed_payload_hint_missing_comma.actual)
 (deps
  (:pp pp.exe)
  (:input malformed_payload_hint_missing_comma.ml))
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
  (diff malformed_payload_hint_missing_comma.expected
    malformed_payload_hint_missing_comma.actual)))

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



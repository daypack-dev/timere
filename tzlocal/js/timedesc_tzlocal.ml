open Js_of_ocaml

let local () =
  let formatter = new%js Intl.dateTimeFormat_constr Js.undefined Js.undefined in
  let tz = Js.to_string (formatter##resolvedOptions ())##.timeZone in
  [ tz ]

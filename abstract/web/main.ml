open Js_of_ocaml
open Expression
open CEK

let history : state list ref = ref []

let render st = 
  let el = Dom_html.getElementById "state" in
  el##.textContent := Js.some (Js.string (string_state st))

let () = 
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    let init = initial_state example_expr in
    history := [init];
    render init;
    let btn id f = 
      (Dom_html.getElementById id)##.onclick := 
        Dom_html.handler (fun _ -> f (); Js._true) in
    btn "step" (fun () ->
      let st = step (List.hd !history) in 
      history := st :: !history; render st);
    btn "back" (fun () ->
      match !history with
      | _ :: (prev :: _ as rest) -> history := rest; render prev 
      | _ -> ());
    Js._true)
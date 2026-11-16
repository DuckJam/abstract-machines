open Js_of_ocaml
module Machine = Machines.Machine

let machines : (module Machine.S) list =
  [(module Machines.CEK); (module Machines.CESK); (module Machines.LK); (module Machines.SECD)]

type session = {
  step : unit -> unit;
  run : unit -> unit;
  back : unit -> unit;
  reset : unit -> unit;
  view : unit -> string;
}

let make_session (module M : Machine.S) expr : session =
  let history = ref [M.inject expr] in
  let error = ref None in
  let top () = List.hd !history in
  let step () = 
    let st = top () in
    if not (M.is_final st) then
      match M.step st with
      | st' -> history := st' :: !history; error := None
      | exception Failure msg -> error := Some msg
  in
  {
    step;
    run = (fun ()->
      let fuel = ref 100_000 in
      while !fuel > 0 && !error = None && not (M.is_final (top ())) do
        step (); decr fuel
      done);
    back = (fun ()-> 
      error := None;
      match !history with
      | _ :: (_ :: _ as rest) -> history := rest
      | _ -> ());
    reset = (fun ()-> error := None; history := [M.inject expr]);
    view = (fun ()-> 
      let st = top () in
      let s = M.string_state st in
      let s = if M.is_final st then s ^ "\n\n(final state)" else s in 
      match !error with
      | None -> s
      | Some e -> s^"\n\nError: "^e);
  }

let parse_result s = 
  try Ok (Expression.parse s) with
  | Expression.Lexer.Lexing_error m -> Error m 
  | Expression.Parser.Error -> Error "syntax error"

let get_el id coerce =
  match Dom_html.getElementById_coerce id coerce with
    | Some e -> e
    | None -> failwith ("missing element #"^id)

let default_text = "(fun x -> x + 1) 2"

let () = 
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    let doc = Dom_html.document in
    let select = get_el "machine" Dom_html.CoerceTo.select in
    let input = get_el "expr" Dom_html.CoerceTo.select in
    let set_text id s =
      (Dom_html.getElementById id)##.textContent := Js.some (Js.string s)
    in
    List.iteri (fun i (module M : Machine.S) ->
      let opt = Dom_html.createOption doc in
      opt##.label := Js.string M.name;
      opt##.value := Js.string (string_of_int i);
      Dom.appendChild select opt)
      machines;
    
    let current_machine () = List.nth machines select##.selectedIndex in
    let expr = ref Expression.expr_If in 
    let session = ref (make_session (List.hd machines) !expr) in
    let render () = set_text "state" (!session.view ()) in

    let load () = 
      match parse_result (Js.to_string input##.value) with
      | Ok e ->
        expr := e;
        session := make_session (current_machine ()) e;
        set_text "error" ""
      | Error m -> set_text "error" ("Parse error: "^m)
    in

    let on_click id f = 
      (Dom_html.getElementById id)##.onclick := 
        Dom_html.handler (fun _ -> f (); render (); Js._true) 
    in
    on_click "load" load;
    on_click "step" (fun () -> !session.step ());
    on_click "run" (fun () -> !session.run ());
    on_click "back" (fun () -> !session.back ());
    on_click "reset" (fun ()-> !session.reset ());

    select##.onchange := Dom_html.handler (fun _ ->
      session := make_session (current_machine ()) !expr;
      render ();
      Js._true);
    
    input##.value := Js.string default_text;
    load ();
    render ();
    Js._true);
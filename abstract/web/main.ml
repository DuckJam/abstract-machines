open Js_of_ocaml
open Expression
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

let expr_If = 
  let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EInt 5))
  and expr_cond = EBinOp(Gt, EInt 3, EInt 5) in
  EIf(expr_cond, expr_cond,expr)

let () = 
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    let doc = Dom_html.document in
    let select = 
      match Dom_html.getElementById_coerce "machine" Dom_html.CoerceTo.select with
      | Some s -> s
      | None -> failwith "missing <select id=\"machine\">"
    in
    List.iteri (fun i (module M : Machine.S) ->
      let opt = Dom_html.createOption doc in
      opt##.label := Js.string M.name;
      opt##.value := Js.string (string_of_int i);
      Dom.appendChild select opt)
      machines;
    let expr = expr_If in 
    let session = ref (make_session (List.hd machines) expr) in
    let render () = 
      (Dom_html.getElementById "state")##.textContent :=
        Js.some (Js.string (!session.view ()))
    in
    let on_click id f = 
      (Dom_html.getElementById id)##.onclick := 
        Dom_html.handler (fun _ -> f (); render (); Js._true) 
    in
    on_click "step" (fun () -> !session.step ());
    on_click "run" (fun () -> !session.run ());
    on_click "back" (fun () -> !session.back ());
    on_click "reset" (fun ()-> !session.reset ());

    select##.onchange := Dom_html.handler (fun _ ->
      session := make_session (List.nth machines select##.selectedIndex) expr;
      render ();
      Js._true);
    render ();
    Js._true);
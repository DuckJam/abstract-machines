open Js_of_ocaml
open Expression
open Machines.CEK

let history : state list ref = ref []

let expr_If = 
  let expr = EApp (EAbs("x", EBinOp(Mul, EVar "x", EInt 9)), (EInt 5))
  and expr_cond = EBinOp(Gt, EInt 3, EInt 5) in
  EIf(expr_cond, expr_cond,expr)

let render st = 
  let el = Dom_html.getElementById "state" in
  el##.textContent := Js.some (Js.string (string_state st))

let () = 
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    let init = inject expr_If in
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
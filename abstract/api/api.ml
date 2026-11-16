open Js_of_ocaml
module Machine = Machines.Machine

let machines : (module Machine.S) list =
  [(module Machines.CEK); (module Machines.CESK); (module Machines.LK); (module Machines.SECD)]

let find_machine name =
  List.find_opt ( fun (module M : Machine.S) -> M.name = name) machines

let parse_result s = 
  try Ok (Expression.parse s) with
  | Expression.Lexer.Lexing_error m -> Error m 
  | Expression.Parser.Error -> Error "syntax error"

type session = {
  step : unit -> unit;
  run : unit -> unit;
  back : unit -> unit;
  reset : unit -> unit;
  view : unit -> string;
}

let json_of_view ~final ~error ~steps registers = 
  `Assoc [
    "registers", `List (List.map (fun (n, items) ->
      `Assoc [ "name", `String n;
               "items", `List (List.map (fun s -> `String s) items)]) registers);
    "final", `Bool final;
    "error", (match error with None -> `Null | Some e -> `String e);
    "steps", `Int steps;
  ] |> Yojson.Basic.to_string

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
      json_of_view ~final:(M.is_final st) ~error:!error
      ~steps:(List.length !history -1) (M.view st))
  }

let js_of_session (s: session) = 
  object%js 
    method step = (s.step (); Js.string (s.view ()))
    method back = (s.back (); Js.string (s.view ()))
    method run = (s.run (); Js.string (s.view ()))
    method reset = (s.reset (); Js.string (s.view ()))
    method view = Js.string (s.view ())
  end

let () = 
  Js.export "abstractMachines"
    (object%js
      method machines = 
        Js.array (Array.of_list
        (List.map (fun (module M : Machine.S) -> Js.string M.name) machines))

      method parse (src : Js.js_string Js.t) =
        Js.string (Yojson.Basic.to_string
          (match parse_result (Js.to_string src) with
            | Ok _ -> `Assoc ["ok", `Bool true]
            | Error m -> `Assoc ["ok", `Bool false; "error", `String m]))
      
      method create (name : Js.js_string Js.t) (src : Js.js_string Js.t) =
        match find_machine (Js.to_string name), parse_result (Js.to_string src) with
          | Some m, Ok e -> Js.some (js_of_session (make_session m e))
          | _ -> Js.null
    end)
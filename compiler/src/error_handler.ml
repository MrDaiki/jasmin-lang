
open Location

type error_severity = 
| Fatal 
| Recoverable
| Warning

type error_step = 
| Pretyping 
| Typing 
| Normalization
| SctCheck

type 'payload compile_error = {
  severity : error_severity;
  step : error_step;
  code : string;
  payload : 'payload;
  location : Location.t;
  build : 'payload -> string;
}

type recover_strategy = 
| TryRecover
| Stop 

exception ExitError

module ErrorHandler = struct 

  type t = {
    strategy : recover_strategy;
  }

  let error_step_string = 
    function 
    | Pretyping -> "Pretyping"
    | Typing -> "Typing"
    | Normalization -> "Normalization"
    | SctCheck -> "SctCheck"

  let raise_warn error = 
    let _message = error.build error.payload in
    let _step = error.step in 
    (* Printf.printf "[Warn][%s][%s] at %a :@.%s" (error.code) (error.location) (error_step_string step) message  (*Should be replaced with call to Logs library or appropriate pretty printer*)
   *)()
  let raise_error error = 
    let _message = error.build error.payload in
    let _step = error.step in 
    (* Printf.printf "[Error][%s][%s] at %a :@.%s" (error.code) (error.location) (error_step_string step) message; (*Should be replaced with call to Logs library or appropriate pretty printer*)
    *)
    raise ExitError 

  let handle_error error handler = 
    match handler.strategy with 
    | TryRecover -> (
      match error.severity with
        | Fatal -> 
          raise_error error
        | Recoverable ->
          raise_warn error
        | Warning -> 
          raise_warn error
      )
    | Stop -> 
      match error.severity with
      | Warning -> 
        raise_warn error
      | _ -> raise_error error
      
end

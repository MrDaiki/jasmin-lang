open Annotations

open Error_handler



let build_unknown_var_error (x:Annotations.symbol) (loc): Annotations.symbol compile_error =
  {
    severity = Fatal;
    step = Pretyping;
    code = "E1001";
    payload = x;
    location = loc;
    build = (fun x -> Printf.sprintf "Unknown variable %s" x)
  }


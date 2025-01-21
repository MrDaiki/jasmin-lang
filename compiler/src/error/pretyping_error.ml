open Annotations

open Error_handler
open Pretyping


let build_unknown_var_error (x:Annotations.symbol) (loc): Annotations.symbol compile_error =
  {
    severity = Fatal;
    step = Pretyping;
    code = "E1001";
    payload = x;
    location = loc;
    build = (fun x -> Format.sprintf "unknown variable: `%s'" x)
  }

let build_unknown_fun_error (x:Annotations.symbol) (loc): Annotations.symbol compile_error =
  {
    severity = Fatal;
    step = Pretyping;
    code = "E1002";
    payload = x;
    location = loc;
    build = (fun x -> Format.asprintf "unknown function: `%s'" x)
  }

let build_invalid_type_error (ty, p) (loc) =
  {
    severity = Fatal;
    step = Pretyping;
    code = "E1003";
    payload = (ty, p);
    location = loc;
    build = (fun (ty, p) -> Format.asprintf "the expression has type %a instead of %a" Printer.pp_ptype ty Pretyping.pp_typat p)
  }

let build_type_mismatch_error (t1,t2) loc  = 
  {
    severity = Fatal;
    step = Pretyping;
    code = "E1004";
    payload = (t1,t2);
    location = loc;
    build = (fun (t1,t2) -> Format.asprintf "the expression has type %a instead of %a" Printer.pp_ptype t1 Printer.pp_ptype t2)
  }

let build_invalid_cast_error (t1,t2) loc  = 
  {
    severity = Fatal;
    step = Pretyping;
    code = "E1005";
    payload = (t1,t2);
    location = loc;
    build = (fun (t1,t2) -> Format.asprintf "can not implicitly cast %a into %a" Printer.pp_ptype t1 Printer.pp_ptype t2)
  }
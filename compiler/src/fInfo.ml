open Annotations

type returnaddress_kind =
  | OnStack
  | OnReg

type f_annot = {
    retaddr_kind          : returnaddress_kind option;
    stack_allocation_size : Z.t option;
    stack_size            : Z.t option;
    stack_align           : wsize option;
    max_call_depth        : Z.t option;
    stack_zero_strategy   : (Stack_zero_strategy.stack_zero_strategy * wsize option) option;
    f_user_annot          : annotations;
}

let f_annot_empty = {
    retaddr_kind          = None;
    stack_allocation_size = None;
    stack_size            = None;
    stack_align           = None;
    max_call_depth        = None;
    stack_zero_strategy   = None;
    f_user_annot          = [];
  }

type arg_ret_info = { returned_params : int option list }
(** When a non-inlined function returns a `ptr` array, it has to be one of its
    arguments. [returned_params] associates to each return value the index of
    the corresponding argument if it is a `ptr` array, and [None] otherwise. *)

type call_conv =
  | Export of arg_ret_info  (** The function should be exported to the outside word *)
  | Subroutine of arg_ret_info (** internal function that should not be inlined *)
  | Internal  (** internal function that should be inlined *)

let is_subroutine = function
  | Subroutine _ -> true
  | _            -> false

let is_export = function
  | Export _ -> true
  | _ -> false

(* ------------------------------------------------------------------------ *)
type return_info = {
    ret_annot : Annotations.annotations list;
    (* annotation attached to return type *)
    ret_loc : Location.t; (* location of the return statement *)
  }

(* ------------------------------------------------------------------------ *)
type t =
  Location.t * f_annot * call_conv * return_info

let entry_info (fi: t) : IInfo.t =
  let (fl, _, _, _) = fi in (Location.i_loc0 fl, [])

let ret_info (fi: t) : IInfo.t =
  let (_, _, _, ri) = fi in (Location.i_loc0 ri.ret_loc, [])

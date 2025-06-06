From mathcomp Require Import ssreflect ssrfun ssrbool.
From mathcomp Require Import word_ssrZ.

Require Import
  psem
  shift_kind.
Require Import
  arch_utils
  sem_params_of_arch_extra.
Require Import
  arm_decl
  arm_extra
  arm_instr_decl.

Lemma ignore_has_shift mn sf ic hs hs' :
  mn \notin has_shift_mnemonics
  -> let opts :=
       {| set_flags := sf; is_conditional := ic; has_shift := hs; |}
     in
     let opts' :=
       {| set_flags := sf; is_conditional := ic; has_shift := hs'; |}
     in
     mn_desc opts mn = mn_desc opts' mn.
Proof. by case: mn. Qed.

(* TODO_ARM: It seems like we need to characterize conditional execution,
   but the variable number of arguments makes it very cumbersome.
   This gets multiplied if they set flags or have shifts. *)

Section WITH_PARAMS.

Context
  {wsw : WithSubWord}
  {atoI : arch_toIdent}
  {syscall_state : Type}
  {sc_sem : syscall_sem syscall_state}
  {pT : progT}
  {sCP : semCallParams}.

Definition truncate_args
  (op : sopn) (vargs : seq value) : exec (seq value) :=
  mapM2 ErrType truncate_val (sopn_tout op) vargs.

Definition cast_op tin1 tin2 tout1 tout2 (h1 : tin1 = tin2) (h2 : tout1 = tout2)
  (f : sem_prod tin1 (exec (sem_tuple tout1))) :
    sem_prod tin2 (exec (sem_tuple tout2)) :=
   eq_rect tout1 (fun tout => sem_prod tin2 (exec (sem_tuple tout)))
     (eq_rect tin1 (fun tin => sem_prod tin (exec (sem_tuple tout1))) f tin2 h1)
     tout2 h2.

Lemma sem_prod_ok_comp A B ts (F : sem_prod ts A) (G:A -> B) vs:
  app_sopn ts (sem_prod_ok ts (sem_prod_app F G)) vs =
  Let a := app_sopn ts (sem_prod_ok ts F) vs in ok (G a).
Proof.
  elim: ts vs F => [ | t ts hrec] [ | v vs] //= F; case: of_val => //=.
Qed.

Lemma exec_sopn_conditional mn sf osk b vargs vprev vres0 vres1 :
  let opts :=
    {| set_flags := sf; is_conditional := false; has_shift := osk; |}
  in
  let op := Oarm (ARM_op mn opts) in
  truncate_args op vprev = ok vres1
  -> exec_sopn op vargs = ok vres0
  -> exec_sopn
       (Oarm (ARM_op mn (set_is_conditional opts)))
       (vargs ++ Vbool b :: vprev)
       = ok (if b then vres0 else vres1).
Proof.
  rewrite /= /exec_sopn /= /set_is_conditional /= /sem_sopn /= /sopn_sem /= /sopn_sem_ /=.
  t_xrbindP.
  set fflags := {| set_flags := sf; is_conditional := false; has_shift := osk |}.
  set tflags := {| set_flags := sf; is_conditional := true; has_shift := osk |}.
  have : id_valid (mn_desc fflags mn) = id_valid (mn_desc tflags mn) /\
         exists (h1 : id_tin (mn_desc fflags mn) = id_tin (mn_desc tflags mn))
                (h2 : id_tout (mn_desc fflags mn) = id_tout (mn_desc tflags mn)),
         cast_op h1 h2 (id_semi (mn_desc fflags mn)) = id_semi (mn_desc tflags mn).
  + by rewrite /fflags /tflags; case: mn; case sf; case osk => [s | ]; split => //;
         exists erefl, erefl.
  move=> [-> [hin [hout hcast]]].
  rewrite /truncate_args /= /sopn_tout /=.
  move=> htr _ -> <- res hres <- /=.
  move: (id_tin (mn_desc fflags mn)) (id_tin (mn_desc tflags mn))
        (id_tout (mn_desc fflags mn)) (id_tout (mn_desc tflags mn))
        (id_semi (mn_desc fflags mn)) (id_semi (mn_desc tflags mn))
        hin hout hcast htr res hres => {fflags tflags vres0}.
  move=> tin tin2 tout tout2 semi semi2 ???; subst tin2 tout2 semi2 => /= hvres1.
  rewrite /mk_semi_cond.
  elim: tin vargs semi => [ | ti tin hreci] [ | va vargs] //=.
  + move=> semi res ->; rewrite /mk_semi_cond /= => {semi}.
    rewrite add_arguments_nil; case: b.
    + have -> // : app_sopn tout (sem_prod_const tout (ok res)) vprev = ok res.
      move: (sem_tuple tout) res => A res.
      elim: tout vprev vres1 hvres1 => [ | to tout hreco] [ | vp vprev] //=; t_xrbindP.
      move=> vres1 v; rewrite /truncate_val; t_xrbindP.
      by move=> vto -> _ vs /hreco ->.
    rewrite -hvres1 => {hvres1 vres1 res}.
    elim: tout vprev => [ | to tout hreco] [ | vp vps] //=.
    rewrite -hreco /truncate_val => {hreco}.
    case: of_val => //= vt.
    case: tout; first by move=> //=; case:vps => //; case: to vt.
    move=> t1 ts; rewrite sem_prod_ok_comp.
    case: app_sopn => //= tl.
    have -> : oto_val (sem_prod_id vt) = to_val vt.
    + by case: to vt.
    by case: ts tl.
  move=> semi res; t_xrbindP => v -> /hreci /=.
  by rewrite add_arguments_app.
Qed.

(* TODO_ARM: Is this the best way of expressing the [write_val] condition? *)
Lemma sem_i_conditional
  {dc : DirectCall} (p : prog)
  ev s0 s1 mn sf osk ii lvs tag args c prev vargs b vprev vprev' vres :
  let opts :=
    {| set_flags := sf; is_conditional := false; has_shift := osk; |}
  in
  let aop := Oarm (ARM_op mn opts) in
  sem_pexprs true (p_globs p) s0 args = ok vargs
  -> sem_pexpr true (p_globs p) s0 c = ok (Vbool b)
  -> sem_pexprs true (p_globs p) s0 prev = ok vprev
  -> truncate_args aop vprev = ok vprev'
  -> exec_sopn aop vargs = ok vres
  -> (if b
      then write_lvals true (p_globs p) s0 lvs vres = ok s1
      else write_lvals true (p_globs p) s0 lvs vprev' = ok s1)
  -> let aop' := Oarm (ARM_op mn (set_is_conditional opts)) in
     let ir := Copn lvs tag aop' (args ++ c :: prev) in
     esem_i p ev (MkI ii ir) s0 = ok s1.
Proof.
  move=> opts aop hsemargs hsemc hsemprev htruncprev hexec hwrite.

  rewrite /= /sem_sopn /=.
  rewrite /sem_pexprs mapM_cat /= -2![mapM _ _]/(sem_pexprs _ _ _ _).
  rewrite hsemargs hsemc hsemprev {hsemargs hsemc hsemprev} /=.

  case: b hwrite => hwrite.
  all: rewrite (exec_sopn_conditional _ htruncprev hexec) {htruncprev hexec} /=.
  all: exact: hwrite.
Qed.

End WITH_PARAMS.

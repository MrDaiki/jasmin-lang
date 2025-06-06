open Glob_options
open Utils
open Prog
open X86_decl

module Core_arch_ARM : Arch_full.Core_arch = Arm_arch_full.Arm (struct
  let call_conv = Arm_decl.arm_linux_call_conv
end)

module Core_arch_RISCV : Arch_full.Core_arch = Riscv_arch_full.Riscv (struct
  let call_conv = Riscv_decl.riscv_linux_call_conv
end)

let core_arch_x86 ~use_lea ~use_set0 call_conv :
    (module Arch_full.Core_arch
       with type reg = register
        and type regx = register_ext
        and type xreg = xmm_register
        and type rflag = rflag
        and type cond = condt
        and type asm_op = X86_instr_decl.x86_op
        and type extra_op = X86_extra.x86_extra_op) =
  let module Lowering_params = struct
    let call_conv =
      match call_conv with
      | Linux -> x86_linux_call_conv
      | Windows -> x86_windows_call_conv

    let lowering_opt =
      let open X86_lowering in
      { use_lea; use_set0 }
  end in
  (module X86_arch_full.X86 (Lowering_params))

let get_arch_module arch call_conv : (module Arch_full.Arch) =
  (module Arch_full.Arch_from_Core_arch
            ((val match arch with
                  | X86_64 ->
                      (module (val core_arch_x86 ~use_lea:!Glob_options.lea
                                     ~use_set0:!Glob_options.set0 call_conv)
                      : Arch_full.Core_arch)
                  | ARM_M4 -> (module Core_arch_ARM : Arch_full.Core_arch)
                  | RISCV -> (module Core_arch_RISCV : Arch_full.Core_arch))))

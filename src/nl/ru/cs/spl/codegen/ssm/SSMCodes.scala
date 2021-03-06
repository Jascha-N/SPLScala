/*
 * Copyright (c) 2013 Jascha Neutelings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package nl.ru.cs.spl.codegen.ssm

object SSMCodes {
  /*
    ldc_descr=Load Constant. Pushes the inline constant on the stack.
    ldc_prepost=SP_post = SP_pre + 1, M_post[SP_post] = M_pre[PC_pre+1]
    ldc_example=@stl_example
  */
  def ldc(constant: SSMConstant) = s"ldc $constant"

  /*
    lds_descr=Load from Stack. Pushes a value relative to the top of the stack.
    lds_prepost=SP_post = SP_pre + 1, M_post[SP_post] = M_pre[SP_pre + M_pre[PC_pre+1]]
    lds_example=lds -1 ; multiply and leave on stack, ldc 2, mul
  */
  def lds(displ: SSMValue) = s"lds $displ"

  /*
    ldms_descr=Load Multiple from Stack. Pushes values relative to the top of the stack. Same as single load variant but second inline parameter is size.
    ldms_prepost=displ = M_pre[PC_pre + 1], size = M_pre[PC_pre + 2], SP_post = SP_pre + size, M_post[SP_post - size + 1 .. SP_post] = M_pre[SP_pre + displ .. SP_pre + displ + size - 1]
    ldms_example=ldms -1 2; multiply and leave on stack, mul
  */
  def ldms(displ: SSMValue, size: SSMValue) = s"ldms $displ $size"

  /*
    sts_descr=Store into Stack. Pops a value from the stack and stores it in a location relative to the top of the stack.
    sts_prepost=SP_post = SP_pre - 1, M_post[SP_pre + M_pre[PC_pre+1]] = M_pre[SP_pre]
    sts_example=lds -1 ; substract and store in stack, ldc 2, sub, sts -2
  */



  /*
    stms_descr=Store Multiple into Stack. Pops values from the stack and stores it in a location relative to the top of the stack. Same as single store variant but second inline parameter is size.
    stms_prepost=displ = M_pre[PC_pre + 1], size = M_pre[PC_pre + 2], SP_post = SP_pre - size, M_post[SP_pre + displ .. SP_pre + displ + size - 1] = M_pre[SP_post + 1 .. SP_post + size]
    stms_example=lds -1 ; substract and store in stack, ldc 2, sub, stms -2 1 ; equivalent to sts -2
  */

  /*
    ldsa_descr=Load Stack Address. Pushes the address of a value relative to the stackpointer.
    ldsa_prepost=SP_post = SP_pre + 1, M_post[SP_post] = SP_pre + M_pre[PC_pre+1]
    ldsa_example=ldsa -2 ; update value on stack using its address, ldc 5, sta 0
  */

  /*
    ldl_descr=Load Local. Pushes a value relative to the markpointer.
    ldl_prepost=SP_post = SP_pre + 1, M_post[SP_post] = M_pre[MP_pre + M_pre[PC_pre+1]]
    ldl_example=ldl -1 ; divide and leave on stack, ldc 3, div
  */

  /*
    ldml_descr=Load Multiple Local. Pushes values relative to the markpointer. Same as single load variant but second inline parameter is size.
    ldml_prepost=displ = M_pre[PC_pre + 1], size = M_pre[PC_pre + 2], SP_post = SP_pre + size, M_post[SP_post - size + 1 .. SP_post] = M_pre[MP_pre + displ .. MP_pre + displ + size - 1]
    ldml_example=ldml -1 2 ; divide and leave on stack, ldc 3, div
  */

  /*
    stl_descr=Store Local. Pops a value from the stack and stores it in a location relative to the markpointer.
    stl_prepost=SP_post = SP_pre - 1, M_post[MP_pre + M_pre[PC_pre+1]] = M_pre[SP_pre]
    stl_example=ldl 2 ; increment local var, ldc 1, add, stl 2
  */

  /*
    stml_descr=Store Multiple Local. Pops values from the stack and stores it in a location relative to the markpointer. Same as single store variant but second inline parameter is size.
    stml_prepost=displ = M_pre[PC_pre + 1], size = M_pre[PC_pre + 2], SP_post = SP_pre - size, M_post[MP_pre + displ .. MP_pre + displ + size - 1] = M_pre[SP_post + 1 .. SP_post + size]
    stml_example=ldl 2 ; increment local var, ldc 1, add, stml 2 1 ; equivalent to stl 2
  */

  /*
    ldla_descr=Load Local Address. Pushes the address of a value relative to the markpointer.
    ldla_prepost=SP_post = SP_pre + 1, M_post[SP_post] = MP_pre + M_pre[PC_pre+1]
    ldla_example=ldla -2 ; update local using its address, ldc 5, sta 0
  */

  /*
    lda_descr=Load via Address. Dereferencing. Pushes the value pointed to by the value at the top of the stack. The pointer value is offset by a constant offset.
    lda_prepost=SP_post = SP_pre, M_post[SP_post] = M_pre[M_pre[SP_pre] + M_pre[PC_pre+1]]
    lda_example=ldla -2 ; a different way of doing ldl -2, lda 0
  */

  /*
    ldma_descr=Load Multiple via Address. Pushes values relative to by the value at the top of the stack. Same as single load variant but second inline parameter is size.
    ldma_prepost=displ = M_pre[PC_pre + 1], size = M_pre[PC_pre + 2], SP_post = SP_pre + size - 1, M_post[SP_post - size + 1 .. SP_post] = M_pre[M_pre[SP_pre] + displ .. M_pre[SP_pre] + displ + size - 1]
    ldma_example=none
  */

  /*
    ldaa_descr=Load Address of Address. Pushes the address of a value relative to the address on top of the stack. This instruction effectively adds a constant to the top of the stack.
    ldaa_prepost=SP_post = SP_pre + 1, M_post[SP_post] = M_pre[SP_pre] + M_pre[PC_pre+1]
    ldaa_example=ldaa -2
  */

  /*
    sta_descr=Store via Address. Pops 2 values from the stack and stores the second popped value in the location pointed to by the first. The pointer value is offset by a constant offset.
    sta_prepost=SP_post = SP_pre - 2, M_post[M_pre[SP_pre] + M_pre[PC_pre+1]] = M_pre[SP_pre-1]
    sta_example=@ldla_example
  */

  /*
    stma_descr=Store Multiple via Address. Pops values from the stack and stores it in a location relative to the value at the top of the stack. Same as single store variant but second inline parameter is size.
    stma_prepost=displ = M_pre[PC_pre + 1], size = M_pre[PC_pre + 2], SP_post = SP_pre - size - 1, M_post[M_pre[SP_pre] + displ .. M_pre[SP_pre] + displ + size - 1] = M_pre[SP_post + 1 .. SP_post + size]
    stma_example=none
  */

  /*
    ldr_descr=Load Register. Pushes a value from a register. Registers 0, 1, 2 and 3 are called PC (programcounter), SP (stackpointer), MP (markpointer) and RR (return register) respectively.
    ldr_prepost=SP_post = SP_pre + 1, M_post[SP_post] = REG_pre[ M_pre[PC_pre+1] ]
    ldr_example=ldr RR ; decrement register, ldc 1, sub, str RR
  */

  /*
    ldrr_descr=Load Register from Register. Copy the content of the second register to the first. Does not affect the stack.
    ldrr_prepost=REG_post[ M_pre[PC_pre+1] ] = REG_pre[ M_pre[PC_pre+2] ]
    ldrr_example=ldrr SP MP ; SP <- MP
  */

  /*
    str_descr=Store Register. Pops a value from the stack and stores it in a location relative to the markpointer. See also ldr.
    str_prepost=SP_post = SP_pre - 1, REG_post[ M_pre[PC_pre+1] ] = M_pre[SP_pre]
    str_example=@ldr_example
  */

  /*
    swp_descr=Swap values. Swaps the 2 topmost values on the stack.
    swp_prepost=SP_post = SP_pre, M_post[SP_post] = M_pre[SP_pre-1], M_post[SP_post-1] = M_pre[SP_pre]
    swp_example=ldc 1 ; variant for ldc 2 followed by ldc 1, ldc 2, swp
  */

  /*
    swpr_descr=Swap Register. Swaps the content of a register with the top of the stack.
    swpr_prepost=SP_post = SP_pre, M_post[SP_post] = REG_pre[ M_pre[PC_pre+1] ], REG_post[ M_pre[PC_pre+1] ] = M_pre[SP_pre]
    swpr_example=
  */

  /*
    swprr_descr=Swap 2 Registers. Swaps the content of a register with another register.
    swprr_prepost=REG_post[ M_pre[PC_pre+1] ] = REG_pre[ M_pre[PC_pre+2] ], REG_post[ M_pre[PC_pre+2] ] = REG_pre[ M_pre[PC_pre+1] ]
    swprr_example=swprr MP R7 ; swap MP with scratch register
  */

  /*
    ajs_descr=Adjust Stack. Adjusts the stackpointer with fixed amount.
    ajs_prepost=SP_post = SP_pre + M_post[PC_pre+1]
    ajs_example=ajs -2 ;lower stack by 2
  */

  /*
    add_descr=Addition. Replaces 2 top stack values with the addition of those values.
    add_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] + M_pre[SP_pre]
    add_example=@stl_example
  */

  /*
    mul_descr=Multiplication. Replaces 2 top stack values with the multiplication of those values.
    mul_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] * M_pre[SP_pre]
    mul_example=@ld_example
  */

  /*
    sub_descr=Substraction. Replaces 2 top stack values with the subtraction of those values.
    sub_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] - M_pre[SP_pre]
    sub_example=@st_example
  */

  /*
    div_descr=Division. Replaces 2 top stack values with the division of those values.
    div_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] / M_pre[SP_pre]
    div_example=@ldl_example
  */

  /*
    mod_descr=Division. Replaces 2 top stack values with the modulo of those values.
    mod_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] % M_pre[SP_pre]
    mod_example=ldl -2 ; x = x % y, ldl -3, mod, stl -2
  */

  /*
    and_descr=And. Replaces 2 top stack values with the bitwise and of those values.
    and_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] & M_pre[SP_pre]
    and_example=ldc 0xFF00 ; variant of ldc 0xF000, ldc 0xF0F0, and
  */

  /*
    or_descr=Or. Replaces 2 top stack values with the bitwise or of those values.
    or_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] | M_pre[SP_pre]
    or_example=ldc 0xFF00 ; variant of ldc 0xFFFF, ldc 0xF0F0, or
  */

  /*
    xor_descr=Exclusive Or. Replaces 2 top stack values with the bitwise exclusive or of those values.
    xor_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] ^ M_pre[SP_pre]
    xor_example=ldc 0xFF00 ; variant of ldc 0x0FF0, ldc 0xF0F0, xor
  */

  /*
    eq_descr=Test for equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with beq.
    eq_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] == M_pre[SP_pre]
    eq_example=ldc 2, ldc 3, eq, brf FalseAction
  */

  /*
    ne_descr=Test for not equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with bne.
    ne_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] != M_pre[SP_pre]
    ne_example=ldc 2, ldc 3, ne, brf FalseAction
  */

  /*
    lt_descr=Test for less then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with blt.
    lt_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] < M_pre[SP_pre]
    lt_example=ldc 2, ldc 3, lt, brf FalseAction
  */

  /*
    le_descr=Test for less or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with ble.
    le_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] <= M_pre[SP_pre]
    le_example=ldc 2, ldc 3, lr, brf FalseAction
  */

  /*
    gt_descr=Test for greater then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with bgt.
    gt_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] > M_pre[SP_pre]
    gt_example=ldc 2, ldc 3, gt, brf FalseAction
  */

  /*
    ge_descr=Test for greater or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as 1. Used in combination with brf. This is a variant of cmp combined with bge.
    ge_prepost=SP_post = SP_pre - 1, M_post[SP_post] = M_pre[SP_pre - 1] >= M_pre[SP_pre]
    ge_example=ldc 2, ldc 3, ge, brf FalseAction
  */

  /*
    neg_descr=Negation. Replaces top stack values with the (integer) negative of the value.
    neg_prepost=SP_post = SP_pre, M_post[SP_post] = - M_pre[SP_pre]
    neg_example=ldc 1 ; variant of ldc -1, neg
  */

  /*
    not_descr=Not. Replaces top stack values with the bitwise complement of the value.
    not_prepost=SP_post = SP_pre, M_post[SP_post] = ~ M_pre[SP_pre]
    not_example=ldc 0x0000FFFF ; variant of ldc 0xFFFF0000, not
  */

  /*
    bsr_descr=Branch to subroutine. Pushes the PC on the stack and jumps to the subroutine.
    bsr_prepost=SP_post = SP_pre + 1, M_post[SP_post] = PC_pre + 2, PC_post = PC_pre + M_pre[PC_pre + 1] + 2
    bsr_example=bra main, subroutine ldc 1, ldc 2, add, str RR, ret, main: bsr subroutine, ldr RR, ...
  */

  /*
    bra_descr=Branch Allways. Jumps to the destination. Replaces the PC with the destination address.
    bra_prepost=PC_post = PC_pre + M_pre[PC_pre + 1] + 2
    bra_example=@bsr_example
  */

  /*
    brf_descr=Branch on False. If a False value is on top of the stack, jump to the destination.
    brf_prepost=SP_post = SP_pre - 1, PC_post = PC_pre + M_pre[PC_pre + 1] + 2 (if false on top of the stack)
    brf_example=@eq_example
  */

  /*
    brt_descr=Branch on True. If a True value is on top of the stack, jump to the destination.
    brt_prepost=SP_post = SP_pre - 1, PC_post = PC_pre + M_pre[PC_pre + 1] + 2 (if true on top of the stack)
    brt_example=
  */

  /*
    jsr_descr=Jump to subroutine. Pops a destination from the stack, pushes the PC on the stack and jumps to the destination.
    jsr_prepost=SP_post = SP_pre, PC_post = M_pre[SP_pre], M_post[SP_post] = PC_pre + 1
    jsr_example=bra main, subroutine ldc 1, ldc 2, add, str RR, ret, main: ldc subroutine, jsr, ldr RR, ...
  */

  /*
    ret_descr=Return from subroutine. Pops a previously pushed PC from the stack and jumps to it.
    ret_prepost=SP_post = SP_pre - 1, PC_post = M_pre[SP_pre]
    ret_example=bra main, subroutine ldc 1, ldc 2, add, str RR, ret, main: bsr subroutine, ldr RR, ...
  */

  /*
    link_descr=Reserve memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.
    link_prepost=MP_post = SP_pre  + 1, M_post[MP_post] = MP_pre, SP_post = MP_post + M_pre[PC_pre+1]
    link_example=bra main, subroutine link 2 ; reserve for 2 locals, ldc 1, ldc 2, add, stl 1 ; store in 2nd local, ldl 1, str RR, unlink, ret, main: bsr subroutine, ldr RR, ...
  */

  /*
    unlink_descr=Free memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.
    unlink_prepost=MP_post = M_pre[MP_pre], SP_post = MP_pre - 1
    unlink_example=@link_example
  */

  /*
    nop_descr=No operation. Well, guess what...
    nop_prepost=
    nop_example=nop
  */

  /*
    halt_descr=Halt execution. Machine stops executing instructions.
    halt_prepost=
    halt_example=halt
  */

  /*
    trap_descr=Trap to environment function. Trap invokes a systemcall, which one is determined by its argument. Currently just 1 call exists, print the topmost element on the stack as an integer in the output window.
    trap_prepost=
    trap_example=ldc 5, trap 0 ; print 5 on output
  */

  /*
    annote_descr=Annotate. A meta instruction (not producing code), annotating the stack display in the user interface with text and color. Annote takes 5 arguments, (1) a register name, (2) a low offset w.r.t. the register (used as starting point for annotating), (3) a high offset, (4) a color, (5) text. Color can be one of {black, blue, cyan, darkGray, gray, green, lightGray, magenta, orange, pink, red, yellow}. Text including spaces need to be enclosed in double quotes. The annote instruction is tied to the preceding (non-meta) instruction and will be performed immediately after the execution of that instruction.
    annote_prepost=
    annote_example=annote SP -1 0 red "Pushed constants" ; annote top 2 stack values
  */

  /*
    ldh_descr=Load from Heap. Pushes a value pointed to by the value at the top of the stack. The pointer value is offset by a constant offset.
    ldh_prepost=
    ldh_example=ldc 5, sth, ldh 0
  */

  /*
    ldmh_descr=Load Multiple from Heap. Pushes values pointed to by the value at the top of the stack. The pointer value is offset by a constant offset. Same as single load variant but the second inline parameter is size.
    ldmh_prepost=
    ldmh_example=ldc 1, ldc 2, ldc 3, stmh 3, ldmh 0 3
  */

  /*
    sth_descr=Store into Heap. Pops 1 value from the stack and stores it into the heap. Pushes the heap address of that value on the stack.
    sth_prepost=
    sth_example=ldc 5, sth
  */

  /*
    stmh_descr=Store Multiple into Heap. Pops values from the stack and stores it into the heap, retaining the order of the values. Same as single store variant but the inline parameter is size. Pushes the heap address of the last value on the stack.
    stmh_prepost=
    stmh_example=ldc 1, ldc 2, ldc 3, stmh 3
  */

}

trait SSMRegister
case object PC extends SSMRegister
case object SP extends SSMRegister
case object MP extends SSMRegister
case object HP extends SSMRegister
case object RR extends SSMRegister

trait SSMConstant

case class SSMLabel(name: String) extends SSMConstant {
  override def toString = name
}

case class SSMValue(value: Int) extends SSMConstant {
  override def toString = value.toString
}

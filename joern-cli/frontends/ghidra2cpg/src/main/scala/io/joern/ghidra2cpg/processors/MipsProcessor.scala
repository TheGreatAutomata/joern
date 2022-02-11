package io.joern.ghidra2cpg.processors
import scala.collection.immutable._

class MipsProcessor extends Processor {
  override def getInstructions: HashMap[String, String] =
    HashMap(
      "_add.D"     -> "<operator>.incBy",
      "_addiu"     -> "<operator>.incBy",
      "_addu"      -> "<operator>.incBy",
      "_and"       -> "<operator>.and",
      "_andi"      -> "<operation>.and",
      "_c.le.D"    -> "<operator>.TODO",
      "_c.lt.D"    -> "<operator>.TODO",
      "_clear"     -> "<operator>.assignment",
      "_clz"       -> "<operator>.assignment",
      "_cvt.d.S"   -> "<operator>.TODO",
      "_cvt.d.W"   -> "<operator>.TODO",
      "_cvt.s.D"   -> "<operator>.TODO",
      "_div"       -> "<operator>.assignmentDivision",
      "_div.D"     -> "<operator>.assignmentDivision",
      "_divu"      -> "<operator>.assignmentDivision",
      "_ext"       -> "<operator>.assignment",
      "_ins"       -> "<operator>.assignment",
      "_lb"        -> "<operator>.assignment",
      "_lbu"       -> "<operator>.assignment",
      "_ldc1"      -> "<operator>.assignment",
      "_ldxc1"     -> "<operator>.assignment",
      "_lh"        -> "<operator>.assignment",
      "_lhu"       -> "<operator>.assignment",
      "_li"        -> "<operator>.assignment",
      "_lui"       -> "<operator>.assignment",
      "_lw"        -> "<operator>.assignment",
      "_lwc1"      -> "<operator>.assignment",
      "_lwl"       -> "<operator>.assignment",
      "_lwr"       -> "<operator>.assignment",
      "_madd"      -> "<operator>.incBy",
      "_madd.D"    -> "<operator>.incBy",
      "_mfc1"      -> "<operator>.assignment",
      "_mfhi"      -> "<operator>.assignment",
      "_mflo"      -> "<operator>.assignment",
      "_mov.D"     -> "<operator>.assignment",
      "_mov.S"     -> "<operator>.assignment",
      "_move"      -> "<operator>.assignment",
      "_movf"      -> "<operator>.assignment",
      "_movf.D"    -> "<operator>.assignment",
      "_movn"      -> "<operator>.assignment",
      "_movn.D"    -> "<operator>.assignment",
      "_movt.D"    -> "<operator>.assignment",
      "_movz"      -> "<operator>.assignment",
      "_mtc1"      -> "<operator>.assignment",
      "_mtlo"      -> "<operator>.assignment",
      "_mul"       -> "<operator>.assignment",
      "_mul.D"     -> "<operator>.assignment",
      "_mult"      -> "<operator>.assignment",
      "_multu"     -> "<operator>.assignment",
      "_nop"       -> "<operator>.NOP",
      "_nor"       -> "<operator>.nor",
      "_or"        -> "<operator>.or",
      "_ori"       -> "<operator>.or",
      "_rotr"      -> "<operator>.rotateRight",
      "_sb"        -> "<operator>.assignment",
      "_sdc1"      -> "<operator>.TODO",
      "_sdxc1"     -> "<operator>.TODO",
      "_seb"       -> "<operator>.TODO",
      "_seh"       -> "<operator>.TODO",
      "_sh"        -> "<operator>.assignment",
      "_sll"       -> "<operator>.assignmentShiftLeft",
      "_sllv"      -> "<operator>.assignmentShiftLeft",
      "_slt"       -> "<operator>.TODO",
      "_slti"      -> "<operator>.assignment",
      "_sltiu"     -> "<operator>.assignment",
      "_sltu"      -> "<operator>.assignment",
      "_sra"       -> "<operator>.TODO",
      "_srav"      -> "<operator>.TODO",
      "_srl"       -> "<operator>.assignmentLogicalShiftRight",
      "_srlv"      -> "<operator>.assignmentLogicalShiftRight",
      "_sub.D"     -> "<operator>.decBy",
      "_subu"      -> "<operator>.decBy",
      "_sw"        -> "<operator>.assignment",
      "_swc1"      -> "<operator>.assignment",
      "_swr"       -> "<operator>.TODO",
      "_trunc.w.D" -> "<operator>.TODO",
      "_wsbh"      -> "<operator>.TODO",
      "_xor"       -> "<operator>.assignmentXor",
      "_xori"      -> "<operator>.assignmentXor",
      "add"        -> "<operator>.incBy",
      "add.D"      -> "<operator>.incBy",
      "add.S"      -> "<operator>.incBy",
      "addi"       -> "<operator>.incBy",
      "addiu"      -> "<operator>.incBy",
      "addu"       -> "<operator>.incBy",
      "and"        -> "<operator>.and",
      "andi"       -> "<operator>.and",
      "b"          -> "<operator>.goto",
      "bal"        -> "CALL",
      "bc1f"       -> "<operator>.goto",
      "bc1fl"      -> "<operator>.goto",
      "bc1t"       -> "<operator>.goto",
      "bc1tl"      -> "<operator>.goto",
      "beq"        -> "<operator>.goto",
      "beql"       -> "<operator>.goto",
      "bgez"       -> "<operator>.goto",
      "bgezl"      -> "<operator>.goto",
      "bgtz"       -> "<operator>.goto",
      "bgtzl"      -> "<operator>.goto",
      "blez"       -> "<operator>.goto",
      "blezl"      -> "<operator>.goto",
      "bltz"       -> "<operator>.goto",
      "bltzl"      -> "<operator>.goto",
      "bne"        -> "<operator>.goto",
      "bnel"       -> "<operator>.goto",
      "break"      -> "<operator>.TODO",
      "c.eq.D"     -> "<operator>.TODO",
      "c.eq.S"     -> "<operator>.TODO",
      "c.le.D"     -> "<operator>.TODO",
      "c.le.S"     -> "<operator>.TODO",
      "c.lt.D"     -> "<operator>.TODO",
      "c.lt.S"     -> "<operator>.TODO",
      "cdqe"       -> "<operator>.TODO",
      "cfc1"       -> "<operator>.TODO",
      "clear"      -> "<operator>.clear",
      "clz"        -> "<operator>.assignment",
      "cmova"      -> "<operator>.assignment",
      "cmovbe"     -> "<operator>.assignment",
      "cmovns"     -> "<operator>.assignment",
      "cmovnz"     -> "<operator>.assignment",
      "cmovz"      -> "<operator>.assignment",
      "cmp"        -> "<operator>.TODO",
      "ctc1"       -> "<operator>.TODO",
      "cvt.d.S"    -> "<operator>.TODO",
      "cvt.d.W"    -> "<operator>.TODO",
      "cvt.s.D"    -> "<operator>.TODO",
      "cvt.s.W"    -> "<operator>.TODO",
      "cvt.w.D"    -> "<operator>.TODO",
      "div"        -> "<operator>.assignmentDivision",
      "div.D"      -> "<operator>.assignmentDivision",
      "div.S"      -> "<operator>.assignmentDivision",
      "divu"       -> "<operator>.assignmentDivision",
      "ext"        -> "<operator>.assignment",
      "hlt"        -> "<operator>.TODO",
      "imul"       -> "<operator>.multiplication",
      "ins"        -> "<operator>.assignment",
      "j"          -> "<operator>.goto",
      "jal"        -> "CALL",
      "jalr"       -> "CALL",
      "jbe"        -> "<operator>.goto",
      "jc"         -> "<operator>.goto",
      "jg"         -> "<operator>.goto",
      "jle"        -> "<operator>.goto",
      "jmp"        -> "<operator>.goto",
      "jnc"        -> "<operator>.goto",
      "jnz"        -> "<operator>.goto",
      "jr"         -> "RET",
      "jz"         -> "<operator>.goto",
      "lb"         -> "<operator>.assignment",
      "lbu"        -> "<operator>.assignment",
      "ldc1"       -> "<operator>.assignment",
      "ldxc1"      -> "<operator>.assignment",
      "lea"        -> "<operator>.assignment",
      "lh"         -> "<operator>.assignment",
      "lhu"        -> "<operator>.assignment",
      "li"         -> "<operator>.assignment",
      "ll"         -> "<operator>.assignment",
      "lui"        -> "<operator>.assignment",
      "lw"         -> "<operator>.assignment",
      "lwc1"       -> "<operator>.assignment",
      "lwl"        -> "<operator>.assignment",
      "lwr"        -> "<operator>.assignment",
      "lwxc1"      -> "<operator>.assignment",
      "madd"       -> "<operator>.incBy",
      "madd.D"     -> "<operator>.incBy",
      "maddu"      -> "<operator>.multiplication",
      "mfc1"       -> "<operator>.assignment",
      "mfhi"       -> "<operator>.assignment",
      "mflo"       -> "<operator>.assignment",
      "mov"        -> "<operator>.assignment",
      "mov.D"      -> "<operator>.assignment",
      "mov.S"      -> "<operator>.assignment",
      "move"       -> "<operator>.assignment",
      "movf"       -> "<operator>.assignment",
      "movf.D"     -> "<operator>.assignment",
      "movn"       -> "<operator>.assignment",
      "movn.D"     -> "<operator>.assignment",
      "movsx"      -> "<operator>.assignment",
      "movsxd"     -> "<operator>.assignment",
      "movt.D"     -> "<operator>.assignment",
      "movz"       -> "<operator>.assignment",
      "movz.D"     -> "<operator>.assignment",
      "movzx"      -> "<operator>.assignment",
      "msub"       -> "<operator>.decBy",
      "msub.D"     -> "<operator>.decBy",
      "msubu"      -> "<operator>.decBy",
      "mtc1"       -> "<operator>.TODO",
      "mthi"       -> "<operator>.TODO",
      "mtlo"       -> "<operator>.TODO",
      "mul"        -> "<operator>.multiplication",
      "mul.D"      -> "<operator>.multiplication",
      "mul.S"      -> "<operator>.multiplication",
      "mult"       -> "<operator>.multiplication",
      "multu"      -> "<operator>.multiplication",
      "neg"        -> "<operator>.negation",
      "nop"        -> "<operator>.NOP",
      "nor"        -> "<operator>.assingmentNor",
      "not"        -> "<operator>.assignmentNot",
      "or"         -> "<operator>.assignmentOr",
      "ori"        -> "<operator>.assignmentOr",
      "pop"        -> "<operator>.assignment",
      "pref"       -> "<operator>.TODO",
      "push"       -> "<operator>.assignment",
      "rdhwr"      -> "<operator>.TODO",
      "ret"        -> "<operator>.TODO",
      "rotr"       -> "<operator>.rotateRight",
      "rotrv"      -> "<operator>.rotateRight",
      "sar"        -> "<operator>.TODO",
      "sb"         -> "<operator>.assignment",
      "sc"         -> "<operator>.TODO",
      "sdc1"       -> "<operator>.TODO",
      "sdxc1"      -> "<operator>.TODO",
      "seb"        -> "<operator>.TODO",
      "seh"        -> "<operator>.assignment",
      "setg"       -> "<operator>.TODO",
      "sh"         -> "<operator>.assignment",
      "shl"        -> "<operator>.TODO",
      "shr"        -> "<operator>.TODO",
      "sll"        -> "<operator>.assignmentShiftLeft",
      "sllv"       -> "<operator>.assignmentShiftLeft",
      "slt"        -> "<operator>.assignment",
      "slti"       -> "<operator>.assignment",
      "sltiu"      -> "<operator>.assignment",
      "sltu"       -> "<operator>.assignment",
      "sra"        -> "<operator>.assignmentArithmeticShiftRight",
      "srav"       -> "<operator>.assignmentArithmeticShiftRight",
      "srl"        -> "<operator>.assignmentLogicalShiftRight",
      "srlv"       -> "<operator>.assignmentLogicalShiftRight",
      "stosq.rep"  -> "<operator>.TODO",
      "sub"        -> "<operator>.decBy",
      "sub.D"      -> "<operator>.decBy",
      "sub.S"      -> "<operator>.decBy",
      "subu"       -> "<operator>.decBy",
      "sw"         -> "<operator>.assignment",
      "swc1"       -> "<operator>.TODO",
      "swl"        -> "<operator>.TODO",
      "swr"        -> "<operator>.TODO",
      "sync"       -> "<operator>.TODO",
      "syscall"    -> "<operator>.syscall",
      "teq"        -> "<operator>.TODO",
      "test"       -> "<operator>.TODO",
      "trunc.w.D"  -> "<operator>.TODO",
      "trunc.w.S"  -> "<operator>.TODO",
      "wsbh"       -> "<operator>.assignment",
      "xor"        -> "<operator>.assignmentXor",
      "xori"       -> "<operator>.assignmentXor"
    )
}

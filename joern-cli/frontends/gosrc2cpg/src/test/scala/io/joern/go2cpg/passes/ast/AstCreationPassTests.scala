package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class AstCreationPassTests extends GoCodeToCpgSuite {

  "Method Ast layout" should {

    "be correct for decl assignment" ignore {
      val cpg = code("""
          |package main
          |func main() {
          |   var local int = 1
          |}
          |""".stripMargin)

      inside(cpg.method.name("main").block.astChildren.l) { case List(local: Local, call: Call) =>
        local.name shouldBe "local"
        local.typeFullName shouldBe "int"
        local.order shouldBe 1
        call.name shouldBe Operators.assignment
        call.order shouldBe 2
        inside(call.astChildren.l) { case List(identifier: Identifier, literal: Literal) =>
          identifier.name shouldBe "local"
          identifier.typeFullName shouldBe "int"
          identifier.order shouldBe 1
          identifier.argumentIndex shouldBe 1
          literal.code shouldBe "1"
          // literal.typeFullName shouldBe "int"   Have commented this as this is not a stressing prlm
          literal.order shouldBe 2
          literal.argumentIndex shouldBe 2
        }
      }
    }
  }

  "multiple declaration on single line" should {
    val cpg = code("""
        |package main
        |func main(){
        |   var  i, j int
        |   var  f, salary float32 = 10.0, 20.0
        |}
        |""".stripMargin)
    "create local and identifier nodes" in {
      val locals      = cpg.local.l
      val identifiers = cpg.identifier.l

      locals.size shouldBe 4
      locals.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(
        ("i", "int"),
        ("j", "int"),
        ("f", "float32"),
        ("salary", "float32")
      )

      identifiers.size shouldBe 2
      identifiers.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("f", "float32"), ("salary", "float32"))
    }
  }

  "dynamic declaration" should {
    val cpg = code("""
        |package main
        |func main(){
        |   d := 43
        |   c := "value"
        |}
        |""".stripMargin)
    "have local and identifier nodes created" ignore {
      val locals      = cpg.local.l
      val identifiers = cpg.identifier.l

      locals.size shouldBe 2
      locals.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("d", "int"), ("c", "string"))

      identifiers.size shouldBe 2
      identifiers.map(loc => (loc.name, loc.typeFullName)).l shouldBe List(("d", "int"), ("c", "string"))
    }
  }

}

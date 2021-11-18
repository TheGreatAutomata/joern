package io.joern.c2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.passes.frontend.MetaDataPass
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._

class MetaDataPassTests extends AnyWordSpec with Matchers {

  "MetaDataPass" should {
    val cpg = Cpg.emptyCpg
    implicit val ec: ExecutionContext = ExecutionContext.global
    new MetaDataPass(cpg, Languages.C).createAndApply()

    "create exactly two nodes" in {
      cpg.graph.V.asScala.size shouldBe 2
    }

    "create no edges" in {
      cpg.graph.E.asScala.size shouldBe 0
    }

    "create a metadata node with correct language" in {
      cpg.metaData.language.l shouldBe List("C")
    }

    "create a '<global>' NamespaceBlock" in {
      cpg.namespaceBlock.name.l shouldBe List(NamespaceTraversal.globalNamespaceName)
    }

  }
}

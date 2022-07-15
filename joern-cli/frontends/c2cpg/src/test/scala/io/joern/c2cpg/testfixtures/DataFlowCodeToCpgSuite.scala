package io.joern.c2cpg.testfixtures

import io.shiftleft.codepropertygraph.Cpg
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot

class DataFlowCodeToCpgSuite extends CCodeToCpgSuite {

  var semanticsFilename: String = ProjectRoot.relativise("joern-cli/src/main/resources/default.semantics")

  var semantics: Semantics = _

  implicit var context: EngineContext = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    semantics = Semantics.fromList(new Parser().parseFile(semanticsFilename))
    context = EngineContext(semantics)
  }

  override def applyPasses(cpg: Cpg): Unit = {
    super.applyPasses(cpg)
    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))
  }

  protected implicit def int2IntegerOption(x: Int): Option[Integer] = Some(x)

  protected def flowToResultPairs(path: Path): List[(String, Option[Integer])] = path.resultPairs()
}

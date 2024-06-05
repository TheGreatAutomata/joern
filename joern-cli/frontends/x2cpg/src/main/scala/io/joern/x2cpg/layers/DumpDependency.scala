package io.joern.x2cpg.layers

import better.files.File
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.{LayerCreator, LayerCreatorContext, LayerCreatorOptions}

case class DependencyDumpOptions(var outDir: String) extends LayerCreatorOptions {}

object DumpDependency {

  val overlayName = "DumpDependency"

  val description = "Dump Dependency json to out/"

  def defaultOpts: DependencyDumpOptions = DependencyDumpOptions("out")
}

class DumpDependency(options: DependencyDumpOptions) extends LayerCreator {
  override val overlayName: String       = DumpDependency.overlayName
  override val description: String       = DumpDependency.description
  override val storeOverlayName: Boolean = false

  override def create(context: LayerCreatorContext, storeUndoInfo: Boolean): Unit = {
    val cpg = context.cpg
    cpg.method.zipWithIndex.foreach { case (method, i) =>
      if(method.block.astChildren.nonEmpty && !method.isExternal)
        {
          val name = method.name +s"-$i"
          val str = method.dependencyJson.head
          (File(options.outDir) / s"$name-dependency.json").write(str)
        }
    }
  }

}

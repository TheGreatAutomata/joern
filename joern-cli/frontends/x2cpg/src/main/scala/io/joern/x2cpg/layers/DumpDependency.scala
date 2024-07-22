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

  def getLastPart(input: String): String = {
    val index = input.lastIndexOf('/')
    if (index != -1) {
      input.substring(index + 1)
    } else {
      input
    }
  }

  override def create(context: LayerCreatorContext): Unit = {
    val cpg = context.cpg
    cpg.method.isExternal(false).zipWithIndex.foreach { case (method, i) =>
      if(method.block.astChildren.nonEmpty)
        {
          try {
            var name = method.name + "-" + getLastPart(method.filename) + "-" + method.lineNumber.getOrElse(0) + s"-$i"
            val str = method.dependencyJson.head
            if (name.length > 254) name = method.name + s"-tooLong-$i"
            if (name.length > 254) name = s"tooLong-$i"
            (File(options.outDir) / s"$name-dependency.json").write(str)
          }
          catch {
            case e: Exception => {}
          }
        }
    }
  }

}

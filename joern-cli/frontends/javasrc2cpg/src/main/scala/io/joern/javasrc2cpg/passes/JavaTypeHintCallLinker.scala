package io.joern.javasrc2cpg.passes

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._

import java.util.regex.Pattern

class JavaTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override protected def calls: Iterator[Call] = {
    cpg.call
      .nameNot("<operator>.*", "<operators>.*")
      .filter(c => calleeNames(c).nonEmpty && c.callee.forall(_.fullName.startsWith(Defines.UnresolvedNamespace)))
  }

}

package io.shiftleft.semanticcpg.other

import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes

package object UsefullNodeEdgeSet {

  val SearchEndNodeInMethodBody:Set[String]=Set(
    NodeTypes.METHOD,
    NodeTypes.NAMESPACE_BLOCK
  )

  val  SearchEdgeForRemovePointerArrayReference:Array[String]=Array(
    EdgeTypes.POINTER_OF,
    EdgeTypes.ARRAY_OF,
    EdgeTypes.L_REFERENCE_OF,
    EdgeTypes.R_REFERENCE_OF
  )

  val SearchEdgeForRemovePointerArrayReferenceAliasSpecialize:Array[String]=Array(
    EdgeTypes.SPECIALIZE_OF,
    EdgeTypes.ALIAS_OF
  ) ++ SearchEdgeForRemovePointerArrayReference
}


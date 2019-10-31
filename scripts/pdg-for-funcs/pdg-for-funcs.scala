/* pdg-for-funcs.scala

   This script returns a Json representation of the PDG for each method contained in the currently loaded CPG.

   Input: A valid CPG
   Output: Json

   Running the Script
   ------------------
   see: README.md

   The JSON generated has the following keys:

   "functions": Array of all methods contained in the currently loaded CPG
     |_ "function": Method name as String
     |_ "id": Method id as String (String representation of the underlying Method node)
     |_ "PDG": Array of all nodes that are reachable by data-flow or control-flow from the current method locals
         |_ "id": Node id as String (String representation of the underlying node)
         |_ "properties": Array of properties of the current node as key-value pair
         |_ "edges": Array of all AST and CFG edges where the current node is referenced as inVertex or outVertex
             |_ "id": Edge id as String (String representation of the edge)
             |_ "in": Node id as String of the inVertex node (String representation of the inVertex node)
             |_ "out": Node id as String of the outVertex node (String representation of the outVertex node)

   Sample Output
   -------------
   {
    "functions" : [
      {
        "function" : "free_list",
        "id" : "io.shiftleft.codepropertygraph.generated.nodes.Method@b",
        "PDG" : [
          {
            "id" : "io.shiftleft.codepropertygraph.generated.nodes.Call@12",
            "edges" : [
              {
                "id" : "io.shiftleft.codepropertygraph.generated.edges.Cfg@1bec0",
                "in" : "io.shiftleft.codepropertygraph.generated.nodes.Call@12",
                "out" : "io.shiftleft.codepropertygraph.generated.nodes.Identifier@15"
              },
              // ...
            ],
            "properties" : [
              {
                "key" : "DISPATCH_TYPE",
                "value" : "STATIC_DISPATCH"
              },
              {
                "key" : "METHOD_INST_FULL_NAME",
                "value" : "<operator>.assignment"
              },
              {
                "key" : "NAME",
                "value" : "<operator>.assignment"
              },
              {
                "key" : "CODE",
                "value" : "*p = head"
              },
              {
                "key" : "LINE_NUMBER",
                "value" : "9"
              },
              // ...
 */

import scala.collection.JavaConverters._
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Encoder, Json}

import io.shiftleft.dataflowengine.language._
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn
import io.shiftleft.semanticcpg.language.types.expressions.Call
import io.shiftleft.semanticcpg.language.types.structure.Local
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes

import gremlin.scala._
import org.apache.tinkerpop.gremlin.structure.Edge
import org.apache.tinkerpop.gremlin.structure.VertexProperty

implicit val encodeFuncResult: Encoder[PdgForFuncsResult] = deriveEncoder
implicit val encodeFuncFunction: Encoder[PdgForFuncsFunction] = deriveEncoder

implicit val encodeEdge: Encoder[Edge] =
  (edge: Edge) =>
    Json.obj(
      ("id", Json.fromString(edge.toString)),
      ("in", Json.fromString(edge.inVertex().toString)),
      ("out", Json.fromString(edge.outVertex().toString))
    )

implicit val encodeVertex: Encoder[nodes.CfgNode] =
  (node: nodes.CfgNode) =>
    Json.obj(
      ("id", Json.fromString(node.toString)),
      ("edges",
        Json.fromValues((node.inE("AST", "CFG").l ++ node.outE("AST", "CFG").l).map(_.asJson))),
      ("properties", Json.fromValues(node.properties().asScala.toList.map { p: VertexProperty[_] =>
        Json.obj(
          ("key", Json.fromString(p.key())),
          ("value", Json.fromString(p.value().toString))
        )
      }))
    )

final case class PdgForFuncsFunction(function: String, id: String, PDG: List[nodes.CfgNode])
final case class PdgForFuncsResult(functions: List[PdgForFuncsFunction])

PdgForFuncsResult(
  cpg.method.map { method =>
    val methodName = method.fullName
    val methodId = method.toString

    val local = new Local(
      method
        .out(EdgeTypes.CONTAINS)
        .hasLabel(NodeTypes.BLOCK)
        .out(EdgeTypes.AST)
        .hasLabel(NodeTypes.LOCAL)
        .cast[nodes.Local])

    val sink = local.referencingIdentifiers.dedup
    val source = new Call(method.out(EdgeTypes.CONTAINS).hasLabel(NodeTypes.CALL).cast[nodes.Call]).nameNot("<operator>.*").dedup

    val dependencies = sink
      .reachableBy(source).dedup
      .l
      .map {
        case trackingPoint @ (_: MethodParameterIn) => trackingPoint.start.method.head
        case trackingPoint                          => trackingPoint.cfgNode
      }
      .filter(_.toString != methodId)
    PdgForFuncsFunction(methodName, methodId, dependencies.distinct)
  }.l
).asJson

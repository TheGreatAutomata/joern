package io.joern.joerncli.console

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.write
import org.json4s.JsonDSL._
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers._
import io.shiftleft.semanticcpg.dependencyJsonGenerator.DependencyJsonGenerator
import io.shiftleft.SerializedCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.Overlays

object myMain {

  def main(args: Array[String]): Unit = {

    val console = new JoernConsole();
    val cpg = console.loadCpg(args(0)).get
    val re = DependencyJsonGenerator(cpg.method.name("GenerateProblem_ref").last).dependencyJson(cpg.method.name("GenerateProblem_ref").last)
    re
  }

  //  implicit val formats: Any = Serialization.formats(NoTypeHints)
  //
  //  // 模拟函数，每次返回 JSON 结构的一部分
  //  def getPart1(): JObject = {
  //    ("name" -> "Alice") ~
  //      ("age" -> 30)
  //  }
  //
  //  def getPart2(): JObject = {
  //    ("email" -> "alice@example.com")
  //  }
  //
  //  def getPart3(): JObject = {
  //    ("address" ->
  //      ("street" -> "123 Main St") ~
  //        ("city" -> "Wonderland") ~
  //        ("zip" -> "12345"))
  //  }
  //
  //  def getFriend1(): JObject = {
  //    ("name" -> "Bob") ~ ("email" -> "bob@example.com")
  //  }
  //
  //  def getFriend2(): JObject = {
  //    ("name" -> "Charlie") ~ ("email" -> "charlie@example.com")
  //  }
  //
  //  def main(args: Array[String]): Unit = {
  //    // 调用各个函数获取 JSON 结构的不同部分
  //    val part1 = getPart1()
  //    val part2 = getPart2()
  //    val part3 = getPart3()
  //
  //    val friendsList = JArray(List(getFriend1(), getFriend2()))
  //
  //    // 合并所有部分构建完整的 JSON 结构
  //    val completeJson = part1 merge part2 merge part3 ~ ("friends" -> friendsList)
  //
  //    // 将完整的 JSON 结构转换为字符串并输出
  //    val jsonString = compact(render(completeJson))
  //    println("Complete JSON: " + jsonString)
  //  }
}

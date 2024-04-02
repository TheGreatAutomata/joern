package io.joern.joerncli.console

object myMain {
  def main(args: Array[String]): Unit = {

    val console = new JoernConsole();
    console.loadCpg(args(0));
  }
}

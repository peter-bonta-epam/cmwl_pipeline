package cromwell.pipeline

import womtool.WomtoolMain

object InpApp extends App {

  val x2 = WomtoolMain.runWomtool(Seq("inputs", "/home/benderbej/WDL/workflows/hello.wdl"))

  println("x2=" + x2)
}

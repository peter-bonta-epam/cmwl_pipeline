package cromwell.pipeline

import cats.data.NonEmptyList
import cromwell.core.path.{ DefaultPath, DefaultPathBuilder, Path, PathBuilder }
import play.api.libs.json.JsValue
import womtool.WomtoolMain.{ BadUsageTermination, SuccessfulTermination, Termination, UnsuccessfulTermination }
import womtool.inputs.Inputs

import scala.util.{ Failure, Success, Try }

object WomTool6 extends App {

  val wdlFileContent: String = """
                                 |task hello {
                                 |  String name
                                 |
                                 |  command {
                                 |    echo 'Hello world!'
                                 |  }
                                 |  output {
                                 |    File response = stdout()
                                 |  }
                                 |}
                                 |
                                 |workflow test {
                                 |  call hello
                                 |}
                                 |""".stripMargin

  def inputs(workFlowJson: String, importResolver: Any): Either[NonEmptyList[String], String] = {

    def createTmpFile(wdlFileContent: String): java.nio.file.Path = {
//    def createTmpFile(wdlFileContent: String): Try[java.nio.file.Path] = {
      import java.nio.file.Files
      import java.nio.file.Path

//      val tempFile: Try[java.nio.file.Path] = Try {
      val tempFile: Path = Files.createTempFile("my-file", ".wdl")
      //      println("temp file created: " + tempFile);
      Files.write(tempFile, wdlFileContent.getBytes())
//      }
      tempFile
    }

    def deleteTmpFile(tempFile: java.nio.file.Path): Unit = {
      import java.nio.file.Files
      Files.delete(tempFile)
    }

    def tmpPath: java.nio.file.Path = createTmpFile(workFlowJson)

    val tmpFile: String = tmpPath.toString
    val triedPath: Try[DefaultPath] = DefaultPathBuilder.build(tmpFile)
    val cp = triedPath
    val termination: Termination = Inputs.inputsJson(cp.get, false)
    val i = termination

    deleteTmpFile(tmpPath)

    def getRes: Either[NonEmptyList[String], String] = i match {
      case SuccessfulTermination(x)   => new Right(x)
      case UnsuccessfulTermination(x) => new Left(NonEmptyList(x, Nil))
      case BadUsageTermination(x)     => new Left(NonEmptyList(x, Nil))
    }
    getRes
  }
  println(inputs(wdlFileContent, None))

}

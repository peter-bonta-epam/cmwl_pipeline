package cromwell.pipeline

import cats.data.NonEmptyList
import cromwell.core.path.{ DefaultPath, DefaultPathBuilder, Path }
import womtool.WomtoolMain.{ BadUsageTermination, SuccessfulTermination, Termination, UnsuccessfulTermination }
import womtool.inputs.Inputs

import scala.util.{ Try }

object WomTool7 extends App {

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

    def createTmpFile(wdlFileContent: String): Try[java.nio.file.Path] = {
      import java.nio.file.Files
      Try(Files.createTempFile("my-file", ".wdl"))
    }

    def writeTempFile(path: java.nio.file.Path): Try[java.nio.file.Path] = {
      import java.nio.file.Files
      Try(Files.write(path, wdlFileContent.getBytes()))
    }

    def deleteTmpFile(tempFile: java.nio.file.Path): Unit = {
      import java.nio.file.Files
      Files.delete(tempFile)
    }

    def getTryiedPath(tmpPath: java.nio.file.Path): Try[DefaultPath] =
      DefaultPathBuilder.build(tmpPath.toString)

    def getTermination(path: Path): Termination = Inputs.inputsJson(path, false)

    def getRes(term: Termination): Either[NonEmptyList[String], String] = term match {
      case SuccessfulTermination(x)   => println(x); new Right(x)
      case UnsuccessfulTermination(x) => new Left(NonEmptyList(x, Nil))
      case BadUsageTermination(x)     => new Left(NonEmptyList(x, Nil))
    }

    val tmpPath: Try[DefaultPath] = for {
      created <- createTmpFile(wdlFileContent)
      written <- writeTempFile(created)
      tPath <- getTryiedPath(written)
    } yield tPath

//    deleteTmpFile(tmpPath.get)//TODO - Do I need to delete this?

    getRes(getTermination(tmpPath.get))
  }

  inputs(wdlFileContent, None).right
//  println(inputs(wdlFileContent, None).right)
}

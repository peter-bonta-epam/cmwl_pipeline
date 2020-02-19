package cromwell.pipeline

import cats.data.NonEmptyList
import cromwell.languages.LanguageFactory
import cromwell.languages.util.ImportResolver.HttpResolver
import org.scalatest.{ Matchers, WordSpec }
import wom.executable.WomBundle

class WomToolTest extends WordSpec with Matchers {

  val womTool: WomToolAPI = new WomTool

  val correctWdl =
    """
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

  val inCorrectWdl =
    """
      |task hessllo {
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

  "WomToolAPI" when {

    "get and validate womBundle" should {

      "return the correct bundle" in {

        val res: Either[NonEmptyList[String], (WomBundle, LanguageFactory)] =
          womTool.validate(correctWdl, HttpResolver(relativeTo = None))

        res.isRight should be(true)
        res.right.get._1.allCallables("hello").name should be("hello")
      }
      "return the error message" in {

        val res: Either[NonEmptyList[String], (WomBundle, LanguageFactory)] =
          womTool.validate(inCorrectWdl, HttpResolver(relativeTo = None))

        res.isRight should be(false)
        res.left.get.head.slice(0, 5) should be("ERROR")
      }
    }

    "get an inputs from wdl" should {

      "return the correct input json" in {

        val res: Either[NonEmptyList[String], String] =
          womTool.inputs(correctWdl, HttpResolver(relativeTo = None))

        res.isRight should be(true)
      }
      "return the ERROR" in {

        val res: Either[NonEmptyList[String], String] =
          womTool.inputs(inCorrectWdl, HttpResolver(relativeTo = None))

        res.isRight should be(false)
      }
    }
  }
}

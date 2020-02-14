package cromwell.pipeline

import cats.data.NonEmptyList
import cromwell.languages.LanguageFactory
import cromwell.languages.util.ImportResolver.HttpResolver
import org.scalatest.{ Matchers, WordSpec }
import wom.executable.WomBundle

class WomToolTest extends WordSpec with Matchers {

  val womTool: WomTool = new WomTool

  "WomToolAPI" when {

    "get womBundle" should {

      "return the correct bundle" in {

        val content =
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

        val res: Either[NonEmptyList[String], (WomBundle, LanguageFactory)] =
          womTool.validate(content, HttpResolver(relativeTo = None))

        res.isRight should be(true)
        res.right.get._1.allCallables("hello").name should be("hello")
      }
      "return the error message" in {
        val content =
          """
            |task hello {
            |  String name
            |
            |  commandZZZdd {
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

        val res: Either[NonEmptyList[String], (WomBundle, LanguageFactory)] =
          womTool.validate(content, HttpResolver(relativeTo = None))

        res.isRight should be(false)
        res.left.get.head.slice(0, 5) should be("ERROR")
      }
    }
  }
}

package cromwell.pipeline

import java.nio.file.{ Files, Paths }

import cats.data.NonEmptyList
import com.typesafe.config.ConfigFactory
import cromwell.core.path.{ Path, PathBuilder }
import cromwell.languages.util.ImportResolver.{ DirectoryResolver, HttpResolver, ImportResolver }
import languages.cwl.CwlV1_0LanguageFactory
import languages.wdl.biscayne.WdlBiscayneLanguageFactory
import languages.wdl.draft2.WdlDraft2LanguageFactory
import languages.wdl.draft3.WdlDraft3LanguageFactory
import common.Checked
import common.validation.Validation._
import cromwell.languages.LanguageFactory
import cwl.Inputs
import spray.json.{ JsObject, JsString, JsValue, JsonWriter }
import wom.executable.WomBundle
import wom.expression.{ NoIoFunctionSet, WomExpression }
import wom.graph.{
  ExternalGraphInputNode,
  OptionalGraphInputNode,
  OptionalGraphInputNodeWithDefault,
  RequiredGraphInputNode
}
import wom.types.{ WomCompositeType, WomOptionalType, WomType }
import womtool.WomtoolMain.{ SuccessfulTermination, Termination, UnsuccessfulTermination }
import womtool.input.WomGraphWithResolvedImports
import spray.json._
import spray.json.DefaultJsonProtocol._
import womtool.inputs.Inputs

import scala.util.{ Failure, Success, Try }

object WomTool4 extends App {

  val string: String = """
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

  private def readContent(filePath: String): Checked[String] =
    Try(string).toChecked

  lazy val importResolvers: List[ImportResolver] =
    DirectoryResolver.localFilesystemResolvers(None) :+ HttpResolver(relativeTo = None)

  val bundleAndFactory: Either[NonEmptyList[String], (WomBundle, LanguageFactory)] = readContent("").flatMap {
    mainFileContents =>
      val languageFactory =
        List(
          new WdlDraft3LanguageFactory(ConfigFactory.empty()),
          new WdlBiscayneLanguageFactory(ConfigFactory.empty()),
          new CwlV1_0LanguageFactory(ConfigFactory.empty())
        ).find(_.looksParsable(mainFileContents)).getOrElse(new WdlDraft2LanguageFactory(ConfigFactory.empty()))

      val bundle = languageFactory.getWomBundle(
        mainFileContents,
        None,
        "{}",
        importResolvers,
        List(languageFactory)
      )
      bundle.map((_, languageFactory))
  }

  println(bundleAndFactory)

  //*******************************************************

  println(inputsJson(false))
//  Inputs.inputsJson(string, false)

  //*******************************************************

  def fromFiles(inputs: Option[Path]): Checked[WomGraphWithResolvedImports] =
    bundleAndFactory.flatMap {
      case (womBundle, languageFactory) =>
        inputs match {
          case None =>
            for {
              executableCallable <- womBundle.toExecutableCallable
            } yield WomGraphWithResolvedImports(
              executableCallable.graph,
              womBundle.resolvedImportRecords
            )
          case Some(inputsFile) =>
            for {
              inputsContents <- readContent(inputsFile.toAbsolutePath.pathAsString)
              validatedWomNamespace <- languageFactory.createExecutable(
                womBundle,
                inputsContents,
                NoIoFunctionSet
              )
            } yield WomGraphWithResolvedImports(
              validatedWomNamespace.executable.graph,
              womBundle.resolvedImportRecords
            )
        }
    }

  //*******************************************************

//  val inputs: Inputs =

  //*******************************************************

  def inputsJson(showOptionals: Boolean): Termination =
    fromFiles(None) match {
      case Right(graphWithImports) =>
        Try(
          graphWithImports.graph.externalInputNodes.toJson(inputNodeWriter(showOptionals)).prettyPrint
        ) match {
          case Success(json) =>
            SuccessfulTermination(json + System.lineSeparator)
          case Failure(error) => UnsuccessfulTermination(error.getMessage)
        }
      case Left(errors) =>
        UnsuccessfulTermination(errors.toList.mkString(System.lineSeparator))
    }

  //*******************************************************

  private def inputNodeWriter(
    showOptionals: Boolean
  ): JsonWriter[Set[ExternalGraphInputNode]] = set => {

    val valueMap: Seq[(String, JsValue)] = set.toList.collect {
      case RequiredGraphInputNode(_, womType, nameInInputSet, _) =>
        nameInInputSet -> womTypeToJson(womType)
      case OptionalGraphInputNode(_, womOptionalType, nameInInputSet, _) if showOptionals =>
        nameInInputSet -> womTypeToJson(womOptionalType)
      case OptionalGraphInputNodeWithDefault(
          _,
          womType,
          _,
          nameInInputSet,
          _
          ) if showOptionals =>
        nameInInputSet -> womTypeToJson(womType)
    }

    valueMap.toMap.toJson
  }

  private def womTypeToJson(womType: WomType): JsValue =
    womType match {
      case (WomCompositeType(typeMap, _)) =>
        JsObject(typeMap.map {
          case (name, wt) => name -> womTypeToJson(wt)
        })
      case (_: WomOptionalType) =>
        JsString(s"${womType.stableName} (optional)")
      case (_) => JsString(s"${womType.stableName}")
    }

}

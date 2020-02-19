package cromwell.pipeline

import java.nio.file.{ Files, Paths }

import cats.data.NonEmptyList
import com.typesafe.config.ConfigFactory
import common.Checked
import cromwell.core.path.{ Path, PathBuilder }
import cromwell.languages.util.ImportResolver.{ DirectoryResolver, HttpResolver, ImportResolver }
import languages.cwl.CwlV1_0LanguageFactory
import languages.wdl.biscayne.WdlBiscayneLanguageFactory
import languages.wdl.draft2.WdlDraft2LanguageFactory
import languages.wdl.draft3.WdlDraft3LanguageFactory

import scala.collection.JavaConverters._
import common.Checked
import common.validation.Validation._
import cromwell.languages.LanguageFactory
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
import womtool.input.{ WomGraphMaker, WomGraphWithResolvedImports }
import womtool.inputs.Inputs.inputNodeWriter
import spray.json._
import spray.json.DefaultJsonProtocol._
import womtool.input.WomGraphMaker.{ getBundleAndFactory, readFile }

import scala.util.{ Failure, Success, Try }

object WomTool3 extends App {

  val mainFile: String = "/home/benderbej/WDL/workflows/hello.wdl"

  private def createTmpFile() = {}

  private def readFile(filePath: String): Checked[String] =
    Try(
      Files.readAllLines(Paths.get("/home/benderbej/WDL/workflows/hello.wdl")).asScala.mkString(System.lineSeparator())
    ).toChecked

//  lazy val importResolvers: List[ImportResolver] =
//    DirectoryResolver.localFilesystemResolvers(Some(mainFile)) :+ HttpResolver(
//      relativeTo = None
//    )

  lazy val importResolvers: List[ImportResolver] =
    DirectoryResolver.localFilesystemResolvers(None) :+ HttpResolver(relativeTo = None)

  val bundleAndFactory: Either[NonEmptyList[String], (WomBundle, LanguageFactory)] = readFile("").flatMap {
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
      // Return the pair with the languageFactory
      bundle.map((_, languageFactory))
  }

  println(bundleAndFactory)

  //*******************************************************

  println(inputsJson(false))

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
              inputsContents <- readFile(inputsFile.toAbsolutePath.pathAsString)
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
        nameInInputSet -> womTypeToJson(womType, None)
      case OptionalGraphInputNode(_, womOptionalType, nameInInputSet, _) if showOptionals =>
        nameInInputSet -> womTypeToJson(womOptionalType, None)
      case OptionalGraphInputNodeWithDefault(
          _,
          womType,
          default,
          nameInInputSet,
          _
          ) if showOptionals =>
        nameInInputSet -> womTypeToJson(womType, Option(default))
    }

    valueMap.toMap.toJson
  }

  private def womTypeToJson(womType: WomType, default: Option[WomExpression]): JsValue =
    (womType, default) match {
      case (WomCompositeType(typeMap, _), _) =>
        JsObject(typeMap.map {
          case (name, wt) => name -> womTypeToJson(wt, None)
        })
      case (_, Some(d)) =>
        JsString(
          s"${womType.stableName} (optional, default = ${d.sourceString})"
        )
      case (_: WomOptionalType, _) =>
        JsString(s"${womType.stableName} (optional)")
      case (_, _) => JsString(s"${womType.stableName}")
    }

}

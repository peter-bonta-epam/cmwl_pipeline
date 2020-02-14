package cromwell.pipeline

import cromwell.languages.util.ImportResolver.{ DirectoryResolver, HttpResolver, ImportResolver }
import womtool.input.{ WomGraphMaker, WomGraphWithResolvedImports }
import java.nio.file.{ Files, Paths }

import com.typesafe.config.ConfigFactory
import common.Checked
import common.validation.Validation._
import cromwell.languages.LanguageFactory
import languages.cwl.CwlV1_0LanguageFactory
import languages.wdl.biscayne.WdlBiscayneLanguageFactory
import languages.wdl.draft2.WdlDraft2LanguageFactory
import languages.wdl.draft3.WdlDraft3LanguageFactory
import wom.executable.WomBundle
import wom.expression.{ NoIoFunctionSet }
import womtool.WomtoolMain.{ SuccessfulTermination, UnsuccessfulTermination }
import wom.graph.{
  ExternalGraphInputNode,
  OptionalGraphInputNode,
  OptionalGraphInputNodeWithDefault,
  RequiredGraphInputNode
}
import spray.json._
import spray.json.DefaultJsonProtocol._
import wom.expression.WomExpression
import wom.types.{ WomCompositeType, WomOptionalType, WomType }
import common.Checked
import scala.util.{ Failure, Success, Try }

object WomTool5 extends App {

  val showOptionals: Boolean = false

  val mainFileContents =
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
  lazy val importResolvers: List[ImportResolver] =
    DirectoryResolver.localFilesystemResolvers(None) :+ HttpResolver(relativeTo = None)
  val languageFactory: LanguageFactory =
    List(
      new WdlDraft3LanguageFactory(ConfigFactory.empty()),
      new WdlBiscayneLanguageFactory(ConfigFactory.empty()),
      new CwlV1_0LanguageFactory(ConfigFactory.empty())
    ).find(_.looksParsable(mainFileContents)).getOrElse(new WdlDraft2LanguageFactory(ConfigFactory.empty()))

  val chackedBundle: Checked[WomBundle] =
    languageFactory.getWomBundle(mainFileContents, None, "{}", importResolvers, List(languageFactory))

  val bundle = languageFactory.getWomBundle(mainFileContents, None, "{}", importResolvers, List(languageFactory))

  val either: Checked[WomGraphWithResolvedImports] = for {
    inputsContents <- readContent(mainFileContents)
//    inputsContents <- readFile("inputsFile.toAbsolutePath.pathAsString")
    womBundle <- bundle
    validatedWomNamespace <- languageFactory.createExecutable(
      womBundle,
      inputsContents,
      NoIoFunctionSet
    )
  } yield {
    val records = womBundle.resolvedImportRecords
    WomGraphWithResolvedImports(
      validatedWomNamespace.executable.graph,
      records
    )
  }

  either match {
    case Right(graphWithImports) =>
      Try(graphWithImports.graph.externalInputNodes.toJson(inputNodeWriter(showOptionals)).prettyPrint) match {
        case Success(json)  => SuccessfulTermination(json + System.lineSeparator)
        case Failure(error) => UnsuccessfulTermination(error.getMessage)
      }
    case Left(errors) => UnsuccessfulTermination(errors.toList.mkString(System.lineSeparator))
  }

  private def inputNodeWriter( //TODO! - this method was initially private in womtool.Validate!
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

  private def womTypeToJson(
    womType: WomType, //TODO! - this method was initially private in womtool.Validate!
    default: Option[WomExpression]
  ): JsValue =
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

  private def readContent(wdlContent: String): Checked[String] =
    Try(mainFileContents).toChecked
}

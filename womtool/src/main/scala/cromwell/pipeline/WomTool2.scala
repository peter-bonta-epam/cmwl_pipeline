package cromwell.pipeline

import java.nio.file.{ Files, Paths }

import cats.data.NonEmptyList
import cromwell.languages.util.ImportResolver.{ DirectoryResolver, HttpResolver, ImportResolver }
import wom.graph.{
  ExternalGraphInputNode,
  OptionalGraphInputNode,
  OptionalGraphInputNodeWithDefault,
  RequiredGraphInputNode
}

import scala.util.{ Failure, Success }
import womtool.input.WomGraphWithResolvedImports
import com.typesafe.config.ConfigFactory
import common.Checked
import common.validation.Validation._
import cromwell.languages.{ LanguageFactory, ValidatedWomNamespace }
import languages.cwl.CwlV1_0LanguageFactory
import languages.wdl.biscayne.WdlBiscayneLanguageFactory
import languages.wdl.draft2.WdlDraft2LanguageFactory
import languages.wdl.draft3.WdlDraft3LanguageFactory
import wom.executable.WomBundle
import wom.expression.NoIoFunctionSet

import scala.util.Try
import spray.json._
import spray.json.DefaultJsonProtocol._
import wom.expression.WomExpression
import wom.types.{ WomCompositeType, WomOptionalType, WomType }
import womtool.inputs.Inputs._

import scala.collection.JavaConverters._
import spray.json._
import spray.json.DefaultJsonProtocol._
import play.api.libs.json.Json
import wdl.transforms.draft2.wdlom2wom.WdlDraft2WomBundleMakers
import wom.core.WorkflowJson //need to make it executable

object WomTool2 extends App {

  def readContent(wdlContent: String): Checked[String] = Try(wdlContent).toChecked

//  private def readContent(wdlContent: String): Checked[String] =
//    Try(mainFileContents).toChecked

  private def readFile(filePath: String): Checked[String] =
    Try(
      Files.readAllLines(Paths.get("/home/benderbej/WDL/workflows/hello.wdl")).asScala.mkString(System.lineSeparator())
    ).toChecked

//  println(readFile("/home/benderbej/WDL/workflows/hello.wdl").right)

  def getInputs(mainFileContents: WorkflowJson, importResolver: Any): Either[String, String] = {

    val workFlowJson: String = """
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

    val rf: Checked[String] = readFile("")

//    val workflowJson: String = {
//      s @ String() <- fileCont
//    } yield s

    val showOptionals: Boolean = false

    lazy val importResolvers: List[ImportResolver] =
      DirectoryResolver.localFilesystemResolvers(None) :+ HttpResolver(relativeTo = None)

    val languageFactory: LanguageFactory =
      List(
        new WdlDraft3LanguageFactory(ConfigFactory.empty()),
        new WdlBiscayneLanguageFactory(ConfigFactory.empty()),
        new CwlV1_0LanguageFactory(ConfigFactory.empty())
      ).find(_.looksParsable(workFlowJson)).getOrElse(new WdlDraft2LanguageFactory(ConfigFactory.empty()))

    val bundle: Checked[WomBundle] =
      languageFactory.getWomBundle(workFlowJson, None, "{}", importResolvers, List(languageFactory))

//    val partialToWB = new PartialFunction[Checked[WomBundle], WomBundle] { //TODO replace this
//      def apply(checked: Checked[WomBundle]) = checked.right.get
//      def isDefinedAt(checked: Checked[WomBundle]) = checked.isRight
//    }
//    val validatedWomNamespace: Checked[ValidatedWomNamespace] =
//      languageFactory.createExecutable(partialToWB(bundle), workFlowJson, NoIoFunctionSet)
//    println("validatedWomNamespace.right=" + validatedWomNamespace.right)
//    println("validatedWomNamespace=" + validatedWomNamespace)

//    val records = partialToWB(bundle).resolvedImportRecords
//    val imports: WomGraphWithResolvedImports = WomGraphWithResolvedImports(validatedWomNamespace.executable.graph, records)

    val rc: Checked[String] = readContent(workFlowJson)

    println("rc.isLeft" + rc.isLeft)

//    val either: Checked[WomGraphWithResolvedImports] = for {
////      workFlowJson <- readContent(workFlowJson)
////      inputsContents <- readFile("/home/benderbej/WDL/workflows/hello.wdl")
//      womBundle <- bundle
//      validatedWomNamespace <- languageFactory.createExecutable(
//        womBundle,
//        workFlowJson,
//        NoIoFunctionSet
//      )
//    } yield {
//      val records = womBundle.resolvedImportRecords
//      WomGraphWithResolvedImports(
//        validatedWomNamespace.executable.graph,
//        records
//      )
//    }

    val either: Checked[WomGraphWithResolvedImports] = for {
      inputsContents <- readFile("")
      womBundle <- bundle
      validatedWomNamespace <- languageFactory.createExecutable(
        womBundle,
        inputsContents,
        NoIoFunctionSet
      )
    } yield WomGraphWithResolvedImports(
      validatedWomNamespace.executable.graph,
      womBundle.resolvedImportRecords
    )

    def inputNodeWriter( //TODO! - this method was initially private in womtool.inputs! - cant create womtool.inputs.Inputs without Path!
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

    def womTypeToJson(
      womType: WomType, //TODO! - this method was initially private in womtool.inputs! - cant create womtool.inputs.Inputs without Path!
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

    either match {
      case Right(graphWithImports) =>
        Try(graphWithImports.graph.externalInputNodes.toJson(inputNodeWriter(showOptionals)).prettyPrint) match {
          case Success(json)  => Right(json + System.lineSeparator)
          case Failure(error) => Left(error.getMessage + "--!")
        }
      case Left(errors) => Left(errors.toList.mkString(System.lineSeparator) + "--!!")
    }
  }

  val contents =
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

  val x = getInputs(contents, None)
  println("==>" + x + "<==")

  /*
    {
    "test.hello.name": "String"
    }
 */

}

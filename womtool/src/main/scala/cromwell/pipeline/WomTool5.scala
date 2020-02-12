package cromwell.pipeline

import cromwell.languages.util.ImportResolver.{ DirectoryResolver, HttpResolver, ImportResolver }
import cromwell.pipeline.WomTool4.{ bundle, languageFactory }
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
import wom.expression.NoIoFunctionSet

import scala.collection.JavaConverters._
import scala.util.Try

object WomTool5 extends App {

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

//  val womBundle = bundle.map((_, languageFactory))
//  val womBundle: = bundle.map(wb => (_, languageFactory))

//  val dummyBundle: WomBundle = womBundle match {
//    case Left(l) => l
//    case Right(s) => womBundle
//  }

//  val dummyBundle: (Any, LanguageFactory) = womBundle.flatMap(_, ))

//  private def stringToChecked(st: String): Checked[String] = st.to

//  for {
//    inputsContents <- readFile(inputsFile.toAbsolutePath.pathAsString)
//    validatedWomNamespace <- languageFactory.createExecutable(
//      womBundle,
//      inputsContents,
//      NoIoFunctionSet
//    )
//  } yield WomGraphWithResolvedImports(
//    validatedWomNamespace.executable.graph,
//    womBundle.resolvedImportRecords
//  )
//
//  for {
//    executableCallable <- bundles.toExecutableCallable
//  } yield WomGraphWithResolvedImports(
//    executableCallable.graph,
//    bundle.resolvedImportRecords
//  )

  //*************************************************

//  for {
//    executableCallable <- womBundle.toExecutableCallable
//  } yield
//    WomGraphWithResolvedImports(
//      executableCallable.graph,
//      womBundle.resolvedImportRecords
//    )

  //*************************************************

//  val either2: Checked[WomGraphWithResolvedImports]

  val bundle = languageFactory.getWomBundle(mainFileContents, None, "{}", importResolvers, List(languageFactory))

  val either: Checked[WomGraphWithResolvedImports] = for {
    //    inputsContents <- readContent(mainFileContents)
    inputsContents <- readFile("inputsFile.toAbsolutePath.pathAsString")
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

  //*************************************************

//  private def readContent(filePath: String): Either[NonEmptyList[String], A] =
  private def readContent(wdlContent: String): Checked[String] =
    Try(mainFileContents).toChecked

  //**************************************************

  private def readFile(filePath: String): Checked[String] =
    Try(
      Files.readAllLines(Paths.get(filePath)).asScala.mkString(System.lineSeparator())
    ).toChecked

}
/*










 */

package cromwell.pipeline

import cats.data.NonEmptyList
import com.typesafe.config.ConfigFactory
import common.Checked
import cromwell.core.path.{ DefaultPath, DefaultPathBuilder, Path }
import cromwell.languages.LanguageFactory
import cromwell.languages.util.ImportResolver.{ DirectoryResolver, HttpResolver, ImportResolver }
import cromwell.pipeline.WomTool7.wdlFileContent
import languages.cwl.CwlV1_0LanguageFactory
import languages.wdl.biscayne.WdlBiscayneLanguageFactory
import languages.wdl.draft2.WdlDraft2LanguageFactory
import languages.wdl.draft3.WdlDraft3LanguageFactory
import wom.executable.WomBundle
import womtool.WomtoolMain.{ BadUsageTermination, SuccessfulTermination, Termination, UnsuccessfulTermination }
import womtool.inputs.Inputs

import scala.util.Try

class WomTool extends WomToolAPI {

  def validate(
    content: String,
    importResolver: Any
  ): Either[NonEmptyList[String], (WomBundle, LanguageFactory)] = {

    lazy val importResolvers: List[ImportResolver] = //TODO - deal with import resolvers
      DirectoryResolver.localFilesystemResolvers(None) :+ HttpResolver(relativeTo = None)
    val languageFactory: LanguageFactory =
      List(
        new WdlDraft3LanguageFactory(ConfigFactory.empty()),
        new WdlBiscayneLanguageFactory(ConfigFactory.empty()),
        new CwlV1_0LanguageFactory(ConfigFactory.empty())
      ).find(_.looksParsable(content)).getOrElse(new WdlDraft2LanguageFactory(ConfigFactory.empty()))

    val bundle: Checked[WomBundle] =
      languageFactory.getWomBundle(content, None, "{}", importResolvers, List(languageFactory))

    def getResult(womBundle: Checked[WomBundle]): Either[NonEmptyList[String], (WomBundle, LanguageFactory)] =
      womBundle match {
        case Right(w) => Right(w, languageFactory)
        case Left(l)  => Left(l)
      }
    getResult(bundle)
  }

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
      case SuccessfulTermination(x)   => new Right(x)
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

}

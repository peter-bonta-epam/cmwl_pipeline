package cromwell.pipeline

import cats.data.NonEmptyList
import com.typesafe.config.ConfigFactory
import common.Checked
import cromwell.languages.LanguageFactory
import cromwell.languages.util.ImportResolver.{ DirectoryResolver, HttpResolver, ImportResolver }
import languages.cwl.CwlV1_0LanguageFactory
import languages.wdl.biscayne.WdlBiscayneLanguageFactory
import languages.wdl.draft2.WdlDraft2LanguageFactory
import languages.wdl.draft3.WdlDraft3LanguageFactory
import wom.executable.WomBundle

//class WomTool extends WomToolAPI {
class WomTool {

  def validate(
//  override def validate(
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
}

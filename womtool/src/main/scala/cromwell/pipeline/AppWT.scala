package cromwell.pipeline

import womtool.WomtoolMain

//***********************************************8
import java.nio.file.{ Files, Paths }

import com.typesafe.config.ConfigFactory
import common.Checked
import common.validation.Validation._
import cromwell.core.path.Path
import cromwell.languages.LanguageFactory
import cromwell.languages.util.ImportResolver._
import languages.cwl.CwlV1_0LanguageFactory
import languages.wdl.biscayne.WdlBiscayneLanguageFactory
import languages.wdl.draft2.WdlDraft2LanguageFactory
import languages.wdl.draft3.WdlDraft3LanguageFactory
import wom.executable.WomBundle

import scala.collection.JavaConverters._
import scala.util.Try

object AppWT extends App {

  val x2 = WomtoolMain.runWomtool(Seq("validate", "/home/benderbej/WDL/workflows/hello.wdl"))
//  val y2 = WomtoolMain.runWomtool(Seq("validate", "-"))

  val x3 = WomtoolMain.runWomtool(Seq("inputs", "/home/benderbej/WDL/workflows/hello.wdl"))
//  val y3 = WomtoolMain.runWomtool(Seq("inputs", "-"))

  //  println("x=" + x + x.returnCode)
  //  println("y=" + y.returnCode + y.output)

//  println("x2=" + x2 + x2.returnCode)
//  println("y2=" + y2.returnCode + y2.stdout)

  println("x3=" + x3 + x3.returnCode)
//  println("y3=" + y3.returnCode + y3.stdout)

  def getBundle(mainFile: Path): Checked[WomBundle] = getBundleAndFactory(mainFile).map(_._1)

  private def getBundleAndFactory(mainFile: Path): Checked[(WomBundle, LanguageFactory)] = {
    lazy val importResolvers: List[ImportResolver] =
      DirectoryResolver.localFilesystemResolvers(Some(mainFile)) :+ HttpResolver(relativeTo = None)

    readFile(mainFile.toAbsolutePath.pathAsString).flatMap { mainFileContents =>
      val languageFactory =
        List(
          new WdlDraft3LanguageFactory(ConfigFactory.empty()),
          new WdlBiscayneLanguageFactory(ConfigFactory.empty()),
          new CwlV1_0LanguageFactory(ConfigFactory.empty())
        ).find(_.looksParsable(mainFileContents)).getOrElse(new WdlDraft2LanguageFactory(ConfigFactory.empty()))

      val bundle = languageFactory.getWomBundle(mainFileContents, None, "{}", importResolvers, List(languageFactory))
      // Return the pair with the languageFactory
      bundle.map((_, languageFactory))
    }
  }

  private def readFile(filePath: String): Checked[String] = Try(
    Files.readAllLines(Paths.get(filePath)).asScala.mkString(System.lineSeparator())
  ).toChecked

}

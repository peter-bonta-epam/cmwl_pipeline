package cromwell.pipeline

import cats.data.NonEmptyList
import cromwell.languages.LanguageFactory
import cromwell.languages.util.ImportResolver.ImportResolver
import play.api.libs.json.JsValue
import wom.core.WorkflowJson
import wom.executable.WomBundle

trait WomToolAPI {

  def validate(content: String, importResolver: Any): Either[NonEmptyList[String], (WomBundle, LanguageFactory)]
//  def inputs(contents: String, importResolver: Any): Either[String, String]

  def inputs(workflowJson: WorkflowJson, importResolver: Any): Either[NonEmptyList[String], JsValue]

}

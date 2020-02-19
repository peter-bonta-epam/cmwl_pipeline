package cromwell.pipeline

import cats.data.NonEmptyList
import cromwell.languages.LanguageFactory
import wom.core.WorkflowJson
import wom.executable.WomBundle

trait WomToolAPI {

  def validate(content: String, importResolver: Any): Either[NonEmptyList[String], (WomBundle, LanguageFactory)]

  def inputs(workflowJson: WorkflowJson, importResolver: Any): Either[NonEmptyList[String], String]

}

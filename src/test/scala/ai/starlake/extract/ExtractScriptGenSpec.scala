package ai.starlake.extract

import ai.starlake.TestHelper
import ai.starlake.schema.model.TransformInput
import better.files.File

class ExtractScriptGenSpec extends TestHelper {

  val scriptOutputFolder: File = File(System.getProperty("java.io.tmpdir"))
  new WithSettings() {

    "templatize domain using mustache" should "generate an export script from a TemplateSettings" in {
      val templateParams: TemplateParams = TemplateParams(
        domainToExport = "domain1",
        tableToExport = "table1",
        columnsToExport = List(
          ("col1", "string", false, TransformInput.None),
          ("col2", "long", false, TransformInput.None)
        ),
        fullExport = false,
        dsvDelimiter = ",",
        deltaColumn = Some("updateCol"),
        auditDB = None,
        activeEnv = Map.empty
      )

      val templatesPayloadFromDir = new ExtractScript(settings.schemaHandler())
        .templatizeFolder(
          File(
            getClass.getResource("/sample/database")
          ),
          templateParams
        )
        .head
        .pathAsString

      println(File(templatesPayloadFromDir).lines.mkString("\n").toLowerCase)

      val templatePayload = new ExtractScript(settings.schemaHandler())
        .templatizeFile(
          File(getClass.getResource("/sample/database/EXTRACT_TABLE.sql.mustache")).pathAsString,
          templateParams
        )
        .pathAsString

      File(templatePayload).lines.mkString("\n").toLowerCase shouldBe File(
        getClass.getResource("/sample/database/expected_script_payload.txt")
      ).lines.mkString("\n").toLowerCase
    }

    "templatize domain using ssp" should "generate an export script from a TemplateSettings" in {
      val templateParams: TemplateParams = TemplateParams(
        domainToExport = "domain1",
        tableToExport = "table1",
        columnsToExport = List(
          ("col1", "string", false, TransformInput.None),
          ("col2", "long", false, TransformInput.None),
          ("col3", "string", true, TransformInput.None),
          ("col4", "string", false, TransformInput.None)
        ),
        fullExport = false,
        dsvDelimiter = ",",
        deltaColumn = Some("updateCol"),
        auditDB = None,
        activeEnv = Map.empty
      )

      val templatePayload: String = new ExtractScript(settings.schemaHandler())
        .templatizeFile(
          File(getClass.getResource("/sample/database/EXTRACT_TABLE.sql.ssp")).pathAsString,
          templateParams
        )
        .pathAsString

      print(getClass.getResource("/sample/database/expected_script_payload2.txt").getPath)
      File(templatePayload).lines.mkString("\n").toLowerCase shouldBe File(
        getClass.getResource("/sample/database/expected_script_payload2.txt")
      ).lines.mkString("\n").toLowerCase
    }
  }
}

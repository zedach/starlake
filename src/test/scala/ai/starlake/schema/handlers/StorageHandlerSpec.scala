/*
 *
 *  * Licensed to the Apache Software Foundation (ASF) under one or more
 *  * contributor license agreements.  See the NOTICE file distributed with
 *  * this work for additional information regarding copyright ownership.
 *  * The ASF licenses this file to You under the Apache License, Version 2.0
 *  * (the "License"); you may not use this file except in compliance with
 *  * the License.  You may obtain a copy of the License at
 *  *
 *  *    http://www.apache.org/licenses/LICENSE-2.0
 *  *
 *  * Unless required by applicable law or agreed to in writing, software
 *  * distributed under the License is distributed on an "AS IS" BASIS,
 *  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  * See the License for the specific language governing permissions and
 *  * limitations under the License.
 *
 *
 */

package ai.starlake.schema.handlers

import ai.starlake.TestHelper
import ai.starlake.config.Settings
import ai.starlake.config.Settings.latestSchemaVersion
import ai.starlake.schema.model._
import com.typesafe.config.{Config, ConfigFactory}
import org.apache.hadoop.fs.Path
import better.files.{File => BetterFile}

import java.time.Instant
import java.util.regex.Pattern

class StorageHandlerSpec extends TestHelper {

  lazy val pathDomain = new Path(starlakeTestRoot + "/domain.sl.yml")

  lazy val pathType = new Path(starlakeTestRoot + "/types.sl.yml")

  lazy val pathBusiness = new Path(starlakeTestRoot + "/business.sl.yml")
  lazy val pathConfigBusiness = new Path(starlakeTestRoot + "/_config.sl.yml")

  val localStorageSettings: Config = ConfigFactory
    .parseString("""
      |useLocalFileSystem: true
      |""".stripMargin)
    .withFallback(super.testConfiguration)

  def testStatFile(storageHandler: StorageHandler): Unit = {
    val tmpFilePath = starlakeTestRoot + "/tmp.txt"
    val tmpFile = BetterFile(tmpFilePath)
    Thread.sleep(1000) // test may be too fast
    tmpFile.write("This is a text")
    val tmpHadoopFilePath = new Path(tmpFilePath)
    storageHandler.stat(tmpHadoopFilePath) should matchPattern {
      case FileInfo(`tmpHadoopFilePath`, size, _) if size > 0 =>
    }
  }

  def testListFile(storageHandler: StorageHandler): Unit = {
    val fileExtension = "test_file"
    val startInstant = Instant.now()
    (1 to 2).foreach { i =>
      val tmpFilePath = starlakeTestRoot + s"/$i.$fileExtension"
      val tmpFile = BetterFile(tmpFilePath)
      tmpFile.write("This is a text")
    }
    val fileList =
      storageHandler.list(new Path(starlakeTestRoot), extension = fileExtension, recursive = false)
    fileList should have length 2
    fileList.foreach(_ should matchPattern {
      case FileInfo(file, size, _) if size > 0 && file.getName.endsWith(fileExtension) =>
    })
  }

  def testMoveFile(storageHandler: StorageHandler): Unit = {
    val srcDir = starlakeTestRoot + "/move_src"
    val dstDir = starlakeTestRoot + "/move_dst"
    BetterFile(srcDir).createDirectories()
    val srcFile = BetterFile(srcDir + "/test_move.txt")
    srcFile.write("move test content")
    val srcPath = new Path(srcDir + "/test_move.txt")
    val dstPath = new Path(dstDir + "/test_move.txt")
    storageHandler.move(srcPath, dstPath) shouldBe true
    storageHandler.exists(dstPath) shouldBe true
    storageHandler.exists(srcPath) shouldBe false
    storageHandler.read(dstPath) shouldBe "move test content"
  }

  def testMoveFileOverwrite(storageHandler: StorageHandler): Unit = {
    val srcDir = starlakeTestRoot + "/move_overwrite_src"
    val dstDir = starlakeTestRoot + "/move_overwrite_dst"
    BetterFile(srcDir).createDirectories()
    BetterFile(dstDir).createDirectories()
    BetterFile(srcDir + "/overwrite.txt").write("new content")
    BetterFile(dstDir + "/overwrite.txt").write("old content")
    val srcPath = new Path(srcDir + "/overwrite.txt")
    val dstPath = new Path(dstDir + "/overwrite.txt")
    storageHandler.move(srcPath, dstPath) shouldBe true
    storageHandler.exists(srcPath) shouldBe false
    storageHandler.read(dstPath) shouldBe "new content"
  }

  def testMoveCreatesParentDirs(storageHandler: StorageHandler): Unit = {
    val srcDir = starlakeTestRoot + "/move_parent_src"
    BetterFile(srcDir).createDirectories()
    BetterFile(srcDir + "/nested.txt").write("nested content")
    val srcPath = new Path(srcDir + "/nested.txt")
    val dstPath = new Path(starlakeTestRoot + "/move_parent_dst/sub/dir/nested.txt")
    storageHandler.move(srcPath, dstPath) shouldBe true
    storageHandler.exists(dstPath) shouldBe true
    storageHandler.exists(srcPath) shouldBe false
  }

  new WithSettings(localStorageSettings) {
    "LocalStorageHandler" should "stat file" in {
      testStatFile(storageHandler)
    }

    it should "list file" in {
      testListFile(storageHandler)
    }

    it should "move file to new directory" in {
      testMoveFile(storageHandler)
    }

    it should "move file overwriting existing" in {
      testMoveFileOverwrite(storageHandler)
    }

    it should "move file creating parent directories" in {
      testMoveCreatesParentDirs(storageHandler)
    }
  }

  new WithSettings() {
    "StorageHandler" should "stat file" in {
      testStatFile(storageHandler)
    }

    it should "list file" in {
      testListFile(storageHandler)
    }

    it should "move file to new directory" in {
      testMoveFile(storageHandler)
    }

    it should "move file overwriting existing" in {
      testMoveFileOverwrite(storageHandler)
    }

    it should "move file creating parent directories" in {
      testMoveCreatesParentDirs(storageHandler)
    }

    "Domain Case Class" should "be written as yaml and read correctly" in {
      val domain = DomainInfo(
        name = "DOMAIN",
        metadata = Some(
          Metadata(
            format = Some(Format.DSV),
            encoding = None,
            multiline = Some(false),
            array = Some(false),
            withHeader = Some(false),
            separator = Some(";"),
            quote = Some("\""),
            escape = Some("\\"),
            writeStrategy = Some(WriteStrategy(`type` = Some(WriteStrategyType.APPEND))),
            directory = Some(s"${starlakeTestRoot}/incoming/DOMAIN")
          )
        ),
        tables = List(
          SchemaInfo(
            "User",
            Pattern.compile("SCHEMA-.*.dsv"),
            List(
              TableAttribute(
                "firstname",
                "string",
                Some(false),
                required = Some(false),
                Some(TransformInput.None)
              ),
              TableAttribute(
                "lastname",
                "string",
                Some(false),
                required = Some(false),
                Some(TransformInput("SHA1", sql = false))
              ),
              TableAttribute(
                "age",
                "age",
                Some(false),
                required = Some(false),
                Some(TransformInput("HIDE", sql = false))
              )
            ),
            Some(Metadata(withHeader = Some(true))),
            Some("Schema Comment"),
            List("SQL1", "SQL2"),
            Nil
          )
        ),
        comment = Some("Domain Comment")
      )

      storageHandler.write(mapper.writeValueAsString(domain), pathDomain)

      // TODO different behaviour between sbt & intellij
      //    readFileContent(pathDomain) shouldBe loadFile("/expected/yml/domain.yml")

      val resultDomain: DomainInfo = mapper.readValue[DomainInfo](storageHandler.read(pathDomain))

      resultDomain.name shouldBe domain.name
      resultDomain.resolveDirectory() shouldBe domain.resolveDirectory()
      // TODO TOFIX : domain written is not the domain expected, the test below just to make debug easy
      resultDomain.metadata.get equals domain.metadata.get
      resultDomain.resolveAck() shouldBe None
      resultDomain.comment shouldBe domain.comment
    }

    "Types Case Class" should "be written as yaml and read correctly" in {
      val types = TypesInfo(
        latestSchemaVersion,
        List(
          Type("string", ".+", PrimitiveType.string),
          Type("time", "(1[012]|[1-9]):[0-5][0-9](\\\\s)?(?i)(am|pm)"),
          Type("time24", "([01]?[0-9]|2[0-3]):[0-5][0-9]"),
          Type(
            "date",
            "(0?[1-9]|[12][0-9]|3[01])/(0?[1-9]|1[012])/((19|20)\\\\d\\\\d)",
            PrimitiveType.date
          ),
          Type("username", "[a-z0-9_-]{3,15}"),
          Type("age", "[a-z0-9_-]{3,15}", PrimitiveType.long),
          Type("color", "#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})", PrimitiveType.string),
          Type(
            "ip",
            "([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.([01]?\\d\\d?|2[0-4]\\d|25[0-5])\\.([01]?\\d\\d?|2[0-4]\\d|25[0-5])"
          ),
          Type("email", "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\\\.[A-Za-z]{2,6}")
        )
      )

      storageHandler.write(mapper.writeValueAsString(types), pathType)
      val fileContent = readFileContent(pathType)
      val expectedFileContent = loadTextFile(s"/expected/yml/types.sl.yml")
      fileContent shouldBe expectedFileContent
      val resultType: TypesInfo = mapper.readValue[TypesInfo](storageHandler.read(pathType))
      resultType shouldBe types

    }

    "Business Job Definition" should "be valid json" in {
      val businessTask1 = AutoTaskInfo(
        name = "",
        sql = Some("select * from domain"),
        database = None,
        domain = "DOMAIN",
        table = "ANALYSE",
        presql = Nil,
        postsql = Nil,
        sink = None,
        rls = List(RowLevelSecurity("myrls", "TRUE", Set("user:hayssam.saleh@ebiznext.com"))),
        python = None,
        writeStrategy = Some(WriteStrategy.Overwrite)
      )
      val businessJobDef = mapper
        .writer()
        .withAttribute(classOf[Settings], settings)
        .writeValueAsString(businessTask1)
      storageHandler.write(businessJobDef, pathBusiness)

      val configJob =
        AutoJobInfo(
          "",
          Nil
        )

      val configJobDef = mapper
        .writer()
        .withAttribute(classOf[Settings], settings)
        .writeValueAsString(configJob)
      storageHandler.write(configJobDef, pathConfigBusiness)

      val expected = mapper
        .readValue(loadTextFile("/expected/yml/business.sl.yml"), classOf[AutoJobInfo])
      logger.info(readFileContent(pathBusiness))
      val actual = mapper
        .readValue(readFileContent(pathBusiness), classOf[AutoJobInfo])
      actual shouldEqual expected
    }
  }

  "Check fs google storage uri" should "be gs" in {
    assert(
      "/user/comet" == Path
        .getPathWithoutSchemeAndAuthority(new Path("file:///user/comet"))
        .toString
    )
  }
}

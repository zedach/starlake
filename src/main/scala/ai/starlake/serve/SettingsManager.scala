package ai.starlake.serve

import ai.starlake.config.{PrivacyLevels, Settings}
import ai.starlake.utils.Utils
import better.files.File

class SettingsWatcherThread(
  settingsMap: scala.collection.mutable.Map[String, Settings],
  settingsTimeMap: scala.collection.mutable.Map[String, Long]
) extends Thread {
  private val ONE_MINUTE = 1000 * 60
  private val TEN_MINUTES = ONE_MINUTE * 10
  override def run(): Unit = {
    while (true) {
      Thread.sleep(ONE_MINUTE)
      val currentTime = System.currentTimeMillis()
      for ((key, time) <- settingsTimeMap.toSet) {
        if (currentTime - time > TEN_MINUTES) {
          settingsTimeMap.remove(key)
          settingsMap.remove(key)
        }
      }
    }
  }
}

object SettingsManager {
  private val settingsMap: scala.collection.mutable.Map[String, Settings] =
    scala.collection.mutable.Map.empty

  private val settingsTimeMap: scala.collection.mutable.Map[String, Long] =
    scala.collection.mutable.Map.empty

  private val watcherThread = new SettingsWatcherThread(settingsMap, settingsTimeMap)
  watcherThread.start()

  def reset(): Boolean = {
    lastSettingsId = ""
    settingsTimeMap.clear()
    settingsMap.clear()
    true
  }

  var lastSettingsId: String = ""

  private def uniqueId(
    root: String,
    metadata: Option[String],
    env: Option[String]
  ): String = root +
    "," + metadata.getOrElse("null") +
    "," + env.getOrElse("null")

  def getUpdatedSettings(
    root: String,
    metadata: Option[String],
    env: Option[String],
    gcpProject: Option[String],
    refresh: Boolean = false
  ): (Settings, Boolean) = {
    val sessionId = uniqueId(root, metadata, env)
    Utils.resetJinjaClassLoader()
    PrivacyLevels.resetAllPrivacy()

    val sysProps = System.getProperties()
    val outFile = File(root, "out")
    outFile.createDirectoryIfNotExists()
    val extensionFile = File(root, "extension.log")
    if (extensionFile.exists) {
      extensionFile.delete(swallowIOExceptions = true)
    }

    gcpProject.foreach { gcpProject =>
      sysProps.setProperty("database", gcpProject)
    }
    if (refresh) {
      settingsMap.remove(sessionId)
    }
    val updatedSession = settingsMap.getOrElse(
      sessionId, {
        sysProps.setProperty("rootServe", outFile.pathAsString)
        sysProps.setProperty("root", root)
        sysProps.setProperty("SL_ROOT", root)
        sysProps.setProperty("metadata", root + "/" + metadata.getOrElse("metadata"))

        env match {
          case None =>
            sysProps.setProperty("env", "None")
          case Some(env) if env.isEmpty || env == "None" =>
            sysProps.setProperty("env", "None")
          case Some(env) =>
            sysProps.setProperty("env", env) // prod is the default value in reference.conf
        }
        Settings.invalidateCaches()
        val settings = Settings(Settings.referenceConfig)
        settingsMap.synchronized {
          if (!settingsMap.contains(sessionId)) {
            settingsMap.put(sessionId, settings)
          }
        }
        settings
      }
    )
    settingsTimeMap.put(sessionId, System.currentTimeMillis())
    val isNew = sessionId != lastSettingsId
    lastSettingsId = sessionId
    (updatedSession, isNew)
  }
}

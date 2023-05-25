import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.3.1`

import de.tobiasroeser.mill.vcs.version.VcsVersion
import mill._, scalalib._

import scala.concurrent.duration.DurationInt

trait DirectiveHandlerPublishModule extends PublishModule {
  import mill.scalalib.publish._
  def publishVersion = T {
    val v        = VcsVersion.vcsState().format()
    val dirtyIdx = v.indexOf("-DIRTY")
    def endsWithCommitHash =
      v.length > 6 && v.substring(v.length - 6).forall(c => c.isDigit || (c >= 'a' && c <= 'f'))
    if (dirtyIdx >= 0) v.take(dirtyIdx) + "-SNAPSHOT"
    else if (endsWithCommitHash) v + "-SNAPSHOT"
    else v
  }
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "io.github.alexarchambault.scala-cli",
    url = "https://github.com/scala-cli/directive-handler.git",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("scala-cli", "directive-handler"),
    developers = Seq(
      Developer("alexarchambault", "Alex Archambault", "https://github.com/alexarchambault")
    )
  )
  def sonatypeUri         = "https://s01.oss.sonatype.org/service/local"
  def sonatypeSnapshotUri = "https://s01.oss.sonatype.org/content/repositories/snapshots"
}

class DirectiveHandler(val crossScalaVersion: String) extends CrossScalaModule
    with DirectiveHandlerPublishModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.9.1",
    ivy"io.get-coursier::dependency:0.2.2",
    ivy"org.virtuslab:using_directives:1.0.0"
  )
}

object `directive-handler` extends Cross[DirectiveHandler]("3.2.2")

def publishSonatype(tasks: mill.main.Tasks[PublishModule.PublishData]) = T.command {
  publishSonatype0(
    data = define.Target.sequence(tasks.value)(),
    log = T.ctx().log
  )
}

def publishSonatype0(
  data: Seq[PublishModule.PublishData],
  log: mill.api.Logger
): Unit = {

  val credentials = sys.env("SONATYPE_USERNAME") + ":" + sys.env("SONATYPE_PASSWORD")
  val pgpPassword = sys.env("PGP_PASSPHRASE")
  val timeout     = 10.minutes

  val artifacts = data.map {
    case PublishModule.PublishData(a, s) =>
      (s.map { case (p, f) => (p.path, f) }, a)
  }

  val isRelease = {
    val versions = artifacts.map(_._2.version).toSet
    val set      = versions.map(!_.endsWith("-SNAPSHOT"))
    assert(
      set.size == 1,
      s"Found both snapshot and non-snapshot versions: ${versions.toVector.sorted.mkString(", ")}"
    )
    set.head
  }
  val publisher = new scalalib.publish.SonatypePublisher(
    uri = "https://oss.sonatype.org/service/local",
    snapshotUri = "https://oss.sonatype.org/content/repositories/snapshots",
    credentials = credentials,
    signed = true,
    // format: off
    gpgArgs = Seq(
      "--detach-sign",
      "--batch=true",
      "--yes",
      "--pinentry-mode", "loopback",
      "--passphrase", pgpPassword,
      "--armor",
      "--use-agent"
    ),
    // format: on
    readTimeout = timeout.toMillis.toInt,
    connectTimeout = timeout.toMillis.toInt,
    log = log,
    awaitTimeout = timeout.toMillis.toInt,
    stagingRelease = isRelease
  )

  publisher.publishAll(isRelease, artifacts: _*)
}

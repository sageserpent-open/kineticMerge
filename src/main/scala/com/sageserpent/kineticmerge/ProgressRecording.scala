package com.sageserpent.kineticmerge

import me.tongfei.progressbar.{ConsoleProgressBarConsumer, ProgressBarBuilder}
import org.jline.utils.WriterOutputStream

import java.io.PrintStream
import java.nio.charset.Charset

import java.time.Duration as JavaDuration

/** Records progress from zero up to some implied maximum set up by
  * [[ProgressRecording.newSession]]. It is a ratchet, so attempting to decrease
  * the recorded progress is ignored. Attempting to record more than the maximum
  * simply records the maximum. <p/> Closing the session implicity records the
  * maximum amount of progress.
  */
trait ProgressRecordingSession extends AutoCloseable:
  /** @param amount
    *   Zero or positive amount of absolute progress.
    */
  def upTo(amount: Int): Unit
end ProgressRecordingSession

trait ProgressRecording:
  /** Create a new progress recording session that has zero recorded progress.
    *
    * @param maximumProgress
    *   The maximum amount of progress that can be recorded.
    * @return
    *   An instance of [[ProgressRecordingSession]] that has zero progress
    *   recorded.
    */
  def newSession(label: String, maximumProgress: Int)(
      initialProgress: Int): ProgressRecordingSession
end ProgressRecording

object NoProgressRecording extends ProgressRecording:
  override def newSession(label: String, maximumProgress: Int)(
      initialProgress: Int): ProgressRecordingSession =
    Session

  private object Session extends ProgressRecordingSession:
    override def upTo(amount: Int): Unit = {}

    override def close(): Unit = {}
  end Session
end NoProgressRecording

object ConsoleProgressRecording extends ProgressRecording:
  override def newSession(label: String, maximumProgress: Int)(
      initialProgress: Int): ProgressRecordingSession =
    new ProgressRecordingSession:
      private val progressBar = Option(System.console()).map(console =>
        val progressBarConsumer = new ConsoleProgressBarConsumer(
          new PrintStream(
            new WriterOutputStream(console.writer(), Charset.defaultCharset())
          )
        )
        val builder = new ProgressBarBuilder
        builder.setTaskName(label)
        builder.hideEta()
        builder.startsFrom(initialProgress, JavaDuration.ZERO)
        builder.setInitialMax(maximumProgress)
        builder.clearDisplayOnFinish()
        builder.setConsumer(progressBarConsumer)
        builder.build()
      )

      override def upTo(amount: Int): Unit =
        progressBar.foreach(_.stepTo(amount))
      override def close(): Unit = progressBar.foreach(_.close())
end ConsoleProgressRecording

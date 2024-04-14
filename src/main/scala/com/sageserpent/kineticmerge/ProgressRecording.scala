package com.sageserpent.kineticmerge

import me.tongfei.progressbar.{ConsoleProgressBarConsumer, ProgressBarBuilder}
import org.jline.utils.WriterOutputStream
import sun.nio.cs.UTF_8

import java.io.PrintStream

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
  def newSession(maximumProgress: Int): ProgressRecordingSession
end ProgressRecording

object NoProgressRecording extends ProgressRecording:
  override def newSession(maximumProgress: Int): ProgressRecordingSession =
    new ProgressRecordingSession:
      override def upTo(amount: Int): Unit = {}

      override def close(): Unit = {}
end NoProgressRecording

object ConsoleProgressRecording extends ProgressRecording:
  override def newSession(maximumProgress: Int): ProgressRecordingSession =
    new ProgressRecordingSession:
      private val progressBar = Option(System.console()).map(console =>
        val progressBarConsumer = new ConsoleProgressBarConsumer(
          new PrintStream(
            new WriterOutputStream(console.writer(), UTF_8.INSTANCE)
          )
        )
        val builder = new ProgressBarBuilder
        builder.setInitialMax(maximumProgress)
        builder.setConsumer(progressBarConsumer)
        builder.build()
      )

      override def upTo(amount: Int): Unit =
        progressBar.foreach(_.stepTo(amount))
      override def close(): Unit = progressBar.foreach(_.close())
end ConsoleProgressRecording

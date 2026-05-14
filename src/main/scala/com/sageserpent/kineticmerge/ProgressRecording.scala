package com.sageserpent.kineticmerge

import me.tongfei.progressbar.{ConsoleProgressBarConsumer, ProgressBarBuilder}
import org.apache.commons.lang3.time.DurationFormatUtils
import org.jline.utils.WriterOutputStream

import java.io.PrintStream
import java.nio.charset.Charset
import java.time.{Instant, Duration as JavaDuration}

/** Records progress from zero up to some implied maximum set up by
  * [[ProgressRecording.newSession]]. Progress may be moved up or down; so it is
  * possible to use this to count progress up from zero to the maximum, or to
  * count progress down from the maximum to zero.
  */
trait ProgressRecordingSession(val label: String, val maximumProgress: Int)
    extends AutoCloseable:
  private val startTime = Instant.now()

  /** @param amount
    *   Zero or positive amount of absolute progress.
    */
  def upTo(amount: Int): Unit

  override def close(): Unit =
    val duration = JavaDuration.between(startTime, Instant.now())
    println(
      s"$label $maximumProgress (Completed in: ${DurationFormatUtils.formatDurationHMS(duration.toMillis)})"
    )
  end close
end ProgressRecordingSession

trait ProgressRecording:
  /** Creates a new progress recording session with some initial progress
    * already recorded.
    *
    * @param label
    *   Labels a specific activity whose progress is being recorded.
    * @param maximumProgress
    *   The maximum amount of progress that can be recorded.
    * @param initialProgress
    *   This may be anywhere between zero and {@code maximumProgress} .
    * @return
    *   An instance of [[ProgressRecordingSession]] with {@code initialProgress}
    *   recorded.
    */
  def newSession(label: String, maximumProgress: Int)(
      initialProgress: Int
  ): ProgressRecordingSession
end ProgressRecording

object NoProgressRecording extends ProgressRecording:
  override def newSession(label: String, maximumProgress: Int)(
      initialProgress: Int
  ): ProgressRecordingSession =
    new ProgressRecordingSession(label, maximumProgress):
      override def upTo(amount: Int): Unit = {}
    end new
  end newSession
end NoProgressRecording

/** An instance of [[ProgressRecordingSession]] created by
  * [[ConsoleProgressRecording.newSession]] displays a progress bar on the
  * console, if one is available to the process. Closing such an instance
  * removes the progress bar.
  */
object ConsoleProgressRecording extends ProgressRecording:
  override def newSession(label: String, maximumProgress: Int)(
      initialProgress: Int
  ): ProgressRecordingSession =
    new ProgressRecordingSession(label, maximumProgress):
      private val progressBar = Option(System.console()).map(console =>
        val progressBarConsumer = new ConsoleProgressBarConsumer(
          new PrintStream(
            new WriterOutputStream(console.writer(), Charset.defaultCharset())
          )
        )
        val builder = new ProgressBarBuilder
        builder.setTaskName(this.label)
        builder.hideEta()
        builder.startsFrom(initialProgress, JavaDuration.ZERO)
        builder.setInitialMax(this.maximumProgress)
        builder.clearDisplayOnFinish()
        builder.setConsumer(progressBarConsumer)
        builder.build()
      )

      override def upTo(amount: Int): Unit =
        progressBar.foreach(_.stepTo(amount))
      override def close(): Unit =
        progressBar.foreach(_.close())
        super.close()
      end close
end ConsoleProgressRecording

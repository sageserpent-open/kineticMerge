package com.sageserpent.kineticmerge

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

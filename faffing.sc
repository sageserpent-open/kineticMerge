import com.sageserpent.kineticmerge.core.{Sources, CodeMotionAnalysis}

val aSources = new Sources:
  override type Path = String
val bSources = new Sources:
  override type Path = Int

val cSources = new Sources:
  override type Path = Boolean

val result: Either[CodeMotionAnalysis.Divergence.type, CodeMotionAnalysis] =
  CodeMotionAnalysis.of(aSources, bSources, cSources)(1.0)

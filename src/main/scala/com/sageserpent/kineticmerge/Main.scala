package com.sageserpent.kineticmerge

import java.nio.file.Path
import scala.language.postfixOps
import scala.sys.process.*
import scala.util.Try

object Main:
  def main(args: Array[String]): Unit =
    args match
      case Array(theirBranchHead) =>
        val workflow = for
          _ <- Try { "git --version" !! }.toEither.left.map(_ =>
            "Git is not available."
          )
          workingTree <- Try {
            "git rev-parse --show-toplevel" !!
          }.toEither.left.map(_ =>
            "The current working directory is not part of a Git working tree."
          )
          _ <- Try { "git diff-index --exit-code HEAD" !! }.toEither.left.map(
            _ => "There are uncommitted changes prior to commencing the merge."
          )
          workingTreePath <- Try { Path.of(workingTree) }.toEither.left.map(_ =>
            s"Unexpected error: working tree reported by Git: $workingTree is not a valid path."
          )
          bestAncestorCommit <- Try {
            s"git merge-base HEAD $theirBranchHead" !!
          }.toEither.left.map(_ =>
            s"$theirBranchHead is not a valid branch or commit."
          )
        yield bestAncestorCommit

        workflow.fold(
          exception =>
            println(exception)
            System.exit(1)
          ,
          commit =>
            println(commit)
            0
        )
      case Array() =>
        Console.err.println("No branch or commit id provided to merge from.")
        System.exit(2)
      case _ =>
        Console.err.println(
          s"Expected a single branch or commit id, but got multiple entries shown below:\n${args.mkString("\n")}"
        )
        System.exit(3)
end Main

package com.sageserpent.kineticmerge

import cats.syntax.traverse.toTraverseOps

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
          ourChanges <- Try {
            s"git diff --no-renames --name-status $bestAncestorCommit HEAD".lazyLines.toList
          }.toEither.left
            .map(_ =>
              s"Can't determine changes made on our branch since ancestor commit $bestAncestorCommit"
            )
            .flatMap(_.traverse(pathChangeFor))
          theirChanges <- Try {
            s"git diff --no-renames --name-status $bestAncestorCommit $theirBranchHead".lazyLines.toList
          }.toEither.left
            .map(_ =>
              s"Can't determine changes made on their branch $theirBranchHead since ancestor commit $bestAncestorCommit"
            )
            .flatMap(_.traverse(pathChangeFor))
        yield (ourChanges, theirChanges)

        workflow.fold(
          exception =>
            println(exception)
            System.exit(1)
          ,
          stuff =>
            println(stuff)
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

  private def pathChangeFor(line: String): Either[String, (Path, Changed)] =
    Try {
      line.split("\\s+") match
        case Array("M", path) => Path.of(path) -> Changed.Modified
        case Array("A", path) => Path.of(path) -> Changed.Added
        case Array("D", path) => Path.of(path) -> Changed.Deleted
    }.toEither.left.map(_ =>
      s"Unexpected error - can't parse changes reported by Git: $line"
    )

  private enum Changed:
    case Modified
    case Added
    case Deleted
  end Changed

end Main

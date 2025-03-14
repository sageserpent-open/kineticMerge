# kineticMerge

[![Maven Central](https://index.scala-lang.org/sageserpent-open/kineticmerge/kinetic-merge/latest-by-scala-version.svg?color=2465cd&style=flat)](https://index.scala-lang.org/sageserpent-open/kineticmerge/kinetic-merge)

[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

Merge a heavily refactored codebase and stay sane.

(**Release - 1.6.0**).

YouTube:

- [Kinetic Merge: Merging through a File Split](https://youtu.be/JHb9DKK0LIA)
- [Kinetic Merge: Complex Merge Demonstration](https://youtu.be/6jry6NKxGJA)

<video src="https://github.com/sageserpent-open/kineticMerge/assets/1765601/2d676e20-dce0-441a-b4a2-fcbaa8aff35d"><video/>

<video src="https://github.com/sageserpent-open/kineticMerge/assets/1765601/783172ef-7688-45ab-83ab-2e0aa633ce01"><video/>

<video src="https://github.com/sageserpent-open/kineticMerge/assets/1765601/370ddbce-98c7-42d2-8c81-f5c2756d051d"><video/>

## Goals

- Merge two branches of a Git repository *across the entire codebase*.
- Take into account the motion of code in either branch due to refactoring.
- Handle file renames, file splits, file concatenation.
- Handle code being excised from one place in a file and moved elsewhere in that file or to somewhere within another
  file, or hived off all by itself in its own new file.
- Work alongside the usual Git workflows, allowing ordinary Git merge to take over at the end if necessary.
- A simple command line tool that tries to do as much as it can without supervision, and with minimal supervision when
  complexities are encountered.

## Installation ##

### Got [Coursier](https://get-coursier.io/) installed? ###

Then run `cs install --contrib kinetic-merge`, and you're all set to go, it will put the latest release of
`kinetic-merge` on your path.

You can also invoke Kinetic Merge via Coursier without actually installing it:

```
cs launch com.sageserpent::kinetic-merge:<RELEASE VERSION FROM GITHUB> -- <command line arguments>
```

### Not got Coursier installed? ###

Install it, then: https://get-coursier.io/docs/cli-installation.

### Don't want to install Coursier? ###

Then download a release for yourself:

```bash
curl -LJO --verbose http://github.com/sageserpent-open/kineticMerge/releases/download/v<RELEASE VERSION FROM GITHUB>/kinetic-merge

chmod a+x kinetic-merge
```

Put it on your path.

Alternatively, go to the [releases](https://github.com/sageserpent-open/kineticMerge/releases) and manually
download `kinetic-merge`. You'll still have to execute `chmod a+x kineticMerge` so you can run it.

If you're on Windows, instead of `kinetic-merge` use the companion download *`kinetic-merge.bat`*.

### Stuck in a firewalled corporate environment and can't just download anything you feel like, but can clone this repository? ###

If so, clone this repository locally (or just download it as a source drop), install SBT and run `sbt packageExecutable`
in the top level of the directory you've cloned / downloaded - that will place a freshly baked executable and companion
Windows batch file in the `target` subdirectory.

### Want to test-drive the programmatic API from JShell and have access to Maven Central or Sonatype? ###

Try the instructions [here](https://github.com/sageserpent-open/kineticMerge/issues/21#issuecomment-1932619838).

## Running it ##

Go to your Git repository. Check your repository has a branch checked out (so not a *bare* repository) and that it
doesn't have uncommitted work; decide what branch you want to merge from and off you go:

```bash
git status

kinetic-merge <the branch you want to merge into the current one>
```

If the merge goes through cleanly, Kinetic Merge will make a merge commit and advance the current branch to it, just
like `git merge` would do.

If the merge encounters conflicts, Kinetic Merge will do as much merging as it can up-front, and stage conflicting
*partially merged* files into the Git index along with writing the file with conflict markers in it, so you can use your
usual workflow to resolve the conflicts. You can open up an editor directly on the file and resolve the conflicts by
hand - mark them as resolved in the usual way with `git add <resolved file>` and the commit with `git commit`, or
just `git merge --continue` - or use your usual IDE to resolve the conflicts; I use IntelliJ, that works nicely.

Unlike a conventional Git merge, if you use an IDE that reads the staged conflicting files, you will see that the *left*
and *right* versions are already partially merged for you.

What's more, the staged files and the conflicts already have any code motion applied to them, so they are less
confusing; the conflicts are shown in the right place.

It supports fast-forwarding, plus the `--no-ff` and `--no-commit` options too. Use `--help` if you need a reminder.

## Why?

Meet Noah Shortcut and Seymour Checks, two likely software engineers
from [ThreePhantasticTales](http://www.octopull.co.uk/sw-dev/ThreePhantasticTails.html), and their manager, Mr Deadline.

Some time has passed since that those tales were told, and Messrs Shortcut and Checks work with Java in some giant
corporate blob these days. Noah likes to work lean and mean with Emacs or vi, or is it Atom or Sublime now? He's pumping
out code straight into CI/CD as fast as possible with no tests to slow him down, and Mr Deadline is very happy. Seymour
likes TDD, also spends a lot of time adding tests to the existing codebase before working on new functionality, and
refactors the old codebase a lot with IntelliJ or Visual Studio Code to keep it tractable.

All this would be great, only each time a PR is raised, all hell breaks loose when Seymour's beautifully rearranged code
hits Noah's latest tidal wave of new functionality that has been hacked in place.

As an example, Seymour likes to extract helper methods from overly long passages of code to make them comprehensible -
so the extracted code is moved around in the file. He has method sorting switched on, so the newly extracted methods are
moved far away from the original locations. In fact, every time Seymour starts working on the codebase, the method
sorting rearranges all the code written in the last tidal wave by Noah, who just lays new code down fast and loose. When
files get too big to read, Seymour extracts classes and puts them into other files, and if implementing classes get too
weighty, some of their methods and state get hoisted up into abstract classes, or into interfaces as default methods.
Classes get renamed, and IntelliJ sensibly renames the file to match.

Come the PR, Noah's changes all live in their original location, and so Git regards the merge as fusing two radically
different sets of changes - other than some fairly simple file renames with perhaps a few edits, it can't follow all the
code motion due to Seymour's refactoring. So the PR is either rejected as unworkable by Mr Deadline, or Seymour
painstakingly and time-consumingly tries to resolve the many conflicts by pick-axeing through the code to match what
went where, or Noah simply copies and pastes code from the head of one branch into the other without any idea as to
whether the 'change' was really made since the shared base commit of the PR, or is simply a reversion back to old code.

Kinetic Merge's job is to augment the process of merging in Git so that the code motion due to refactoring is sensibly
interpreted, taking into account all the files in the repository. If it can do a clean merge, it will and Git will see
an ordinary merge commit. If it can't fully complete the merge, it writes the same staging information that Git would in
a conflicted merge; it then hands over to Git and you, the user, to resolve the final conflicts - but it tries to take
the code motion pain out of the process before it hands over, so that the final manual merge should feel like a simple
one.

## Status

Well, it works; there are stable releases. Code motion is tracked both intra-file and inter-file. You can split a file
into pieces on one branch and
edit the original file on another - those edits will find their way into the right places when you merge.
Correspondingly, you can join several files together on one branch and edit all of them on another - again, the edits
will arrive in the right place on merging. Cool.

As befits any piece of non-vapourware, there are known bugs / deficiencies and there are always new features to add, but
that's why Github provides an issue tracker.

The author uses it regularly on his own projects.

Give it a spin, do raise bug tickets,
see [https://github.com/sageserpent-open/kineticMerge/issues/21](https://github.com/sageserpent-open/kineticMerge/issues/21)
for guidance.

Bear in mind you can use either `--no-commit` or rollback with `git merge --abort` or `git reset --hard`, but know what
you're doing before you use the third technique.

## Simple Use Cases

[Behold the Chamber of Horrors...](./documents/EXAMPLES.md)

## Honourable Mentions

For a while, this project made heavy use of Rabin fingerprinting, which is a kind of rolling hash *distinct* from the
rolling polynomial hash frequently used in the Rabin-Karp algorithm we all know and love.

In the end, a simpler rolling polynomial hash was good enough and a lot faster to compute, but if you're looking for a
good collision resistant hash, head over to this
project: [rabinfingerprint](https://github.com/themadcreator/rabinfingerprint).
The price paid by Kinetic Merge for dropping this was the appearance of a bug due to the higher probability of
collisions using rolling polynomial hashing; that has been since fixed. The fact that the bug was latent in the code for
so long is a testament to the collision resistance of Rabin fingerprinting!

## Design Documents

- [Job Description](./documents/designNotes/jobDescription.md)
- [Problem Space](./documents/designNotes/problemSpace.md)
- [Code Motion](./documents/designNotes/codeMotion.md)
- [Component Breakdown](./documents/designNotes/componentBreakdown.md)
- [`CodeMotionAnalysis.of`](./documents/designNotes/codeMotionAnalysis.md)
- [Blind Alleys](./documents/designNotes/blindAlleys.md)
- [Skeletons in the Cupboard](./documents/designNotes/skeletonsInTheCupboard.md)

## Do you use these libraries yet?

Direct dependencies in `build.sbt` and why they are used...

- `scala-logging` and `logback-*`. Duh, of course. Lots of documentation, does what it says on the tin.
- `cats-*`. For all those slightly high-church functional programming idioms that you don't want to write and test from
  scratch, not to mention keep maintaining. Again, *documentation*.
- `alleycats-core`. For when you need the Devil to accompany you to mass in the high-church.
- `scopt`. No-nonsense, robust command line parsing. Small and gets the job done.
- `scala-collection-contrib`. `MultiDict`, and in the past for `MapDecorator`. Move on from `MultiMap`!
- `monocle`. Try rewriting the code without lenses and you'll see why.
- `scala-parser-combinators`. No-nonsense, robust tokenization in a pure functional manner.
- `os-lib`. Stop using the various messy Java and Scala APIs for paths, files and processes and just use this for a
  well-documented, coherent, one-stop-shop experience.
- `fansi`. I like <ins>underlined</ins> text in my application messages for things that vary.
- `pprint`. *Readable* debugging output.
- `tagging`. Stop your code from being both *stringly-typed* and looking like 1970s C with all those integers.
- `guava`. Hashing, hashing and yet more hashing.
- `fingertree`. A performant and robust way of dealing with interval testing, as well as nice example of a
  pure-functional core fingertree implementation with various front-end APIs. At time of writing, the APIs are a bit
  rough-and-ready, but it does the job well.
- `caffeine`. If you want a well-documented, one-stop-shop Java caching API that plugs into everything and is endlessly
  configurable, here it is.
- `progressbar`. A straightforward way of displaying a progress bar for a console application.
- `americium`. Possibly the world's best parameterised test support framework. Your scribe is of course completely
  unbiased. If you want tests that automatically shrink down failing test cases, you're in the right place.
- `expecty`. Scala's most minimal and yet most effective assertion framework. Less is more!
- `jupiter-interface`. Yes, use *JUnit5 for your Scala tests*. Works a treat in concert with `americium` and `expecty`.

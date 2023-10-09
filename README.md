# kineticMerge
Merge a heavily refactored codebase and stay sane.

## Goals
- Merge two branches of a Git repository *across the entire codebase*.
- Take into account the motion of code in either branch due to refactoring.
- Handle file renames, file splits, file concatenation.
- Handle code being excised from one place in a file and moved elsewhere in that file or to somewhere within another file, or hived off all by itself in its own new file.
- Work alongside the usual Git workflows, allowing ordinary Git merge to take over at the end if necessary.
- A simple command line tool that tries to do as much as it can without supervision, and with minimal supervision when complexities are encountered.

## Installation ##

```bash
curl -LJO --verbose http://github.com/sageserpent-open/kineticMerge/releases/download/v<release version>/kinetic-merge

chmod ug+x kineticMerge
```

Alternatively, go to the [releases](https://github.com/sageserpent-open/kineticMerge/releases) and manually download `kinetic-merge`. You'll still have to execute `chmod ug+x kineticMerge` so you can run it.

If you're on Windows, change `kinetic-merge` to *`kinetic-merge.bat`*.

## Why?
Meet Noah Shortcut and Seymour Checks, two likely software engineers from [ThreePhantasticTales](http://www.octopull.co.uk/sw-dev/ThreePhantasticTails.html), and their manager, Mr Deadline.

Some time has passed since that those tales were told, and Messrs Shortcut and Checks work with Java in some giant corporate blob these days. Noah likes to work lean and mean with Emacs or vi, or is it Atom or Sublime now? He's pumping out code straight into CI/CD as fast as possible with no tests to slow him down, and Mr Deadline is very happy. Seymour likes TDD, also spends a lot of time adding tests to the existing codebase before working on new functionality, and refactors the old codebase a lot with IntelliJ or Visual Studio Code to keep it tractable.

All this would be great, only each time a PR is raised, all hell breaks loose when Seymour's beautifully rearranged code hits Noah's latest tidal wave of new functionality that has been hacked in place.

As an example, Seymour likes to extract helper methods from overly long passages of code to make them comprehensible - so the extracted code is moved around in the file. He has method sorting switched on, so the newly extracted methods are moved far away from the original locations. In fact, every time Seymour starts working on the codebase, the method sorting rearranges all the code written in the last tidal wave by Noah, who just lays new code down fast and loose. When files get too big to read, Seymour extracts classes and puts them into other files, and if implementing classes get too weighty, some of their methods and state get hoisted up into abstract classes, or into interfaces as default methods. Classes get renamed, and IntelliJ sensibly renames the file to match.

Come the PR, Noah's changes all live in their original location, and so Git regards the merge as fusing two radically different sets of changes - other than some fairly simple file renames with perhaps a few edits, it can't follow all the code motion due to Seymour's refactoring. So the PR is either rejected as unworkable by Mr Deadline, or Seymour painstakingly and time-consumingly tries to resolve the many conflicts by pick-axeing through the code to match what went where, or Noah simply copies and pastes code from the head of one branch into the other without any idea as to whether the 'change' was really made since the shared base commit of the PR, or is simply a reversion back to old code.

Kinetic Merge's job is to augment the process of merging in Git so that the code motion due to refactoring is sensibly interpreted, taking into account all the files in the repository. If it can do a clean merge, it will and Git will see an ordinary merge commit. If it can't fully complete the merge, it writes the same staging information that Git would in a conflicted merge; it then hands over to Git and you, the user, to resolve the final conflicts - but it tries to take the code motion pain out of the process before it hands over, so that the final manual merge should feel like a simple one.

## Status
Well, that's the plan. It's no longer vapourware, there is a release, but the code motion aspect isn't supported at all yet - it is currently a conventional file-by-file merge tool, although the merge algorithm is I believe novel. Whether you like what is currently does is for you to judge, try it out.

Be aware that things will be changing in that department as support for code motion is introduced - this has quite a large bearing on the granularity of the merging, which is currently extremely fine grained and will sometimes produce some _surprising_ results.

## Simple Use Cases
[Behold the Chamber of Horrors...](./documents/EXAMPLES.md)

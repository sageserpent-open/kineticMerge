# Project: kineticMerge

# Orientation

The project is split into three subprojects:

1. A core library.
2. A CLI application that works with a Git repository.
3. A tool CLI application that works with directory trees.

Before exploring Scala source files or proposing architectural changes, **read the relevant design documents in
`documents/design-notes/` first** so you do not rediscover existing abstractions:

- `job-description.md` / `problem-space.md` — Core merge goals, code motion rules, and edge cases.
- `component-breakdown.md` — High-level architecture across the three subprojects (`core-library`,
  `git-cli-application`, `tool-cli-application`).
- `match-analysis-and-sectioned-code-construction.md` — Tokenization, rolling hashes, and how files are partitioned into
  sections.
- `parallel-matches-groups-and-block-construction.md` — Grouping matches across base/left/right and forming blocks.
- `two-tier-approximate-longest-common-subsequence.md` — The LCS matching algorithm and threshold parameters.
- `code-motion-and-migration.md` — How moved, split, and concatenated code blocks are migrated during a merge.

## Stack & Build Tool

- Language: Scala 3 (inspect `build.sbt` first for exact Scala version, compiler flags, and library dependencies).
- Build tool: `sbt`.
- From moment to moment, run targeted compilation or single test suites rather than full test runs to keep feedback
  fast:
    - Compile: `sbt Test/compile`
    - Single test: `sbt "testOnly *TestName*"`
- Some of the heavyweight property-based tests are run on a fixed time budget: these take several minutes to run, and optimising the code results in more test cases being run, rather than shorter test durations! `SectionedCodeTest` has several of these, so be careful there.
- `SectionedCodeExtensionTest` is fairly quick to run, and exercises quite a lot of the codebase, so it makes a good first choice for moment to moment validation.
- Releases are done on GitHub, using GitHub workflows that invoke
  both https://github.com/guardian/gha-scala-library-release-workflow and JReleaser.

## Coding Conventions

- Write idiomatic, functional Scala; prefer immutable data structures (`Vector`, `List`, `Map`) and pure functions.
- By all means use monadic workflows and augment this with abstractions from the Cats library, but be sparing regarding
  use of `IO` - this has been confined to the `Main` classes so far, where exceptions and logging are unavoidable.
- 'Direct-style' syntax is used for `IO`-based workflows to make giant workflows easier to read and less daunting to
  people coming from an imperative programming background, but stick with conventional monad workflow syntax for things
  like `Option`, `Either`, collections etc.
- Include explicit return types on all public methods and values.
- Match existing formatting, naming, and package structure in the repository. Do use the Scalafmt tool to perform
  reformatting when changes are made; there is a configuration file `.scalafmt.conf` in the project root for this.
  Refrain from reformatting code that is otherwise untouched.

## Agent Workflow

- Before editing files, read the relevant source files and tests to understand existing abstractions.
- It's OK to break up work into incremental commits, as long as these represent chunks of activity that make sense in
  isolation - eg, adding a new test, or refactoring some existing code as a preliminary step to following work. It's OK
  to have a test break in a commit if the intent is to fix it up as soon as possible in a following commit - this is
  handy for providing a backstop in a complex piece of work. This also allows proof of "red-green-refactor" over a
  series of commits.
- Work isn't finished until:
    1. It is fully verified with `sbt test`.
    2. The manual benchmark has been run.
    3. Any major design changes have been recorded in directory `documents/design-notes`.

## Manual Benchmark Procedure

This reproduces a particularly tricky merge, one which has demonstrated pathological outcomes in the past. It is not
expected to go smoothly, but the idea is to aim for best effort.

1. **Set up a reproduction repository**: create a *separate clone* of the Kinetic Merge repository in a temporary
   location (e.g., `/tmp/kinetic-merge-repro`):
   ```bash
   git clone https://github.com/sageserpent-open/kineticMerge.git /tmp/kinetic-merge-repro
   cd /tmp/kinetic-merge-repro
   git checkout 6e0b6821fa8fe53465c3355793b61ac425a10c1a
   ```

2. **Run the benchmark**: run the Kinetic Merge Git CLI application *in the reproduction repository*:

   ```bash
   sbt '; project gitCliApplication ; set Compile / run / fork := true ; set Compile / run / baseDirectory := file("/tmp/kinetic-merge-repro") ; run --no-commit 55fef2f785334b7bf99fc592dd101a462cbb2c6d'
   ```
   **DANGER**: *do not* run the benchmark merge directly in the session workspace. This will clobber the project completely!

3. **Assess printed output and logs from Kinetic Merge, also looking at resulting merged files.**

    ```bash
    git status
    ```

   The execution should be timed, ideally aiming for under one minute. This is often used as a regression test, looking
   qualitatively at the outcome of the merge using one build of Kinetic Merge against another, as well as for evidence
   of performance improvement or regression.

4**Cleanup**:

   ```bash
   git merge --abort
   ```

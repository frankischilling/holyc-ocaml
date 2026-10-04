# Build provenance

`holyc version` and compiler reports embed the implementation revision selected
during the build. Running a built, staged or installed executable retains that
value regardless of its current working directory. A normal incremental build
refreshes it after a source checkout's HEAD or refs change.

Dune action paths such as `%{workspace_root}` and `%{project_root}` refer to the
build context. They can lie outside the source checkout, including under an
unrelated repository. The metadata rule passes the project path relative to its
build workspace. `version_gen` combines that path with Dune's original absolute
`DUNE_SOURCEROOT` to select the source project. See the
[Dune variable documentation](https://dune.readthedocs.io/en/latest/concepts/variables.html).

If that project has its own `.git` directory or worktree gitfile, Git must resolve
the project itself as the checkout root. An empty marker cannot grant a parent
repository's identity. Without a marker, a parent checkout must track the project's
`dune-project`, `src/dune` and `tools/version_gen.ml`. This supports a compiler
project within a larger repository while leaving an untracked archive unknown.
A nested project checkout takes precedence over workspace Git. The generator
removes inherited Git location overrides before querying the selected source
directory; build and invocation locations cannot select another repository.

Source archives can supply `HOLYC_IMPLEMENTATION_COMMIT` with an exact lowercase
40-character revision. That explicit release value takes precedence. An invalid
override falls back to the original source checkout; an archive without source
authority reports `unknown`. The label identifies a revision. Release verification
still compares the actual source files with the committed tree.

`tools/test-version-metadata.ps1` checks 30 scenarios through metadata, executable,
full-build and install targets with both Dune cache modes. Its consumers run the
already-built artifacts directly, including staged and independently installed
executables. The matrix covers incremental refs, linked worktrees, independent
source/build/invocation/install directories, workspace projects, inherited Git
location overrides and archives. Fixture refs exercise Git's real resolution;
they are test identities rather than implementation commits or compiler
compatibility measurements.

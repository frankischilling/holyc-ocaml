param(
  [string]$Dune = 'dune',
  [ValidateNotNullOrEmpty()]
  [ValidateSet('src/build_metadata.ml', 'bin/probe.exe', '@all', '@install')]
  [string[]]$Targets = @('src/build_metadata.ml', 'bin/probe.exe', '@all', '@install'),
  [ValidateNotNullOrEmpty()]
  [ValidateSet('disabled', 'enabled')]
  [string[]]$CacheModes = @('disabled', 'enabled')
)

$ErrorActionPreference = 'Stop'
$repositoryRoot = Split-Path -Parent $PSScriptRoot
$temporaryRoot = [System.IO.Path]::GetFullPath([System.IO.Path]::GetTempPath())
$fixtureRoot = Join-Path $temporaryRoot ('holyc-version-' + [Guid]::NewGuid().ToString('N'))
$utf8 = New-Object System.Text.UTF8Encoding($false)
$savedEnvironment = @{}
$environmentNames = @(
  'HOLYC_IMPLEMENTATION_COMMIT', 'GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR',
  'GIT_INDEX_FILE', 'GIT_OBJECT_DIRECTORY', 'GIT_ALTERNATE_OBJECT_DIRECTORIES',
  'DUNE_SOURCEROOT'
)

function Write-FixtureText([string]$Path, [string]$Text) {
  [System.IO.File]::WriteAllText($Path, $Text, $utf8)
}

function Remove-FixtureEnvironment([string]$Name) {
  # On PowerShell 7.5+, SetEnvironmentVariable with $null sets an empty value.
  $environmentPath = 'Env:' + $Name
  if (Test-Path -LiteralPath $environmentPath) {
    Remove-Item -LiteralPath $environmentPath
  }
}

function New-ProjectFixture([string]$Name, [string]$BuildTarget, [string]$CacheMode, [string]$SourceRoot = '') {
  $projectRoot = if ($SourceRoot -eq '') { Join-Path $fixtureRoot $Name } else { $SourceRoot }
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'src') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'src/driver') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'tools') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'bin') -Force | Out-Null
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'dune-project') -Destination $projectRoot
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'holyc-ocaml.opam') -Destination $projectRoot
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/dune') -Destination (Join-Path $projectRoot 'src/dune')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/native_execution_stubs.c') -Destination (Join-Path $projectRoot 'src/native_execution_stubs.c')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/compiler_hash_stubs.c') -Destination (Join-Path $projectRoot 'src/compiler_hash_stubs.c')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/compiler_control_stubs.c') -Destination (Join-Path $projectRoot 'src/compiler_control_stubs.c')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/driver/version.ml') -Destination (Join-Path $projectRoot 'src/driver/version.ml')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/driver/version.mli') -Destination (Join-Path $projectRoot 'src/driver/version.mli')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'tools/version_gen.ml') -Destination (Join-Path $projectRoot 'tools/version_gen.ml')
  Write-FixtureText (Join-Path $projectRoot 'tools/dune') "(executable (name version_gen) (modules version_gen) (libraries unix))`n"
  Write-FixtureText (Join-Path $projectRoot 'bin/dune') "(executable (name probe) (public_name holyc-provenance-probe) (package holyc-ocaml) (libraries holyc_lib))`n"
  Write-FixtureText (Join-Path $projectRoot 'bin/probe.ml') "let () = print_endline Holyc_lib.Driver.Version.implementation_commit`n"
  return [PSCustomObject]@{
    Root = $projectRoot
    WorkspaceRoot = $projectRoot
    SourceRelativePath = '.'
    BuildDirectory = Join-Path $projectRoot '_build'
    InvocationDirectory = $projectRoot
    InstallPrefix = Join-Path $fixtureRoot ("install-$Name")
    BuildTarget = $BuildTarget
    CacheMode = $CacheMode
    LastExpected = $null
  }
}

function Read-Consumer([string]$Path) {
  $actual = & $Path
  if ($LASTEXITCODE -ne 0) { throw "Version consumer failed: $Path" }
  return ($actual -join "`n").Trim()
}

function Assert-Metadata($Fixture, [string]$Expected, [string]$Label) {
  $label = "$($Fixture.CacheMode) $($Fixture.BuildTarget): $Label"
  $projectBuild = Join-Path (Join-Path $Fixture.BuildDirectory 'default') $Fixture.SourceRelativePath
  $executable = Join-Path $projectBuild 'bin/probe.exe'
  Push-Location -LiteralPath $Fixture.InvocationDirectory
  try {
    if ($null -ne $Fixture.LastExpected -and $Fixture.BuildTarget -ne 'src/build_metadata.ml') {
      $before = Read-Consumer $executable
      if ($before -ne $Fixture.LastExpected) {
        throw "Existing executable changed identity before rebuilding for ${label}: got $before"
      }
    }
    # Supply the target as a string argument. Bare @all/@install are PowerShell
    # variable splats and may disappear before Dune receives the command.
    $target = if ($Fixture.SourceRelativePath -eq '.') {
      $Fixture.BuildTarget
    } elseif ($Fixture.BuildTarget.StartsWith('@')) {
      '@' + $Fixture.SourceRelativePath + '/' + $Fixture.BuildTarget.Substring(1)
    } else {
      "$($Fixture.SourceRelativePath)/$($Fixture.BuildTarget)"
    }
    & $Dune build --root $Fixture.WorkspaceRoot --build-dir $Fixture.BuildDirectory "--cache=$($Fixture.CacheMode)" --display=quiet $target
    if ($LASTEXITCODE -ne 0) { throw "Provenance build failed: $label" }
    $actual = [System.IO.File]::ReadAllText((Join-Path $projectBuild 'src/build_metadata.ml')).Trim()
    $expectedLine = 'let implementation_commit = "' + $Expected + '"'
    if ($actual -ne $expectedLine) {
      throw "Stale metadata for ${label}: expected $expectedLine; got $actual"
    }
    if ($Fixture.BuildTarget -ne 'src/build_metadata.ml') {
      # Do not use dune exec here: the assertion must not refresh the artifact.
      $actual = Read-Consumer $executable
      if ($actual -ne $Expected) {
        throw "Stale executable for ${label}: expected $Expected; got $actual"
      }
    }
    if ($Fixture.BuildTarget -eq '@install') {
      $installedName = 'holyc-provenance-probe'
      if ([System.IO.Path]::DirectorySeparatorChar -eq '\') { $installedName += '.exe' }
      $installed = Join-Path $Fixture.BuildDirectory "install/default/bin/$installedName"
      $actual = Read-Consumer $installed
      if ($actual -ne $Expected) {
        throw "Stale install artifact for ${label}: expected $Expected; got $actual"
      }
      & $Dune install --root $Fixture.WorkspaceRoot --build-dir $Fixture.BuildDirectory --prefix $Fixture.InstallPrefix --display=quiet
      if ($LASTEXITCODE -ne 0) { throw "Provenance installation failed: $label" }
      $actual = Read-Consumer (Join-Path $Fixture.InstallPrefix "bin/$installedName")
      if ($actual -ne $Expected) {
        throw "Stale installed executable for ${label}: expected $Expected; got $actual"
      }
    }
    $Fixture.LastExpected = $Expected
    Write-Output "Verified $label"
  } finally {
    Pop-Location
  }
}

function Test-Scenarios([string]$BuildTarget, [string]$CacheMode, [int]$Index) {
  Remove-FixtureEnvironment 'HOLYC_IMPLEMENTATION_COMMIT'
  $normal = New-ProjectFixture "$Index-normal" $BuildTarget $CacheMode
  $gitDir = Join-Path $normal.Root '.git'
  New-Item -ItemType Directory -Path (Join-Path $gitDir 'objects') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $gitDir 'refs/heads') -Force | Out-Null
  Write-FixtureText (Join-Path $gitDir 'config') "[core]`nrepositoryformatversion = 0`nbare = false`n"
  Write-FixtureText (Join-Path $gitDir 'HEAD') "ref: refs/heads/probe`n"

  # Synthetic refs exercise Git's real resolution without creating commits or
  # changing the implementation checkout. No source or build rule changes
  # between successive assertions in one fixture.
  $first = 'a' * 40
  $second = 'b' * 40
  $third = 'c' * 40
  $fourth = 'd' * 40
  $looseRef = Join-Path $gitDir 'refs/heads/probe'
  Write-FixtureText $looseRef "$first`n"
  Assert-Metadata $normal $first 'initial loose ref'
  Write-FixtureText $looseRef "$second`n"
  Assert-Metadata $normal $second 'changed loose ref without cleaning'
  Write-FixtureText (Join-Path $gitDir 'refs/heads/other') "$first`n"
  Write-FixtureText (Join-Path $gitDir 'HEAD') "ref: refs/heads/other`n"
  Assert-Metadata $normal $first 'branch switch without cleaning'
  Write-FixtureText (Join-Path $gitDir 'HEAD') "ref: refs/heads/probe`n"
  Remove-Item -LiteralPath $looseRef
  Write-FixtureText (Join-Path $gitDir 'packed-refs') "$third refs/heads/probe`n"
  Assert-Metadata $normal $third 'packed ref without cleaning'
  Write-FixtureText (Join-Path $gitDir 'HEAD') "$fourth`n"
  Assert-Metadata $normal $fourth 'detached HEAD without cleaning'

  $linked = New-ProjectFixture "$Index-linked" $BuildTarget $CacheMode
  $linkedGitDir = Join-Path $gitDir 'worktrees/probe'
  New-Item -ItemType Directory -Path $linkedGitDir -Force | Out-Null
  Write-FixtureText (Join-Path $linkedGitDir 'HEAD') "ref: refs/heads/probe`n"
  Write-FixtureText (Join-Path $linkedGitDir 'commondir') "../..`n"
  Write-FixtureText (Join-Path $linked.Root '.git') "gitdir: $linkedGitDir`n"
  Assert-Metadata $linked $third 'linked worktree gitfile'
  Write-FixtureText (Join-Path $gitDir 'packed-refs') "$first refs/heads/probe`n"
  Assert-Metadata $linked $first 'changed shared packed ref'

  $env:HOLYC_IMPLEMENTATION_COMMIT = $second
  Assert-Metadata $linked $second 'explicit release override'
  $env:HOLYC_IMPLEMENTATION_COMMIT = $fourth
  Assert-Metadata $linked $fourth 'changed release override'
  $env:HOLYC_IMPLEMENTATION_COMMIT = 'invalid-override'
  Assert-Metadata $linked $first 'invalid override falls back to Git'
  Remove-FixtureEnvironment 'HOLYC_IMPLEMENTATION_COMMIT'
  Assert-Metadata $linked $first 'removed release override'
  $archive = New-ProjectFixture "$Index-archive" $BuildTarget $CacheMode
  $env:HOLYC_IMPLEMENTATION_COMMIT = $third
  Assert-Metadata $archive $third 'explicit source-archive provenance'
  Remove-FixtureEnvironment 'HOLYC_IMPLEMENTATION_COMMIT'
  Assert-Metadata $archive 'unknown' 'source archive without an override'

  $foreignRoot = Join-Path $fixtureRoot "$Index-foreign"
  $foreignGit = Join-Path $foreignRoot '.git'
  New-Item -ItemType Directory -Path (Join-Path $foreignGit 'objects') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $foreignGit 'refs/heads') -Force | Out-Null
  Write-FixtureText (Join-Path $foreignGit 'config') "[core]`nrepositoryformatversion = 0`nbare = false`n"
  Write-FixtureText (Join-Path $foreignGit 'HEAD') ((('e' * 40) + "`n"))

  $external = $normal.PSObject.Copy()
  $external.BuildDirectory = Join-Path $foreignRoot 'external output with spaces'
  $external.InvocationDirectory = $foreignRoot
  $external.LastExpected = $null
  Assert-Metadata $external $fourth 'external output under a different checkout'
  Write-FixtureText (Join-Path $foreignGit 'HEAD') ((('f' * 40) + "`n"))
  Assert-Metadata $external $fourth 'unrelated checkout changes do not retag the source'
  Write-FixtureText (Join-Path $gitDir 'HEAD') "$second`n"
  Assert-Metadata $external $second 'external output refreshes changed source HEAD'
  Write-FixtureText (Join-Path $gitDir 'HEAD') "ref: refs/heads/probe`n"
  Write-FixtureText (Join-Path $gitDir 'packed-refs') "$third refs/heads/probe`n"
  Assert-Metadata $external $third 'external output follows original packed refs'

  $env:GIT_DIR = $foreignGit
  $env:GIT_WORK_TREE = $foreignRoot
  $env:GIT_COMMON_DIR = $foreignGit
  try {
    Assert-Metadata $external $third 'ambient Git location cannot select another checkout'
  } finally {
    Remove-FixtureEnvironment 'GIT_DIR'
    Remove-FixtureEnvironment 'GIT_WORK_TREE'
    Remove-FixtureEnvironment 'GIT_COMMON_DIR'
  }
  $env:DUNE_SOURCEROOT = $foreignRoot
  try {
    Assert-Metadata $external $third 'Dune supplies the original root despite caller environment'
  } finally {
    Remove-FixtureEnvironment 'DUNE_SOURCEROOT'
  }

  $externalLinked = $linked.PSObject.Copy()
  $externalLinked.BuildDirectory = Join-Path $foreignRoot 'linked-output'
  $externalLinked.InvocationDirectory = $foreignRoot
  $externalLinked.LastExpected = $null
  Assert-Metadata $externalLinked $third 'external output retains linked worktree identity'
  Write-FixtureText (Join-Path $gitDir 'packed-refs') "$first refs/heads/probe`n"
  Assert-Metadata $externalLinked $first 'external linked output follows changed shared ref'

  $outside = $normal.PSObject.Copy()
  $outside.BuildDirectory = Join-Path $fixtureRoot "$Index-output-outside-git"
  $outside.InvocationDirectory = $fixtureRoot
  $outside.LastExpected = $null
  Assert-Metadata $outside $first 'external output and invocation outside Git'

  $nested = New-ProjectFixture "$Index-nested" $BuildTarget $CacheMode (Join-Path $foreignRoot 'nested-source')
  $nestedGit = Join-Path $nested.Root '.git'
  New-Item -ItemType Directory -Path (Join-Path $nestedGit 'objects') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $nestedGit 'refs/heads') -Force | Out-Null
  Write-FixtureText (Join-Path $nestedGit 'config') "[core]`nrepositoryformatversion = 0`nbare = false`n"
  Write-FixtureText (Join-Path $nestedGit 'HEAD') "$second`n"
  $nested.BuildDirectory = Join-Path $fixtureRoot "$Index-nested-output"
  $nested.InvocationDirectory = $foreignRoot
  Assert-Metadata $nested $second 'nested source checkout owns its revision'

  $nestedArchive = New-ProjectFixture "$Index-nested-archive" $BuildTarget $CacheMode (Join-Path $foreignRoot 'archive-source')
  $nestedArchive.BuildDirectory = Join-Path $fixtureRoot "$Index-archive-output"
  $nestedArchive.InvocationDirectory = $foreignRoot
  Assert-Metadata $nestedArchive 'unknown' 'archive cannot adopt an ancestor checkout'
  $env:HOLYC_IMPLEMENTATION_COMMIT = $second
  Assert-Metadata $nestedArchive $second 'nested archive accepts explicit release provenance'
  $env:HOLYC_IMPLEMENTATION_COMMIT = 'invalid-override'
  Assert-Metadata $nestedArchive 'unknown' 'invalid archive override cannot acquire ancestor identity'
  Remove-FixtureEnvironment 'HOLYC_IMPLEMENTATION_COMMIT'

  $incomplete = New-ProjectFixture "$Index-incomplete" $BuildTarget $CacheMode (Join-Path $foreignRoot 'incomplete-source')
  New-Item -ItemType Directory -Path (Join-Path $incomplete.Root '.git') -Force | Out-Null
  $incomplete.BuildDirectory = Join-Path $fixtureRoot "$Index-incomplete-output"
  $incomplete.InvocationDirectory = $foreignRoot
  Assert-Metadata $incomplete 'unknown' 'incomplete checkout marker cannot adopt an ancestor'

  $workspaceRoot = Join-Path $fixtureRoot "$Index-workspace"
  $workspaceGit = Join-Path $workspaceRoot '.git'
  New-Item -ItemType Directory -Path (Join-Path $workspaceGit 'objects') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $workspaceGit 'refs/heads') -Force | Out-Null
  Write-FixtureText (Join-Path $workspaceGit 'config') "[core]`nrepositoryformatversion = 0`nbare = false`n"
  Write-FixtureText (Join-Path $workspaceGit 'HEAD') "ref: refs/heads/probe`n"
  Write-FixtureText (Join-Path $workspaceRoot 'dune-workspace') "(lang dune 3.12)`n"
  $workspace = New-ProjectFixture "$Index-workspace-project" $BuildTarget $CacheMode (Join-Path $workspaceRoot 'compiler')
  $workspace.WorkspaceRoot = $workspaceRoot
  $workspace.SourceRelativePath = 'compiler'
  $workspace.BuildDirectory = Join-Path $fixtureRoot "$Index-workspace-output"
  $workspace.InvocationDirectory = $fixtureRoot
  Assert-Metadata $workspace 'unknown' 'untracked workspace archive has no checkout identity'

  # Only the temporary fixture's index is populated. This creates no commits.
  & git -C $workspaceRoot add -- compiler/dune-project compiler/src/dune compiler/tools/version_gen.ml
  if ($LASTEXITCODE -ne 0) { throw 'Failed to populate the fixture source index' }
  Write-FixtureText (Join-Path $workspaceGit 'HEAD') "$first`n"
  Assert-Metadata $workspace $first 'tracked project inside a parent checkout retains its identity'
  $childGit = Join-Path $workspace.Root '.git'
  New-Item -ItemType Directory -Path (Join-Path $childGit 'objects') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $childGit 'refs/heads') -Force | Out-Null
  Write-FixtureText (Join-Path $childGit 'config') "[core]`nrepositoryformatversion = 0`nbare = false`n"
  Write-FixtureText (Join-Path $childGit 'HEAD') "$second`n"
  Assert-Metadata $workspace $second 'nested project checkout takes precedence over workspace Git'
}

try {
  foreach ($name in $environmentNames) {
    $savedEnvironment[$name] = [Environment]::GetEnvironmentVariable($name, 'Process')
    Remove-FixtureEnvironment $name
  }
  $index = 0
  foreach ($cacheMode in $CacheModes) {
    foreach ($target in $Targets) {
      Test-Scenarios $target $cacheMode $index
      $index++
    }
  }
} finally {
  foreach ($name in $savedEnvironment.Keys) {
    if ($null -eq $savedEnvironment[$name]) {
      Remove-FixtureEnvironment $name
    } else {
      [Environment]::SetEnvironmentVariable($name, $savedEnvironment[$name], 'Process')
    }
  }
  $resolvedFixture = [System.IO.Path]::GetFullPath($fixtureRoot)
  $expectedParent = [System.IO.Path]::GetFullPath($temporaryRoot).TrimEnd([System.IO.Path]::DirectorySeparatorChar)
  $actualParent = [System.IO.Path]::GetDirectoryName($resolvedFixture).TrimEnd([System.IO.Path]::DirectorySeparatorChar)
  if ($actualParent -ne $expectedParent -or
      -not ([System.IO.Path]::GetFileName($resolvedFixture).StartsWith('holyc-version-'))) {
    throw "Refusing to remove a fixture outside the temporary directory: $resolvedFixture"
  }
  if (Test-Path -LiteralPath $resolvedFixture) {
    Remove-Item -LiteralPath $resolvedFixture -Recurse -Force
  }
}

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
  'GIT_INDEX_FILE', 'GIT_OBJECT_DIRECTORY', 'GIT_ALTERNATE_OBJECT_DIRECTORIES'
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

function New-ProjectFixture([string]$Name, [string]$BuildTarget, [string]$CacheMode) {
  $projectRoot = Join-Path $fixtureRoot $Name
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'src') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'src/driver') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'tools') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'bin') -Force | Out-Null
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'dune-project') -Destination $projectRoot
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'holyc-ocaml.opam') -Destination $projectRoot
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/dune') -Destination (Join-Path $projectRoot 'src/dune')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/driver/version.ml') -Destination (Join-Path $projectRoot 'src/driver/version.ml')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/driver/version.mli') -Destination (Join-Path $projectRoot 'src/driver/version.mli')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'tools/version_gen.ml') -Destination (Join-Path $projectRoot 'tools/version_gen.ml')
  Write-FixtureText (Join-Path $projectRoot 'tools/dune') "(executable (name version_gen) (modules version_gen) (libraries unix))`n"
  Write-FixtureText (Join-Path $projectRoot 'bin/dune') "(executable (name probe) (public_name holyc-provenance-probe) (package holyc-ocaml) (libraries holyc_lib))`n"
  Write-FixtureText (Join-Path $projectRoot 'bin/probe.ml') "let () = print_endline Holyc_lib.Driver.Version.implementation_commit`n"
  return [PSCustomObject]@{
    Root = $projectRoot
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
  $executable = Join-Path $Fixture.Root '_build/default/bin/probe.exe'
  Push-Location -LiteralPath $Fixture.Root
  try {
    if ($null -ne $Fixture.LastExpected -and $Fixture.BuildTarget -ne 'src/build_metadata.ml') {
      $before = Read-Consumer $executable
      if ($before -ne $Fixture.LastExpected) {
        throw "Existing executable changed identity before rebuilding for ${label}: got $before"
      }
    }
    # Supply the target as a string argument. Bare @all/@install are PowerShell
    # variable splats and may disappear before Dune receives the command.
    & $Dune build "--cache=$($Fixture.CacheMode)" --display=quiet $Fixture.BuildTarget
    if ($LASTEXITCODE -ne 0) { throw "Provenance build failed: $label" }
    $actual = [System.IO.File]::ReadAllText((Join-Path $Fixture.Root '_build/default/src/build_metadata.ml')).Trim()
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
      $installed = Join-Path $Fixture.Root "_build/install/default/bin/$installedName"
      $actual = Read-Consumer $installed
      if ($actual -ne $Expected) {
        throw "Stale install artifact for ${label}: expected $Expected; got $actual"
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

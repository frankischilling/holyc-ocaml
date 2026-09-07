param([string]$Dune = 'dune')

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

function New-ProjectFixture([string]$Name) {
  $projectRoot = Join-Path $fixtureRoot $Name
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'src') -Force | Out-Null
  New-Item -ItemType Directory -Path (Join-Path $projectRoot 'tools') -Force | Out-Null
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'dune-project') -Destination $projectRoot
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'src/dune') -Destination (Join-Path $projectRoot 'src/dune')
  Copy-Item -LiteralPath (Join-Path $repositoryRoot 'tools/version_gen.ml') -Destination (Join-Path $projectRoot 'tools/version_gen.ml')
  Write-FixtureText (Join-Path $projectRoot 'tools/dune') "(executable (name version_gen) (modules version_gen) (libraries unix))`n"
  return $projectRoot
}

function Assert-Metadata([string]$ProjectRoot, [string]$Expected, [string]$Label) {
  Push-Location -LiteralPath $ProjectRoot
  try {
    & $Dune build --cache=disabled --display=quiet src/build_metadata.ml
    if ($LASTEXITCODE -ne 0) { throw "Metadata build failed: $Label" }
    $actual = [System.IO.File]::ReadAllText((Join-Path $ProjectRoot '_build/default/src/build_metadata.ml')).Trim()
    $expectedLine = 'let implementation_commit = "' + $Expected + '"'
    if ($actual -ne $expectedLine) {
      throw "Stale metadata for ${Label}: expected $expectedLine; got $actual"
    }
    Write-Output "Verified $Label"
  } finally {
    Pop-Location
  }
}

try {
  foreach ($name in $environmentNames) {
    $savedEnvironment[$name] = [Environment]::GetEnvironmentVariable($name, 'Process')
    Remove-FixtureEnvironment $name
  }
  $normal = New-ProjectFixture 'normal'
  $gitDir = Join-Path $normal '.git'
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
  Remove-Item -LiteralPath $looseRef
  Write-FixtureText (Join-Path $gitDir 'packed-refs') "$third refs/heads/probe`n"
  Assert-Metadata $normal $third 'packed ref without cleaning'
  Write-FixtureText (Join-Path $gitDir 'HEAD') "$fourth`n"
  Assert-Metadata $normal $fourth 'detached HEAD without cleaning'

  $linked = New-ProjectFixture 'linked'
  $linkedGitDir = Join-Path $gitDir 'worktrees/probe'
  New-Item -ItemType Directory -Path $linkedGitDir -Force | Out-Null
  Write-FixtureText (Join-Path $linkedGitDir 'HEAD') "ref: refs/heads/probe`n"
  Write-FixtureText (Join-Path $linkedGitDir 'commondir') "../..`n"
  Write-FixtureText (Join-Path $linked '.git') "gitdir: $linkedGitDir`n"
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
  $archive = New-ProjectFixture 'archive'
  $env:HOLYC_IMPLEMENTATION_COMMIT = $third
  Assert-Metadata $archive $third 'explicit source-archive provenance'
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

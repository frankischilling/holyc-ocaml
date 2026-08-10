[CmdletBinding()]
param(
  [string]$ReferenceRoot
)

$ErrorActionPreference = 'Stop'
$repositoryRoot = Split-Path -Parent $PSScriptRoot
if ([string]::IsNullOrWhiteSpace($ReferenceRoot)) {
  $ReferenceRoot = Join-Path (Join-Path $repositoryRoot 'third_party') 'TempleOS'
}
$ReferenceRoot = [System.IO.Path]::GetFullPath($ReferenceRoot)
$expectedCommit = 'c26482bb6ad3f80106d28504ec5db3c6a360732c'
$manifestPath = Join-Path (Join-Path $repositoryRoot 'reference') 'manifest.json'

if (-not (Test-Path -LiteralPath $ReferenceRoot -PathType Container)) {
  throw "TempleOS reference checkout is missing: $ReferenceRoot"
}

$actualCommit = (& git -C $ReferenceRoot rev-parse HEAD).Trim()
if ($LASTEXITCODE -ne 0) {
  throw 'Could not read the TempleOS reference commit.'
}
if ($actualCommit -ne $expectedCommit) {
  throw "TempleOS reference mismatch. Expected $expectedCommit but found $actualCommit."
}

$dirty = & git -C $ReferenceRoot status --porcelain --untracked-files=all
if ($LASTEXITCODE -ne 0) {
  throw 'Could not inspect the TempleOS reference worktree.'
}
if ($dirty) {
  throw 'TempleOS reference checkout is dirty.'
}

$manifest = Get-Content -LiteralPath $manifestPath -Raw | ConvertFrom-Json
if ($manifest.commit -ne $expectedCommit) {
  throw 'The reference manifest records a different commit.'
}

foreach ($entry in $manifest.files) {
  $file = Join-Path $ReferenceRoot $entry.path
  if (-not (Test-Path -LiteralPath $file -PathType Leaf)) {
    throw "Reference file is missing: $($entry.path)"
  }
  $actualHash = (Get-FileHash -LiteralPath $file -Algorithm SHA256).Hash.ToLowerInvariant()
  if ($actualHash -ne $entry.sha256) {
    throw "Reference checksum mismatch: $($entry.path)"
  }
}

Write-Output "Verified TempleOS reference $expectedCommit"
Write-Output "Verified $($manifest.files.Count) audited file checksums"

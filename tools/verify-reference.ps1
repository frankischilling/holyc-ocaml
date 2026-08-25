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

function Get-GitBlobSha256 {
  param(
    [Parameter(Mandatory = $true)]
    [string]$Repository,
    [Parameter(Mandatory = $true)]
    [string]$ObjectName
  )

  $startInfo = New-Object System.Diagnostics.ProcessStartInfo
  $startInfo.FileName = 'git'
  $startInfo.Arguments = "-C `"$Repository`" cat-file blob `"$ObjectName`""
  $startInfo.UseShellExecute = $false
  $startInfo.RedirectStandardOutput = $true
  $startInfo.RedirectStandardError = $true

  $process = New-Object System.Diagnostics.Process
  $process.StartInfo = $startInfo
  try {
    if (-not $process.Start()) {
      throw "Could not start git while reading $ObjectName."
    }
    $sha256 = [System.Security.Cryptography.SHA256]::Create()
    try {
      $hashBytes = $sha256.ComputeHash($process.StandardOutput.BaseStream)
    } finally {
      $sha256.Dispose()
    }
    $errorOutput = $process.StandardError.ReadToEnd()
    $process.WaitForExit()
    if ($process.ExitCode -ne 0) {
      throw "Could not read reference object $ObjectName. $errorOutput"
    }
    return [System.BitConverter]::ToString($hashBytes).Replace('-', '').ToLowerInvariant()
  } finally {
    $process.Dispose()
  }
}

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
if ($manifest.checksum_basis -ne 'git-blob-bytes') {
  throw 'The reference manifest uses an unsupported checksum basis.'
}

$parserCorpus = $manifest.parser_corpus
if ($null -eq $parserCorpus -or
    $parserCorpus.path -ne 'reference/parser-corpus-aot.json') {
  throw 'The reference manifest does not name the reviewed AOT parser corpus.'
}
$parserCorpusPath = Join-Path $repositoryRoot ([string]$parserCorpus.path)
if (-not (Test-Path -LiteralPath $parserCorpusPath -PathType Leaf)) {
  throw 'The reviewed AOT parser corpus is missing.'
}
$parserCorpusHash = (Get-FileHash -LiteralPath $parserCorpusPath -Algorithm SHA256).Hash.ToLowerInvariant()
if ($parserCorpusHash -ne $parserCorpus.baseline_sha256) {
  throw 'The reviewed AOT parser corpus checksum does not match the manifest.'
}
$parserReport = Get-Content -LiteralPath $parserCorpusPath -Raw | ConvertFrom-Json
if ($parserReport.schema -ne $parserCorpus.schema -or
    $parserReport.reference_commit -ne $expectedCommit -or
    $parserReport.compilation_mode -ne $parserCorpus.mode) {
  throw 'The reviewed AOT parser corpus metadata does not match the manifest.'
}
foreach ($field in @('files', 'both_parse', 'standalone_only',
    'project_prelude_only', 'neither_parses')) {
  if ($parserReport.summary.$field -ne $parserCorpus.$field) {
    throw "The reviewed AOT parser corpus summary differs at $field."
  }
}
foreach ($scope in @('standalone', 'project_prelude')) {
  foreach ($field in @('parses', 'frontend_diagnostics',
      'parser_diagnostics', 'read_errors', 'internal_errors', 'diagnostics')) {
    if ($parserReport.summary.$scope.$field -ne $parserCorpus.$scope.$field) {
      throw "The reviewed AOT parser corpus $scope summary differs at $field."
    }
  }
}

foreach ($entry in $manifest.files) {
  $relativePath = [string]$entry.path
  $segments = $relativePath -split '/'
  if ([System.IO.Path]::IsPathRooted($relativePath) -or
      $relativePath -match '[\x00-\x1f"\\]' -or
      $segments.Count -eq 0 -or
      $segments -contains '' -or
      $segments -contains '.' -or
      $segments -contains '..') {
    throw "Reference manifest contains an unsafe path: $relativePath"
  }
  $file = Join-Path $ReferenceRoot $relativePath
  if (-not (Test-Path -LiteralPath $file -PathType Leaf)) {
    throw "Reference file is missing: $relativePath"
  }
  $objectName = "${expectedCommit}:$relativePath"
  $actualHash = Get-GitBlobSha256 -Repository $ReferenceRoot -ObjectName $objectName
  if ($actualHash -ne $entry.sha256) {
    throw "Reference checksum mismatch: $relativePath"
  }
}

Write-Output "Verified TempleOS reference $expectedCommit"
Write-Output "Verified $($manifest.files.Count) audited file checksums"
Write-Output 'Verified the reviewed AOT parser corpus'

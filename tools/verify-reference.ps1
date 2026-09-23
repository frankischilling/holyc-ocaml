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

function Get-PinnedSourceEntries {
  param([string]$Repository, [string]$Commit, [string[]]$Extensions)

  $startInfo = New-Object System.Diagnostics.ProcessStartInfo
  $startInfo.FileName = 'git'
  $startInfo.Arguments = "-C `"$Repository`" ls-tree -rlz --full-tree $Commit"
  $startInfo.UseShellExecute = $false
  $startInfo.RedirectStandardOutput = $true
  $startInfo.RedirectStandardError = $true
  $startInfo.StandardOutputEncoding = New-Object System.Text.UTF8Encoding($false, $true)
  $process = New-Object System.Diagnostics.Process
  $process.StartInfo = $startInfo
  try {
    if (-not $process.Start()) { throw 'Could not inspect the pinned source tree.' }
    $errorTask = $process.StandardError.ReadToEndAsync()
    $output = $process.StandardOutput.ReadToEnd()
    $process.WaitForExit()
    $errorOutput = $errorTask.GetAwaiter().GetResult()
    if ($process.ExitCode -ne 0) { throw "Could not read pinned source entries. $errorOutput" }
    foreach ($record in $output.Split([char]0)) {
      if ($record.Length -eq 0) { continue }
      if ($record -cnotmatch '^([0-9]{6}) (blob|commit) ([0-9a-f]{40}) +([0-9]+|-)\t(.+)$') {
        throw 'The pinned source tree has an unexpected entry encoding.'
      }
      if ($Matches[2] -ceq 'blob') {
        $name = $Matches[5]
        if ($Extensions -ccontains [System.IO.Path]::GetExtension($name)) {
          [PSCustomObject]@{ Path=$name; Blob=$Matches[3]; Bytes=[long]$Matches[4] }
        }
      }
    }
  } finally { $process.Dispose() }
}

function Assert-ReleaseSourceSets {
  param($Contract, $LexerCorpus, [string]$Repository, [string]$Commit)

  if ($null -eq $Contract -or $Contract.schema -cne 'holyc-release-contract-v1' -or
      $Contract.documentation -cne 'docs/release-contract.md' -or
      ($Contract.required_hosts -join ',') -cne 'linux-x86_64,windows-x86_64' -or
      ($Contract.required_source_modes -join ',') -cne 'jit,aot' -or
      ($Contract.project_roots -join ',') -cne 'Compiler/Compiler.PRJ,Kernel/Kernel.PRJ' -or
      ($Contract.extensions -join ',') -cne '.HC,.HH,.PRJ' -or
      $Contract.selection -cne 'case-sensitive Git blob path prefix and exact extension at the manifest commit' -or
      $Contract.members_hash_format -cne 'SHA-256 of UTF-8 path, NUL, lowercase Git blob id, LF for every member, sorted by ordinal path') {
    throw 'The release contract metadata differs from its supported definition.'
  }
  $expectedSets = @(
    @('whole-tree',''), @('compiler','Compiler/'), @('kernel','Kernel/'),
    @('adam','Adam/'), @('demo','Demo/')
  )
  if (@($Contract.source_sets).Count -ne $expectedSets.Count) {
    throw 'The release contract must retain every declared source set.'
  }
  $dependencies = $Contract.dependency_inputs
  if ($null -eq $dependencies -or
      $dependencies.root_tree -cne $LexerCorpus.root_tree -or
      $dependencies.selection -cne 'all Git blobs recursively reachable from root_tree, without extension filtering' -or
      $dependencies.scope -cne 'canonical available source and asset inputs; actual ordered consumption is required run evidence' -or
      ($dependencies.consumption_trace_owners -join ',') -cne '722,723,725') {
    throw 'The release dependency inputs must retain the complete canonical tree and ordered-run evidence requirement.'
  }
  $tree = & git -C $Repository rev-parse "${Commit}^{tree}"
  if ($LASTEXITCODE -ne 0 -or ($tree -join '').Trim() -cne $dependencies.root_tree) {
    throw 'The release dependency input tree does not match the pinned revision.'
  }
  $entries = @(Get-PinnedSourceEntries -Repository $Repository -Commit $Commit -Extensions $Contract.extensions)
  $byPath = New-Object 'System.Collections.Generic.Dictionary[string,object]' ([System.StringComparer]::Ordinal)
  foreach ($entry in $entries) { $byPath.Add($entry.Path, $entry) }
  foreach ($project in $Contract.project_roots) {
    if (-not $byPath.ContainsKey($project)) { throw "Missing release project root: $project" }
  }
  for ($index = 0; $index -lt $expectedSets.Count; $index++) {
    $set = $Contract.source_sets[$index]
    $expected = $expectedSets[$index]
    if ($set.id -cne $expected[0] -or $set.path_prefix -cne $expected[1]) {
      throw 'The release contract changed a source-set identity or selector.'
    }
    [string[]]$names = @($byPath.Keys | Where-Object { $_.StartsWith($set.path_prefix, [System.StringComparison]::Ordinal) })
    [Array]::Sort($names, [System.StringComparer]::Ordinal)
    $members = New-Object System.Text.StringBuilder
    [long]$byteCount = 0
    foreach ($name in $names) {
      $entry = $byPath[$name]
      $null = $members.Append($name).Append([char]0).Append($entry.Blob).Append([char]10)
      $byteCount += $entry.Bytes
    }
    $sha256 = [System.Security.Cryptography.SHA256]::Create()
    try {
      $hash = $sha256.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($members.ToString()))
      $digest = [System.BitConverter]::ToString($hash).Replace('-', '').ToLowerInvariant()
    } finally { $sha256.Dispose() }
    if ($names.Length -ne $set.files -or $byteCount -ne $set.canonical_bytes -or
        $digest -cne $set.members_sha256) {
      throw "Release source membership, bytes or digest differs for $($set.id)."
    }
    if ($index -eq 0 -and ($set.files -ne $LexerCorpus.files -or
        $set.canonical_bytes -ne $LexerCorpus.canonical_bytes)) {
      throw 'The release whole-tree set differs from the canonical corpus denominator.'
    }
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

Assert-ReleaseSourceSets -Contract $manifest.release_contract -LexerCorpus $manifest.lexer_corpus `
  -Repository $ReferenceRoot -Commit $expectedCommit

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
Write-Output 'Verified all five release source sets against the pinned Git tree'
Write-Output 'Verified the complete canonical source and asset input tree'

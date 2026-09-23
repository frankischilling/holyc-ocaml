[CmdletBinding()]
param([string]$ReferenceRoot)

$ErrorActionPreference = 'Stop'
$projectRoot = Split-Path -Parent $PSScriptRoot
if ([string]::IsNullOrWhiteSpace($ReferenceRoot)) {
  $ReferenceRoot = Join-Path $projectRoot 'third_party/TempleOS'
}
$ReferenceRoot = [IO.Path]::GetFullPath($ReferenceRoot)
$temporaryRoot = [IO.Path]::GetFullPath([IO.Path]::GetTempPath())
$fixtureRoot = Join-Path $temporaryRoot ('holyc-release-contract-' + [Guid]::NewGuid().ToString('N'))
$encoding = New-Object System.Text.UTF8Encoding($false)
$original = [IO.File]::ReadAllText((Join-Path $projectRoot 'reference/manifest.json'))

try {
  $null = New-Item -ItemType Directory -Path (Join-Path $fixtureRoot 'tools')
  $null = New-Item -ItemType Directory -Path (Join-Path $fixtureRoot 'reference')
  Copy-Item -LiteralPath (Join-Path $PSScriptRoot 'verify-reference.ps1') -Destination (Join-Path $fixtureRoot 'tools/verify-reference.ps1')
  Copy-Item -LiteralPath (Join-Path $projectRoot 'reference/parser-corpus-aot.json') -Destination (Join-Path $fixtureRoot 'reference/parser-corpus-aot.json')
  $cases = @(
    @{ name='original manifest'; mutate={ param($doc) }; error=$null },
    @{ name='missing source set'; mutate={ param($doc) $doc.release_contract.source_sets=@($doc.release_contract.source_sets | Select-Object -First 4) }; error='every declared source set' },
    @{ name='reordered source sets'; mutate={ param($doc) $sets=$doc.release_contract.source_sets; $doc.release_contract.source_sets=@($sets[1],$sets[0],$sets[2],$sets[3],$sets[4]) }; error='identity or selector' },
    @{ name='narrowed Demo selector'; mutate={ param($doc) $doc.release_contract.source_sets[4].path_prefix='Demo/Asm/' }; error='identity or selector' },
    @{ name='changed member count'; mutate={ param($doc) $doc.release_contract.source_sets[3].files-- }; error='membership, bytes or digest' },
    @{ name='changed canonical size'; mutate={ param($doc) $doc.release_contract.source_sets[3].canonical_bytes++ }; error='membership, bytes or digest' },
    @{ name='changed member identity digest'; mutate={ param($doc) $doc.release_contract.source_sets[4].members_sha256='0' * 64 }; error='membership, bytes or digest' },
    @{ name='missing extension'; mutate={ param($doc) $doc.release_contract.extensions=@('.HC','.HH') }; error='metadata differs' },
    @{ name='missing required host'; mutate={ param($doc) $doc.release_contract.required_hosts=@('linux-x86_64') }; error='metadata differs' },
    @{ name='changed project entry'; mutate={ param($doc) $doc.release_contract.project_roots[0]='Compiler/CompilerA.HH' }; error='metadata differs' },
    @{ name='different corpus denominator'; mutate={ param($doc) $doc.lexer_corpus.files-- }; error='canonical corpus denominator' },
    @{ name='changed dependency tree'; mutate={ param($doc) $doc.release_contract.dependency_inputs.root_tree='0' * 40 }; error='complete canonical tree' },
    @{ name='mutually changed tree identifiers'; mutate={ param($doc) $doc.release_contract.dependency_inputs.root_tree='0' * 40; $doc.lexer_corpus.root_tree='0' * 40 }; error='does not match the pinned revision' },
    @{ name='source-only dependency filter'; mutate={ param($doc) $doc.release_contract.dependency_inputs.selection='source extensions only' }; error='complete canonical tree' },
    @{ name='restored manifest'; mutate={ param($doc) }; error=$null }
  )
  foreach ($case in $cases) {
    $document = $original | ConvertFrom-Json
    & $case.mutate $document
    [IO.File]::WriteAllText((Join-Path $fixtureRoot 'reference/manifest.json'), ($document | ConvertTo-Json -Depth 20), $encoding)
    $failure = $null
    try {
      $null = & (Join-Path $fixtureRoot 'tools/verify-reference.ps1') -ReferenceRoot $ReferenceRoot
    } catch { $failure = $_.Exception.Message }
    if ($null -eq $case.error) {
      if ($null -ne $failure) { throw "$($case.name) failed: $failure" }
    } elseif ($null -eq $failure -or $failure -notmatch $case.error) {
      throw "$($case.name) did not reach its expected rejection: $failure"
    }
    Write-Output "Verified release contract: $($case.name)"
  }
  Write-Output "Release contract checks passed: $($cases.Count)"
} finally {
  $resolved = [IO.Path]::GetFullPath($fixtureRoot)
  $parent = [IO.Path]::GetDirectoryName($resolved).TrimEnd([IO.Path]::DirectorySeparatorChar)
  $expectedParent = $temporaryRoot.TrimEnd([IO.Path]::DirectorySeparatorChar)
  if ($parent -ne $expectedParent -or -not [IO.Path]::GetFileName($resolved).StartsWith('holyc-release-contract-')) {
    throw "Refusing to remove an unexpected fixture directory: $resolved"
  }
  if (Test-Path -LiteralPath $resolved) { Remove-Item -LiteralPath $resolved -Recurse -Force }
}

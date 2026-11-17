param([string]$Dune = 'dune')

$ErrorActionPreference = 'Stop'
$repositoryRoot = Split-Path -Parent $PSScriptRoot
$consumerRoot = Join-Path $repositoryRoot 'third_party/TempleOS'
$fixtureRoot = Join-Path ([System.IO.Path]::GetTempPath()) ('holyc-installed-' + [Guid]::NewGuid().ToString('N'))
$referenceRevision = 'c26482bb6ad3f80106d28504ec5db3c6a360732c'

if (Test-Path Env:HOLYC_IMPLEMENTATION_COMMIT) {
  throw 'Installed compiler verification requires the real checkout revision, without an implementation override'
}
$expectedRevision = (& git -C $repositoryRoot rev-parse HEAD) -join ''
if ($LASTEXITCODE -ne 0 -or $expectedRevision -notmatch '^[0-9a-f]{40}$') {
  throw 'Cannot read the compiler checkout revision'
}
$consumerRevision = (& git -C $consumerRoot rev-parse HEAD) -join ''
if ($LASTEXITCODE -ne 0 -or $consumerRevision -ne $referenceRevision -or $consumerRevision -eq $expectedRevision) {
  throw 'Installed compiler consumers require the pinned, distinct TempleOS checkout'
}

$stagedDirectory = Join-Path $repositoryRoot '_build/install/default/bin'
$binaryName = if (Test-Path -LiteralPath (Join-Path $stagedDirectory 'holyc.exe')) { 'holyc.exe' } else { 'holyc' }
$cases = @(
  @{ Example = 'native-f64-defaults.hc'; Target = 'host-jit-task'; Mode = 'jit'; Type = 'f64'; Bits = '0x400e000000000000'; DefaultBytes = 16 },
  @{ Example = 'aot-native-defaults.hc'; Target = 'host-jit-task'; Mode = 'aot'; Type = 'f64'; Bits = '0x4015000000000000'; DefaultBytes = 32 },
  @{ Example = 'aot-native-defaults.hc'; Target = 'host-jit'; Mode = 'aot'; Type = 'f64'; Bits = '0x4015000000000000'; DefaultBytes = 32 },
  @{ Example = 'aot-native-defaults.hc'; Target = 'host-jit'; Mode = 'jit'; Type = 'f64'; Bits = '0x4015000000000000'; DefaultBytes = 32 },
  @{ Example = 'native-f64-runtime-dimensions.hc'; Target = 'host-jit-task'; Mode = 'jit'; Type = 'i64'; Bits = '0x000000000000002a'; DefaultBytes = 0 },
  @{ Example = 'aot-intrinsic-defaults.hc'; Target = 'host-jit-task'; Mode = 'aot'; Type = 'f64'; Bits = '0x4045000000000000'; DefaultBytes = 24 },
  @{ Example = 'aot-intrinsic-defaults.hc'; Target = 'host-jit'; Mode = 'aot'; Type = 'f64'; Bits = '0x4045000000000000'; DefaultBytes = 24 },
  @{ Example = 'native-lastclass-defaults.hc'; Target = 'host-jit-task'; Mode = 'jit'; Type = 'i64'; Bits = '0x00000000000000dd'; DefaultBytes = 8 },
  @{ Example = 'native-lastclass-defaults.hc'; Target = 'host-jit-task'; Mode = 'aot'; Type = 'i64'; Bits = '0x00000000000000dd'; DefaultBytes = 8 }
)

$fixtureDirectory = New-Item -ItemType Directory -Path $fixtureRoot
try {
  Push-Location -LiteralPath $repositoryRoot
  try {
    & $Dune install --root . --prefix $fixtureRoot --sections=bin
    if ($LASTEXITCODE -ne 0) { throw 'Private compiler installation failed; build @install first' }
  } finally { Pop-Location }

  $consumers = @(
    (Join-Path $repositoryRoot '_build/default/bin/holyc.exe'),
    (Join-Path $stagedDirectory $binaryName),
    (Join-Path (Join-Path $fixtureRoot 'bin') $binaryName)
  )
  $hashes = @($consumers | ForEach-Object { (Get-FileHash -LiteralPath $_ -Algorithm SHA256).Hash })
  if (@($hashes | Select-Object -Unique).Count -ne 1) {
    throw 'Built, staged and installed compiler binaries differ'
  }

  Push-Location -LiteralPath $consumerRoot
  try {
    foreach ($consumer in $consumers) {
      $version = (& $consumer version) -join "`n"
      if ($LASTEXITCODE -ne 0 -or $version -notmatch [regex]::Escape($expectedRevision)) {
        throw "Wrong built-in implementation revision: $consumer"
      }
      foreach ($case in $cases) {
        $example = Join-Path (Join-Path $repositoryRoot 'examples') $case.Example
        $report = (& $consumer run "--target=$($case.Target)" "--mode=$($case.Mode)" --code-byte-limit=1048576 --format=json $example) | ConvertFrom-Json
        if ($LASTEXITCODE -ne 0 -or $report.outcome -ne 'success' -or
            $report.implementation_commit -ne $expectedRevision -or
            $report.reference_commit -ne $referenceRevision -or
            $report.final_value.type -ne $case.Type -or
            $report.final_value.bits -ne $case.Bits -or
            $report.prepared_default_bytes -ne $case.DefaultBytes) {
          throw "Installed example failed: $consumer $($case.Target) $($case.Mode) $($case.Example)"
        }
      }
      [PSCustomObject]@{
        Consumer = $consumer
        Implementation = $expectedRevision
        InvocationCheckout = $consumerRevision
        SHA256 = $hashes[0]
        Examples = $cases.Count
      } | ConvertTo-Json -Compress
    }
  } finally { Pop-Location }
  Write-Output "Installed compiler checks passed: 3 identical binaries, 3 version reports, $($cases.Count * $consumers.Count) native example reports."
} finally {
  $resolvedFixture = (Resolve-Path -LiteralPath $fixtureRoot).ProviderPath
  if ($resolvedFixture -ne $fixtureDirectory.FullName) {
    throw 'Temporary installation path changed before cleanup'
  }
  Remove-Item -LiteralPath $resolvedFixture -Recurse -Force
}

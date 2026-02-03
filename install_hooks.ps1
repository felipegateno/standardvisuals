Param(
    [string]$RepoRoot = (Get-Location).Path
)

$HookDir = Join-Path $RepoRoot ".git/hooks"
$Source = Join-Path $RepoRoot ".git/hooks/pre-commit"
$Target = Join-Path $HookDir "pre-commit"

if (!(Test-Path $HookDir)) {
    Write-Error "No se encontró .git/hooks. ¿Estás en la raíz del repo?"
    exit 1
}

Copy-Item -Force $Source $Target
Write-Host "Hook pre-commit instalado en $Target"

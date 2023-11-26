# haskell_sort_algo
```powershell
# Use this command to install Haskell on a Windows machine
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }

# Additionally, you need to run these commands to install ansi-terminal for adding color to the output by using ANSI escape codes
cabal update
cabal install ansi-terminal

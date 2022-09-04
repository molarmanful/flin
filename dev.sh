#!/usr/bin/env bash

set -e

dotnet pack LIN.Cli/LIN.Cli.fsproj
dotnet tool update -g --add-source nupkg LIN.Cli
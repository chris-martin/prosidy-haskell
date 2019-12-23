#!/usr/bin/env bash

nix-shell tools/shell.nix --pure --run 'ghcide --lsp'

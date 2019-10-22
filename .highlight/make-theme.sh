#!/bin/bash
cd "$(dirname "$0")"
mkdir -p themes
nvim -es -u make-theme.vim

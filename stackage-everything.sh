#!/usr/bin/env bash

constraints="$(curl -s "https://www.stackage.org/lts-$1/cabal.config")"

packages="$(sed 's/ ==.\+$//' <<< "$constraints" \
  | awk '{print $1}' \
  | sed -e '/constraints:/,$!d' -e '/constraints:/d' \
  | sed '/\(ghc\|rts\|ghc-boot\|ghc-boot-th\|ghc-heap\|ghci\|Win32-notify\)/d' \
  | tr $'\n' ' ')"

# shellcheck disable=SC2086
cd && stack build --dry-run --prefetch --resolver "lts-$1" $packages

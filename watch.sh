#!/bin/bash

TEST_CMD="cabal run test:haskell-json-parser-test -v0 -- --color=auto"

ls -d watch.sh app/*.hs src/*.hs test/*.hs *.cabal | entr -c $TEST_CMD

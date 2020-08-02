#!/bin/bash
rerun --dir "src" --dir "tests" --pattern "**/*.{elm}" --exit --clear --name "Tests" -- elm-app test
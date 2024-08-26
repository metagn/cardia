# Package

version       = "0.1.0"
author        = "metagn"
description   = ""
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 2.0.2"

task docs, "build docs for all modules":
  when declared(buildDocs):
    buildDocs(gitUrl = "https://github.com/metagn/myskibidiacademia")
  else:
    echo "docs task not implemented, need nimbleutils"

task tests, "run tests for multiple backends":
  when declared(runTests):
    runTests(backends = {c, js, nims})
    runTests("tests/generated", backends = {c, js, nims})
  else:
    echo "tests task not implemented, need nimbleutils"

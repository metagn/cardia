# Package

version       = "0.1.0"
author        = "metagn"
description   = "a"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 2.0.2"

when (compiles do: import nimbleutils):
  import nimbleutils

task docs, "build docs for all modules":
  when declared(buildDocs):
    buildDocs(gitUrl = "https://github.com/metagn/myskibidiacademia")
  else:
    echo "docs task not implemented, need nimbleutils"

task tests, "run tests for multiple backends":
  when declared(runTests):
    when (NimMajor, NimMinor) >= (2, 1):
      const backends = {c, js, nims}
    else:
      const backends = {c, js}
    runTests(backends = backends)
  else:
    echo "tests task not implemented, need nimbleutils"

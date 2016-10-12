module Main exposing (..)

import ElmTest exposing (..)
import Test.SHA as SHA


tests : Test
tests =
    suite "Tests for SHA cryptographic hash library"
        [ SHA.sha1sumTests
        , SHA.sha224sumTests
        , SHA.sha256sumTests
        ]


main : Program Never
main =
    runSuite tests

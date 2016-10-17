module Main exposing (..)

import ElmTest exposing (..)
import Test.SHA as SHA
import Test.SHA1ShortMsg as SHA1ShortMsg
import Test.SHA224ShortMsg as SHA224ShortMsg
import Test.SHA256ShortMsg as SHA256ShortMsg
import Test.SHA1LongMsg as SHA1LongMsg
import Test.SHA224LongMsg as SHA224LongMsg
import Test.SHA256LongMsg as SHA256LongMsg


tests : Test
tests =
    suite "Tests for SHA cryptographic hash library"
        [ SHA.sha1sumTests
        , SHA.sha224sumTests
        , SHA.sha256sumTests
        , SHA1ShortMsg.sha1ShortMsgTests
        , SHA224ShortMsg.sha224ShortMsgTests
        , SHA256ShortMsg.sha256ShortMsgTests
        , SHA1LongMsg.sha1LongMsgTests
        , SHA224LongMsg.sha224LongMsgTests
        , SHA256LongMsg.sha256LongMsgTests
        ]


main : Program Never
main =
    runSuite tests

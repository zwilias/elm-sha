port module Main exposing (..)

import Json.Encode exposing (Value)
import Test exposing (..)
import Test.Runner.Node exposing (TestProgram, run)
import Test.SHA as SHA
import Test.SHA1ShortMsg as SHA1ShortMsg
import Test.SHA224ShortMsg as SHA224ShortMsg
import Test.SHA256ShortMsg as SHA256ShortMsg
import Test.SHA1LongMsg as SHA1LongMsg
import Test.SHA224LongMsg as SHA224LongMsg
import Test.SHA256LongMsg as SHA256LongMsg


tests : Test
tests =
    describe "Tests for SHA cryptographic hash library"
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


main : TestProgram
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg

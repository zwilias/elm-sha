module Test.SHA exposing (sha1sumTests, sha224sumTests, sha256sumTests)

import ElmTest exposing (..)
import SHA exposing (sha1sum, sha224sum, sha256sum)


sha1sum : String -> String
sha1sum =
    SHA.sha1sum


sha224sum : String -> String
sha224sum =
    SHA.sha224sum


sha256sum : String -> String
sha256sum =
    SHA.sha256sum


sha1sumTests : Test
sha1sumTests =
    suite "SHA-1 Hashing"
        [ test "foo æ ø å ñ" <|
            assertEqual "48a24ab372f84906d5f02386f94adf8f00238a9c" <|
                sha1sum "foo æ ø å ñ"
        , test "The quick brown fox jumps over the lazy dog" <|
            assertEqual "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12" <|
                sha1sum "The quick brown fox jumps over the lazy dog"
        , test "0x00" <|
            assertEqual "da39a3ee5e6b4b0d3255bfef95601890afd80709" <|
                sha1sum "0x00"
        , test "0x195a" <|
            assertEqual "0a1c2d555bbe431ad6288af5a54f93e0449c9232" <|
                sha1sum "0x195a"
        ]


sha224sumTests : Test
sha224sumTests =
    suite "SHA-224 Hashing"
        [ test "foo æ ø å ñ" <|
            assertEqual "d028a5ce22044de77a30518c1e9fb46e39c3fb07b3ef07ee0c1b51bc" <|
                sha224sum "foo æ ø å ñ"
        , test "The quick brown fox jumps over the lazy dog" <|
            assertEqual "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525" <|
                sha224sum "The quick brown fox jumps over the lazy dog"
        ]


sha256sumTests : Test
sha256sumTests =
    suite "SHA-256 Hashing"
        [ test "foo æ ø å ñ" <|
            assertEqual "d781c519c6ada91ab548b1c44e5682499c025f1367c59037dc3b0b17dc9a1f8e" <|
                sha256sum "foo æ ø å ñ"
        , test "The quick brown fox jumps over the lazy dog" <|
            assertEqual "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592" <|
                sha256sum "The quick brown fox jumps over the lazy dog"
        , test "SHA224sum((SHA224sum 42) + (SHA224sum 43))" <|
            assertEqual "2d98f505b9fae0055bc87b299a90a69b81df01bbd68d7f08c21dde4da87fcdb2" <|
                sha256sum ((sha256sum "42") ++ (sha256sum "43"))
        ]

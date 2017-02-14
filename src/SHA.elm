module SHA exposing (sha1sum, sha224sum, sha256sum)

{-| SHA hash functions for cryptographic hashing strings. Ported from: (c)
[Chris Veness](http://www.movable-type.co.uk).

# SHA-1

@docs sha1sum

# SHA-2

@docs sha224sum,sha256sum

-}

-- ELM-LANG LIBS

import Array exposing (Array, fromList, get, indexedMap, initialize)
import Basics exposing (always, ceiling, floor, toFloat)
import Bitwise
    exposing
        ( and
        , complement
        , or
        , shiftLeftBy
        , shiftRightZfBy
        , xor
        )
import Char exposing (KeyCode, fromCode, toCode)
import List exposing (foldl, map2)
import Maybe exposing (withDefault)
import String exposing (fromChar, length, toList)


-- OTHER LIBS

import UTF8 exposing (toSingleByte)


{-| Takes a string. Produces a sha1sum (string). In order to pass [NIST
tests](http://www.nsrl.nist.gov/testdata/) the interface needs to be able to
handle `HEX` values. Therefore, if a string starts with `0x` it will be handled
as a `HEX` value so the following behaviour can be reproduced: `echo -n '195a'
| xxd -r -p | sha1sum` (example done on a `Linux` box):

    sha1sum "foo æ ø å ñ"
        == "48a24ab372f84906d5f02386f94adf8f00238a9c"

    sha1sum "The quick brown fox jumps over the lazy dog"
        == "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12"

    sha1sum "0x00"
        == "da39a3ee5e6b4b0d3255bfef95601890afd80709"

    sha1sum "0x195a"
        == "0a1c2d555bbe431ad6288af5a54f93e0449c9232"

-}
sha1sum : String -> String
sha1sum str =
    let
        bytes =
            str |> isHex

        padded =
            pad bytes
    in
        hashComputation padded hs1 ks1 80 sha1prep sha1addmod 0


{-| Takes a string. Produces a sha224sum (string). In order to pass [NIST
tests](http://www.nsrl.nist.gov/testdata/) the interface needs to be able to
handle `HEX` values. Therefore, if a string starts with `0x` it will be handled
as a `HEX` value so the following behaviour can be reproduced: `echo -n '195a'
| xxd -r -p | sha224sum` (example done on a `Linux` box):

    sha224sum "foo æ ø å ñ"
        == "d028a5ce22044de77a30518c1e9fb46e39c3fb07b3ef07ee0c1b51bc"

    sha224sum "The quick brown fox jumps over the lazy dog"
        == "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525"

    sha224sum "0x00"
        == "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f"

    sha224sum "0x5c7b"
        == "daff9bce685eb831f97fc1225b03c275a6c112e2d6e76f5faf7a36e6"
-}
sha224sum : String -> String
sha224sum str =
    let
        bytes =
            str |> isHex

        padded =
            pad bytes
    in
        hashComputation padded hs224 ks2 64 sha2prep sha2addmod 1


{-| Takes a string. Produces a sha256sum (string). In order to pass [NIST
tests](http://www.nsrl.nist.gov/testdata/) the interface needs to be able to
handle `HEX` values. Therefore, if a string starts with `0x` it will be handled
as a `HEX` value so the following behaviour can be reproduced: `echo -n '195a'
| xxd -r -p | sha256sum` (example done on a `Linux` box):

    sha256sum "foo æ ø å ñ"
        == "b4c0dfecf21e1ea3cf64b0a0fe6a82fd24e4bb4480df6d0657f701d5d7c8ac18"

    sha256sum "The quick brown fox jumps over the lazy dog"
        == "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"

    sha256sum "0x00"
        == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

    sha256sum "0xd3"
        == "28969cdfa74a12c82f3bad960b0b000aca2ac329deea5c2328ebc6f2ba9802c1"
-}
sha256sum : String -> String
sha256sum str =
    let
        bytes =
            str |> isHex

        padded =
            pad bytes

        n =
            padded |> String.length
    in
        hashComputation padded hs256 ks2 64 sha2prep sha2addmod 0



-- CONSTANTS


ks1 : Array Int
ks1 =
    -- constants [§4.2.1]
    Array.fromList
        [ 0x5A827999
        , 0x6ED9EBA1
        , 0x8F1BBCDC
        , 0xCA62C1D6
        ]


ks2 : Array Int
ks2 =
    -- constants [§4.2.2]
    Array.fromList
        [ 0x428A2F98
        , 0x71374491
        , 0xB5C0FBCF
        , 0xE9B5DBA5
        , 0x3956C25B
        , 0x59F111F1
        , 0x923F82A4
        , 0xAB1C5ED5
        , 0xD807AA98
        , 0x12835B01
        , 0x243185BE
        , 0x550C7DC3
        , 0x72BE5D74
        , 0x80DEB1FE
        , 0x9BDC06A7
        , 0xC19BF174
        , 0xE49B69C1
        , 0xEFBE4786
        , 0x0FC19DC6
        , 0x240CA1CC
        , 0x2DE92C6F
        , 0x4A7484AA
        , 0x5CB0A9DC
        , 0x76F988DA
        , 0x983E5152
        , 0xA831C66D
        , 0xB00327C8
        , 0xBF597FC7
        , 0xC6E00BF3
        , 0xD5A79147
        , 0x06CA6351
        , 0x14292967
        , 0x27B70A85
        , 0x2E1B2138
        , 0x4D2C6DFC
        , 0x53380D13
        , 0x650A7354
        , 0x766A0ABB
        , 0x81C2C92E
        , 0x92722C85
        , 0xA2BFE8A1
        , 0xA81A664B
        , 0xC24B8B70
        , 0xC76C51A3
        , 0xD192E819
        , 0xD6990624
        , 0xF40E3585
        , 0x106AA070
        , 0x19A4C116
        , 0x1E376C08
        , 0x2748774C
        , 0x34B0BCB5
        , 0x391C0CB3
        , 0x4ED8AA4A
        , 0x5B9CCA4F
        , 0x682E6FF3
        , 0x748F82EE
        , 0x78A5636F
        , 0x84C87814
        , 0x8CC70208
        , 0x90BEFFFA
        , 0xA4506CEB
        , 0xBEF9A3F7
        , 0xC67178F2
        ]


hs1 : List Int
hs1 =
    -- initial hash value [§5.3.1]
    [ 0x67452301
    , 0xEFCDAB89
    , 0x98BADCFE
    , 0x10325476
    , 0xC3D2E1F0
    ]


hs256 : List Int
hs256 =
    -- initial hash value [§5.3.2]
    -- square roots of the first 8 primes 2..19
    [ 0x6A09E667
    , 0xBB67AE85
    , 0x3C6EF372
    , 0xA54FF53A
    , 0x510E527F
    , 0x9B05688C
    , 0x1F83D9AB
    , 0x5BE0CD19
    ]


hs224 : List Int
hs224 =
    -- initial hash value [§5.3.3]
    -- square roots of the 9th through 16th primes 23..53
    [ 0xC1059ED8
    , 0x367CD507
    , 0x3070DD17
    , 0xF70E5939
    , 0xFFC00B31
    , 0x68581511
    , 0x64F98FA7
    , 0xBEFA4FA4
    ]


pad : String -> String
pad str =
    -- add trailing '1' bit (+ 0's padding) to string [§5.1.1]
    str ++ (0x80 |> fromCode |> fromChar)



-- PREPROCESSING


preprocessing : String -> Array (Array KeyCode)
preprocessing str =
    -- convert string into 512-bit/16-integer blocks arrays of ints [§5.2.1]
    let
        xs =
            strToCharArray str

        l_ =
            String.length str

        l =
            -- length (in 32-bit integers) of msg + ‘1’ + appended length
            (l_ |> toFloat) / 4.0 + 2.0

        n =
            -- number of 16-integer-blocks required to hold 'l' ints
            ceiling (l / 16.0)

        m =
            Array.initialize n
                (\i ->
                    Array.initialize 16
                        (\j ->
                            case ( n == (i + 1), j ) of
                                ( True, 14 ) ->
                                    helperPrePenultimate l_

                                ( True, 15 ) ->
                                    helperPreLast l_

                                ( _, _ ) ->
                                    helperPre i j xs
                        )
                )
    in
        m


strToCharArray : String -> Array Char
strToCharArray =
    String.toList >> Array.fromList


helperPrePenultimate : Int -> Int
helperPrePenultimate n =
    {-
       add length (in bits) into final pair of 32-bit integers (big-endian) [§5.1.1]
       note: most significant word would be (len-1)*8 >>> 32, but since JS converts
       bitwise-op args to 32 bits, we need to simulate this by arithmetic operators
    -}
    ((n - 1) * 8 |> toFloat) / twoPower32 |> floor


twoPower32 : Float
twoPower32 =
    -- Math.pow(2, 32) or just (0xffffffff + 1)
    4294967296.0


helperPreLast : Int -> Int
helperPreLast n =
    {-
       add length (in bits) into final pair of 32-bit integers (big-endian) [§5.1.1]
       note: most significant word would be (len-1)*8 >>> 32, but since JS converts
       bitwise-op args to 32 bits, we need to simulate this by arithmetic operators
    -}
    ((n - 1) * 8)
        |> Bitwise.and 0xFFFFFFFF


helperPre : Int -> Int -> Array Char -> Int
helperPre i j xs =
    -- encode 4 chars per integer, big-endian encoding
    -- note running off the end of msg is ok 'cos bitwise ops on NaN return 0
    (getKeyCode (i * 64 + j * 4) xs
        |> Bitwise.shiftLeftBy 24
    )
        |> Bitwise.or
            (getKeyCode (i * 64 + j * 4 + 1) xs
                |> Bitwise.shiftLeftBy 16
            )
        |> Bitwise.or
            (getKeyCode (i * 64 + j * 4 + 2) xs
                |> Bitwise.shiftLeftBy 8
            )
        |> Bitwise.or (getKeyCode (i * 64 + j * 4 + 3) xs)


getKeyCode : Int -> Array Char -> KeyCode
getKeyCode i xs =
    case Array.get i xs of
        Just c ->
            c |> toCode

        Nothing ->
            0



-- HASH COMPUTATION [§6.1.2]


hashComputation :
    String
    -> List Int
    -> Array Int
    -> Int
    -> (Int -> Array Int -> Int)
    -> (List Int -> Int -> Int -> Array Int -> List Int)
    -> Int
    -> String
hashComputation str hs ks rnds shaprep addmod drop =
    let
        n =
            hs |> List.length

        xss =
            preprocessing str
    in
        xss
            |> Array.foldl
                (\xs acc ->
                    let
                        -- 1) prepare message schedule 'W'
                        ws =
                            messageSchedule xs rnds shaprep
                    in
                        -- 2) initialise with previous hash value
                        -- 3) main loop (note 'addition modulo 2^32')
                        loop addmod acc ws ks
                )
                hs
            |> List.take (n - drop)
            |> List.foldl (\x a -> a ++ (numberToHex x)) ""


messageSchedule :
    Array Int
    -> Int
    -> (Int -> Array Int -> Int)
    -> Array Int
messageSchedule xs rnds shaprep =
    let
        init =
            Array.initialize rnds
                (\i ->
                    case i < 16 of
                        True ->
                            getInt i xs

                        False ->
                            0
                )
    in
        Array.initialize (rnds - 16) (\i -> i + 16)
            |> Array.foldl
                (\i acc -> acc |> Array.set i (shaprep i acc))
                init


sha1prep : Int -> Array Int -> Int
sha1prep i xs =
    let
        im3 =
            getInt (i - 3) xs

        im8 =
            getInt (i - 8) xs

        im14 =
            getInt (i - 14) xs

        im16 =
            getInt (i - 16) xs
    in
        rotl
            (im3
                |> Bitwise.xor im8
                |> Bitwise.xor im14
                |> Bitwise.xor im16
            )
            1


sha2prep : Int -> Array Int -> Int
sha2prep i xs =
    let
        im2 =
            getInt (i - 2) xs

        im7 =
            getInt (i - 7) xs

        im15 =
            getInt (i - 15) xs

        im16 =
            getInt (i - 16) xs
    in
        ((sigmaOne im2)
            + im7
            + (sigmaZero im15)
            + im16
        )
            |> Bitwise.and 0xFFFFFFFF


loop :
    (List Int -> a -> number -> b -> List Int)
    -> List Int
    -> Array a
    -> b
    -> List Int
loop addmod hs ws ks =
    let
        ( _, hs_ ) =
            ws
                |> Array.foldl
                    (\x ( i, acc ) -> ( i + 1, addmod acc x i ks ))
                    ( 0, hs )
    in
        List.map2 (\h h_ -> (h + h_) |> Bitwise.and 0xFFFFFFFF) hs hs_


sha1addmod : List Int -> Int -> Int -> Array Int -> List Int
sha1addmod hs x i ks =
    let
        ( a, b, c, d, e ) =
            case hs of
                a_ :: b_ :: c_ :: d_ :: e_ :: [] ->
                    ( a_, b_, c_, d_, e_ )

                _ ->
                    Debug.crash "Not supported amount of variables"

        s =
            (i |> toFloat) / 20 |> floor

        t =
            ((rotl a 5)
                + (sha1f s b c d)
                + e
                + (getInt s ks)
                + x
            )
                |> Bitwise.and 0xFFFFFFFF

        e_ =
            d

        d_ =
            c

        c_ =
            rotl b 30

        b_ =
            a

        a_ =
            t
    in
        [ a_, b_, c_, d_, e_ ]


sha2addmod : List Int -> Int -> Int -> Array Int -> List Int
sha2addmod hs x i ks =
    let
        ( a, b, c, d, e, f, g, h ) =
            case hs of
                a_ :: b_ :: c_ :: d_ :: e_ :: f_ :: g_ :: h_ :: [] ->
                    ( a_, b_, c_, d_, e_, f_, g_, h_ )

                _ ->
                    Debug.crash "Not supported amount of variables"

        t1 =
            h
                + (capSigmaOne e)
                + (choice e f g)
                + (getInt i ks)
                + x

        t2 =
            (capSigmaZero a)
                + (majority a b c)

        h_ =
            g

        g_ =
            f

        f_ =
            e

        e_ =
            (d + t1)
                |> Bitwise.and 0xFFFFFFFF

        d_ =
            c

        c_ =
            b

        b_ =
            a

        a_ =
            (t1 + t2)
                |> Bitwise.and 0xFFFFFFFF
    in
        [ a_, b_, c_, d_, e_, f_, g_, h_ ]


getInt : Int -> Array Int -> Int
getInt i xs =
    Maybe.withDefault 0 (Array.get i xs)


numberToHex : Int -> String
numberToHex n =
    -- Hexadecimal representation of a number
    -- note can't use toString(16) as it is implementation-dependant,
    -- and in IE returns signed numbers when used on full words
    Array.initialize 8 (\i -> numberToHexHlp (8 - (i + 1)) n)
        |> Array.foldl (\x a -> a ++ x) ""


numberToHexHlp : Int -> Int -> String
numberToHexHlp i n =
    let
        x =
            -- x & 0x0F =>  x mod 16
            n
                |> Bitwise.shiftRightZfBy (i * 4)
                |> Bitwise.and 0x0F

        x_ =
            case x < 10 of
                -- ASCII codes for [0-9] => [48-57]
                True ->
                    (x + 48)

                -- ASCII codes for [a-f] => [97-102]
                False ->
                    (x + 97 - 10)
    in
        x_ |> stringify


stringify : KeyCode -> String
stringify =
    fromCode >> fromChar



-- LOGICAL FUNCTIONS [§4.1.1]


sha1f : number -> Int -> Int -> Int -> Int
sha1f s x y z =
    -- s is limited to mod 4
    case s of
        0 ->
            choice x y z

        1 ->
            parity x y z

        2 ->
            majority x y z

        n ->
            parity x y z


parity : Int -> Int -> Int -> Int
parity x y z =
    x
        |> Bitwise.xor y
        |> Bitwise.xor z



-- LOGICAL FUNCTIONS [§4.1.2]


capSigmaZero : Int -> Int
capSigmaZero x =
    (rotr 2 x)
        |> Bitwise.xor (rotr 13 x)
        |> Bitwise.xor (rotr 22 x)


capSigmaOne : Int -> Int
capSigmaOne x =
    (rotr 6 x)
        |> Bitwise.xor (rotr 11 x)
        |> Bitwise.xor (rotr 25 x)


sigmaZero : Int -> Int
sigmaZero x =
    (rotr 7 x)
        |> Bitwise.xor (rotr 18 x)
        |> Bitwise.xor (x |> Bitwise.shiftRightZfBy 3)


sigmaOne : Int -> Int
sigmaOne x =
    (rotr 17 x)
        |> Bitwise.xor (rotr 19 x)
        |> Bitwise.xor (x |> Bitwise.shiftRightZfBy 10)


choice : Int -> Int -> Int -> Int
choice x y z =
    (x |> Bitwise.and y)
        |> Bitwise.xor ((complement x) |> Bitwise.and z)


majority : Int -> Int -> Int -> Int
majority x y z =
    (x |> Bitwise.and y)
        |> Bitwise.xor (x |> Bitwise.and z)
        |> Bitwise.xor (y |> Bitwise.and z)



-- OPERATIONS ON WORDS


rotr : Int -> Int -> Int
rotr n x =
    -- Rotates right (circular right shift) value x by n positions [§3.2.4]
    (x |> Bitwise.shiftRightZfBy n)
        |> Bitwise.or (x |> Bitwise.shiftLeftBy (32 - n))


rotl : Int -> Int -> Int
rotl x n =
    -- Rotate left (circular left shift) value x by n positions [§3.2.5]
    (x |> Bitwise.shiftLeftBy n)
        |> Bitwise.or (x |> Bitwise.shiftRightZfBy (32 - n))



-- HELPERS


isHex : String -> String
isHex str =
    case String.startsWith "0x" str of
        True ->
            hexToString (String.dropLeft 2 str)

        False ->
            toSingleByte str


hexToString : String -> String
hexToString hex =
    -- Take chunks of 2 hex digits and apply formula: (a * 16^1) + (b * 16^0)
    -- Example (d_16 = 13_10): 0xd3 => (d * 16^1) + (3 * 16^0) = 208 + 3 = 211
    let
        zero =
            hex |> String.all (\c -> c == '0')

        asciiToNumber x =
            let
                y =
                    x |> Char.toCode
            in
                case y > 96 of
                    -- ASCII codes for [a-f] => [97-102]
                    True ->
                        y - 97 + 10

                    -- ASCII codes for [0-9] => [48-57]
                    False ->
                        y - 48

        rec xs acc =
            case xs == "" of
                False ->
                    let
                        ( y, ys ) =
                            case String.uncons xs of
                                Nothing ->
                                    ( Nothing, "" )

                                Just ( a, rest ) ->
                                    ( Just a, rest )

                        ( z, zs ) =
                            case String.uncons ys of
                                Nothing ->
                                    ( Nothing, "" )

                                Just ( b, rest ) ->
                                    ( Just b, rest )

                        keyCode =
                            case ( y, z ) of
                                ( Just a, Just b ) ->
                                    ((asciiToNumber a) * 16)
                                        + (asciiToNumber b)

                                ( Just a, _ ) ->
                                    (asciiToNumber a) * 16

                                ( _, _ ) ->
                                    0
                    in
                        rec zs (acc ++ (keyCode |> stringify))

                True ->
                    acc
    in
        case zero of
            True ->
                ""

            False ->
                rec (hex |> String.toLower) ""

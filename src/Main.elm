module Main exposing (JValue(..), createTuple, jsonParser, nameParser, nameValueParser, parse, parseArray, parseObject)

import Char
import Parser as P exposing ((|.), (|=))
import Set


type JValue
    = JObject (List ( String, JValue ))
    | JArray (List JValue)
    | JString String
    | JNull
    | JInt Int
    | JFloat Float


jsonParser : P.Parser JValue
jsonParser =
    P.oneOf
        [ parseObject
        , parseArray
        , parseInt
        ]


parseInt : P.Parser JValue
parseInt =
    P.map JInt P.int


nameParser : P.Parser String
nameParser =
    P.variable
        { start = \_ -> True
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


createTuple : a -> b -> ( a, b )
createTuple a b =
    ( a, b )


nameValueParser : P.Parser ( String, JValue )
nameValueParser =
    P.succeed createTuple
        |. P.symbol "\""
        |= nameParser
        |. P.symbol "\""
        |. P.symbol ":"
        |. P.spaces
        |= P.lazy (\_ -> jsonParser)


parseObject : P.Parser JValue
parseObject =
    P.map
        JObject
        (P.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = P.spaces
            , item = nameValueParser
            , trailing = P.Optional
            }
        )


parseArray : P.Parser JValue
parseArray =
    P.map
        JArray
        (P.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = P.spaces
            , item = P.lazy (\_ -> jsonParser)
            , trailing = P.Optional
            }
        )


parse : String -> Result (List P.DeadEnd) JValue
parse =
    P.run jsonParser

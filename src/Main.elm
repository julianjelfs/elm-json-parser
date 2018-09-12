module Main exposing (JValue(..), alphaParser, createTuple, jsonParser, nameValueParser, parse, parseArray, parseObject, sampleJson, stringify)

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
        , parseFloat
        , parseString
        , parseNull
        ]


parseNull : P.Parser JValue
parseNull =
    P.map (always JNull) (P.keyword "null")


parseInt : P.Parser JValue
parseInt =
    P.map JInt P.int


parseFloat : P.Parser JValue
parseFloat =
    P.map JFloat P.float


parseString : P.Parser JValue
parseString =
    P.succeed JString
        |. P.symbol "\""
        |= alphaParser
        |. P.symbol "\""


stringVariable =
    { start = \_ -> True
    , inner = \c -> Char.isAlphaNum c || c == '-' || c == '.' || c == '/'
    , reserved = Set.empty
    }


alphaParser : P.Parser String
alphaParser =
    P.variable stringVariable


createTuple : a -> b -> ( a, b )
createTuple a b =
    ( a, b )


nameValueParser : P.Parser ( String, JValue )
nameValueParser =
    P.succeed createTuple
        |. P.symbol "\""
        |= alphaParser
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


stringify : JValue -> String
stringify val =
    case val of
        JNull ->
            "null"

        JInt i ->
            String.fromInt i

        JString str ->
            "\"" ++ str ++ "\""

        JFloat f ->
            String.fromFloat f

        JArray arr ->
            "[" ++ (arr |> List.map stringify |> String.join ", ") ++ "]"

        JObject obj ->
            "{"
                ++ (obj
                        |> List.map (\( str, jval ) -> "\"" ++ str ++ "\"" ++ ": " ++ stringify jval)
                        |> String.join ", "
                   )
                ++ "}"


sampleJson : String
sampleJson =
    "{\"type\": \"application\", \"source-directories\": [ \"src\" ], \"elm-version\": \"0.19.0\",  \"dependencies\": { \"direct\": { \"elm/browser\": \"1.0.0\",  \"elm/core\": \"1.0.0\", \"elm/html\": \"1.0.0\", \"elm/parser\": \"1.1.0\" }, \"indirect\": { \"elm/json\": \"1.0.0\", \"elm/time\": \"1.0.0\", \"elm/url\": \"1.0.0\", \"elm/virtual-dom\": \"1.0.0\" } }, \"test-dependencies\": { \"direct\": {}, \"indirect\": {}}}"

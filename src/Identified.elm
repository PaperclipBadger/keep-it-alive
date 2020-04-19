module Identified exposing (Identified, Identifier, append, empty, get, identifierToString, remove, set, toList)

import Dict exposing (Dict)
import List


{-| Type for seqeuential identifiers.
-}
type Identifier
    = Identifier Int


someIdentifier : Identifier
someIdentifier =
    Identifier 0


nextIdentifier : Identifier -> Identifier
nextIdentifier =
    Identifier << (+) 1 << identifierToInt


identifierToInt : Identifier -> Int
identifierToInt i =
    case i of
        Identifier int ->
            int


identifierToString : Identifier -> String
identifierToString =
    String.fromInt << identifierToInt


type IdentifierGenerator
    = IdentifierGenerator Identifier


newIdentifierGenerator : IdentifierGenerator
newIdentifierGenerator =
    IdentifierGenerator someIdentifier


generateIdentifier : IdentifierGenerator -> ( Identifier, IdentifierGenerator )
generateIdentifier igen =
    case igen of
        IdentifierGenerator last ->
            let
                next =
                    nextIdentifier last
            in
            ( next, IdentifierGenerator next )


type alias Identified a =
    { generator : IdentifierGenerator
    , table : Dict Int a
    }


empty : Identified a
empty =
    { generator = newIdentifierGenerator
    , table = Dict.empty
    }


toList : Identified a -> List ( Identifier, a )
toList =
    .table >> Dict.toList >> List.map (\( k, v ) -> ( Identifier k, v ))


append : a -> Identified a -> ( Identifier, Identified a )
append a identified =
    let
        ( i, gen ) =
            generateIdentifier identified.generator
    in
    ( i
    , { generator = gen
      , table = Dict.insert (identifierToInt i) a identified.table
      }
    )


get : Identifier -> Identified a -> Maybe a
get i identified =
    Dict.get (identifierToInt i) identified.table


set : Identifier -> a -> Identified a -> Identified a
set i a identified =
    { identified | table = Dict.insert (identifierToInt i) a identified.table }


remove : Identifier -> Identified a -> Identified a
remove i identified =
    { identified | table = Dict.remove (identifierToInt i) identified.table }

module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Keyed
import Random



-- COMBINATORS


{-| Compose a function of two arguments with a function of one argument.
The name "blackbird" comes from this StrangeLoop talk: <https://www.youtube.com/watch?v=seVSlKazsNk>
Can also be written >>> or ((>>) >> (>>)).
naknaknaknaknak
-}
blackbird : (a -> b) -> (c -> d -> a) -> c -> d -> b
blackbird f g a b =
    f (g a b)


on : (a -> a -> b) -> (c -> a) -> c -> c -> b
on f g a b =
    f (g a) (g b)


fork : (a -> b -> c) -> (d -> a) -> (e -> b) -> d -> e -> c
fork f g h a b =
    f (g a) (h b)



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type PlantSize
    = Small
    | Medium
    | Large


type alias Plant =
    { size : PlantSize
    , hunger : Int
    }


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


type alias Name =
    String


type alias Weight =
    Int


type alias Security =
    Int


type alias Popularity =
    Int


type alias Goodness =
    Int


type alias Person =
    { identifier : Identifier
    , firstName : Name
    , middleName : Maybe Name
    , lastName : Name
    , weight : Weight
    , security : Security
    , popularity : Popularity
    , goodness : Goodness
    }


genPerson : Random.Generator Person
genPerson =
    Random.map (Person someIdentifier) genFirstName
        |> Random.andThen (\f -> Random.map f genMiddleName)
        |> Random.andThen (\f -> Random.map f genLastName)
        |> Random.andThen (\f -> Random.map f genWeight)
        |> Random.andThen (\f -> Random.map f genSecurity)
        |> Random.andThen (\f -> Random.map f genPopularity)
        |> Random.andThen (\f -> Random.map f genGoodness)


genFirstName : Random.Generator Name
genFirstName =
    Random.uniform "Greg" [ "Steve", "Esther", "Blaine", "Yanni", "Jonathan", "Mickey", "Eve", "Remi", "Ribena", "Jamie", "James" ]


genMiddleName : Random.Generator (Maybe Name)
genMiddleName =
    let
        constructor : Random.Generator (Name -> Maybe Name)
        constructor =
            Random.weighted ( 0.9, always Nothing ) [ ( 0.1, Just ) ]

        value : Random.Generator Name
        value =
            Random.uniform "\"The Death\"" [ "Godfrey", "\"Hopscotch\"", "William", "Horton" ]
    in
    Random.map2 (<|) constructor value


genLastName : Random.Generator Name
genLastName =
    Random.uniform "Smith" [ "Rogers", "Van Der Pant", "Carlsberg", "Sutton", "Li", "Wang", "Forger" ]


genWeight : Random.Generator Weight
genWeight =
    Random.int 1 10


genSecurity : Random.Generator Security
genSecurity =
    Random.int 1 10


genPopularity : Random.Generator Popularity
genPopularity =
    Random.int 1 10


genGoodness : Random.Generator Goodness
genGoodness =
    Random.int 1 10


type SystemState
    = Loop
    | GameOver


type alias Model =
    { systemState : SystemState
    , identifierGenerator : IdentifierGenerator
    , plant : Plant
    , targets : Array Person
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { systemState = Loop
      , identifierGenerator = newIdentifierGenerator
      , plant = { size = Small, hunger = 10 }
      , targets = Array.empty
      }
    , Cmd.batch (List.repeat 4 (Random.generate NewTarget genPerson))
    )



-- UPDATE


type Msg
    = NewTarget Person
    | Select Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.systemState of
        Loop ->
            case msg of
                NewTarget person ->
                    ( { model | targets = Array.push person model.targets }
                    , Cmd.none
                    )

                Select i ->
                    eat model i

        GameOver ->
            ( model, Cmd.none )


{-| Have the plant eat the person at index i
-}
eat : Model -> Int -> ( Model, Cmd Msg )
eat model i =
    case Array.get i model.targets of
        Nothing ->
            ( model, Cmd.none )

        Just person ->
            let
                plant =
                    model.plant

                newplant =
                    { plant | hunger = max (plant.hunger + 5 - person.weight) 0 }
            in
            ( { model
                | systemState =
                    if newplant.hunger > 10 then
                        GameOver

                    else
                        Loop
                , plant = newplant
                , targets = remove i model.targets
              }
            , Random.generate NewTarget genPerson
            )


{-| Remove the element at the given index. If the index is out of range, the array is
unaltered.
-}
remove : Int -> Array a -> Array a
remove i a =
    Array.append (Array.slice 0 i a) (Array.slice (i + 1) (Array.length a) a)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.systemState of
        Loop ->
            { title = "The button, oooh"
            , body =
                [ viewPlant model.plant
                , Html.Keyed.node "div" [] (List.indexedMap viewTarget (Array.toList model.targets))
                ]
            }

        GameOver ->
            { title = "Game over!"
            , body = [ text "You died!" ]
            }


viewPlant : Plant -> Html Msg
viewPlant plant =
    div []
        [ text ("Plant size: " ++ viewPlantSize plant.size)
        , text ("Plant hunger: " ++ String.fromInt plant.hunger)
        ]


viewPlantSize : PlantSize -> String
viewPlantSize size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


viewTarget : Int -> Person -> ( String, Html Msg )
viewTarget i person =
    ( identifierToString person.identifier
    , div [ onClick (Select i) ]
        [ div [] [ text ("Name: " ++ viewFullName person) ]
        , div [] [ text ("Weight: " ++ String.fromInt person.weight ++ "kg") ]
        , div [] [ text ("Security: " ++ String.fromInt person.security) ]
        , div [] [ text ("Popularity: " ++ String.fromInt person.popularity) ]
        , div [] [ text ("Goodness: " ++ String.fromInt person.goodness) ]
        ]
    )


viewFullName : Person -> String
viewFullName person =
    String.join " " [ person.firstName, Maybe.withDefault "" person.middleName, person.lastName ]

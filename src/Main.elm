module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Animation
import Animation.Messenger
import Browser
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Keyed
import Identified exposing (Identified, identifierToString)
import List
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


type alias Animation =
    Animation.Messenger.State Msg


type PlantSize
    = Small
    | Medium
    | Large


type alias Plant =
    { size : PlantSize
    , hunger : Int
    }


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
    { details : PersonDetails
    , animation : Animation
    }


type alias PersonDetails =
    { firstName : Name
    , middleName : Maybe Name
    , lastName : Name
    , weight : Weight
    , security : Security
    , popularity : Popularity
    , goodness : Goodness
    }


genPersonDetails : Random.Generator PersonDetails
genPersonDetails =
    Random.map PersonDetails genFirstName
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


transparent : Animation
transparent =
    Animation.style [ Animation.opacity 0.0 ]


targetEnter : Animation
targetEnter =
    Animation.interrupt [ Animation.to [ Animation.opacity 1.0 ] ] transparent


targetExit : Identified.Identifier -> Animation -> Animation
targetExit i =
    Animation.interrupt
        [ Animation.to [ Animation.opacity 0.0 ]
        , Animation.Messenger.send (TargetExited i)
        ]


type SystemState
    = Loop
    | GameOver


type alias Model =
    { systemState : SystemState
    , plant : Plant
    , targets : Identified Person
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { systemState = Loop
      , plant = { size = Small, hunger = 10 }
      , targets = Identified.empty
      }
    , Cmd.batch (List.repeat 4 (Random.generate NewTarget genPersonDetails))
    )



-- UPDATE


type Msg
    = Animate Animation.Msg
    | NewTarget PersonDetails
    | Select Identified.Identifier
    | TargetExited Identified.Identifier


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.systemState of
        Loop ->
            case msg of
                Animate animMsg ->
                    updateAnimations model animMsg

                NewTarget details ->
                    let
                        ( i, targets ) =
                            Identified.append { details = details, animation = targetEnter } model.targets
                    in
                    ( { model | targets = targets }
                    , Cmd.none
                    )

                Select i ->
                    eat model i

                TargetExited i ->
                    ( { model | targets = Identified.remove i model.targets }
                    , Cmd.none
                    )

        GameOver ->
            ( model, Cmd.none )


updateAnimations : Model -> Animation.Msg -> ( Model, Cmd Msg )
updateAnimations model animMsg =
    let
        ( is, persons ) =
            List.unzip (Identified.toList model.targets)

        ( animations, cmds ) =
            persons
                |> List.map .animation
                |> List.map (Animation.Messenger.update animMsg)
                |> List.unzip

        newpersons =
            List.map2 (\person animation -> { person | animation = animation }) persons animations

        newtargets =
            List.map2 Tuple.pair is newpersons
                |> List.foldr (\( i, person ) targets -> Identified.set i person targets) model.targets
    in
    ( { model | targets = newtargets }, Cmd.batch cmds )


{-| Have the plant eat the person at index i
-}
eat : Model -> Identified.Identifier -> ( Model, Cmd Msg )
eat model i =
    case Identified.get i model.targets of
        Nothing ->
            ( model, Cmd.none )

        Just person ->
            let
                plant =
                    model.plant

                newplant =
                    { plant | hunger = max (plant.hunger + 5 - person.details.weight) 0 }
            in
            ( { model
                | systemState =
                    if newplant.hunger > 10 then
                        GameOver

                    else
                        Loop
                , plant = newplant
                , targets = Identified.set i { person | animation = targetExit i person.animation } model.targets
              }
            , Random.generate NewTarget genPersonDetails
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ( is, persons ) =
            List.unzip (Identified.toList model.targets)
    in
    Animation.subscription Animate (List.map .animation persons)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.systemState of
        Loop ->
            { title = "The button, oooh"
            , body =
                [ viewPlant model.plant
                , Html.Keyed.node "div" [] (List.map viewTarget (Identified.toList model.targets))
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


viewTarget : ( Identified.Identifier, Person ) -> ( String, Html Msg )
viewTarget ( i, person ) =
    ( identifierToString i
    , div (Animation.render person.animation ++ [ onClick (Select i) ])
        [ div [] [ text (identifierToString i) ]
        , div [] [ text ("Name: " ++ viewFullName person) ]
        , div [] [ text ("Weight: " ++ viewWeight person) ]
        , div [] [ text ("Security: " ++ viewSecurity person) ]
        , div [] [ text ("Popularity: " ++ viewPopularity person) ]
        , div [] [ text ("Goodness: " ++ viewGoodness person) ]
        ]
    )


viewFullName : Person -> String
viewFullName person =
    String.join " " [ person.details.firstName, Maybe.withDefault "" person.details.middleName, person.details.lastName ]


viewWeight : Person -> String
viewWeight person =
    String.fromInt person.details.weight ++ "kg"


viewSecurity : Person -> String
viewSecurity person =
    String.fromInt person.details.security


viewPopularity : Person -> String
viewPopularity person =
    String.fromInt person.details.popularity


viewGoodness : Person -> String
viewGoodness person =
    String.fromInt person.details.goodness

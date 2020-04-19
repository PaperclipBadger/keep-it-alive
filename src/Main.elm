module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Animation
import Animation.Messenger
import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Identified exposing (Identified, identifierToString)
import List
import Random
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Keyed



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
    { firstName : Name
    , middleName : Maybe Name
    , lastName : Name
    , weight : Weight
    , security : Security
    , popularity : Popularity
    , goodness : Goodness
    }


somePerson : Person
somePerson =
    { firstName = "Defaulty"
    , middleName = Nothing
    , lastName = "McDefaultFace"
    , weight = 0
    , security = 0
    , popularity = 0
    , goodness = 0
    }


genPersonDetails : Random.Generator Person
genPersonDetails =
    Random.map Person genFirstName
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


type View
    = TargetSelect TargetSelectView
    | GameOver


type alias Slot =
    { identifier : Identified.Identifier
    , animation : Animation
    }


type alias TargetSelectView =
    { slots : Array (Maybe Slot)
    , queue : List Identified.Identifier
    }


enqueueIdentifier : Identified.Identifier -> TargetSelectView -> TargetSelectView
enqueueIdentifier i v =
    loadIdentifiers { v | queue = i :: v.queue }


firstEmptySlot : TargetSelectView -> Maybe Int
firstEmptySlot v =
    let
        helper : Int -> Maybe Int
        helper i =
            case Array.get i v.slots of
                Nothing ->
                    Nothing

                Just Nothing ->
                    Just i

                Just (Just _) ->
                    helper (i + 1)
    in
    helper 0


loadIdentifiers : TargetSelectView -> TargetSelectView
loadIdentifiers v =
    case v.queue of
        [] ->
            v

        identifier :: newQueue ->
            case firstEmptySlot v of
                Nothing ->
                    v

                Just i ->
                    let
                        value =
                            Just
                                { identifier = identifier
                                , animation = targetEnter
                                }
                    in
                    loadIdentifiers { slots = Array.set i value v.slots, queue = newQueue }


getSlot : Identified.Identifier -> TargetSelectView -> Maybe ( Int, Slot )
getSlot identifier v =
    let
        helper : Int -> Maybe ( Int, Slot )
        helper i =
            case Array.get i v.slots of
                Nothing ->
                    Nothing

                Just Nothing ->
                    Nothing

                Just (Just slot) ->
                    if identifier == slot.identifier then
                        Just ( i, slot )

                    else
                        helper (i + 1)
    in
    helper 0


cueUnloadIdentifier : Identified.Identifier -> TargetSelectView -> TargetSelectView
cueUnloadIdentifier identifier v =
    case getSlot identifier v of
        Nothing ->
            v

        Just ( index, slot ) ->
            { v | slots = Array.set index (Just { slot | animation = targetExit identifier slot.animation }) v.slots }


unloadIdentifier : Identified.Identifier -> TargetSelectView -> TargetSelectView
unloadIdentifier identifier v =
    case getSlot identifier v of
        Nothing ->
            v

        Just ( index, _ ) ->
            loadIdentifiers { v | slots = Array.set index Nothing v.slots }


catMaybes : List (Maybe a) -> List a
catMaybes l =
    case l of
        [] ->
            []

        Nothing :: xs ->
            catMaybes xs

        (Just x) :: xs ->
            x :: catMaybes xs


updateAnimations : TargetSelectView -> Animation.Msg -> ( TargetSelectView, Cmd Msg )
updateAnimations v animMsg =
    let
        updateSlotAnimation : Maybe Slot -> ( Maybe Slot, Maybe (Cmd Msg) )
        updateSlotAnimation slot =
            case slot of
                Nothing ->
                    ( Nothing, Nothing )

                Just { identifier, animation } ->
                    let
                        ( newAnimation, cmd ) =
                            Animation.Messenger.update animMsg animation
                    in
                    ( Just { identifier = identifier, animation = newAnimation }, Just cmd )

        ( newSlotsList, maybecmds ) =
            v.slots
                |> Array.toList
                |> List.map updateSlotAnimation
                |> List.unzip

        newSlots =
            Array.fromList newSlotsList

        cmds =
            catMaybes maybecmds
    in
    ( { v | slots = newSlots }, Cmd.batch cmds )


type alias Model =
    { currentView : View
    , plant : Plant
    , targets : Identified Person
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentView = TargetSelect { slots = Array.repeat 4 Nothing, queue = [] }
      , plant = { size = Small, hunger = 10 }
      , targets = Identified.empty
      }
    , Cmd.batch (List.repeat 4 (Random.generate NewTarget genPersonDetails))
    )



-- UPDATE


type Msg
    = Animate Animation.Msg
    | NewTarget Person
    | Select Identified.Identifier
    | TargetExited Identified.Identifier


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.currentView of
        TargetSelect v ->
            case msg of
                Animate animMsg ->
                    let
                        ( newV, cmd ) =
                            updateAnimations v animMsg
                    in
                    ( { model | currentView = TargetSelect newV }, cmd )

                NewTarget person ->
                    let
                        ( i, targets ) =
                            Identified.append person model.targets
                    in
                    ( { model | currentView = TargetSelect (enqueueIdentifier i v), targets = targets }
                    , Cmd.none
                    )

                Select i ->
                    eat model i

                TargetExited i ->
                    ( { model
                        | currentView = TargetSelect (unloadIdentifier i v)
                      }
                    , Cmd.none
                    )

        GameOver ->
            ( model, Cmd.none )


{-| Have the plant eat the person with identifier i
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

                newPlant =
                    { plant | hunger = max (plant.hunger + 5 - person.weight) 0 }

                newView =
                    case model.currentView of
                        GameOver ->
                            GameOver

                        TargetSelect v ->
                            if newPlant.hunger > 10 then
                                GameOver

                            else
                                TargetSelect (cueUnloadIdentifier i v)
            in
            ( { model
                | currentView = newView
                , plant = newPlant
              }
            , Random.generate NewTarget genPersonDetails
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentView of
        GameOver ->
            Sub.none

        TargetSelect v ->
            let
                animations : List Animation
                animations =
                    v.slots
                        |> Array.toList
                        |> catMaybes
                        |> List.map .animation
            in
            Animation.subscription Animate animations



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.currentView of
        TargetSelect v ->
            { title = "The button, oooh"
            , body =
                [ Svg.svg
                    [ Svg.Attributes.width (String.fromFloat pageWidth)
                    , Svg.Attributes.height (String.fromFloat pageHeight)
                    , Svg.Attributes.viewBox
                        ([ 0, 0, pageWidth, pageHeight ] |> List.map String.fromFloat |> String.join " ")
                    ]
                    [ viewPlant model.plant
                    , v.slots
                        |> Array.toList
                        |> indexedMap (\i -> Maybe.map (viewSlot i model.targets))
                        |> catMaybes
                        |> keyByIndex
                        |> Svg.Keyed.node "g"
                            [ translate optionsOriginX optionsOriginY ]
                    ]
                ]
            }

        GameOver ->
            { title = "Game over!"
            , body = [ Html.text "You died!" ]
            }


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap f =
    let
        helper : Int -> List a -> List b
        helper i l =
            case l of
                [] ->
                    []

                x :: xs ->
                    f i x :: helper (i + 1) xs
    in
    helper 0


keyByIndex : List a -> List ( String, a )
keyByIndex =
    let
        helper : Int -> List a -> List ( String, a )
        helper i l =
            case l of
                [] ->
                    []

                x :: xs ->
                    ( String.fromInt i, x ) :: helper (i + 1) xs
    in
    helper 0


pageHeight : Float
pageHeight =
    1000


pageWidth : Float
pageWidth =
    1000


verticalMargin : Float
verticalMargin =
    5


lineHeight : Float
lineHeight =
    15


plantOriginX : Float
plantOriginX =
    0


plantOriginY : Float
plantOriginY =
    0


plantHeight : Float
plantHeight =
    2 * lineHeight


plantWidth : Float
plantWidth =
    pageWidth


optionsOriginX : Float
optionsOriginX =
    plantOriginX


optionsOriginY : Float
optionsOriginY =
    plantOriginY + plantHeight + verticalMargin


optionHeight : Float
optionHeight =
    6 * lineHeight


optionWidth : Float
optionWidth =
    pageWidth


optionCornerRounding : Float
optionCornerRounding =
    lineHeight / 2


optionOriginX : Int -> Float
optionOriginX _ =
    0


optionOriginY : Int -> Float
optionOriginY index =
    toFloat index * (optionHeight + verticalMargin)


viewPlant : Plant -> Svg Msg
viewPlant plant =
    Svg.g
        [ translate plantOriginX plantOriginY
        ]
        [ Svg.rect
            [ Svg.Attributes.height (String.fromFloat plantHeight)
            , Svg.Attributes.width (String.fromFloat plantWidth)
            , Svg.Attributes.fill "grey"
            ]
            []
        , Svg.text_
            [ translate 0 (1 * lineHeight), Svg.Attributes.fill "black" ]
            [ Svg.text ("Plant size: " ++ viewPlantSize plant.size) ]
        , Svg.text_
            [ translate 0 (2 * lineHeight), Svg.Attributes.fill "black" ]
            [ Svg.text ("Plant hunger: " ++ String.fromInt plant.hunger) ]
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


translate : Float -> Float -> Html.Attribute Msg
translate a b =
    Svg.Attributes.transform (String.concat [ "translate(", String.fromFloat a, ",", String.fromFloat b, ")" ])


viewSlot : Int -> Identified Person -> Slot -> Svg Msg
viewSlot index people slot =
    let
        person =
            Maybe.withDefault somePerson (Identified.get slot.identifier people)
    in
    Svg.g
        (List.concat
            [ Animation.render slot.animation
            , [ translate (optionOriginX index) (optionOriginY index)
              , onClick (Select slot.identifier)
              ]
            ]
        )
        [ Svg.rect
            [ Svg.Attributes.height (String.fromFloat optionHeight)
            , Svg.Attributes.width (String.fromFloat optionWidth)
            , Svg.Attributes.rx (String.fromFloat optionCornerRounding)
            , Svg.Attributes.ry (String.fromFloat optionCornerRounding)
            , Svg.Attributes.fill "grey"
            ]
            []
        , Svg.text_
            [ translate 0 (lineHeight * 1), Svg.Attributes.fill "black" ]
            [ Svg.text ("Id: " ++ identifierToString slot.identifier) ]
        , Svg.text_
            [ translate 0 (lineHeight * 2), Svg.Attributes.fill "black" ]
            [ Svg.text ("Name: " ++ viewFullName person) ]
        , Svg.text_
            [ translate 0 (lineHeight * 3), Svg.Attributes.fill "black" ]
            [ Svg.text ("Weight: " ++ viewWeight person) ]
        , Svg.text_
            [ translate 0 (lineHeight * 4), Svg.Attributes.fill "black" ]
            [ Svg.text ("Security: " ++ viewSecurity person) ]
        , Svg.text_
            [ translate 0 (lineHeight * 5), Svg.Attributes.fill "black" ]
            [ Svg.text ("Popularity: " ++ viewPopularity person) ]
        , Svg.text_
            [ translate 0 (lineHeight * 6), Svg.Attributes.fill "black" ]
            [ Svg.text ("Goodness: " ++ viewGoodness person) ]
        ]


viewFullName : Person -> String
viewFullName person =
    String.join " " [ person.firstName, Maybe.withDefault "" person.middleName, person.lastName ]


viewWeight : Person -> String
viewWeight person =
    String.fromInt person.weight ++ "kg"


viewSecurity : Person -> String
viewSecurity person =
    String.fromInt person.security


viewPopularity : Person -> String
viewPopularity person =
    String.fromInt person.popularity


viewGoodness : Person -> String
viewGoodness person =
    String.fromInt person.goodness

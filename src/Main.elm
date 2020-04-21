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
import CmdState exposing (CmdState)
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Identified exposing (Identified, identifierToString)
import List
import Random
import Set exposing (Set)
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



-- Utilities


normalise : Float -> Float -> Float -> Float
normalise bottom top x =
    (x - bottom) / (top - bottom)


translate : Float -> Float -> Html.Attribute Msg
translate a b =
    Svg.Attributes.transform (String.concat [ "translate(", String.fromFloat a, ",", String.fromFloat b, ")" ])


catMaybes : List (Maybe a) -> List a
catMaybes l =
    case l of
        [] ->
            []

        Nothing :: xs ->
            catMaybes xs

        (Just x) :: xs ->
            x :: catMaybes xs


interpolateFloat : Float -> Float -> Float -> Float
interpolateFloat bottom top x =
    bottom + x * (top - bottom)


interpolateInt : Int -> Int -> Float -> Int
interpolateInt bottom top x =
    floor (toFloat bottom + x * toFloat (1 + top - bottom))


interpolateColor : Animation.Color -> Animation.Color -> Float -> Animation.Color
interpolateColor bottom top x =
    { red = interpolateInt bottom.red top.red x
    , green = interpolateInt bottom.green top.green x
    , blue = interpolateInt bottom.blue top.blue x
    , alpha = interpolateFloat bottom.alpha top.alpha x
    }


{-| A colormap is a list of color keyframes and cut off points.
-}
type alias ColorMap =
    { low : Animation.Color
    , high : Animation.Color
    , lowestTone : ( Float, Animation.Color )
    , otherTones : List ( Float, Animation.Color )
    }


applyColorMap : ColorMap -> Float -> Animation.Color
applyColorMap { low, high, lowestTone, otherTones } x =
    let
        ( lowestThresh, _ ) =
            lowestTone

        helper : ( Float, Animation.Color ) -> List ( Float, Animation.Color ) -> Animation.Color
        helper ( lowThresh, lower ) l =
            case l of
                [] ->
                    high

                ( highThresh, higher ) :: rest ->
                    if x < highThresh then
                        interpolateColor lower higher (normalise lowThresh highThresh x)

                    else
                        helper ( highThresh, higher ) rest
    in
    if x < lowestThresh then
        low

    else
        helper lowestTone otherTones



-- Game parameters


numSlots : Int
numSlots =
    6


startSlots : Int
startSlots =
    3


initPlantMass : Mass
initPlantMass =
    0


initPlantHunger : Float
initPlantHunger =
    hungerRefresh (plantMassToSize initPlantMass)


plantMediumThreshold : Mass
plantMediumThreshold =
    5


plantLargeThreshold : Mass
plantLargeThreshold =
    10


hungerRefresh : PlantSize -> Float
hungerRefresh size =
    case size of
        Small ->
            0.5

        Medium ->
            0.7

        Large ->
            0.9


minHunger : Float
minHunger =
    0


maxHunger : Float
maxHunger =
    1


minMass : Mass
minMass =
    0


maxMass : Mass
maxMass =
    1


minSecurity : Security
minSecurity =
    0


maxSecurity : Security
maxSecurity =
    10


minPopularity : Popularity
minPopularity =
    0


maxPopularity : Popularity
maxPopularity =
    3


minGoodness : Goodness
minGoodness =
    0


maxGoodness : Goodness
maxGoodness =
    10


maxAge : Int
maxAge =
    2


someFirstName : Name
someFirstName =
    "Greg"


otherFirstNames : List Name
otherFirstNames =
    [ "Steve", "Esther", "Blaine", "Yanni", "Jonathan", "Mickey", "Eve", "Remi", "Ribena", "Jamie", "James" ]


someMiddleName : Name
someMiddleName =
    "\"The Death\""


otherMiddleNames : List Name
otherMiddleNames =
    [ "Godfrey", "\"Hopscotch\"", "William", "Horton" ]


someLastName : Name
someLastName =
    "Smith"


otherLastNames : List Name
otherLastNames =
    [ "Rogers", "Van Der Pant", "Carlsberg", "Sutton", "Li", "Wang", "Forger" ]


somePerson : Person
somePerson =
    { firstName = "Defaulty"
    , middleName = Nothing
    , lastName = "McDefaultFace"
    , mass = 0
    , security = 0
    , popularity = 0
    , goodness = 0
    }



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
    { hunger : Float
    , mass : Float
    }


plantMassToSize : Mass -> PlantSize
plantMassToSize mass =
    if mass < plantMediumThreshold then
        Small

    else if mass < plantLargeThreshold then
        Medium

    else
        Large


plantSize : Plant -> PlantSize
plantSize plant =
    plantMassToSize plant.mass


type alias Name =
    String


type alias Mass =
    Float


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
    , mass : Mass
    , popularity : Popularity
    , security : Security
    , goodness : Goodness
    }


genPersonDetails : Random.Generator Person
genPersonDetails =
    let
        genMassAndPopularitySeed : Random.Generator Float
        genMassAndPopularitySeed =
            Random.float 0 1

        genSecuritySeed : Random.Generator Float
        genSecuritySeed =
            Random.float 0 1

        genGoodnessSeed : Random.Generator Float
        genGoodnessSeed =
            Random.float 0 1
    in
    Random.map Person genFirstName
        |> Random.andThen (\f -> Random.map f genMiddleName)
        |> Random.andThen (\f -> Random.map f genLastName)
        |> Random.andThen
            (\f ->
                Random.map (\seed -> ( seed, f )) genMassAndPopularitySeed
                    |> Random.andThen
                        (\( seed, f_ ) -> Random.map (\mass -> ( seed, f_ mass )) (genMass seed))
                    |> Random.andThen
                        (\( seed, f_ ) -> Random.map (\popularity -> ( seed, f_ popularity )) (genPopularity (1 - seed)))
                    |> Random.map
                        (\( _, f_ ) -> f_)
            )
        |> Random.andThen (\f -> genSecuritySeed |> Random.andThen genSecurity |> Random.map f)
        |> Random.andThen (\f -> genGoodnessSeed |> Random.andThen genGoodness |> Random.map f)


genFirstName : Random.Generator Name
genFirstName =
    Random.uniform someFirstName otherFirstNames


genMiddleName : Random.Generator (Maybe Name)
genMiddleName =
    let
        constructor : Random.Generator (Name -> Maybe Name)
        constructor =
            Random.weighted ( 0.9, always Nothing ) [ ( 0.1, Just ) ]

        value : Random.Generator Name
        value =
            Random.uniform someMiddleName otherMiddleNames
    in
    Random.map2 (<|) constructor value


genLastName : Random.Generator Name
genLastName =
    Random.uniform someLastName otherLastNames


genMass : Float -> Random.Generator Mass
genMass seed =
    Random.constant (interpolateFloat minMass maxMass seed)


genSecurity : Float -> Random.Generator Security
genSecurity seed =
    Random.constant (interpolateInt minSecurity maxSecurity seed)


genPopularity : Float -> Random.Generator Popularity
genPopularity seed =
    Random.constant (interpolateInt minPopularity maxPopularity seed)


genGoodness : Float -> Random.Generator Goodness
genGoodness seed =
    Random.constant (interpolateInt minGoodness maxGoodness seed)


type alias Model =
    { currentView : View
    , freshAnimationIdentifier : AnimationIdentifier
    , animationsRunning : Set AnimationIdentifier
    , plant : Plant
    , targets : Identified Person
    }


type View
    = TargetSelect TargetSelectView
    | GameOver


type alias TargetSelectView =
    { slots : Array (Maybe Slot)
    , queue : List Identified.Identifier
    , hungerBarAnimation : Animation
    }


type alias Slot =
    { identifier : Identified.Identifier
    , age : Int
    , fading : Bool
    , animation : Animation
    }


newSlot : Identified.Identifier -> CmdState Msg Model Slot
newSlot identifier =
    targetEnter identifier |> CmdState.map (\animation -> Slot identifier 0 False animation)


enqueueIdentifier : Identified.Identifier -> TargetSelectView -> CmdState Msg Model TargetSelectView
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


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Nothing ->
            False

        Just _ ->
            True


allSlotsEmpty : TargetSelectView -> Bool
allSlotsEmpty v =
    Array.foldr (&&) True (Array.map (not << isJust) v.slots)


loadIdentifiers : TargetSelectView -> CmdState Msg Model TargetSelectView
loadIdentifiers v =
    case v.queue of
        [] ->
            CmdState.state v

        identifier :: newQueue ->
            case firstEmptySlot v of
                Nothing ->
                    CmdState.state v

                Just i ->
                    let
                        setSlotAndQueue : Slot -> TargetSelectView
                        setSlotAndQueue slot =
                            { v | slots = Array.set i (Just slot) v.slots, queue = newQueue }
                    in
                    newSlot identifier
                        |> CmdState.map setSlotAndQueue
                        |> CmdState.andThen loadIdentifiers


getSlot : Identified.Identifier -> TargetSelectView -> Maybe ( Int, Slot )
getSlot identifier v =
    let
        helper : Int -> Maybe ( Int, Slot )
        helper i =
            case Array.get i v.slots of
                Nothing ->
                    Nothing

                Just Nothing ->
                    helper (i + 1)

                Just (Just slot) ->
                    if identifier == slot.identifier then
                        Just ( i, slot )

                    else
                        helper (i + 1)
    in
    helper 0


incrementAges : TargetSelectView -> CmdState Msg Model TargetSelectView
incrementAges v =
    let
        incrementAge : Slot -> Slot
        incrementAge slot =
            { slot | age = slot.age + 1 }

        agedV : TargetSelectView
        agedV =
            { v | slots = Array.map (Maybe.map incrementAge) v.slots }

        senesce : Maybe Slot -> CmdState Msg Model TargetSelectView -> CmdState Msg Model TargetSelectView
        senesce maybeSlot =
            case maybeSlot of
                Nothing ->
                    identity

                Just slot ->
                    if slot.age >= maxAge then
                        CmdState.andThen (cueUnloadIdentifier slot.identifier)

                    else
                        identity
    in
    Array.foldr senesce (CmdState.state agedV) agedV.slots


cueUnloadIdentifier : Identified.Identifier -> TargetSelectView -> CmdState Msg Model TargetSelectView
cueUnloadIdentifier identifier v =
    case getSlot identifier v of
        Nothing ->
            CmdState.state v

        Just ( index, slot ) ->
            -- It should be safe to unload slots repeatedly
            if slot.fading then
                CmdState.state v

            else
                targetExit identifier slot.animation
                    |> CmdState.map (\animation -> { v | slots = Array.set index (Just { slot | fading = True, animation = animation }) v.slots })


unloadIdentifier : Identified.Identifier -> TargetSelectView -> CmdState Msg Model TargetSelectView
unloadIdentifier identifier v =
    case getSlot identifier v of
        Nothing ->
            CmdState.state v

        Just ( index, _ ) ->
            loadIdentifiers { v | slots = Array.set index Nothing v.slots }


setHungerBarAnimation : Plant -> TargetSelectView -> CmdState Msg Model TargetSelectView
setHungerBarAnimation plant v =
    hungerBarUpdateAnimation plant v.hungerBarAnimation
        |> CmdState.map (\animation -> { v | hungerBarAnimation = animation })


type alias AnimationIdentifier =
    Int


animationStart : CmdState Msg Model AnimationIdentifier
animationStart =
    CmdState.get
        |> CmdState.andThen
            (\model ->
                CmdState.put
                    { model
                        | freshAnimationIdentifier = model.freshAnimationIdentifier + 1
                        , animationsRunning = Set.insert model.freshAnimationIdentifier model.animationsRunning
                    }
                    |> CmdState.andThen (\_ -> CmdState.state model.freshAnimationIdentifier)
            )


animationDone : AnimationIdentifier -> CmdState Msg Model ()
animationDone animationIdentifier =
    CmdState.modify (\model -> { model | animationsRunning = Set.remove animationIdentifier model.animationsRunning })


anyAnimationsRunning : Model -> Bool
anyAnimationsRunning =
    not << Set.isEmpty << .animationsRunning


init : () -> ( Model, Cmd Msg )
init _ =
    let
        plant =
            { hunger = initPlantHunger, mass = initPlantMass }

        targets =
            Identified.empty

        currentView =
            TargetSelect
                { slots = Array.repeat numSlots Nothing
                , queue = []
                , hungerBarAnimation = hungerBarInitAnimation plant 0
                }

        initModel : Model
        initModel =
            { currentView = currentView
            , freshAnimationIdentifier = 1
            , animationsRunning = Set.singleton 0
            , plant = plant
            , targets = targets
            }
    in
    ( initModel, Cmd.batch (List.repeat startSlots (Random.generate NewTarget genPersonDetails)) )



-- UPDATE


type AnimationType
    = SlotEnter Identified.Identifier
    | SlotExit Identified.Identifier
    | HungerBarUpdate


type Msg
    = Animate Animation.Msg
    | NewTarget Person
    | Select Identified.Identifier
    | AnimationDone AnimationIdentifier AnimationType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    CmdState.finalState model Cmd.none (update_ msg)


update_ : Msg -> CmdState Msg Model ()
update_ msg =
    case msg of
        Animate animMsg ->
            updateAnimate animMsg

        AnimationDone animationIdentifier animationType ->
            animationDone animationIdentifier
                |> CmdState.andThen (\_ -> updateOnAnimationDone animationType)

        NewTarget person ->
            updateNewTarget person

        Select identifier ->
            updateSelect identifier


updateAnimate : Animation.Msg -> CmdState Msg Model ()
updateAnimate animMsg =
    let
        updateSlotAnimation : Maybe Slot -> ( Maybe Slot, Maybe (Cmd Msg) )
        updateSlotAnimation maybeSlot =
            case maybeSlot of
                Nothing ->
                    ( Nothing, Nothing )

                Just ({ animation } as slot) ->
                    let
                        ( newAnimation, cmd ) =
                            Animation.Messenger.update animMsg animation
                    in
                    ( Just { slot | animation = newAnimation }, Just cmd )

        updateTargetSelectAnimations : TargetSelectView -> CmdState Msg Model ()
        updateTargetSelectAnimations v =
            let
                ( newSlotsList, maybecmds ) =
                    v.slots
                        |> Array.toList
                        |> List.map updateSlotAnimation
                        |> List.unzip

                newSlots =
                    Array.fromList newSlotsList

                ( newHungerBarAnimation, hungerBarCmd ) =
                    Animation.Messenger.update animMsg v.hungerBarAnimation

                cmds =
                    hungerBarCmd :: catMaybes maybecmds

                newV =
                    { v | slots = newSlots, hungerBarAnimation = newHungerBarAnimation }
            in
            CmdState.batchCommands cmds
                |> CmdState.andThen (\_ -> setCurrentView (TargetSelect newV))
    in
    ifTargetSelect updateTargetSelectAnimations (CmdState.state ())


andThenSilently : (a -> CmdState Msg Model b) -> CmdState Msg Model a -> CmdState Msg Model a
andThenSilently f =
    CmdState.andThen (\a -> CmdState.andThen (always (CmdState.state a)) (f a))


setCurrentView : View -> CmdState Msg Model ()
setCurrentView currentView =
    CmdState.modify (\model -> { model | currentView = currentView })


ifTargetSelect : (TargetSelectView -> CmdState Msg Model a) -> CmdState Msg Model a -> CmdState Msg Model a
ifTargetSelect if_ else_ =
    CmdState.get
        |> CmdState.andThen
            (\model ->
                case model.currentView of
                    TargetSelect v ->
                        if_ v

                    _ ->
                        else_
            )


updateOnAnimationDone : AnimationType -> CmdState Msg Model ()
updateOnAnimationDone animationType =
    case animationType of
        SlotExit i ->
            let
                updateTargetSelect : TargetSelectView -> CmdState Msg Model ()
                updateTargetSelect v =
                    unloadIdentifier i v
                        |> andThenSilently (\v_ -> setCurrentView (TargetSelect v_))
                        |> CmdState.andThen spawnPersonIfAllSlotsEmpty

                spawnPersonIfAllSlotsEmpty : TargetSelectView -> CmdState Msg Model ()
                spawnPersonIfAllSlotsEmpty v =
                    if allSlotsEmpty v then
                        CmdState.batchCommands [ Random.generate NewTarget genPersonDetails ]

                    else
                        CmdState.state ()
            in
            ifTargetSelect updateTargetSelect (CmdState.state ())

        HungerBarUpdate ->
            CmdState.get
                |> CmdState.andThen
                    (\model ->
                        if model.plant.hunger > maxHunger then
                            setCurrentView GameOver

                        else
                            CmdState.state ()
                    )

        _ ->
            CmdState.state ()


updateNewTarget : Person -> CmdState Msg Model ()
updateNewTarget person =
    let
        updateTargetSelect : TargetSelectView -> CmdState Msg Model ()
        updateTargetSelect v =
            CmdState.get
                |> CmdState.andThen
                    (\model ->
                        let
                            ( i, newTargets ) =
                                Identified.append person model.targets
                        in
                        enqueueIdentifier i v
                            |> CmdState.andThen (\v_ -> CmdState.put { model | currentView = TargetSelect v_, targets = newTargets })
                    )
    in
    ifTargetSelect updateTargetSelect (CmdState.state ())


updateSelect : Identified.Identifier -> CmdState Msg Model ()
updateSelect identifier =
    let
        updateView : TargetSelectView -> Plant -> CmdState Msg Model ()
        updateView v plant =
            cueUnloadIdentifier identifier v
                |> CmdState.andThen incrementAges
                |> CmdState.andThen (setHungerBarAnimation plant)
                |> CmdState.andThen (setCurrentView << TargetSelect)

        updateTargetSelect : TargetSelectView -> CmdState Msg Model ()
        updateTargetSelect v =
            CmdState.get
                |> CmdState.andThen
                    (\model ->
                        case Identified.get identifier model.targets of
                            Nothing ->
                                CmdState.state ()

                            Just person ->
                                let
                                    plant =
                                        model.plant

                                    newPlant =
                                        { plant
                                            | hunger = max minHunger (plant.hunger + hungerRefresh (plantSize plant) - person.mass)
                                            , mass = plant.mass + person.mass
                                        }

                                    cmds =
                                        List.repeat person.popularity (Random.generate NewTarget genPersonDetails)
                                in
                                updateView v newPlant
                                    |> CmdState.andThen (\_ -> CmdState.modify (\model_ -> { model_ | plant = newPlant }))
                                    |> CmdState.andThen (\_ -> CmdState.batchCommands cmds)
                    )
    in
    ifTargetSelect updateTargetSelect (CmdState.state ())



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
                    List.concat
                        [ v.slots
                            |> Array.toList
                            |> catMaybes
                            |> List.map .animation
                        , [ v.hungerBarAnimation ]
                        ]
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
                    [ Svg.rect
                        [ Svg.Attributes.width (String.fromFloat pageWidth)
                        , Svg.Attributes.height (String.fromFloat pageHeight)
                        , Svg.Attributes.fill "black"
                        ]
                        []
                    , viewPlant v model.plant
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


fontSize : Float
fontSize =
    20


{-| Padding _within_ text lines
-}
textPadding : Float
textPadding =
    2


{-| How much to move text up by to pretend the achor is at the bottom rather than at the line
-}
textOffset : Float
textOffset =
    4


{-| Padding _around_ text lines
-}
textMargin : Float
textMargin =
    5


lineHeight : Float
lineHeight =
    fontSize + 2 * textPadding


plantOriginX : Float
plantOriginX =
    0


plantOriginY : Float
plantOriginY =
    0


plantHeight : Float
plantHeight =
    2 * lineHeight + 2 * textMargin


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
    6 * lineHeight + 2 * textMargin


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


hungerBarMargin : Float
hungerBarMargin =
    textMargin


hungerBarHeight : Float
hungerBarHeight =
    lineHeight - 2 * hungerBarMargin


hungerBarWidth : Float
hungerBarWidth =
    plantWidth - 2 * textMargin - 2 * hungerBarMargin


hungerBarColorMap : ColorMap
hungerBarColorMap =
    { low = Animation.Color 0 255 0 1
    , high = Animation.Color 255 0 0 1
    , lowestTone = ( 0, Animation.Color 0 255 0 1 )
    , otherTones =
        [ ( 0.5, Animation.Color 255 255 0 1 )
        , ( 1.0, Animation.Color 255 0 0 1 )
        ]
    }


hungerBarColor : Float -> Animation.Color
hungerBarColor hunger =
    applyColorMap hungerBarColorMap (normalise minHunger maxHunger hunger)


hungerBarCornerRounding : Float
hungerBarCornerRounding =
    hungerBarHeight / 2


hungerBarInitAnimation : Plant -> AnimationIdentifier -> Animation
hungerBarInitAnimation plant animationIdentifier =
    hungerBarUpdateAnimation_ plant
        (Animation.style
            [ Animation.width (Animation.px 0)
            , Animation.fill (hungerBarColor minHunger)
            ]
        )
        animationIdentifier


hungerBarUpdateAnimation_ : Plant -> Animation -> AnimationIdentifier -> Animation
hungerBarUpdateAnimation_ plant animation animationIdentifier =
    let
        normalisedHunger =
            normalise minHunger maxHunger plant.hunger

        width =
            interpolateFloat 0 hungerBarWidth normalisedHunger
    in
    Animation.interrupt
        [ Animation.to
            [ Animation.width (Animation.px width)
            , Animation.fill (hungerBarColor plant.hunger)
            ]
        , Animation.Messenger.send (AnimationDone animationIdentifier HungerBarUpdate)
        ]
        animation


hungerBarUpdateAnimation : Plant -> Animation -> CmdState Msg Model Animation
hungerBarUpdateAnimation plant animation =
    animationStart
        |> CmdState.map (hungerBarUpdateAnimation_ plant animation)


textLine : Int -> String -> Svg Msg
textLine i s =
    Svg.text_
        -- Text is anchored at the bottom, so we translate up to pad.
        [ translate textPadding (toFloat (i + 1) * lineHeight - textPadding - textOffset)
        , Svg.Attributes.fill "black"
        , Svg.Attributes.fontSize (String.fromFloat fontSize)
        ]
        [ Svg.text s ]


viewPlant : TargetSelectView -> Plant -> Svg Msg
viewPlant v plant =
    Svg.g
        [ translate plantOriginX plantOriginY
        ]
        [ Svg.rect
            [ Svg.Attributes.height (String.fromFloat plantHeight)
            , Svg.Attributes.width (String.fromFloat plantWidth)
            , Svg.Attributes.fill "grey"
            ]
            []
        , Svg.g
            [ translate textMargin textMargin ]
            [ textLine 0 ("Plant size: " ++ viewPlantSize plant)
            , Svg.clipPath
                [ Svg.Attributes.id "hungerBarClip"
                , Svg.Attributes.clipPathUnits "userSpaceOnUse"
                ]
                [ Svg.rect
                    [ Svg.Attributes.height (String.fromFloat hungerBarHeight)
                    , Svg.Attributes.width (String.fromFloat hungerBarWidth)
                    , Svg.Attributes.rx (String.fromFloat hungerBarCornerRounding)
                    , Svg.Attributes.ry (String.fromFloat hungerBarCornerRounding)
                    ]
                    []
                ]
            , Svg.rect
                [ translate hungerBarMargin (1 * lineHeight + hungerBarMargin)
                , Svg.Attributes.height (String.fromFloat hungerBarHeight)
                , Svg.Attributes.width (String.fromFloat hungerBarWidth)
                , Svg.Attributes.clipPath "url(#hungerBarClip)"
                ]
                []
            , Svg.rect
                (List.concat
                    [ Animation.render v.hungerBarAnimation
                    , [ translate hungerBarMargin (1 * lineHeight + hungerBarMargin)
                      , Svg.Attributes.height (String.fromFloat hungerBarHeight)
                      , let
                            normalisedHunger =
                                normalise minHunger maxHunger plant.hunger

                            width =
                                interpolateFloat 0 hungerBarWidth normalisedHunger
                        in
                        Svg.Attributes.width (String.fromFloat width)
                      , Svg.Attributes.clipPath "url(#hungerBarClip)"
                      ]
                    ]
                )
                []
            ]
        ]


viewPlantSize : Plant -> String
viewPlantSize plant =
    case plantSize plant of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


transparent : Animation
transparent =
    Animation.style [ Animation.opacity 0.0 ]


targetEnter : Identified.Identifier -> CmdState Msg Model Animation
targetEnter targetIdentifier =
    animationStart
        |> CmdState.map
            (\animationIdentifier ->
                Animation.interrupt
                    [ Animation.to [ Animation.opacity 1.0 ]
                    , Animation.Messenger.send (AnimationDone animationIdentifier (SlotEnter targetIdentifier))
                    ]
                    transparent
            )


targetExit : Identified.Identifier -> Animation -> CmdState Msg Model Animation
targetExit targetIdentifier animation =
    animationStart
        |> CmdState.map
            (\animationIdentifier ->
                Animation.interrupt
                    [ Animation.to [ Animation.opacity 0.0 ]
                    , Animation.Messenger.send (AnimationDone animationIdentifier (SlotExit targetIdentifier))
                    ]
                    animation
            )


viewSlot : Int -> Identified Person -> Slot -> Svg Msg
viewSlot index people slot =
    let
        person =
            Maybe.withDefault somePerson (Identified.get slot.identifier people)
    in
    Svg.g
        (List.concat
            [ Animation.render slot.animation
            , [ translate (optionOriginX index) (optionOriginY index) ]
            , if not slot.fading then
                [ onClick (Select slot.identifier) ]

              else
                []
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
        , Svg.g
            [ translate textMargin textMargin ]
            [ textLine 0 ("Id: " ++ identifierToString slot.identifier)
            , textLine 1 ("Name: " ++ viewFullName person)
            , textLine 2 ("Mass: " ++ viewMass person)
            , textLine 3 ("Security: " ++ viewSecurity person)
            , textLine 4 ("Popularity: " ++ viewPopularity person)
            , textLine 5 ("Goodness: " ++ viewGoodness person)
            ]
        ]


viewFullName : Person -> String
viewFullName person =
    String.join " " [ person.firstName, Maybe.withDefault "" person.middleName, person.lastName ]


roundTo : Int -> Float -> Float
roundTo decimals f =
    let
        scale =
            toFloat (10 ^ decimals)
    in
    toFloat (round (f * scale)) / scale


prettyFloat : Float -> String
prettyFloat f =
    String.fromFloat (roundTo 2 f)


viewMass : Person -> String
viewMass person =
    prettyFloat (interpolateFloat 40 100 person.mass) ++ "kg" ++ " (" ++ String.fromFloat person.mass ++ ")"


viewSecurity : Person -> String
viewSecurity person =
    String.fromInt person.security


viewPopularity : Person -> String
viewPopularity person =
    String.fromInt person.popularity


viewGoodness : Person -> String
viewGoodness person =
    String.fromInt person.goodness

port module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes as Attr exposing (id, class, src, type_, classList, name, title)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (string, int, list, Decoder, at, Value)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Html.Events exposing (onClick, on)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    , status : String
    }


type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))
    | SetHue Int
    | SetRipple Int
    | SetNoise Int
    | SetStatus String


type ThumbnailSize
    = Small
    | Medium
    | Large


photoDecoder : Decoder Photo
photoDecoder =
    decode Photo
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


initialModel : Model
initialModel =
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Small
    , hue = 0
    , ripple = 0
    , noise = 0
    , status = ""
    }


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ "[" ++ toString thumbnail.size ++ "]KB")
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick (SelectByUrl thumbnail.url)
        ]
        []


viewLarge : Maybe String -> Html Msg
viewLarge url =
    case url of
        Nothing ->
            text ""

        Just val ->
            canvas [ id "main-canvas", class "large" ]
                []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (SetSize size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Attr.max "11", onImmeditateValuChange toMsg ] []
        , label [] [ text (toString magnitude) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me" ]
        , div
            [ class "status" ]
            [ text model.status ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
            ]
        , h3 [] [ text "Thumnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]


handleSelectByIndex : Int -> Model -> ( Model, Cmd Msg )
handleSelectByIndex index model =
    let
        newSelectedUrl : Maybe String
        newSelectedUrl =
            model.photos
                |> Array.fromList
                |> Array.get index
                |> Maybe.map .url
    in
        applyFilters { model | selectedUrl = newSelectedUrl }


handleSurpriseMe : Model -> ( Model, Cmd Msg )
handleSurpriseMe model =
    let
        randomPhotos =
            Random.int 0 ((List.length model.photos) - 1)
    in
        ( model, Random.generate SelectByIndex randomPhotos )


handleLoadPhotos : Result Http.Error (List Photo) -> Model -> ( Model, Cmd Msg )
handleLoadPhotos result model =
    case result of
        Ok photos ->
            applyFilters
                { model
                    | photos = photos
                    , selectedUrl = Maybe.map .url (List.head photos)
                }

        Err _ ->
            ( { model | loadingError = Just "Try restarting the server" }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.selectedUrl of
        Just selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
                ( model, setFilters { url = url, filters = filters } )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByIndex index ->
            handleSelectByIndex index model

        SelectByUrl selectedUrl ->
            applyFilters { model | selectedUrl = Just selectedUrl }

        SurpriseMe ->
            handleSurpriseMe model

        SetSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        LoadPhotos result ->
            handleLoadPhotos result model

        SetHue val ->
            ( { model | hue = val }, Cmd.none )

        SetRipple val ->
            ( { model | ripple = val }, Cmd.none )

        SetNoise val ->
            ( { model | noise = val }, Cmd.none )

        SetStatus val ->
            ( { model | status = val }, Cmd.none )


onImmeditateValuChange : (Int -> Msg) -> Attribute Msg
onImmeditateValuChange toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed"


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "http://elm-in-action.com/photos/list.json"
        |> Http.send LoadPhotos


viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model

        Just err ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text err ]
                ]


paperSlider : List (Attribute Msg) -> List (Html Msg) -> Html Msg
paperSlider =
    node "paper-slider"


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        status =
            "Initializing Pasta v" ++ toString flags
    in
        ( { initialModel | status = status }, initialCmd )


main : Program Float Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> statusChanges SetStatus)
        }


port setFilters : FilterOptions -> Cmd msg


port statusChanges : (String -> msg) -> Sub msg

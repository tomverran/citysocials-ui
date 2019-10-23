module Main exposing (User)

import Browser exposing (Document, UrlRequest, application)
import Browser.Navigation as Nav
import Debug exposing (log)
import Element exposing (Color, Element, centerX, el, fill, height, layout, none, padding, paddingXY, rgb255, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, map2, string)
import Url exposing (Url)


type alias User =
    { twitterId : String
    , name : String
    }


decodeUser : Decoder User
decodeUser =
    map2 User
        (field "twitterId" string)
        (field "name" string)


gotUser : Result Http.Error User -> Msg
gotUser msg =
    case msg of
        Result.Err _ ->
            Failure "HTTP call failed, soz"

        Result.Ok user ->
            UserFetched user


type Page
    = Welcome
    | Loading
    | Error String
    | Dashboard User


type alias Model =
    { navKey : Nav.Key
    , page : Page
    }


type Msg
    = NoOp
    | LogIn
    | UserFetched User
    | LinkClicked Url
    | Failure String


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { navKey = key
      , page = Welcome
      }
    , route url
    )


fetchProfile : Cmd Msg
fetchProfile =
    Http.get
        { url = "/api/user"
        , expect = Http.expectJson gotUser decodeUser
        }


internalRoute : Url -> Cmd Msg
internalRoute url =
    case url.fragment of
        Just "profile" ->
            fetchProfile

        a ->
            Cmd.none


route : Url -> Cmd Msg
route url =
    case url.path of
        "/" ->
            internalRoute url

        a ->
            Nav.load a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked url ->
            ( { model | page = Loading }, route url )

        UserFetched u ->
            ( { model | page = Dashboard u }, Cmd.none )

        LogIn ->
            ( { model | page = Error "sorry" }, Cmd.none )

        Failure why ->
            ( { model | page = Error why }, Cmd.none )

        NoOp ->
            ( { model | page = model.page }, Cmd.none )


onUrlChange : Url -> Msg
onUrlChange url =
    NoOp


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest req =
    case req of
        Browser.Internal url ->
            LinkClicked url

        Browser.External blah ->
            NoOp


blue =
    rgb255 86 163 166


brown =
    rgb255 72 69 56


yellowyGreen =
    rgb255 202 212 157


greenyYellow =
    rgb255 212 234 200


eggshell =
    rgb255 192 216 224


white =
    rgb255 255 255 255


blueYellowGradient =
    Background.gradient { angle = 90, steps = [ blue, greenyYellow ] }


nav : String -> Element msg
nav title =
    Element.el
        [ padding 30
        , width fill
        , Font.center
        , Font.size 84
        , Font.color white
        ]
        (text title)


standard : List (Element.Attribute msg)
standard =
    [ Background.color white
    , Font.color blue
    ]


inverted : List (Element.Attribute msg)
inverted =
    [ Background.color blue
    , Font.color white
    ]


clicky : List (Element.Attribute msg)
clicky =
    [ paddingXY 20 10
    , Border.rounded 20
    ]


body : Element Msg -> Element Msg
body content =
    Element.el
        [ padding 30
        , width fill
        , height fill
        , Background.color blue
        , Font.family
            [ Font.external
                { name = "Archivo Black"
                , url = "https://fonts.googleapis.com/css?family=Archivo+Black&display=swap"
                }
            , Font.sansSerif
            ]
        ]
        content


page : Element Msg -> Browser.Document Msg
page content =
    { title = "Convivial"
    , body = [ layout [] (body content) ]
    }


welcome : Element msg
welcome =
    Element.column [ width fill ]
        [ nav "City Socials"
        , Element.link (centerX :: (standard ++ clicky))
            { url = "/twitter/login"
            , label = Element.text "Log in with Twitter"
            }
        ]


view : Page -> Browser.Document Msg
view model =
    case model of
        Welcome ->
            page welcome

        Error why ->
            (nav >> page) ("sorry I'm not good at this: " ++ why)

        Loading ->
            page (Element.column [ width fill ] [ nav "Hang on, loading" ])

        Dashboard user ->
            page
                (Element.column [ width fill ]
                    [ nav ("Welcome " ++ user.name)
                    , el (inverted ++ [ centerX ]) (Element.text "Everything else coming soon")
                    ]
                )


main =
    Browser.application
        { init = init
        , view = \a -> view a.page
        , update = update
        , subscriptions = \a -> Sub.none
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }

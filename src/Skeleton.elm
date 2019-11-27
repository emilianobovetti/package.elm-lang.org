module Skeleton exposing
    ( Details
    , Segment
    , Warning(..)
    , authorSegment
    , helpSegment
    , moduleSegment
    , projectSegment
    , versionSegment
    , view
    )

import Browser
import Elm.Version as V
import Href
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as D



-- NODE


type alias Details msg =
    { title : String
    , header : List Segment
    , warning : Warning
    , attrs : List (Attribute msg)
    , kids : List (Html msg)
    }


type Warning
    = NoProblems
    | NewerVersion String V.Version



-- SEGMENT


type Segment
    = Text String
    | Link String String


helpSegment : Segment
helpSegment =
    Text "help"


authorSegment : String -> Segment
authorSegment author =
    Text author


projectSegment : String -> String -> Segment
projectSegment author project =
    Link (Href.toProject author project) project


versionSegment : String -> String -> Maybe V.Version -> Segment
versionSegment author project version =
    Link (Href.toVersion author project version) (vsnToString version)


moduleSegment : String -> String -> Maybe V.Version -> String -> Segment
moduleSegment author project version moduleName =
    Link (Href.toModule author project version moduleName Nothing) moduleName


vsnToString : Maybe V.Version -> String
vsnToString maybeVersion =
    case maybeVersion of
        Nothing ->
            "latest"

        Just version ->
            V.toString version



-- VIEW


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title =
        details.title
    , body =
        [ viewHeader details.header
        , lazy viewWarning details.warning
        , Html.map toMsg <|
            div (class "center" :: details.attrs) details.kids
        , viewFooter
        ]
    }



-- VIEW HEADER


viewHeader : List Segment -> Html msg
viewHeader segments =
    div
        [ style "background-color" "#eeeeee"
        ]
        [ div [ class "center" ]
            [ h1 [ class "header" ]
                [ text "⚠️ not official "
                , a
                    [ class "spacey-char", href "https://package.elm-lang.org" ]
                    [ text "package.elm-lang.org" ]
                ]
            , h1 [ class "header" ] <|
                List.intersperse slash (List.map viewSegment segments)
            ]
        ]


slash : Html msg
slash =
    span [ class "spacey-char" ] [ text "/" ]


viewSegment : Segment -> Html msg
viewSegment segment =
    case segment of
        Text string ->
            text string

        Link address string ->
            a [ href address ] [ text string ]



-- VIEW WARNING


viewWarning : Warning -> Html msg
viewWarning warning =
    div [ class "header-underbar" ] <|
        case warning of
            NoProblems ->
                []

            NewerVersion url version ->
                [ p [ class "version-warning" ]
                    [ text "Warning! The latest version of this package is "
                    , a [ href url ] [ text (V.toString version) ]
                    ]
                ]



-- VIEW FOOTER


viewFooter : Html msg
viewFooter =
    div [ class "footer" ]
        [ text "All code for this site is open source and written in Elm. "
        , a [ class "grey-link", href "https://github.com/elm/package.elm-lang.org/" ] [ text "Check it out" ]
        , text "! — © 2012-present Evan Czaplicki"
        ]

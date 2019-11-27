module Session exposing
    ( Data
    , addDocs
    , addEntries
    , addReadme
    , addReleases
    , empty
    , fetchDocs
    , fetchReadme
    , fetchReleases
    , getDocs
    , getEntries
    , getReadme
    , getReleases
    )

import Dict
import Elm.Docs as Docs
import Elm.Version as V
import Http
import Json.Decode as Decode
import Page.Search.Entry as Entry
import Release
import Utils.OneOrMore exposing (OneOrMore(..))
import Utils.Url



-- SESSION DATA


type alias Data =
    { entries : Maybe (List Entry.Entry)
    , releases : Dict.Dict String (OneOrMore Release.Release)
    , readmes : Dict.Dict String String
    , docs : Dict.Dict String (List Docs.Module)
    }


empty : Data
empty =
    Data Nothing Dict.empty Dict.empty Dict.empty



-- ENTRIES


getEntries : Data -> Maybe (List Entry.Entry)
getEntries data =
    data.entries


addEntries : List Entry.Entry -> Data -> Data
addEntries entries data =
    { data | entries = Just entries }



-- RELEASES


toPkgKey : String -> String -> String
toPkgKey author project =
    author ++ "/" ++ project


getReleases : Data -> String -> String -> Maybe (OneOrMore Release.Release)
getReleases data author project =
    Dict.get (toPkgKey author project) data.releases


addReleases : String -> String -> OneOrMore Release.Release -> Data -> Data
addReleases author project releases data =
    let
        newReleases =
            Dict.insert (toPkgKey author project) releases data.releases
    in
    { data | releases = newReleases }


fetchReleases : (Result Http.Error (OneOrMore Release.Release) -> msg) -> String -> String -> Cmd msg
fetchReleases toMsg author project =
    Http.get
        { url = Utils.Url.package author project
        , expect = Http.expectJson toMsg Release.decoder
        }



-- README


toVsnKey : String -> String -> V.Version -> String
toVsnKey author project version =
    author ++ "/" ++ project ++ "@" ++ V.toString version


getReadme : Data -> String -> String -> V.Version -> Maybe String
getReadme data author project version =
    Dict.get (toVsnKey author project version) data.readmes


addReadme : String -> String -> V.Version -> String -> Data -> Data
addReadme author project version readme data =
    let
        newReadmes =
            Dict.insert (toVsnKey author project version) readme data.readmes
    in
    { data | readmes = newReadmes }


fetchReadme : (Result Http.Error String -> msg) -> String -> String -> V.Version -> Cmd msg
fetchReadme toMsg author project version =
    Http.get
        { url = Utils.Url.readme author project version
        , expect = Http.expectString toMsg
        }



-- DOCS


getDocs : Data -> String -> String -> V.Version -> Maybe (List Docs.Module)
getDocs data author project version =
    Dict.get (toVsnKey author project version) data.docs


addDocs : String -> String -> V.Version -> List Docs.Module -> Data -> Data
addDocs author project version docs data =
    let
        newDocs =
            Dict.insert (toVsnKey author project version) docs data.docs
    in
    { data | docs = newDocs }


fetchDocs : (Result Http.Error (List Docs.Module) -> msg) -> String -> String -> V.Version -> Cmd msg
fetchDocs toMsg author project version =
    Http.get
        { url = Utils.Url.docsJson author project version
        , expect = Http.expectJson toMsg (Decode.list Docs.decoder)
        }

module Utils.Url exposing (docsJson, package, readme, searchJson)

import Elm.Version
import Url.Builder exposing (crossOrigin)


base : String
base =
    "https://package.elm-lang.org"


searchJson : String
searchJson =
    "search.json"


package : String -> String -> String
package author project =
    crossOrigin base [ "packages", author, project, "releases.json" ] []


readme : String -> String -> Elm.Version.Version -> String
readme author project version =
    crossOrigin base [ "packages", author, project, Elm.Version.toString version, "README.md" ] []


docsJson : String -> String -> Elm.Version.Version -> String
docsJson author project version =
    crossOrigin base [ "packages", author, project, Elm.Version.toString version, "docs.json" ] []

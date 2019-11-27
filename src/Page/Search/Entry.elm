module Page.Search.Entry exposing
    ( Entry
    , decoder
    , fuzzyTerms
    , search
    )

import EditDistance exposing (levenshteinOfStrings)
import Elm.Version as V
import Json.Decode as D
import Set exposing (Set)



-- ENTRY


type alias Entry =
    { name : String
    , author : String
    , project : String
    , summary : String
    , license : String
    , versions : List V.Version
    }



-- SEARCH


search : String -> List Entry -> List Entry
search query entries =
    let
        queryTerms =
            String.words (String.toLower query)

        matchesAllTerms entry =
            let
                lowerName =
                    String.toLower entry.name

                lowerSummary =
                    String.toLower entry.summary

                matchesTerm term =
                    String.contains term lowerName
                        || String.contains term lowerSummary
            in
            List.all matchesTerm queryTerms
    in
    List.filter matchesAllTerms entries


fuzzyMatch : String -> String -> Bool
fuzzyMatch text pattern =
    let
        textLength =
            String.length text

        patternLength =
            String.length pattern

        lengthDiff =
            abs (textLength - patternLength)
    in
    (String.length text > 3)
        && (String.length pattern > 3)
        && not (String.contains pattern text)
        && (lengthDiff < 5)
        && (levenshteinOfStrings text pattern - lengthDiff < 2)


fuzzyTerms : String -> List Entry -> Set String
fuzzyTerms query entries =
    let
        lowerQuery =
            String.toLower query

        aux es set =
            case es of
                entry :: tl ->
                    if fuzzyMatch (String.toLower entry.author) lowerQuery then
                        aux tl (Set.insert entry.author set)

                    else if fuzzyMatch (String.toLower entry.project) lowerQuery then
                        aux tl (Set.insert entry.project set)

                    else
                        aux tl set

                [] ->
                    set
    in
    aux entries Set.empty



-- DECODER


decoder : D.Decoder Entry
decoder =
    D.map4 (\f a b c -> f a b c)
        (D.field "name" (D.andThen splitName D.string))
        (D.field "summary" D.string)
        (D.field "license" D.string)
        (D.field "versions" (D.list V.decoder))


splitName : String -> D.Decoder (String -> String -> List V.Version -> Entry)
splitName name =
    case String.split "/" name of
        [ author, project ] ->
            D.succeed (Entry name author project)

        _ ->
            D.fail ("Ran into an invalid package name: " ++ name)

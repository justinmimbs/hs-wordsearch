module Main where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (listToMaybe)
import           System.Environment (getArgs)

import qualified Dictionary
import qualified WordSearch


main :: IO ()
main =
    getArgs
        >>= map (toGrid .> fmap findWordsInGrid .> maybe "invalid grid" show)
        .> unlines
        .> putStrLn


toGrid :: String -> Maybe ( Map Int Char, ( Int, Int ) )
toGrid s =
    case words s of
        rows@(row1 : row2 : rest) ->
            if (row2 : rest) |> all (length .> (==) (length row1)) then
                Just
                    ( concat rows
                        |> foldl
                            (\( map, i ) char -> ( Map.insert i char map, i + 1 ))
                            ( Map.empty, 1 )
                        |> fst
                    , ( length row1, length rows )
                    )
            else
                Nothing

        _ ->
            Nothing


findWordsInGrid :: ( Map Int Char, ( Int, Int ) ) -> [ ( String, [ Int ] ) ]
findWordsInGrid ( values, ( width, height ) ) =
    WordSearch.findPaths
        (WordSearch.graphGrid width height)
        values
        dict
        -- reify words
        |> map (\path -> ( map (\i -> Map.findWithDefault '_' i values) path, path ))


dict :: WordSearch.Dict
dict =
    WordSearch.dictFromList Dictionary.list


-- helpers

infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) x f =
    f x


infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) f g =
    g . f

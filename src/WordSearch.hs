module WordSearch where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Set as Set
import           Data.Set (Set)


type Graph =
    Map Int (Set Int)


data Dict = Dict
    { end :: Bool
    , cont :: Map Char Dict
    }
    deriving (Eq, Show)


findPaths :: Graph -> Map Int Char -> Dict -> [ [ Int ] ]
findPaths adjacencies values dict =
    adjacencies |> Map.keys |> concatMap (findPathsHelp adjacencies values dict []) |> fmap reverse


findPathsHelp :: Graph -> Map Int Char -> Dict -> [ Int ] -> Int -> [ [ Int ] ]
findPathsHelp adjacencies values dict1 previous current =
    Map.lookup current values
        >>= (\val -> Map.lookup val (cont dict1))
        |> maybe []
            (\dict2 ->
                let
                    path = current : previous
                in
                (if (end dict2) then [ path ] else [])
                ++
                (Map.lookup current adjacencies
                    |> fmap (\nexts -> foldr Set.delete nexts path)
                    |> maybe [] Set.toList
                    |> concatMap (findPathsHelp adjacencies values dict2 path)
                )
            )


dictAddWord :: [ Char ] -> Dict -> Dict
dictAddWord word dict =
    case word of
        [] ->
            Dict True (cont dict)

        x : rest ->
             Dict
                (end dict)
                (cont dict |> Map.alter (fromMaybe (Dict False Map.empty) .> dictAddWord rest .> Just) x)


dictFromList :: [ String ] -> Dict
dictFromList =
    foldl (flip dictAddWord) (Dict False Map.empty)


graphAddEdge :: Int -> Int -> Graph -> Graph
graphAddEdge x y =
    Map.alter (maybe (Set.singleton y) (Set.insert y) .> Just) x


graphAddEdges :: Int -> Int -> Graph -> Graph
graphAddEdges x y =
    graphAddEdge x y .> graphAddEdge y x


graphGrid :: Int -> Int -> Graph
graphGrid width height =
    foldl
        (\g n ->
            let
                right = mod n width /= 0
                down = div (n - 1) width + 1 < height
                left = mod (n - 1) width /= 0
            in
            [ if right then Just (n + 1) else Nothing
            , if right && down then Just (n + 1 + width) else Nothing
            , if down then Just (n + width) else Nothing
            , if down && left then Just (n - 1 + width) else Nothing
            ]
                |> catMaybes
                |> foldr (graphAddEdges n) g
        )
        Map.empty
        [ 1 .. width * height ]


-- example

ex :: [ [ Int ] ]
ex =
    findPaths
        (graphGrid 2 2)
        (Map.fromList [ ( 1, 'b' ), ( 2, 'a' ), ( 3, 't' ), ( 4, 'n' ) ])
        (dictFromList
            [ "an"
            , "ana"
            , "and"
            , "ant"
            , "anti"
            , "bad"
            , "bat"
            , "bot"
            , "boy"
            ]
        )


-- helpers

infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) x f =
    f x


infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) f g =
    g . f


-- tests

tests :: [ Bool ]
tests =
    [ let
        dict1 :: Dict
        dict1 =
            Dict False $ Map.fromList
                [ ( 'a'
                  , Dict False $ Map.fromList
                        [ ( 'n'
                          , Dict True $ Map.fromList
                                [ ( 'a', Dict True Map.empty )
                                , ( 'd', Dict True Map.empty )
                                , ( 't'
                                  , Dict True $ Map.fromList
                                        [ ( 'i', Dict True Map.empty )
                                        ]
                                  )
                                ]
                          )
                        ]
                  )
                , ( 'b'
                  , Dict False $ Map.fromList
                        [ ( 'a'
                          , Dict False $ Map.fromList
                                [ ( 'd', Dict True Map.empty )
                                , ( 't', Dict True Map.empty )
                                ]
                          )
                        , ( 'o'
                          , Dict False $ Map.fromList
                                [ ( 't', Dict True Map.empty )
                                , ( 'y', Dict True Map.empty )
                                ]
                          )
                        ]
                  )
                ]

        dict2 :: Dict
        dict2 =
            dictFromList
                [ "an"
                , "ana"
                , "and"
                , "ant"
                , "anti"
                , "bad"
                , "bat"
                , "bot"
                , "boy"
                ]
      in
      dict1 == dict2
    , let
        grid1 =
            Map.fromList
                [ ( 1, Set.fromList [ 2, 3, 4] )
                , ( 2, Set.fromList [ 1, 3, 4] )
                , ( 3, Set.fromList [ 1, 2, 4] )
                , ( 4, Set.fromList [ 1, 2, 3] )
                ]

        grid2 =
            foldl (\g (x, y) -> graphAddEdges x y g) Map.empty [ (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4) ]
      in
      grid1 == grid2
    , let
        grid1 =
            foldl (\g (x, y) -> graphAddEdges x y g) Map.empty [ (1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4) ]

        grid2 =
            graphGrid 2 2
      in
      grid1 == grid2
    ]

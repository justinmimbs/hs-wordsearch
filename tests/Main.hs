module Main
    ( main
    , spec
    )
    where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Test.Hspec
import qualified WordSearch
import           WordSearch (Dict(Dict))


main :: IO ()
main =
    hspec spec


spec :: Spec
spec = do
    describe "WordSearch.dictFromList" $
        it "given a list of words, creates a tree of maps indexing all words by every letter" $ do
            shouldBe
                (WordSearch.dictFromList
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
                (Dict False $ Map.fromList
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
                )

    describe "WordSearch.graphGrid" $
        it "given the dimensions of a grid, creates a graph representing the adjacencies between cells" $
            shouldBe
                (WordSearch.graphGrid 2 2)
                (Map.fromList
                    [ ( 1, Set.fromList [ 2, 3, 4] )
                    , ( 2, Set.fromList [ 1, 3, 4] )
                    , ( 3, Set.fromList [ 1, 2, 4] )
                    , ( 4, Set.fromList [ 1, 2, 3] )
                    ]
                )

    describe "WordSearch.findPaths" $
        it "given a graph, graph labels, and dictionary, finds simple paths in the graph that match words in the dictionary" $
            shouldBe
            (WordSearch.findPaths
                (WordSearch.graphGrid 2 2)
                (Map.fromList [ ( 1, 'b' ), ( 2, 'a' ), ( 3, 't' ), ( 4, 'n' ) ])
                (WordSearch.dictFromList
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
            )
            ([ [ 1, 2, 3 ], [ 2, 4 ], [ 2, 4, 3 ] ])

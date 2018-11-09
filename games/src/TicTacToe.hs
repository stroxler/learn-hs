{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TicTacToe where

import qualified Data.List as List

import Control.Exception (catch, evaluate, SomeException)
import Data.Foldable (foldl')


data Player = X | O | B
  deriving (Show, Eq)

data Board = Board { ownership :: [[Player]]
                   , size :: Int
                   }
  deriving (Show, Eq)

type Position = (Int, Int)

type Move = (Player, Position)


initialBoard :: Int -> Board
initialBoard size =
  Board { ownership = (replicate size $ replicate size B)
        , size = size }

isFull :: Board -> Bool
isFull Board {..} =
  let nBlank = length $ filter (== B) (concat ownership)
  in nBlank == 0

rows :: Board -> [[Player]]
rows Board { ownership } = ownership

cols :: Board -> [[Player]]
cols Board { ownership } = List.transpose ownership

diags :: Board -> [[Player]]
diags Board { .. } =
  let
    diag grid = [(grid !! i) !! i | i <- [0..(size - 1)]]
  in [diag ownership, diag $ List.reverse ownership]


winner :: Board -> Player
winner board =
  let
    triples = List.concatMap ($ board) [rows, cols, diags]
    winnerOf triple =
      let first = triple !! 0
      in if all (== first) triple
         then first
         else B
    scanForWinner winnerSoFar triple =
      if winnerSoFar == B
      then winnerOf triple
      else winnerSoFar
  in foldl' scanForWinner B triples


showPlayer :: Player -> String
showPlayer B = " "
showPlayer p = show p


showBoard :: Board -> String
showBoard Board {..} =
  let
    emptyRow = " " ++ (List.intersperse '+' $ List.replicate size '-')
    showRow row = " " ++ (List.intercalate "|" $ map showPlayer row)
    displayedRows = List.map showRow ownership
    boardLines = ["\n"] ++ List.intersperse emptyRow displayedRows ++ ["\n"]
    joinLines = List.intercalate "\n"
  in
    joinLines boardLines


legalMoves :: Board -> [Position]
legalMoves Board {..} =
  let
    positionsAndOwnership = [(ownership !! i !! j, (i+1, j+1)) |
                             i <- [0..(size-1)], j <- [0..(size-1)]]
    getPositions = map snd
    filterForOpen = filter ((==B) . fst)
  in getPositions $ filterForOpen $ positionsAndOwnership


updateList :: Int -> (a -> a) -> [a] -> [a]
updateList n f xs =
  let
    (x:after) = drop n xs
    before = take n xs
  in before ++ (f x : after)


insertList :: Int -> a -> [a] -> [a]
insertList n x xs = updateList n (const x) xs


makeMove :: Move -> Board -> Board
makeMove (player, (row, col)) b@Board { ownership } =
  let
    (r, c) = (row - 1, col - 1)
    updateCol  = insertList c player
    updateRows = updateList r updateCol
    newOwnership = updateRows ownership
  in b { ownership = newOwnership }


data TttState = TttState
  { board :: Board,
    currentPlayer :: Player }


initialTttState :: Int -> TttState
initialTttState size = TttState { board = (initialBoard size),
                                  currentPlayer = O }

nextTurn :: Player -> Player
nextTurn X = O
nextTurn O = X
nextTurn B = B


getNumber :: String -> IO Int
getNumber prompt = do
  putStr prompt
  rawResponse <- getLine
  let
    readAsInt = evaluate (read rawResponse :: Int)
    handleError (_ :: SomeException) = do
      putStrLn $ "Could not parse \"" ++ rawResponse ++ "\" as a number"
      getNumber prompt
    in catch readAsInt handleError


getMove :: Board -> Player -> IO Move
getMove board player =
  let permittedMoves = legalMoves board
  in do
    putStrLn $ "Player " ++ show player ++ "'s turn."
    row <- getNumber "Row: "
    col <- getNumber "Col: "
    if elem (row, col) permittedMoves
    then pure (player, (row, col))
    else do
      putStrLn "Not a legal move"
      getMove board player


play :: TttState -> IO ()
play TttState { board, currentPlayer } = do
  putStrLn $ showBoard board
  if winner board /= B
  then
    putStrLn $ "Player " ++ show (winner board) ++ " won!"
  else if isFull board
  then
    putStrLn $ "Tie game!"
  else do
    move <- getMove board currentPlayer
    play TttState { board = (makeMove move board)
                   , currentPlayer = (nextTurn currentPlayer)
                   }


ticTacToeGame :: IO ()
ticTacToeGame = play $ initialTttState 3

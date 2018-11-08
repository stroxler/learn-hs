{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-} -- needed in our error handler
{-# LANGUAGE FlexibleContexts #-}
module Nim 
            where

import Control.Exception (catch, evaluate, SomeException)
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import qualified Data.Map as Map

import Control.Monad.State (
  evalStateT, get, gets, modify, MonadState,
  )
import Control.Monad.IO.Class (liftIO, MonadIO)

data Player = First | Second
  deriving (Show, Eq)

type NimBoard = Map.Map Int Int

data NimState = NimState { board :: NimBoard
                         , player :: Player }
  deriving (Show, Eq)

type NimPlay = (Int, Int)

rowNumbers :: [Int]
rowNumbers = [1..5]

nextPlayer :: Player -> Player
nextPlayer First = Second
nextPlayer Second = First


getNStones :: Int -> NimBoard -> Int
getNStones row = Maybe.fromJust . (Map.lookup row)

initialNimState :: NimState
initialNimState = NimState
  { board = Map.fromList [(r, 6-r) | r <- rowNumbers]
  , player = First
  }

updateNimState :: NimPlay -> NimState -> NimState
updateNimState
    (row, nRemove)
    NimState {board = oldBoard, player = oldPlayer} =
  let
    nStonesBefore = getNStones row oldBoard
    nStonesAfter = nStonesBefore - nRemove
    board = Map.insert row nStonesAfter oldBoard
    player = nextPlayer oldPlayer
  in
    NimState { board, player }

hasWinner :: NimState -> Maybe Player
hasWinner NimState {..} =
  if foldr (+) 0 board == 0
  then Just $ nextPlayer player
  else Nothing

getMove :: NimState -> IO NimPlay
getMove ns@NimState {..} = do
  putStrLn $ showNimState ns
  row <- getNumber "row" 1 5
  let n_stones = getNStones row board
  if n_stones == 0
  then do
    putStrLn "That row is already empty"
    getMove ns
  else do
    nRemove <- getNumber "number of stones to remove" 1 n_stones
    pure (row, nRemove)



getNumber :: String -> Int -> Int -> IO Int
getNumber descr min_ max_ = do
  putStrLn $
    "Please enter a number between " ++ show min_ ++
    " and " ++ show max_ ++ " for the " ++ descr ++ ":"
  rawResponse <- getLine
  response <- let
    readAsInt = evaluate (read rawResponse :: Int)
    handleError (_ :: SomeException) = do
      putStrLn $ "Could not parse \"" ++ rawResponse ++ "\" as a number"
      getNumber descr min_ max_
    in catch readAsInt handleError
  if (min_ <= response) && (response <= max_)
  then pure response
  else do putStrLn "Out of range"; getNumber descr min_ max_

showNimState :: NimState -> String
showNimState NimState {..} =
  let
    rows = concatMap (showRow board) rowNumbers
    playerMsg = show player ++ " player's move.\n\n"
  in rows ++ "\n" ++ playerMsg


showRow :: NimBoard -> Int -> String
showRow board i =
  let n = getNStones i board
  in show i ++ ": " ++ List.intersperse ' ' (replicate n '*') ++ "\n"


playTurn :: (MonadState NimState m, MonadIO m) => m ()
playTurn = do
  move <- get >>= (liftIO . getMove)
  modify (updateNimState move)

play :: (MonadState NimState m, MonadIO m) => m ()
play = do
  playTurn
  isWinner <- gets hasWinner
  case isWinner of
    Just theWinner -> do
      liftIO . putStrLn $ "The winner is " ++ show theWinner
    Nothing ->
      play

nimGame :: IO ()
nimGame = (evalStateT play) initialNimState

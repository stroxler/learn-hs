-- these two pragmas are handy for destructuring records
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
-- FlexibleContexts seems to be necessary to use the signature
-- I gave on `play`; without it, I have to use a concrete signature
{-# LANGUAGE FlexibleContexts #-}
module Hangman
    ( hangmanGame
    ) where

-- standard library
import qualified Data.List as List
import Data.Char (toLower)
import System.IO (hSetEcho, stdin)
-- containers
import qualified Data.Set as Set
-- transformers
import Control.Monad.State (get, put, MonadState, evalStateT)
import Control.Monad.IO.Class (liftIO, MonadIO)


data HangmanState = HangmanState { word :: String
                                 , guesses :: Set.Set Char
                                 , remainingMisses :: Int }
  deriving (Show, Eq)

newHangmanState :: String -> HangmanState
newHangmanState word = HangmanState { word
                                    , guesses = Set.empty
                                    , remainingMisses = 5 }

hangmanGame :: IO ()
hangmanGame = do
  putStrLn "Enter a word to guess"
  word <- getTheWord
  (evalStateT play) (newHangmanState word)


-- Here's the concrete type...
--  play :: StateT HangmanState IO ()
play :: (MonadState HangmanState m, MonadIO m) => m ()
play = do
  hs@HangmanState { .. } <- get
  -- To debug progress, uncomment this...
  --  liftIO $ print hs
  if playerWon guesses word
  then
    liftIO $ putStrLn "You Win!"
  else if (remainingMisses <= 0)
  then
    liftIO $ do
      putStrLn "I'm sorry, you lost"
      putStrLn $ "The word was: " ++ word
  else
    do
      guess <- liftIO $ getGuess hs
      put $
        if (List.elem guess word)
        then hs { guesses = (Set.insert guess guesses) }
        else hs { remainingMisses = remainingMisses - 1 }
      play


playerWon :: Set.Set Char -> String -> Bool
playerWon guesses word = all ((flip Set.member) guesses) word


getGuess :: HangmanState -> IO Char
getGuess hs@HangmanState {..} = liftIO $ do
  putStrLn $ "The word so far is:\n\t" ++ showWord word guesses
  putStrLn $ "You have " ++ show remainingMisses ++ " wrong guesses left."
  putStrLn "Enter a guess as a letter then press ENTER"
  line <- (map (toLower)) <$> getLine
  case line of
    [ch] -> if (Set.member ch guesses)
            then do
              putStrLn $ "You already guessed '" ++ word ++
                         "', please guess a new letter then press ENTER"
              getGuess hs
            else
              pure ch
    _ -> do
      putStrLn "Please guess a single letter, then press ENTER"
      getGuess hs


showWord :: String -> Set.Set Char -> String
showWord word guesses =
   let
     mask char = if (Set.member char guesses) then char else '_'
     masked = map mask word
   in List.intersperse ' ' masked


getTheWord :: IO String
getTheWord = do
  hSetEcho stdin False
  out <- getLine
  hSetEcho stdin True
  pure $ map toLower out

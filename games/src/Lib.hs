-- these two pragmas are handy for destructuring records
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
-- FlexibleContexts seems to be necessary to use the signature
-- I gave on `play`; without it, I have to use a concrete signature
{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( hangmanGame
    , nimGame
    ) where


import Hangman ( hangmanGame )
import Nim ( nimGame )

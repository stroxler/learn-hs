# games

Some games in haskell, from Graham Hutton's book, Chapters 10 and 11.

The only part I skipped was making an AI player for tic-tac-toe; I'd
like to do that later, hopefully with alpha-beta pruning.

I'd also like to add a commandline tool for playing all these games.

## Notes on the AI project

Just some notes to make sure I know how to do the AI project when the
time comes:
 - The project involves building a game tree
 - The book brute-forces the tree with no pruning beyond fixed-level,
   but we could probably add alpha-beta pruning and it would be more
   interesting
 - One interesting haskell-specific thing: the top-level `play` function
   is recursive, and whenever the computer takes a turn we call
   `putStrLn "The computer is thinking..."` followed by
   `play $! <results_of_computer_stuff>`. Why do we do this? Because
   if we don't haskell's laziness would cause the board to not get evaluated
   until the next time it's displayed, which would make the timing of
   the computation-heavy stuff unpredictable from a UI point of view.

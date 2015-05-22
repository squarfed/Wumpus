% Hunt the Wumpus!
% Federico Squartini
%

> {-# Language GeneralizedNewtypeDeriving, MultiWayIf, FlexibleContexts #-}

> module Main where

Imports
-------

> import Prelude hiding       (break)
> import Data.Char            (toUpper)
> import Data.Map             ((!), Map, fromList, insert)
> import Control.Monad        (forM_, void, when)
> import Control.Monad.Trans.Either
> import Control.Monad.Random (MonadRandom,  RandT,     getRandomR,
>                              getRandomRs,  evalRandT            )
> import Control.Monad.State  (MonadState, StateT,  gets,
>                              put,        modify,  evalStateT)
> import Control.Monad.Trans
> import System.IO            (hFlush, stdout)
> import System.Random        (StdGen, getStdGen)
> import Text.Printf          (printf)

Functions
---------
Are we in debugging mode? I should have used the C pre-processor, but it
does not play well with Haskell's multiline strings.

> debug :: Bool
> debug = True

During the game we can be in one of these rooms:

> type Room = Int

The cave topology, a [dodechaedron](http://en.wikipedia.org/wiki/Dodecahedron):

> cave :: Map Room [Room]
> cave = fromList $ zip [1..] [[2,5,8],    [1,3,10],   [2,4,12],
>                              [3,5,14],   [1,4,6],    [5,7,15],
>                              [6,8,17],   [1,7,9],    [8,10,18],
>                              [2,9,11],   [10,12,19], [3,11,13],
>                              [12,14,20], [4,13,15],  [6,14,16],
>                              [15,17,20], [7,16,18],  [9,17,19],
>                              [11,18,20], [13,16,19]]

Different characters in the game: you, the wumpus, deadly pits and bats
which will carry you to random places in the cave.

> data Character = You | Wumpus | Pit1 | Pit2 | Bats1 |Bats2
>                  deriving (Eq,Enum,Ord)

Type for characters position:

> type Locations = Map Character Room

Global variables:

> data Globals = Globals { -- Used to start a new game with previous setup
>                          oldlocations::Locations
>                          -- Characters locations
>                        , locations::Locations
>                          -- Number of arrows left
>                        , arrows::Int
>                        }

A monad stack with global state, random numbers and IO:

> newtype Game a = Game { unGame::StateT Globals (RandT StdGen IO) a }
>     deriving (Applicative,  Functor,             Monad,
>               MonadIO,      MonadState Globals,  MonadRandom)

The monad runner:

> runGame :: Game a -> IO a
> runGame m = evalRandT (evalStateT (unGame m) undefined) =<< getStdGen

> data Branch = Start | GenLocs | ActionsLoop | Lose | Win | AskSetup

> data Action = Move | Shoot

Random number generators:

> rand20 :: MonadRandom m => m Int
> rand20 = getRandomR (1,20) -- random room

> rand3 :: MonadRandom m => m Int
> rand3 = getRandomR (1,3)   -- random neighboring room

> rand4 :: MonadRandom m => m Int
> rand4 = getRandomR (1,4)   -- used to decide whether and where the
>                            -- wumpus will move

Read a number from the prompt:

> getNum :: MonadIO m => String -> m Int
> getNum prompt = liftIO $ do
>     putStr $ prompt ++ "\n?"
>     hFlush stdout
>     fmap read getLine


Read a letter from the prompt:

> getLet :: MonadIO m => String -> m Char
> getLet prompt = liftIO $ do
>     putStr $ prompt ++ "\n?"
>     hFlush stdout
>     fmap (toUpper . head) getLine

Print game instructions:

> printInstructions :: MonadIO m => m ()
> printInstructions = liftIO $ do
>     putStrLn
>        "\tWelcome to 'Hunt the Wumpus'\n\n\
>        \  The Wumpus lives in a cave of 20 rooms. Each room has 3 tunnels\n\
>        \leading to other rooms. (look at a dodecahedron to see how this works.\n\
>        \If you don't know what a dodecahedron is, ask someone).\n\
>        \     HAZARDS:\n\
>        \ Bottomless Pits - Two rooms have bottomless pits in them. If you go\n\
>        \     there, you fall into the pit (& lose!)\n\
>        \ Super Bats - Two other rooms have super bats. If you go there a bat\n\
>        \     grabs you and takes you to some other room at random. (Which may\n\
>        \     be troublesome)\n\n\
>        \     THE WUMPUS:\n\
>        \ The wumpus is not bothered by hazards (he has sucker feet and is too\n\
>        \ big for a bat to lift). Usually he is asleep. Two things wake him up\n\
>        \ you shooting an arrow or you entering his room. If the wumpus wakes he\n\
>        \ moves (P=.75) one room or stays still (P=.25). After that, if he is\n\
>        \ where you are he eats you up and you lose!"
>     void $ getLet "Type an E then RETURN"
>     putStr
>        "     YOU:\n\
>        \ Each turn you may move or shoot a crooked arrow\n\
>        \ Moving: You can move one room (thru one tunnel)\n\
>        \ Arrows: You have 5 arrows. You lose when you run out. Each arrow can\n\
>        \ go from 1 to 5 rooms. you aim by telling the computer the\n\
>        \ room #s you want the arrow to go to. If the arrow can't go\n\
>        \ that way (if no tunnel) it moves at random to the next room.\n\
>        \ If the arrow hits the wumpus, you win.\n\
>        \ If the arrow hits you, you lose.\n\n\
>        \     WARNINGS:\n\
>        \When you are one room away from a wumpus or hazard the computer says:\n\
>        \ Wumpus: 'I smell a wumpus'\n\
>        \ BAT   : 'Bats nearby'\n\
>        \ PIT   : 'I feel a draft'\n"
>     void $ getLet "Type an E then RETURN"

Report hazards in the neighboring rooms:

> reportHazards :: (MonadIO m, MonadState Globals m,Functor m) => m ()
> reportHazards = do
>     liftIO $ putStrLn ""
>     locs <- gets locations
>     let rs@[r1,r2,r3] = cave!(locs!You)
>     forM_ rs $ \ room ->
>         if | room == locs!Wumpus
>                            -> liftIO $ putStrLn "I smell a wumpus!"

>            | room `elem` [locs!Pit1,locs!Pit2]
>                            -> liftIO $ putStrLn "I feel a draft"

>            | room `elem` [locs!Bats1,locs!Bats2]
>                            -> liftIO $ putStrLn "Bats nearby!"

>            | otherwise     -> return ()
>     void $ liftIO $ printf "You are in room %d\n" (locs!You)
>     void $ liftIO $ printf "Tunnels lead to %d %d %d\n\n" r1 r2 r3

Read user input and determine whether to move to a new room or shoot an arrow:

> moveOrShoot :: Game Action
> moveOrShoot = do
>     letter <- liftIO $ getLet "Shoot or Move (S-M)"
>     case letter of
>       'S' -> return Shoot
>       'M' -> return Move
>       _   -> moveOrShoot

Shoot an arrow:

> shoot :: EitherT Branch Game ()
> shoot = do
>     i <- getRange
>     rs <- getPath i
>     you <- gets $ (!You) . locations
>     shootOnPath you rs
>     liftIO $ putStrLn "Missed"
>     moveWumpus -- accomodate decreaseArrwows
>     decreaseArrows

Decrease the number of arrows by one:

> decreaseArrows :: EitherT  Branch Game ()
> decreaseArrows = do 
>   modify $ \g -> g { arrows = arrows g - 1 }
>   arrs <- gets arrows
>   case arrs of
>       0 -> left Lose
>       _ -> left ActionsLoop

How many rooms we want to shoot the arrow through:

> getRange :: MonadIO m => m Room
> getRange = do n <- liftIO $ getNum "No. of rooms (1-5)"
>               if | n < 1 || n > 5 -> getRange
>                  | otherwise      -> return n

Which room numbers we want to shoot through:

> getPath :: MonadIO m => Int -> m [Room]
> getPath n = go n []
>   where
>     go :: MonadIO m => Int -> [Room] -> m [Room]
>     go 0 path = return $ reverse path
>     go k path = do room <- liftIO $ getNum "Room #"
>                    if | k >= n-1 || room /= path!!1 ->
>                                 go (k-1) (room:path)
>
>                       | otherwise ->
>                              do liftIO $ putStrLn "Arrows aren't that crooked\
>                                                   \ - Try another room"
>                                 go k path

Shoot the arrow on the given path and check the result. If one of the rooms we
input is not a neighboring one, substitute it with a random neighbor:

> shootOnPath :: Room -> [Room] -> EitherT Branch Game ()
> shootOnPath _ [] = return ()
> shootOnPath scratchLoc (r:rs) = do
>   if | r `elem` cave!scratchLoc ->
>                     do checkShot r
>                        shootOnPath r rs
>
>      | otherwise -> do r' <- rand3 
>                        checkShot $ (cave!scratchLoc)!!(r'-1)
>                        shootOnPath r rs

Check the result of a shot. We can hit the wumpus and win, hit ourself
and lose or hit nothing and go on with the game:

> checkShot :: Room -> EitherT Branch Game ()
> checkShot scratchLoc = do
>   locs <- gets locations
>   when (scratchLoc == locs!Wumpus) $
>                  do liftIO $ putStrLn "AHA! You got the wumpus!"
>                     left Win
>   when (scratchLoc == locs!You) $
>                  do liftIO $ putStrLn "OUCH! Arrow got you!"
>                     left Lose

When the Wumpus is disturbed it moves to a neighbor room with probability 75\%.
If after the move he is in our room we lose:

> moveWumpus :: EitherT Branch Game ()
> moveWumpus = do
>   k <- rand4
>   when (k <= 3) $
>      modify $ \g -> let locs = locations g
>                         wumpusNewRoom = cave!(locs!Wumpus)!!(k-1)
>                     in g { locations = insert Wumpus wumpusNewRoom locs }
>   locs <- gets locations
>   if | locs!Wumpus == locs!You ->
>                  do liftIO $ putStrLn "Tsk tsk tsk - Wumpus got you!"
>                     left Lose
>
>      | otherwise -> left ActionsLoop

Move the character:

> move :: EitherT Branch Game ()
> move = getMove >>= goodMove

Get the room number we want to move to and check if it's a neighbor:

> getMove :: (MonadIO m, MonadState Globals m) => m Room
> getMove = do
>   you <- gets $ (!You) . locations
>   scratchLoc <- liftIO $ getNum "Where to"
>   if scratchLoc >= 1 && scratchLoc <= 20 &&
>      (scratchLoc == you || scratchLoc `elem` cave!you)
>        then return scratchLoc
>        else do liftIO $ putStrLn "Not possible -"
>                getMove

Move to the choosen room and do something depeding on what's in there:

> goodMove :: Room -> EitherT Branch Game ()
> goodMove scratchLoc = do
>   modify $ \g -> g {locations = insert You scratchLoc (locations g)}
>   locs <- gets locations
>   if | scratchLoc == locs!Wumpus ->
>            do liftIO $ putStrLn "... OOPS! Bumped a wumpus!"
>               moveWumpus
>      | scratchLoc `elem` [locs!Pit1,locs!Pit2] ->
>            do liftIO $ putStrLn "YYYYIIIIEEEE... Fell in pit"
>               left Lose
>      | scratchLoc `elem` [locs!Bats1,locs!Bats2] ->
>            do liftIO $ putStrLn "ZAP--SUPER BAT SNATCH! Elsewhereville for you!"
>               goodMove =<< rand20
>      | otherwise -> left ActionsLoop

Generate the starting locations:

> genLocs :: (MonadRandom m,Functor m) => m Locations
> genLocs = loop
>     where
>       loop = do
>         let chars = [You ..]
>         locs <- fromList . zip chars <$> getRandomRs (1,20)
>         if | or [locs!i == locs!j| i <- chars, j <- chars, i < j] -> loop
>            | otherwise                                            -> return locs


The main game loop.

> gameLoop :: Branch -> Game ()
> gameLoop branch  = case branch of
>   Start -> do c <- getLet "Instructions (Y-N)"
>               when (c =='Y') printInstructions
>               liftIO $ putStrLn "HUNT THE WUMPUS"
>               gameLoop GenLocs
>   GenLocs -> do locs <- genLocs
>                 when debug $ void $ liftIO $
>                    printf "Wumpus is at %d, pits at %d & %d, bats at %d & %d\n"
>                           (locs!Wumpus) (locs!Pit1) (locs!Pit2)
>                           (locs!Bats1)  (locs!Bats2)
>                 put $ Globals { oldlocations=locs, locations=locs, arrows=5 }
>                 gameLoop ActionsLoop
>   ActionsLoop -> do reportHazards
>                     action <- moveOrShoot
>                     result <- case action of
>                                  Move  -> runEitherT move
>                                  Shoot -> runEitherT shoot
>                     case result of
>                       Left x  -> gameLoop x
>   Lose -> do liftIO $ putStrLn "HA HA HA - You lose!"
>              gameLoop AskSetup
>   Win  -> do liftIO $ putStrLn "Hee hee hee - The wumpus'll get you next time!!"
>              gameLoop AskSetup
>   AskSetup -> do c <- getLet "Same setup (Y-N)"
>                  case c of
>                    'Y' -> do modify $ \g -> g { locations = oldlocations g
>                                               , arrows=5 }
>                              gameLoop ActionsLoop
>                    _   -> gameLoop GenLocs

Main
----

> main :: IO ()
> main = runGame $ gameLoop Start


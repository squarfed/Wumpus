% Hunt the Wumpus!
% Federico Squartini
%

> {-# Language GeneralizedNewtypeDeriving,DoAndIfThenElse,
>   FlexibleInstances,MultiParamTypeClasses,UndecidableInstances #-}

> module Main where

Imports
-------

> import Prelude hiding (break)
> import Data.Char (toUpper)
> import Data.Map (Map,(!),fromList,insert)
> import Control.Monad (forM_,forever,void,when)
> import Control.Monad.Random (MonadRandom,Rand,RandT,
>                              getRandom,getRandoms,getRandomR,getRandomRs,
>                              runRand,runRandT)
> import Control.Monad.State (MonadState,StateT,get,gets,put,modify,runStateT)
> import Control.Monad.Trans (lift)
> import Control.Monad.Trans.Either
> import Control.Monad.IO.Class (MonadIO,liftIO)
> import System.IO (hFlush,stdout)
> import System.Random (StdGen,getStdGen)
> import Text.Printf (printf)

Functions
---------
Are we in debugging mode? I should have used the C pre-processor, but it
does not play well with Haskell's multiline strings.

> debug :: Bool
> debug = False

Data type for the rooms we can be in the game:

> type Room = Int

The cave topology, a dodechaedron.

> cave :: Map Room [Room]
> cave = fromList $ zip [1..] [[2,5,8],[1,3,10],[2,4,12],
>                              [3,5,14],[1,4,6],[5,7,15],
>                              [6,8,17],[1,7,9],[8,10,18],
>                              [2,9,11],[10,12,19],[3,11,13],
>                              [12,14,20],[4,13,15],[6,14,16],
>                              [15,17,20],[7,16,18],[9,17,19],
>                              [11,18,20],[13,16,19]]

Different characters in the game: you, the wumpus, deadly pits and bats
which will carry you to random places in the cave.

> data Character = You | Wumpus | Pit1 | Pit2 | Bats1 |Bats2
>                  deriving (Eq,Enum,Ord)

Type for characters position

> type Locations = Map Character Room

Global variables:

> data Globals = Globals {locations::Locations, -- Characters locations
>                         arrows::Int}          -- Number of arrows left

Game possible outcomes:

> data GameOver = Win | Lose

Actions at your disposal:

> data Action = Shoot   -- Shoot an arrow
>             | Move    -- move to a neighboring room

Break from a loop

> break :: GameOver -> Game ()
> break = Game . EitherT. return . Left

A monad stack with exceptions, global state, random numbers and IO:

> newtype Game a = Game {unGame::EitherT GameOver (StateT Globals (RandT StdGen IO)) a}
>     deriving (Functor,Monad,MonadIO,MonadState Globals,MonadRandom)

A couple of instances for `EitherT`

> instance (MonadRandom m) => MonadRandom (EitherT e m) where
>     getRandom   = lift getRandom
>     getRandoms  = lift getRandoms
>     getRandomR  = lift . getRandomR
>     getRandomRs = lift . getRandomRs

> instance (MonadState s m) => MonadState s (EitherT e m) where
>     get = lift get
>     put = lift . put


The monad runner:

> runGame :: StdGen -> Globals -> Game a -> IO (Either GameOver a, StdGen)
> runGame g s m = do
>     ((outcome,_),g') <- runRandT (runStateT (runEitherT $ unGame m) s) g
>     return (outcome,g')

Random number generators:

> rand20 :: Game Int
> rand20 = getRandomR (1,20) -- random room

> rand3 :: Game Int
> rand3 = getRandomR (1,3)   -- random neighboring room

> rand4 :: Game Int
> rand4 = getRandomR (1,4)   -- used for deciding whether and where the
>                            -- wumpus will move

Read a number from the prompt:

> getNum :: String -> IO Int
> getNum prompt = do
>     putStr $ prompt ++ "\n?"
>     hFlush stdout
>     fmap read getLine


Read a letter from the prompt:

> getLet :: String -> IO Char
> getLet prompt = do
>     putStr $ prompt ++ "\n?"
>     hFlush stdout
>     fmap (toUpper . head) getLine

Print game instructions:

> printInstructions :: IO ()
> printInstructions = do
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

Report hazards in the rooms neighboring yours:

> reportHazards :: Game ()
> reportHazards = do
>     loc <- gets locations
>     forM_ (cave!(loc!You)) $ \room ->
>         if room == loc!Wumpus then
>              liftIO $ putStrLn "I smell a wumpus!"
>         else if room == loc!Pit1 || room == loc!Pit2  then
>              liftIO $ putStrLn "I feel a draft"
>         else when (room == loc!Bats1 || room == loc!Bats2) $
>              liftIO $ putStrLn "Bats nearby!"
>     void $ liftIO $ printf "You are in room %d\n" (loc!You )
>     let [r1,r2,r3] = cave!(loc!You)
>     void $ liftIO $ printf "Tunnels lead to %d %d %d\n\n" r1 r2 r3

Read user input and determine whether to move to a new room or shoot an arrow:

> moveOrShoot :: Game Action
> moveOrShoot = do
>     letter <-  liftIO $ getLet "Shoot or Move (S-M)"
>     case letter of
>       'S' -> return Shoot
>       'M' -> return Move
>       _   -> moveOrShoot

Shoot an arrow:

> shoot :: Game ()
> shoot = do
>     path <- getPath =<< getRange
>     you <- gets $ (!You) . locations
>     shootOnPath you path
>     liftIO $ putStrLn "Missed"
>     moveWumpus
>     modify $ \g -> g {arrows = arrows g - 1}
>     arrs <- gets arrows
>     case arrs of
>              0 -> break Lose
>              _ -> return ()
>      where

Read the number of rooms the player wants to shoot through:

>        getRange :: Game Int
>        getRange = do n <- liftIO $ getNum "No. of rooms (1-5)"
>                      if n < 1 || n > 5
>                         then getRange
>                         else return n

Read the sequence of rooms the player wants to shoot through:

>        getPath :: Int -> Game [Room]
>        getPath n = go 0 []
>            where
>              go :: Int -> [Room] -> Game [Room]
>              go k path | k == n = return $ reverse path
>                        | otherwise = do
>                             room <- liftIO $ getNum "Room #"
>                             if k <= 1 || (room /= path!!0 && room /= path!!1)
>                                then go (k+1) (room:path)
>                                else do liftIO $ putStrLn "Arrows aren't that crooked\
>                                                          \ - Try another room"
>                                        go k path

Shoot the arrow on the given path and check the result:

>        shootOnPath :: Room -> [Room] -> Game ()
>        shootOnPath scratchLoc =
>            mapM_ $ \r -> if r `elem` (cave!scratchLoc)
>                             then checkShot r
>                             else checkShot =<< rand3

Check the result of a shot. You can hit the wumpus and win, hit yourself
and lose or hit nothing and go on with the game:

>        checkShot :: Room -> Game ()
>        checkShot scratchLoc = do
>            loc <- gets locations
>            if scratchLoc == loc!Wumpus then
>                  do liftIO $ putStrLn "AHA! You got the wumpus!"
>                     break Win
>            else when (scratchLoc == loc!You) $ do
>                             liftIO $ putStrLn "OUCH! Arrow got you!"
>                             break Lose

Move the wumpus, if he moves in your character's room, you lose:

> moveWumpus :: Game ()
> moveWumpus = do
>    k <- rand4
>    when (k <= 3) $
>          modify $ \g -> let loc = locations g
>                             wumpus = loc!Wumpus
>                         in g {locations = insert Wumpus (cave!wumpus!!(k-1)) loc}
>    loc <- gets locations
>    when (loc!Wumpus == loc!You) $
>         do liftIO $ putStrLn "Tsk tsk tsk - Wumpus got you!"
>            break Lose

Move the character:

> move :: Game ()
> move = getMove >>= goodMove
>     where
>       getMove :: Game Room
>       getMove = do
>           you <- gets $ (!You) . locations
>           scratchLoc <- liftIO $ getNum "Where to"
>           if scratchLoc >= 1 && scratchLoc <= 20 &&
>              (scratchLoc == you || elem scratchLoc (cave!you))
>                then return scratchLoc
>                else do liftIO $ putStrLn "Not possible -"
>                        getMove
>
>       goodMove :: Room -> Game ()
>       goodMove scratchLoc = do
>          modify $ \g -> g {locations = insert You scratchLoc (locations g)}
>          loc <- gets locations
>          if scratchLoc == loc!Wumpus then
>                 do liftIO $ putStrLn "... OOPS! Bumped a wumpus!"
>                    moveWumpus
>          else if scratchLoc == loc!Pit1 || scratchLoc == loc!Pit2 then
>                 do liftIO $ putStrLn "YYYYIIIIEEEE... Fell in pit"
>                    break Lose
>          else when (scratchLoc == loc!Bats1 || scratchLoc == loc!Bats2) $ do
>                    liftIO $ putStrLn "ZAP--SUPER BAT SNATCH! Elsewhereville for you!"
>                    goodMove =<< rand20

Generate starting locations:

> genLocs :: StdGen -> (Locations,StdGen)
> genLocs = runRand loop
>     where
>       loop :: Rand StdGen Locations
>       loop = do
>         let chars = [You ..]
>         loc <- fmap (fromList . zip chars) $ getRandomRs (1,20)
>         if or [loc!i == loc!j| i <- chars, j <- chars, i < j]
>             then loop
>             else return loc


The main game loop. Initialize characters locations and then start the game:

> gameLoop :: Locations -> StdGen -> IO ()
> gameLoop loc g = do
>     (outcome,g') <- runGame g Globals {locations=loc,arrows=5} newGame
>     case outcome of
>        Left Lose -> putStrLn "HA HA HA - You lose!"
>        Left Win  -> putStrLn "Hee hee hee - The wumpus'll get you next time!!"
>     c <- getLet "Same setup (Y-N)"
>     case c of
>        'Y' -> gameLoop loc g'
>        _   -> uncurry gameLoop $ genLocs g'
>       where
>         newGame :: Game ()
>         newGame = do
>            liftIO $ putStrLn "HUNT THE WUMPUS"
>            when debug $ void $ liftIO $
>                      printf "Wumpus is at %d, pits at %d & %d, bats at %d & %d\n"
>                      (loc!Wumpus) (loc!Pit1) (loc!Pit2) (loc!Bats1) (loc!Bats2)
>            forever $ do reportHazards
>                         action <- moveOrShoot
>                         case action of
>                           Shoot -> shoot
>                           Move  -> move

Main
----

> main :: IO ()
> main = do
>   c <- getLet "Instructions (Y-N)"
>   when (c =='Y') printInstructions
>   uncurry gameLoop . genLocs =<< getStdGen

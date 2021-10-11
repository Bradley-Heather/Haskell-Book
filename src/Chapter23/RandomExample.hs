module RandomExample where 

import Control.Applicative  (liftA3)
import Control.Monad        (replicateM)
import Control.Monad.Trans.State 
import System.Random 

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix 
   deriving (Eq, Show)

intToDie :: Int -> Die 
intToDie n = case n of 
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix

    x -> error $ "intToDie go non 1-6 integer: " ++ show x 

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do 
    let s = mkStdGen 3
        (d1, s1) = randomR (1, 6) s 
        (d2, s2) = randomR (1, 6) s1
        (d3, _)  = randomR (1, 6) s2 
    (intToDie d1, intToDie d2, intToDie d3)

----------------------------------

-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie :: State StdGen Die 
rollDie = state $ do 
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die 
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

-- repeat :: a -> [a]

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie 

-- replicateM :: Monad m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die] 
nDie n = replicateM n rollDie 

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g 
    where 
        go :: Int -> Int -> StdGen -> Int 
        go sum count gen 
           | sum >= n = count
           | otherwise = let (die, nextGen) = 
                               randomR (1, 6) gen 
                         in go (sum + die)
                               (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g 
    where 
        go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die]) 
        go sum (count, rolls) gen 
           | sum >= n = (count, rolls)
           | otherwise = let (die, nextGen) = 
                               randomR (1, 6) gen 
                         in go (sum + die) ((count + 1), (intToDie die) : rolls)  nextGen
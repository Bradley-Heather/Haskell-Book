module Main where 

import Criterion.Main
import qualified Data.Map as M 
import qualified Data.Set as S 

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int 
m = M.fromList $ take 10000 stream 
    where stream = iterate bumpIt (0, 0)

s :: S.Set Int 
s = S.fromList $ take 10000 stream 
   where stream = iterate (+1) 0 

membersMap :: Int -> Bool
membersMap i = M.member i m 

insertMap :: Int -> Int -> M.Map Int Int 
insertMap 777 a = M.insert 777 a m 

membersSet :: Int -> Bool 
membersSet i = S.member i s 

insertSet :: Int -> S.Set Int 
insertSet a = S.insert a s 

main :: IO ()
main = defaultMain 
   [ bench "member check map"
     $ whnf membersMap 9999 
   , bench "member check set"
     $ whnf membersSet 9999 
   , bench "insert check map"
     $ whnf insertMap 12
   , bench "insert check set"
     $ whnf insertSet 777 ]
module DiffList where 

import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a]}

empty :: DList a 
empty = DL id 
{-# INLINE empty #-}

singleton :: a -> DList a 
singleton a = DL (a :) 
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList (DL a) = a []
{-# INLINE toList #-}

fromList :: [a] -> DList a
fromList a = DL (a ++)
{-# INLINE fromList #-}

infixr `cons`
cons :: a -> DList a -> DList a 
cons a as = DL ((a :) . unDL as)
{-# INLINE cons #-}

infixl `snoc` 
snoc :: DList a -> a -> DList a 
snoc as a = DL (unDL as . (a :)) 
{-# INLINE snoc #-} 

append :: DList a -> DList a -> DList a 
append as bs = DL (unDL as . unDL bs) 
{-# INLINE append #-}


schlemiel :: Int -> [Int] 
schlemiel i = go i [] 
    where go 0 xs = xs 
          go n xs = go (n-1) ([n] ++ xs) 

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty 
   where go 0 xs = xs 
         go n xs = 
             go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
   [ bench "concat list" $
     whnf schlemiel 123456 
   , bench "concat dlist" $
     whnf constructDlist 123456 ]
module Exercises  where 

data State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where 
    fmap f (State sa) = State $ \s -> let (a, s) = sa s 
                                      in (f a, s)

instance Applicative (State s) where 
    pure a = State $ \s -> (a, s)

    State f <*> State g = State $ \s -> let (a, s') = g s 
                                            (a', s'') = f s' 
                                        in (a' a, s'')

instance Monad (State s) where
    return = pure 

    State g >>= f = State $ \s -> let (a, s') = g s 
                                  in runState (f a) s' 

get :: State s s 
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s 
exec (State sa) s = snd $ sa s 

eval :: State s a -> s -> a 
eval (State sa) s = fst $ sa s

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s) 
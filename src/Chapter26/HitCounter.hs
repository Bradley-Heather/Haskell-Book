{-# LANGUAGE OverloadedStrings #-}

module HitCounter where 

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Web.Scotty.Trans

data Config = 
    Config { counts :: IORef (M.Map Text Integer)
           , prefix :: Text }

type Scotty = ScottyT Text (Reader Config IO)

type Handler = ActionT Text (ReaderT Config IO) 

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = 
    case M.lookup k m of 
        Nothing -> undefined
        Just a -> undefined

app :: Scotty ()
app = 
    get "/:key" $ do 
        unprefixed <- param "key"
        let key' = mappend undefined unprefixed 
        newInteger <- undefined

        html $ 
          mconcat [ "<h1>Success! Count was: "
                  , TL.pack $ show newInteger
                  , "</h1>"
                  ]

main :: IO () 
main = do 
    [prefixArg] <- getArgs
    counter <- newIORef M.empty 
    let config = undefined
        runR = undefined 
    scotty 3000 runR app 


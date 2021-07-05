module Main where

import Lib
import Control.Monad.Reader
import Protolude 

import Control.Monad.Except.CoHas hiding (throwError)

data Ctx = Ctx deriving Show
data RunTimeError = E2 Error2  | E3 Error3 deriving (Generic, CoHas Error2, CoHas Error3, Show)
newtype Error2 = Error2 Text deriving (Generic, Show)
newtype Error3 = Error3 Int deriving (Generic, Show)

main :: IO ()
main = do 
    val <- runExceptT . flip runReaderT Ctx $  (test  >> test2 :: MonadT RunTimeError)
    print val

type MonadT e = ReaderT Ctx (ExceptT e IO) ()

test :: (CoHas Error2 e) => MonadT e
test = do 
    throwError . inject $ Error2 "hello"

test2 :: (CoHas Error3 e) => MonadT e
test2 = do 
    throwError . inject $ Error3 10


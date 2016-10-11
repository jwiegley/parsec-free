{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts,
             UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, FlexibleInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Text.Parsec.Prim
    ( P.unknownError
    , P.sysUnExpectError
    , unexpected
    , ParsecT
    , runParsecT
    , P.mkPT
    , Parsec
    , P.Consumed(..)
    , P.Reply(..)
    , P.State(..)
    , parsecMap
    , parserReturn
    , parserBind
    , P.mergeErrorReply
    , parserFail
    , parserZero
    , parserPlus
    , (<?>)
    , (<|>)
    , label
    , labels
    , lookAhead
    , Stream(..)
    , tokens
    , try
    , token
    , tokenPrim
    , tokenPrimEx
    , many
    , skipMany
    , manyAccum
    , runPT
    , runPTLog
    , runP
    , runParserT
    , runParserTLog
    , runParser
    , parse
    , parseTest
    , parseTestLog
    , P.getPosition
    , P.getInput
    , P.setPosition
    , P.setInput
    , getParserState
    , setParserState
    , updateParserState
    , getState
    , putState
    , modifyState
    , setState
    , updateState
    ) where


import qualified Control.Exception as E
import Control.Monad()
import Control.Monad.Free (hoistFree)
import Control.Monad.Trans
import Control.Monad.Identity

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Cont.Class
import Control.Monad.Error.Class

import Data.IORef
import Text.Parsec.Pos
import Text.Parsec.Error
import qualified Text.Parsec.Free as F
import qualified Text.Parsec.Free.Eval as F
import qualified Text.Parsec.Free.Log as F
import qualified "parsec" Text.Parsec.Prim as P
import "parsec" Text.Parsec.Prim (Stream, State(..))

type ParsecT = F.ParsecDSL

runParsecT :: ParsecT s u m a -> F.ParsecDSL s u m a
runParsecT = id

unexpected :: (Stream s m t) => String -> ParsecT s u m a
unexpected = F.unexpected

type Parsec s u = ParsecT s u Identity

parsecMap :: (a -> b) -> ParsecT s u m a -> ParsecT s u m b
parsecMap = fmap

parserReturn :: a -> ParsecT s u m a
parserReturn = return

parserBind :: ParsecT s u m a -> (a -> ParsecT s u m b) -> ParsecT s u m b
parserBind = (>>=)

parserFail :: String -> ParsecT s u m a
parserFail = F.parserFail

parserZero :: ParsecT s u m a
parserZero = F.parserZero

parserPlus :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
parserPlus = F.parserPlus

infix  0 <?>
infixr 1 <|>

(<?>) :: (ParsecT s u m a) -> String -> (ParsecT s u m a)
(<?>) = label

(<|>) :: (ParsecT s u m a) -> (ParsecT s u m a) -> (ParsecT s u m a)
(<|>) = parserPlus

-- | A synonym for @\<?>@, but as a function instead of an operator.
label :: ParsecT s u m a -> String -> ParsecT s u m a
label = F.label

labels :: ParsecT s u m a -> [String] -> ParsecT s u m a
labels = F.labels

tokens :: (Monad m, Stream s m t, Eq t)
       => ([t] -> String)      -- Pretty print a list of tokens
       -> (SourcePos -> [t] -> SourcePos)
       -> [t]                  -- List of tokens to parse
       -> ParsecT s u m [t]
tokens = F.tokens

try :: ParsecT s u m a -> ParsecT s u m a
try = F.try

lookAhead :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
lookAhead = F.lookAhead

token :: (Stream s Identity t)
      => (t -> String)            -- ^ Token pretty-printing function.
      -> (t -> SourcePos)         -- ^ Computes the position of a token.
      -> (t -> Maybe a)           -- ^ Matching function for the token to parse.
      -> Parsec s u a
token showToken tokpos test = tokenPrim showToken nextpos test
    where
        nextpos _ tok ts = case runIdentity (P.uncons ts) of
                             Nothing -> tokpos tok
                             Just (tok',_) -> tokpos tok'

tokenPrim :: (Stream s m t)
          => (t -> String)                      -- ^ Token pretty-printing function.
          -> (SourcePos -> t -> s -> SourcePos) -- ^ Next position calculating function.
          -> (t -> Maybe a)                     -- ^ Matching function for the token to parse.
          -> ParsecT s u m a
tokenPrim showToken nextpos test = tokenPrimEx showToken nextpos Nothing test

tokenPrimEx :: (Stream s m t)
            => (t -> String)
            -> (SourcePos -> t -> s -> SourcePos)
            -> Maybe (SourcePos -> t -> s -> u -> u)
            -> (t -> Maybe a)
            -> ParsecT s u m a
tokenPrimEx = F.tokenPrimEx

many :: ParsecT s u m a -> ParsecT s u m [a]
many = F.many

skipMany :: ParsecT s u m a -> ParsecT s u m ()
skipMany = F.skipMany

manyAccum :: (a -> [a] -> [a])
          -> ParsecT s u m a
          -> ParsecT s u m [a]
manyAccum = F.manyAccum

runPT :: (Monad m, Stream s m t)
      => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runPT = P.runPT . F.eval

runPTLog :: (MonadIO m, MonadReader F.LogType m, Stream s m t)
         => ParsecT s u m a -> u -> SourceName -> s
         -> m (Either ParseError a)
runPTLog = P.runPT . F.evalLog

runP :: (Stream s Identity t)
     => Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runP p u n s = runIdentity $ P.runPT (F.eval p) u n s

runParserT :: (Stream s m t)
           => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runParserT = runPT

runParserTLog :: (MonadIO m, MonadReader F.LogType m, Stream s m t)
              => ParsecT s u m a
              -> u
              -> SourceName
              -> s
              -> m (Either ParseError a)
runParserTLog = runPTLog

runParser :: (Stream s Identity t)
          => Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runParser = runP

parse :: (Stream s Identity t)
      => Parsec s () a -> SourceName -> s -> Either ParseError a
parse p = runP p ()

parseTest :: (Stream s Identity t, Show a)
          => Parsec s () a -> s -> IO ()
parseTest p input
    = case parse p "" input of
        Left err -> do putStr "parse error at "
                       print err
        Right x  -> print x

parseTestLog' :: (MonadIO m, MonadReader F.LogType m, Stream s m t, Show a)
              => ParsecT s () m a -> s -> m ()
parseTestLog' p input = do
    eres <- runPTLog p () "" input
    liftIO $ case eres of
        Left err -> do putStr "parse error at "
                       print err
        Right x -> print x

parseTestLog :: (Stream s (ReaderT F.LogType IO) t, Show a)
             => Bool -- ^ If True, display every parse, not just the interesting ones
             -> ParsecT s () (ReaderT F.LogType IO) a -> s -> IO ()
parseTestLog b p input = do
    lg <- newIORef []
    eres <- E.try $ runReaderT (parseTestLog' p input) lg
    putStrLn $ case eres of
        Left err -> "EXCEPTION => " ++ show (err :: E.SomeException)
        Right a  -> "Result => " ++ show a
    theLog <- readIORef lg
    putStrLn $ F.renderLog b theLog

-- | Returns the full parser state as a 'State' record.

getParserState :: Monad m => ParsecT s u m (State s u)
getParserState = F.getParserState

-- | @setParserState st@ set the full parser state to @st@.

setParserState :: Monad m => State s u -> ParsecT s u m (State s u)
setParserState = F.setParserState

-- | @updateParserState f@ applies function @f@ to the parser state.

updateParserState :: Monad m => (State s u -> State s u) -> ParsecT s u m (State s u)
updateParserState = F.updateParserState

-- < User state combinators

-- | Returns the current user state.

getState :: (Monad m) => ParsecT s u m u
getState = F.getState

-- | @putState st@ set the user state to @st@.

putState :: (Monad m) => u -> ParsecT s u m ()
putState = F.putState

-- | @modifyState f@ applies function @f@ to the user state. Suppose
-- that we want to count identifiers in a source, we could use the user
-- state as:
--
-- >  expr  = do{ x <- identifier
-- >            ; modifyState (+1)
-- >            ; return (Id x)
-- >            }

modifyState :: (Monad m) => (u -> u) -> ParsecT s u m ()
modifyState = F.modifyState

-- XXX Compat

-- | An alias for putState for backwards compatibility.

setState :: (Monad m) => u -> ParsecT s u m ()
setState = putState

-- | An alias for modifyState for backwards compatibility.

updateState :: (Monad m) => (u -> u) -> ParsecT s u m ()
updateState = modifyState

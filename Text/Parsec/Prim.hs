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


import qualified Control.Applicative as Applicative (Alternative(..))
import Control.Monad()
import Control.Monad.Trans
import Control.Monad.Identity

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

newtype ParsecT s u m a = ParsecT { runParsecT :: F.ParsecDSL s u m a }
    deriving (Functor, Applicative, Applicative.Alternative, Monad,
              MonadPlus, MonadReader r, MonadState s, MonadCont,
              MonadError e, MonadTrans)

unexpected :: (Stream s m t) => String -> ParsecT s u m a
unexpected msg = ParsecT $ F.unexpected msg

type Parsec s u = ParsecT s u Identity

parsecMap :: (a -> b) -> ParsecT s u m a -> ParsecT s u m b
parsecMap = fmap

parserReturn :: a -> ParsecT s u m a
parserReturn = return

parserBind :: ParsecT s u m a -> (a -> ParsecT s u m b) -> ParsecT s u m b
parserBind = (>>=)

parserFail :: String -> ParsecT s u m a
parserFail str = ParsecT $ F.parserFail str

parserZero :: ParsecT s u m a
parserZero = ParsecT F.parserZero

parserPlus :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
parserPlus (ParsecT p) (ParsecT q) = ParsecT $ F.parserPlus p q

infix  0 <?>
infixr 1 <|>

(<?>) :: (ParsecT s u m a) -> String -> (ParsecT s u m a)
ParsecT p <?> str = ParsecT $ F.label p str

(<|>) :: (ParsecT s u m a) -> (ParsecT s u m a) -> (ParsecT s u m a)
(<|>) = parserPlus

-- | A synonym for @\<?>@, but as a function instead of an operator.
label :: ParsecT s u m a -> String -> ParsecT s u m a
label = (<?>)

labels :: ParsecT s u m a -> [String] -> ParsecT s u m a
labels (ParsecT p) xs = ParsecT $ F.labels p xs

tokens :: (Monad m, Stream s m t, Eq t)
       => ([t] -> String)      -- Pretty print a list of tokens
       -> (SourcePos -> [t] -> SourcePos)
       -> [t]                  -- List of tokens to parse
       -> ParsecT s u m [t]
tokens showTokens nextposs tts = ParsecT $ F.tokens showTokens nextposs tts

try :: ParsecT s u m a -> ParsecT s u m a
try (ParsecT p) = ParsecT $ F.try p

lookAhead :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
lookAhead (ParsecT p) = ParsecT $ F.lookAhead p

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
tokenPrimEx showToken nextpos f test =
    ParsecT $ F.tokenPrimEx showToken nextpos f test

many :: ParsecT s u m a -> ParsecT s u m [a]
many (ParsecT p) = ParsecT $ F.many p

skipMany :: ParsecT s u m a -> ParsecT s u m ()
skipMany (ParsecT p) = ParsecT $ F.skipMany p

manyAccum :: (a -> [a] -> [a])
          -> ParsecT s u m a
          -> ParsecT s u m [a]
manyAccum acc (ParsecT p) = ParsecT $ F.manyAccum acc p

runPT :: (Monad m, Stream s m t)
      => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runPT = P.runPT . F.eval (const id) id . runParsecT

runPTLog :: (MonadIO m, MonadReader F.LogType m, Stream s m t)
         => ParsecT s u m a -> u -> SourceName -> s
         -> m (Either ParseError a)
runPTLog (ParsecT p) = P.runPT (F.evalLog p)

runP :: (Stream s Identity t)
     => Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runP (ParsecT p) u n s = runIdentity $ P.runPT (F.eval (const id) id p) u n s

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

parseTestLog :: (MonadIO m, MonadReader F.LogType m, Stream s m t, Show a)
             => ParsecT s () m a -> s -> m ()
parseTestLog p input = do
    eres <- runPTLog p () "" input
    liftIO $ case eres of
        Left err -> do putStr "parse error at "
                       print err
        Right x -> print x

-- | Returns the full parser state as a 'State' record.

getParserState :: Monad m => ParsecT s u m (State s u)
getParserState = ParsecT F.getParserState

-- | @setParserState st@ set the full parser state to @st@.

setParserState :: Monad m => State s u -> ParsecT s u m (State s u)
setParserState s = ParsecT $ F.setParserState s

-- | @updateParserState f@ applies function @f@ to the parser state.

updateParserState :: Monad m => (State s u -> State s u) -> ParsecT s u m (State s u)
updateParserState f = ParsecT $ F.updateParserState f

-- < User state combinators

-- | Returns the current user state.

getState :: (Monad m) => ParsecT s u m u
getState = ParsecT F.getState

-- | @putState st@ set the user state to @st@.

putState :: (Monad m) => u -> ParsecT s u m ()
putState u = ParsecT $ F.putState u

-- | @modifyState f@ applies function @f@ to the user state. Suppose
-- that we want to count identifiers in a source, we could use the user
-- state as:
--
-- >  expr  = do{ x <- identifier
-- >            ; modifyState (+1)
-- >            ; return (Id x)
-- >            }

modifyState :: (Monad m) => (u -> u) -> ParsecT s u m ()
modifyState f = ParsecT $ F.modifyState f

-- XXX Compat

-- | An alias for putState for backwards compatibility.

setState :: (Monad m) => u -> ParsecT s u m ()
setState = putState

-- | An alias for modifyState for backwards compatibility.

updateState :: (Monad m) => (u -> u) -> ParsecT s u m ()
updateState = modifyState

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Parsec.Free.Log where

import                    Control.Lens
import                    Control.Monad (when)
import                    Control.Monad.IO.Class
import                    Control.Monad.Reader.Class
import                    Control.Monad.Trans.Class
import                    Control.Monad.Trans.State
import                    Data.Foldable (forM_)
import                    Data.IORef
import                    Data.Map (Map)
import qualified          Data.Map as M
import                    Text.Parsec.Free
import                    Text.Parsec.Free.Eval
import qualified "parsec" Text.Parsec.Prim as P

type LogType = IORef [ParseLog]

type LogParsecT s u m a = MonadReader LogType m => P.ParsecT s u m a

data ParseLog
    = forall s u m a. ParseAttempt Bool (ParsecF s u m a)
    | forall s u m a. ParseFailed (ParsecF s u m a)
    | forall s u m a b. Show b => ParseSuccess b (ParsecF s u m a)
    | forall s u m a. ParseSuccessful (ParsecF s u m a)
    | Indent
    | Dedent

instance Show ParseLog where
    show (ParseAttempt b p)  = "ParseAttempt " ++ show b ++ " " ++ show p
    show (ParseFailed p)     = "ParseFailed " ++ show p
    show (ParseSuccess a p)  = "ParseSuccess " ++ show a ++ " " ++ show p
    show (ParseSuccessful p) = "ParseSuccessful " ++ show p
    show Indent              = "Indent"
    show Dedent              = "Dedent"

$(makePrisms ''ParseLog)

data Result = Failure | Success | SuccessValue String | Pending

data LogEntry = LogEntry
    { _leDepth  :: Int
    , _leShow   :: Bool
    , _leParser :: String
    , _leResult :: Result
    }

$(makeLenses ''LogEntry)

instance Show LogEntry where
    show LogEntry {..} =
        replicate (_leDepth * 2) ' '
          ++ case _leResult of
              Failure          -> "("  ++ _leParser ++ ")"
              Success          -> _leParser
              SuccessValue str -> _leParser ++ " => " ++ str
              Pending          -> _leParser ++ "..."

data RenderState = RenderState
    { _rsIndex :: Int
    , _rsStack :: [Int]
    , _rsMap   :: Map Int LogEntry
    }

$(makeLenses ''RenderState)

newRenderState :: RenderState
newRenderState = RenderState 1 [] M.empty

renderLog :: Bool -> [ParseLog] -> String
renderLog showAll l =
    foldMap
        (\a -> if showAll || _leShow a
               then '\n' : show a
               else mempty)
        (_rsMap (execState (go (0 :: Int) (reverse l)) newRenderState))
  where
    go _ [] = return ""
    go n (x:xs) = case x of
        Indent -> go (n+1) xs
        Dedent -> go (n-1) xs

        ParseAttempt b p -> do
            i <- use rsIndex
            rsMap.at i ?= LogEntry { _leDepth  = n
                                   , _leShow   = b
                                   , _leParser = show p
                                   , _leResult = Pending
                         }
            rsStack %= (i:)
            rsIndex += 1
            go n xs

        ParseFailed _     -> setResult Failure >> go n xs
        ParseSuccessful _ -> setResult Success >> go n xs
        ParseSuccess v _  -> setResult (SuccessValue (show v)) >> go n xs
      where
        setResult str = do
            i <- gets (^?! rsStack._head)
            rsMap.ix i.leResult .= str
            rsStack %= tail

appendLog :: MonadIO m => ParseLog -> LogParsecT s u m ()
appendLog l = do
    ref <- lift ask
    liftIO $ modifyIORef ref (l:)

attempt :: MonadIO m
        => Bool -> ParsecF s u' m b -> LogParsecT s u m a
        -> LogParsecT s u m a
attempt b t p = do
    appendLog (ParseAttempt b t)
    P.parserPlus
        (P.try p <* appendLog (ParseSuccessful t))
        (appendLog (ParseFailed t) >> P.parserZero)

attemptShow :: (MonadIO m, Show a)
            => Bool -> ParsecF s u' m b -> LogParsecT s u m a
            -> LogParsecT s u m a
attemptShow b t p = do
    appendLog (ParseAttempt b t)
    P.parserPlus
        (do a <- P.try p
            appendLog (ParseSuccess a t)
            return a)
        (appendLog (ParseFailed t) >> P.parserZero)

indented :: MonadIO m => LogParsecT s u m a -> LogParsecT s u m a
indented p = do
    appendLog Indent
    P.parserPlus
        (P.try p <* appendLog Dedent)
        (appendLog Dedent >> P.parserZero)

evalLog :: (MonadIO m, P.Stream s m t)
        => ParsecDSL s u m a -> LogParsecT s u m a
evalLog = eval' attempt attemptShow indented

dumpLog :: MonadIO m => [ParseLog] -> m ()
dumpLog theLog = flip evalStateT (0, M.empty :: Map Int String) $
    forM_ (reverse theLog) $ \l -> do
    (i, m) <- get
    let go p = do
            let p' = m M.! (i-1)
            when (p /= p') $
                liftIO $ putStrLn $ p ++ " /= " ++ p'
            indent (i-1) >> put (i-1, M.delete i m)
    case l of
        ParseAttempt _ p   -> indent i >> put (i+1, M.insert i (show p) m)
        ParseSuccess _ p   -> go (show p)
        ParseSuccessful p  -> go (show p)
        ParseFailed p      -> go (show p)
        _ -> return ()
    case l of
        Indent -> return ()
        Dedent -> return ()
        _ -> liftIO $ print l
  where
    indent n = liftIO $ putStr $ replicate n ' '

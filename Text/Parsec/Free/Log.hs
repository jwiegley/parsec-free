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
import qualified "parsec" Text.Parsec.Error as P
import                    Text.Parsec.Free
import                    Text.Parsec.Free.Eval
import qualified "parsec" Text.Parsec.Prim as P

type LogType = IORef [ParseLog]

type LogParsecT s u m a = MonadReader LogType m => P.ParsecT s u m a

data ParseLog
    = forall s u m a. ParseAttempt Bool (ParsecF s u m a)
    | forall s u m a. ParseFailed Bool P.ParseError (ParsecF s u m a)
    | forall s u m a b. Show b => ParseSuccess Bool b (ParsecF s u m a)
    | forall s u m a. ParseSuccessful Bool (ParsecF s u m a)
    | Indent Bool
    | Dedent

instance Show ParseLog where
    show (ParseAttempt b p)    = "ParseAttempt " ++ show b ++ " " ++ show p
    show (ParseFailed b err p) = "ParseFailed " ++ show b ++ " " ++ show err ++ " " ++ show p
    show (ParseSuccess b a p)  = "ParseSuccess " ++ show b ++ " " ++ show a ++ " " ++ show p
    show (ParseSuccessful b p) = "ParseSuccessful " ++ show b ++ " " ++ show p
    show (Indent b)            = "Indent " ++ show b
    show Dedent                = "Dedent"

$(makePrisms ''ParseLog)

data Result = Failure Bool P.ParseError
            | Success Bool
            | SuccessValue Bool String
            | Pending

data LogEntry = LogEntry
    { _leDepth  :: Int
    , _leBranch :: Bool
    , _leShow   :: Bool
    , _leParser :: String
    , _leResult :: Result
    }

$(makeLenses ''LogEntry)

instance Show LogEntry where
    show LogEntry {..} =
        (if _leBranch
         then replicate (pred _leDepth * 2) ' '
                ++ case _leResult of
                    Failure _ _err   -> "- "
                    Success _        -> "+ "
                    SuccessValue _ _ -> "+ "
                    Pending          -> "? "
         else replicate (_leDepth * 2) ' ')
            ++ (case _leResult of
                    Failure _ _err     -> "("  ++ _leParser ++ ")"
                    Success _          -> _leParser
                    SuccessValue _ str -> _leParser ++ " => " ++ str
                    Pending          -> _leParser ++ "...")
            ++ if case _leResult of
                      Failure b _      -> b
                      Success b        -> b
                      SuccessValue b _ -> b
                      Pending          -> False
               then " *"
               else ""

data RenderState = RenderState
    { _rsIndex  :: Int
    , _rsBranch :: Bool
    , _rsStack  :: [Int]
    , _rsMap    :: Map Int LogEntry
    }

$(makeLenses ''RenderState)

newRenderState :: RenderState
newRenderState = RenderState 1 False [] M.empty

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
        Indent b -> rsBranch .= b >> go (n+1) xs
        Dedent   -> go (n-1) xs

        ParseAttempt shouldShow p -> do
            i <- use rsIndex
            b <- use rsBranch
            rsBranch .= False
            rsMap.at i ?= LogEntry { _leDepth  = n
                                   , _leBranch = b
                                   , _leShow   = shouldShow
                                   , _leParser = show p
                                   , _leResult = Pending
                         }
            rsStack %= (i:)
            rsIndex += 1
            go n xs

        ParseFailed b err _ -> setResult (Failure b err) >> go n xs
        ParseSuccessful b _ -> setResult (Success b) >> go n xs
        ParseSuccess b v _  -> setResult (SuccessValue b (show v)) >> go n xs
      where
        setResult str = do
            i <- gets (^?! rsStack._head)
            rsMap.ix i.leResult .= str
            rsStack %= tail

appendLog :: (MonadIO m, MonadReader LogType m) => ParseLog -> m ()
appendLog l = ask >>= \ref -> liftIO (modifyIORef ref (l:))

attempt :: MonadIO m
        => Bool -> ParsecF s u' m b -> LogParsecT s u m a
        -> LogParsecT s u m a
attempt b t p = P.mkPT $ \s -> do
    appendLog (ParseAttempt b t)
    res <- P.runParsecT p s
    r   <- parserReply res
    let consumed = case res of
            P.Consumed _ -> True
            P.Empty    _ -> False
    case r of
        P.Ok _ _ _  -> appendLog (ParseSuccessful consumed t)
        P.Error err -> appendLog (ParseFailed consumed err t)
    return res
  where
    parserReply res = case res of
        P.Consumed r -> r
        P.Empty    r -> r

attemptShow :: (MonadIO m, Show a)
            => Bool -> ParsecF s u' m b -> LogParsecT s u m a
            -> LogParsecT s u m a
attemptShow b t p = P.mkPT $ \s -> do
    appendLog (ParseAttempt b t)
    res <- P.runParsecT p s
    r   <- parserReply res
    let consumed = case res of
            P.Consumed _ -> True
            P.Empty    _ -> False
    case r of
        P.Ok x _ _  -> appendLog (ParseSuccess consumed x t)
        P.Error err -> appendLog (ParseFailed consumed err t)
    return res
  where
    parserReply res = case res of
        P.Consumed r -> r
        P.Empty    r -> r

indented :: MonadIO m
         => Bool -> LogParsecT s u m a -> LogParsecT s u m a
indented b p = P.mkPT $ \s -> do
    appendLog (Indent b)
    res <- P.runParsecT p s
    appendLog Dedent
    return res

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
        ParseAttempt _ p    -> indent i >> put (i+1, M.insert i (show p) m)
        ParseSuccess _ _ p  -> go (show p)
        ParseSuccessful _ p -> go (show p)
        ParseFailed _ _ p   -> go (show p)
        _ -> return ()
    case l of
        Indent _ -> return ()
        Dedent   -> return ()
        _ -> liftIO $ print l
  where
    indent n = liftIO $ putStr $ replicate n ' '

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Text.Parsec.Free.Log where

import                    Control.Monad.IO.Class
import                    Control.Monad.Trans.State
import                    Data.IORef
import                    Text.Parsec.Free
import                    Text.Parsec.Free.Eval
import qualified "parsec" Text.Parsec.Prim as P

type LogType = IORef [ParseLog]

type LogParsecT s u m a = P.ParsecT s (u, LogType) m a

data ParseLog
    = forall s u m a. ParseAttempt (ParsecF s u m a)
    | ParseFailed
    | forall a. Show a => ParseSuccess a
    | ParseSuccessful
    | Indent
    | Dedent

renderLog :: [ParseLog] -> String
renderLog l = evalState (go (0 :: Int) l) (1 :: Int)
  where
    go _ [] = return ""
    go n (x:xs) = case x of
        ParseAttempt p -> do
            idx <- get
            modify (+1)
            rest <- go n xs
            return $ display idx $ show p ++ rest
        ParseFailed -> do
            modify (subtract 1)
            idx <- get
            rest <- go n xs
            return $ display idx $ "=> FAIL" ++ rest
        ParseSuccess v -> do
            modify (subtract 1)
            idx <- get
            rest <- go n xs
            return $ display idx $ "=> " ++ show v ++ rest
        ParseSuccessful -> do
            modify (subtract 1)
            idx <- get
            rest <- go n xs
            return $ display idx $ "=> ok" ++ rest

        Indent -> go (n+1) xs
        Dedent -> go (n-1) xs
      where
        display idx s = '\n' : show idx ++ ": " ++ replicate (n * 2) ' ' ++ s

appendLog :: MonadIO m => ParseLog -> LogParsecT s u m ()
appendLog l = do
    (_, ref) <- P.getState
    liftIO $ modifyIORef ref (l:)

attempt :: MonadIO m
        => ParsecF s u' m b -> LogParsecT s u m a -> LogParsecT s u m a
attempt t p = do
    appendLog (ParseAttempt t)
    a <- p P.<|> do
        appendLog ParseFailed
        P.parserZero
    appendLog ParseSuccessful
    return a

indented :: MonadIO m => LogParsecT s u m a -> LogParsecT s u m a
indented p = do
    appendLog Indent
    a <- p P.<|> do
        appendLog Dedent
        P.parserZero
    appendLog Dedent
    return a

evalLog :: (MonadIO m, P.Stream s m t)
        => ParsecDSL s (u, LogType) m a -> LogParsecT s u m a
evalLog = eval attempt indented

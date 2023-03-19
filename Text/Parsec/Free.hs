{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Text.Parsec.Free where

import                    Control.Applicative hiding (many)
import                    Control.Monad
import                    Control.Monad.Cont.Class
import                    Control.Monad.Error.Class
import                    Control.Monad.Free
import                    Control.Monad.IO.Class
import                    Control.Monad.Reader.Class
import                    Control.Monad.State.Class
import                    Control.Monad.Trans.Class
import qualified "parsec" Text.Parsec.Pos as P
import qualified "parsec" Text.Parsec.Prim as P

newtype ParsecDSL s u m a = ParsecDSL {
    runParsecDSL :: Free (ParsecF s u m) a
    }
    deriving (Functor, Applicative, Monad)

instance Alternative (ParsecDSL s u m) where
    empty = parserZero
    (<|>) = parserPlus

#if __GLASGOW_HASKELL__ >= 808
instance MonadFail (ParsecDSL s u m) where
    fail _ = parserZero
#endif

instance MonadPlus (ParsecDSL s u m) where
    mzero = parserZero
    mplus = parserPlus

instance (MonadReader r m) => MonadReader r (ParsecDSL s u m) where
    ask = lift ask
    -- local f p = ParsecDSL $ \s -> local f (runParsecDSL p s)
    local f p = error "NYI Free.MonadReader"

-- I'm presuming the user might want a separate, non-backtracking
-- state aside from the Parsec user state.
instance (MonadState s m) => MonadState s (ParsecDSL s' u m) where
    get = lift get
    put = lift . put

instance (MonadCont m) => MonadCont (ParsecDSL s u m) where
    callCC f = error "NYI Free.callCC"

instance (MonadError e m) => MonadError e (ParsecDSL s u m) where
    throwError = lift . throwError
    p `catchError` h = error "NYI Free.catchError"

instance MonadTrans (ParsecDSL s u) where
    lift m = liftF' $ Peffect m id

instance MonadIO m => MonadIO (ParsecDSL s u m) where
    liftIO = lift . liftIO

data ParsecF s u m r
    = forall a. Plifted (P.ParsecT s u m a) (a -> r)
    | Preturn r
    | Pbind r
    | forall a. Peffect (m a) (a -> r)
    | forall a. Pquiet (ParsecDSL s u m a) (a -> r)

    | PgetState (u -> r)
    | PputState u r
    | PmodifyState (u -> u) r
    | PgetPosition (P.SourcePos -> r)
    | PsetPosition P.SourcePos r
    | PgetInput (s -> r)
    | PsetInput s r
    | PgetParserState (P.State s u -> r)
    | PsetParserState (P.State s u) (P.State s u -> r)
    | PupdateParserState (P.State s u -> P.State s u) (P.State s u -> r)

    | forall t. (P.Stream s m t, Eq t)
      => Ptokens ([t] -> String) (P.SourcePos -> [t] -> P.SourcePos)
                [t] ([t] -> r)
    | forall t a. P.Stream s m t
      => PtokenPrimEx (t -> String) (P.SourcePos -> t -> s -> P.SourcePos)
                     (Maybe (P.SourcePos -> t -> s -> u -> u))
                     (t -> Maybe a) (a -> r)

    | P.Stream s m Char => PalphaNum (Char -> r)
    | P.Stream s m Char => PanyChar (Char -> r)
    | forall t. (Show t, P.Stream s m t) => PanyToken (t -> r)
    | P.Stream s m Char => Pchar Char r
    | P.Stream s m Char => Pcrlf (Char -> r)
    | P.Stream s m Char => Pdigit (Char -> r)
    | P.Stream s m Char => PendOfLine (Char -> r)
    | forall t. (Show t, P.Stream s m t) => Peof r
    | P.Stream s m Char => PhexDigit (Char -> r)
    | P.Stream s m Char => Pletter (Char -> r)
    | P.Stream s m Char => Plower (Char -> r)
    | P.Stream s m Char => Pnewline (Char -> r)
    | P.Stream s m Char => PnoneOf [Char] (Char -> r)
    | P.Stream s m Char => PoctDigit (Char -> r)
    | P.Stream s m Char => PoneOf [Char] (Char -> r)
    | PparserFail String
    | PparserZero
    | P.Stream s m Char => Psatisfy (Char -> Bool) (Char -> r)
    | P.Stream s m Char => Pspace (Char -> r)
    | P.Stream s m Char => Pspaces r
    | P.Stream s m Char => Pstring String r
    | P.Stream s m Char => Ptab (Char -> r)
    | P.Stream s m Char => Pupper (Char -> r)
    | Punexpected String

    | forall a. PparserPlus (ParsecDSL s u m a) (ParsecDSL s u m a) (a -> r)
    | forall a. Plabel (ParsecDSL s u m a) String (a -> r)
    | forall a. Plabels (ParsecDSL s u m a) [String] (a -> r)
    | forall a. Ptry (ParsecDSL s u m a) (a -> r)
    | forall a. Pchainl (ParsecDSL s u m a) (ParsecDSL s u m (a -> a -> a)) a (a -> r)
    | forall a. Pchainl1 (ParsecDSL s u m a) (ParsecDSL s u m (a -> a -> a)) (a -> r)
    | forall a. Pchainr (ParsecDSL s u m a) (ParsecDSL s u m (a -> a -> a)) a (a -> r)
    | forall a. Pchainr1 (ParsecDSL s u m a) (ParsecDSL s u m (a -> a -> a)) (a -> r)
    | forall a. Pchoice [ParsecDSL s u m a] (a -> r)
    | forall a. Pcount Int (ParsecDSL s u m a) ([a] -> r)
    | forall a. PlookAhead (ParsecDSL s u m a) (a -> r)
    | forall a. Pmany (ParsecDSL s u m a) ([a] -> r)
    | forall a. Pmany1 (ParsecDSL s u m a) ([a] -> r)
    | forall a. PmanyAccum (a -> [a] -> [a]) (ParsecDSL s u m a) ([a] -> r)
    | forall a. Show a => PnotFollowedBy (ParsecDSL s u m a) r
    | forall a. Poption a (ParsecDSL s u m a) (a -> r)
    | forall a. PoptionMaybe (ParsecDSL s u m a) (Maybe a -> r)
    | forall a. Poptional (ParsecDSL s u m a) r
    | forall a. PskipMany (ParsecDSL s u m a) r
    | forall a. PskipMany1 (ParsecDSL s u m a) r
    | forall end a. PmanyTill (ParsecDSL s u m a) (ParsecDSL s u m end) ([a] -> r)
    | forall open close a. Pbetween (ParsecDSL s u m open) (ParsecDSL s u m close)
                               (ParsecDSL s u m a) (a -> r)
    | forall sep a. PendBy (ParsecDSL s u m a) (ParsecDSL s u m sep) ([a] -> r)
    | forall sep a. PendBy1 (ParsecDSL s u m a) (ParsecDSL s u m sep) ([a] -> r)
    | forall sep a. PsepBy (ParsecDSL s u m a) (ParsecDSL s u m sep) ([a] -> r)
    | forall sep a. PsepBy1 (ParsecDSL s u m a) (ParsecDSL s u m sep) ([a] -> r)
    | forall sep a. PsepEndBy (ParsecDSL s u m a) (ParsecDSL s u m sep) ([a] -> r)
    | forall sep a. PsepEndBy1 (ParsecDSL s u m a) (ParsecDSL s u m sep) ([a] -> r)

    | Pidentifier (ParsecDSL s u m String) (String -> r)
    | Preserved (ParsecDSL s u m ()) String r
    | Poperator (ParsecDSL s u m String) (String -> r)
    | PreservedOp (ParsecDSL s u m ()) String r
    | PcharLiteral (ParsecDSL s u m Char) (Char -> r)
    | PstringLiteral (ParsecDSL s u m String) (String -> r)
    | Pnatural (ParsecDSL s u m Integer) (Integer -> r)
    | Pinteger (ParsecDSL s u m Integer) (Integer -> r)
    | Pfloat (ParsecDSL s u m Double) (Double -> r)
    | PnaturalOrFloat (ParsecDSL s u m (Either Integer Double))
                      (Either Integer Double -> r)
    | Pdecimal (ParsecDSL s u m Integer) (Integer -> r)
    | Phexadecimal (ParsecDSL s u m Integer) (Integer -> r)
    | Poctal (ParsecDSL s u m Integer) (Integer -> r)
    | Psymbol (ParsecDSL s u m String) String (String -> r)
    | forall a. Plexeme (ParsecDSL s u m a) (a -> r)
    | PwhiteSpace (ParsecDSL s u m ()) r
    | forall a. Pparens (ParsecDSL s u m a) (a -> r)
    | forall a. Pbraces (ParsecDSL s u m a) (a -> r)
    | forall a. Pangles (ParsecDSL s u m a) (a -> r)
    | forall a. Pbrackets (ParsecDSL s u m a) (a -> r)
    | forall a. Psquares (ParsecDSL s u m a) (a -> r)
    | Psemi (ParsecDSL s u m String) (String -> r)
    | Pcomma (ParsecDSL s u m String) (String -> r)
    | Pcolon (ParsecDSL s u m String) (String -> r)
    | Pdot (ParsecDSL s u m String) (String -> r)
    | forall a. PsemiSep (ParsecDSL s u m [a]) ([a] -> r)
    | forall a. PsemiSep1 (ParsecDSL s u m [a]) ([a] -> r)
    | forall a. PcommaSep (ParsecDSL s u m [a]) ([a] -> r)
    | forall a. PcommaSep1 (ParsecDSL s u m [a]) ([a] -> r)

instance Functor (ParsecF s u m) where
    fmap f = \case
        Plifted p k            -> Plifted p (f . k)
        Preturn x              -> Preturn (f x)
        Pbind r                -> Pbind (f r)
        Peffect m k            -> Peffect m (f . k)
        Pquiet p k             -> Pquiet p (f . k)

        PgetState k            -> PgetState (f . k)
        PputState u r          -> PputState u (f r)
        PmodifyState g r       -> PmodifyState g (f r)
        PgetPosition k         -> PgetPosition (f . k)
        PsetPosition p r       -> PsetPosition p (f r)
        PgetInput k            -> PgetInput (f . k)
        PsetInput s r          -> PsetInput s (f r)
        PgetParserState k      -> PgetParserState (f . k)
        PsetParserState s k    -> PsetParserState s (f . k)
        PupdateParserState g k -> PupdateParserState g (f . k)

        Ptokens a b c k        -> Ptokens a b c (f . k)
        PtokenPrimEx a b c d k -> PtokenPrimEx a b c d (f . k)

        PalphaNum k            -> PalphaNum (f . k)
        PanyChar k             -> PanyChar (f . k)
        PanyToken k            -> PanyToken (f . k)
        Pchar x r              -> Pchar x (f r)
        Pcrlf k                -> Pcrlf (f . k)
        Pdigit k               -> Pdigit (f . k)
        PendOfLine k           -> PendOfLine (f . k)
        Peof r                 -> Peof (f r)
        PhexDigit k            -> PhexDigit (f . k)
        Pletter k              -> Pletter (f . k)
        Plower k               -> Plower (f . k)
        Pnewline k             -> Pnewline (f . k)
        PnoneOf xs k           -> PnoneOf xs (f . k)
        PoctDigit k            -> PoctDigit (f . k)
        PoneOf xs k            -> PoneOf xs (f . k)
        PparserFail s          -> PparserFail s
        PparserZero            -> PparserZero
        Psatisfy g k           -> Psatisfy g (f . k)
        Pspace k               -> Pspace (f . k)
        Pspaces r              -> Pspaces (f r)
        Pstring x r            -> Pstring x (f r)
        Ptab k                 -> Ptab (f . k)
        Pupper k               -> Pupper (f . k)
        Punexpected s          -> Punexpected s

        PparserPlus p q k      -> PparserPlus p q (f . k)
        Plabel p a k           -> Plabel p a (f . k)
        Plabels p a k          -> Plabels p a (f . k)
        Ptry p k               -> Ptry p (f . k)
        Pchainl p q a k        -> Pchainl p q a (f . k)
        Pchainl1 p q k         -> Pchainl1 p q (f . k)
        Pchainr p q a k        -> Pchainr p q a (f . k)
        Pchainr1 p q k         -> Pchainr1 p q (f . k)
        Pchoice xs k           -> Pchoice xs (f . k)
        Pcount n p k           -> Pcount n p (f . k)
        PlookAhead p k         -> PlookAhead p (f . k)
        Pmany p k              -> Pmany p (f . k)
        Pmany1 p k             -> Pmany1 p (f . k)
        PmanyAccum g p k       -> PmanyAccum g p (f . k)
        PnotFollowedBy p r     -> PnotFollowedBy p (f r)
        Poption a p k          -> Poption a p (f . k)
        PoptionMaybe p k       -> PoptionMaybe p (f . k)
        Poptional p r          -> Poptional p (f r)
        PskipMany p r          -> PskipMany p (f r)
        PskipMany1 p r         -> PskipMany1 p (f r)
        PmanyTill p e k        -> PmanyTill p e (f . k)
        Pbetween o c p k       -> Pbetween o c p (f . k)
        PendBy p s k           -> PendBy p s (f . k)
        PendBy1 p s k          -> PendBy1 p s (f . k)
        PsepBy p s k           -> PsepBy p s (f . k)
        PsepBy1 p s k          -> PsepBy1 p s (f . k)
        PsepEndBy p s k        -> PsepEndBy p s (f . k)
        PsepEndBy1 p s k       -> PsepEndBy1 p s (f . k)

        Pidentifier t k        -> Pidentifier t (f . k)
        Preserved t s r        -> Preserved t s (f r)
        Poperator t k          -> Poperator t (f . k)
        PreservedOp t s r      -> PreservedOp t s (f r)
        PcharLiteral t k       -> PcharLiteral t (f . k)
        PstringLiteral t k     -> PstringLiteral t (f . k)
        Pnatural t k           -> Pnatural t (f . k)
        Pinteger t k           -> Pinteger t (f . k)
        Pfloat t k             -> Pfloat t (f . k)
        PnaturalOrFloat t k    -> PnaturalOrFloat t (f . k)
        Pdecimal t k           -> Pdecimal t (f . k)
        Phexadecimal t k       -> Phexadecimal t (f . k)
        Poctal t k             -> Poctal t (f . k)
        Psymbol t s k          -> Psymbol t s (f . k)
        Plexeme p k            -> Plexeme p (f . k)
        PwhiteSpace t r        -> PwhiteSpace t (f r)
        Pparens p k            -> Pparens p (f . k)
        Pbraces p k            -> Pbraces p (f . k)
        Pangles p k            -> Pangles p (f . k)
        Pbrackets p k          -> Pbrackets p (f . k)
        Psquares p k           -> Psquares p (f . k)
        Psemi t k              -> Psemi t (f . k)
        Pcomma t k             -> Pcomma t (f . k)
        Pcolon t k             -> Pcolon t (f . k)
        Pdot t k               -> Pdot t (f . k)
        PsemiSep p k           -> PsemiSep p (f . k)
        PsemiSep1 p k          -> PsemiSep1 p (f . k)
        PcommaSep p k          -> PcommaSep p (f . k)
        PcommaSep1 p k         -> PcommaSep1 p (f . k)

instance Show (ParsecF s u m r) where
    show = \case
        Plifted _ _            -> "lifted"
        Preturn _              -> "return"
        Pbind _                -> "bind"
        Peffect _ _            -> "effect"
        Pquiet _ _             -> "quiet"

        PgetState _            -> "getState"
        PputState _ _          -> "putState"
        PmodifyState _ _       -> "modifyState"
        PgetPosition _         -> "getPosition"
        PsetPosition _ _       -> "setPosition"
        PgetInput _            -> "getInput"
        PsetInput _ _          -> "setInput"
        PgetParserState _      -> "getParserState"
        PsetParserState _ _    -> "setParserState"
        PupdateParserState _ _ -> "updateParserState"

        Ptokens _ _ _ _        -> "tokens"
        PtokenPrimEx _ _ _ _ _ -> "tokenPrim"

        PalphaNum _            -> "alphaNum"
        PanyChar _             -> "anyChar"
        PanyToken _            -> "anyToken"
        Pchar x _              -> "char " ++ show x
        Pcrlf _                -> "crlf"
        Pdigit _               -> "digit"
        PendOfLine _           -> "endOfLine"
        Peof _                 -> "eof"
        PhexDigit _            -> "hexDigit"
        Pletter _              -> "letter"
        Plower _               -> "lower"
        Pnewline _             -> "newline"
        PnoneOf xs _           -> "noneOf " ++ show xs
        PoctDigit _            -> "octDigit"
        PoneOf xs _            -> "oneOf " ++ show xs
        PparserFail s          -> "parserFail " ++ show s
        PparserZero            -> "parserZero"
        Psatisfy _ _           -> "satisfy"
        Pspace _               -> "space"
        Pspaces _              -> "spaces"
        Pstring x _            -> "string " ++ show x
        Ptab _                 -> "tab"
        Pupper _               -> "upper"
        Punexpected s          -> "unexpected " ++ show s

        PparserPlus _ _ _      -> "parserPlus"
        Plabel _ a _           -> "label " ++ show a
        Plabels _ a _          -> "labels " ++ show a
        Ptry _ _               -> "try"
        Pchainl _ _ _ _        -> "chainl"
        Pchainl1 _ _ _         -> "chainl1"
        Pchainr _ _ _ _        -> "chainr"
        Pchainr1 _ _ _         -> "chainr1"
        Pchoice _ _            -> "choice"
        Pcount n _ _           -> "count " ++ show n
        PlookAhead _ _         -> "lookAhead"
        Pmany _ _              -> "many"
        Pmany1 _ _             -> "many1"
        PmanyAccum _ _ _       -> "manyAccum"
        PnotFollowedBy _ _     -> "notFollowedBy"
        Poption _ _ _          -> "option"
        PoptionMaybe _ _       -> "optionMaybe"
        Poptional _ _          -> "optional"
        PskipMany _ _          -> "skipMany"
        PskipMany1 _ _         -> "skipMany1"
        PmanyTill _ _ _        -> "manyTill"
        Pbetween _ _ _ _       -> "between"
        PendBy _ _ _           -> "endBy"
        PendBy1 _ _ _          -> "endBy1"
        PsepBy _ _ _           -> "sepBy"
        PsepBy1 _ _ _          -> "sepBy1"
        PsepEndBy _ _ _        -> "sepEndBy"
        PsepEndBy1 _ _ _       -> "sepEndBy1"

        Pidentifier _ _        -> "identifier"
        Preserved _ s _        -> "reserved " ++ show s
        Poperator _ _          -> "operator"
        PreservedOp _ s _      -> "reservedOp " ++ show s
        PcharLiteral _ _       -> "charLiteral"
        PstringLiteral _ _     -> "stringLiteral"
        Pnatural _ _           -> "natural"
        Pinteger _ _           -> "integer"
        Pfloat _ _             -> "float"
        PnaturalOrFloat _ _    -> "naturalOrFloat"
        Pdecimal _ _           -> "decimal"
        Phexadecimal _ _       -> "hexadecimal"
        Poctal _ _             -> "octal"
        Psymbol _ s _          -> "symbol " ++ show s
        Plexeme _ _            -> "lexeme"
        PwhiteSpace _ _        -> "whiteSpace"
        Pparens _ _            -> "parens"
        Pbraces _ _            -> "braces"
        Pangles _ _            -> "angles"
        Pbrackets _ _          -> "brackets"
        Psquares _ _           -> "squares"
        Psemi _ _              -> "semi"
        Pcomma _ _             -> "comma"
        Pcolon _ _             -> "colon"
        Pdot _ _               -> "dot"
        PsemiSep _ _           -> "semiSep"
        PsemiSep1 _ _          -> "semiSep1"
        PcommaSep _ _          -> "commaSep"
        PcommaSep1 _ _         -> "commaSep1"

liftF' :: ParsecF s u m a -> ParsecDSL s u m a
liftF' x = ParsecDSL $ Free (fmap pure x)

lifted :: P.ParsecT s u m a -> ParsecDSL s u m a
lifted p = liftF' $ Plifted p id

quiet :: ParsecDSL s u m a -> ParsecDSL s u m a
quiet p = liftF' $ Pquiet p id

getState :: ParsecDSL s u m u
getState = liftF' $ PgetState id

putState :: u -> ParsecDSL s u m ()
putState u = liftF' $ PputState u ()

modifyState :: (u -> u) -> ParsecDSL s u m ()
modifyState g = liftF' $ PmodifyState g ()

getPosition :: ParsecDSL s u m P.SourcePos
getPosition = liftF' $ PgetPosition id

setPosition :: P.SourcePos -> ParsecDSL s u m ()
setPosition p = liftF' $ PsetPosition p ()

getInput :: ParsecDSL s u m s
getInput = liftF' $ PgetInput id

setInput :: s -> ParsecDSL s u m ()
setInput s = liftF' $ PsetInput s ()

getParserState :: ParsecDSL s u m (P.State s u)
getParserState = liftF' $ PgetParserState id

setParserState :: P.State s u -> ParsecDSL s u m (P.State s u)
setParserState s = liftF' $ PsetParserState s id

updateParserState :: (P.State s u -> P.State s u)
                  -> ParsecDSL s u m (P.State s u)
updateParserState g = liftF' $ PupdateParserState g id

tokens :: (P.Stream s m t, Eq t)
       => ([t] -> String)      -- Pretty print a list of tokens
       -> (P.SourcePos -> [t] -> P.SourcePos)
       -> [t]                  -- List of tokens to parse
       -> ParsecDSL s u m [t]
tokens a b c = liftF' $ Ptokens a b c id

tokenPrimEx :: (P.Stream s m t)
            => (t -> String)
            -> (P.SourcePos -> t -> s -> P.SourcePos)
            -> Maybe (P.SourcePos -> t -> s -> u -> u)
            -> (t -> Maybe a)
            -> ParsecDSL s u m a
tokenPrimEx a b c d = liftF' $ PtokenPrimEx a b c d id

alphaNum :: P.Stream s m Char => ParsecDSL s u m Char
alphaNum = liftF' $ PalphaNum id

anyChar :: P.Stream s m Char => ParsecDSL s u m Char
anyChar = liftF' $ PanyChar id

anyToken :: (Show t, P.Stream s m t) => ParsecDSL s u m t
anyToken = liftF' $ PanyToken id

char :: P.Stream s m Char => Char -> ParsecDSL s u m Char
char x = liftF' $ Pchar x x

crlf :: P.Stream s m Char => ParsecDSL s u m Char
crlf = liftF' $ Pcrlf id

digit :: P.Stream s m Char => ParsecDSL s u m Char
digit = liftF' $ Pdigit id

endOfLine :: P.Stream s m Char => ParsecDSL s u m Char
endOfLine = liftF' $ PendOfLine id

eof :: (Show t, P.Stream s m t) => ParsecDSL s u m ()
eof = liftF' $ Peof ()

hexDigit :: P.Stream s m Char => ParsecDSL s u m Char
hexDigit = liftF' $ PhexDigit id

letter :: P.Stream s m Char => ParsecDSL s u m Char
letter = liftF' $ Pletter id

lower :: P.Stream s m Char => ParsecDSL s u m Char
lower = liftF' $ Plower id

newline :: P.Stream s m Char => ParsecDSL s u m Char
newline = liftF' $ Pnewline id

noneOf :: P.Stream s m Char => [Char] -> ParsecDSL s u m Char
noneOf xs = liftF' $ PnoneOf xs id

octDigit :: P.Stream s m Char => ParsecDSL s u m Char
octDigit = liftF' $ PoctDigit id

oneOf :: P.Stream s m Char => [Char] -> ParsecDSL s u m Char
oneOf xs = liftF' $ PoneOf xs id

parserFail :: String -> ParsecDSL s u m a
parserFail s = liftF' $ PparserFail s

parserZero :: ParsecDSL s u m a
parserZero = liftF' PparserZero

satisfy :: P.Stream s m Char => (Char -> Bool) -> ParsecDSL s u m Char
satisfy g = liftF' $ Psatisfy g id

space :: P.Stream s m Char => ParsecDSL s u m Char
space = liftF' $ Pspace id

spaces :: P.Stream s m Char => ParsecDSL s u m ()
spaces = liftF' $ Pspaces ()

string :: P.Stream s m Char => String -> ParsecDSL s u m String
string x = liftF' $ Pstring x x

tab :: P.Stream s m Char => ParsecDSL s u m Char
tab = liftF' $ Ptab id

upper :: P.Stream s m Char => ParsecDSL s u m Char
upper = liftF' $ Pupper id

unexpected :: String -> ParsecDSL s u m a
unexpected s = liftF' $ Punexpected s

parserPlus :: ParsecDSL s u m a -> ParsecDSL s u m a -> ParsecDSL s u m a
parserPlus p q = liftF' $ PparserPlus p q id

label :: ParsecDSL s u m a -> String -> ParsecDSL s u m a
label p a = liftF' $ Plabel p a id

labels :: ParsecDSL s u m a -> [String] -> ParsecDSL s u m a
labels p a = liftF' $ Plabels p a id

try :: ParsecDSL s u m a -> ParsecDSL s u m a
try p = liftF' $ Ptry p id

chainl :: ParsecDSL s u m a -> ParsecDSL s u m (a -> a -> a) -> a -> ParsecDSL s u m a
chainl p q a = liftF' $ Pchainl p q a id

chainl1 :: ParsecDSL s u m a -> ParsecDSL s u m (a -> a -> a) -> ParsecDSL s u m a
chainl1 p q = liftF' $ Pchainl1 p q id

chainr :: ParsecDSL s u m a -> ParsecDSL s u m (a -> a -> a) -> a -> ParsecDSL s u m a
chainr p q a = liftF' $ Pchainr p q a id

chainr1 :: ParsecDSL s u m a -> ParsecDSL s u m (a -> a -> a) -> ParsecDSL s u m a
chainr1 p q = liftF' $ Pchainr1 p q id

choice :: [ParsecDSL s u m a] -> ParsecDSL s u m a
choice xs = liftF' $ Pchoice xs id

count :: Int -> ParsecDSL s u m a -> ParsecDSL s u m [a]
count n p = liftF' $ Pcount n p id

lookAhead :: ParsecDSL s u m a -> ParsecDSL s u m a
lookAhead p = liftF' $ PlookAhead p id

many :: ParsecDSL s u m a -> ParsecDSL s u m [a]
many p = liftF' $ Pmany p id

many1 :: ParsecDSL s u m a -> ParsecDSL s u m [a]
many1 p = liftF' $ Pmany1 p id

manyAccum :: (a -> [a] -> [a]) -> ParsecDSL s u m a -> ParsecDSL s u m [a]
manyAccum acc p = liftF' $ PmanyAccum acc p id

notFollowedBy :: Show a => ParsecDSL s u m a -> ParsecDSL s u m ()
notFollowedBy p = liftF' $ PnotFollowedBy p ()

option :: a -> ParsecDSL s u m a -> ParsecDSL s u m a
option a p = liftF' $ Poption a p id

optionMaybe :: ParsecDSL s u m a -> ParsecDSL s u m (Maybe a)
optionMaybe p = liftF' $ PoptionMaybe p id

optional :: ParsecDSL s u m a -> ParsecDSL s u m ()
optional p = liftF' $ Poptional p ()

skipMany :: ParsecDSL s u m a -> ParsecDSL s u m ()
skipMany p = liftF' $ PskipMany p ()

skipMany1 :: ParsecDSL s u m a -> ParsecDSL s u m ()
skipMany1 p = liftF' $ PskipMany1 p ()

manyTill :: ParsecDSL s u m a -> ParsecDSL s u m end -> ParsecDSL s u m [a]
manyTill p e = liftF' $ PmanyTill p e id

between :: ParsecDSL s u m open -> ParsecDSL s u m close -> ParsecDSL s u m a
        -> ParsecDSL s u m a
between o c p = liftF' $ Pbetween o c p id

endBy :: ParsecDSL s u m a -> ParsecDSL s u m sep -> ParsecDSL s u m [a]
endBy p s = liftF' $ PendBy p s id

endBy1 :: ParsecDSL s u m a -> ParsecDSL s u m sep -> ParsecDSL s u m [a]
endBy1 p s = liftF' $ PendBy1 p s id

sepBy :: ParsecDSL s u m a -> ParsecDSL s u m sep -> ParsecDSL s u m [a]
sepBy p s = liftF' $ PsepBy p s id

sepBy1 :: ParsecDSL s u m a -> ParsecDSL s u m sep -> ParsecDSL s u m [a]
sepBy1 p s = liftF' $ PsepBy1 p s id

sepEndBy :: ParsecDSL s u m a -> ParsecDSL s u m sep -> ParsecDSL s u m [a]
sepEndBy p s = liftF' $ PsepEndBy p s id

sepEndBy1 :: ParsecDSL s u m a -> ParsecDSL s u m sep -> ParsecDSL s u m [a]
sepEndBy1 p s = liftF' $ PsepEndBy1 p s id

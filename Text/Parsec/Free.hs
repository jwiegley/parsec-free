{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Parsec.Free where

import                    Control.Applicative hiding (many)
import                    Control.Monad
import                    Control.Monad.Cont.Class
import                    Control.Monad.Error.Class
import                    Control.Monad.IO.Class
import                    Control.Monad.Reader.Class
import                    Control.Monad.State.Class
import                    Control.Monad.Trans.Class
import                    Control.Monad.Free
import qualified "parsec" Text.Parsec.Pos as P
import qualified "parsec" Text.Parsec.Prim as P

newtype ParsecDSL s u m a = ParsecDSL {
    runParsecDSL :: Free (ParsecF s u m) a
    }
    deriving (Functor, Applicative, Monad, Show)

instance Alternative (ParsecDSL s u m) where
    empty = parserZero
    (<|>) = parserPlus

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
    | forall a. Peffect (m a) (a -> r)

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

{-
    | Pidentifier (String -> r)
    | Preserved String r
    | Poperator (String -> r)
    | PreservedOp String r
    | PcharLiteral (Char -> r)
    | PstringLiteral (String -> r)
    | Pnatural (Integer -> r)
    | Pinteger (Integer -> r)
    | Pfloat (Double -> r)
    | PnaturalOrFloat (Either Integer Double -> r)
    | Pdecimal (Integer -> r)
    | Phexadecimal (Integer -> r)
    | Poctal (Integer -> r)
    | Psymbol String (String -> r)
    | forall a. Plexeme (ParsecDSL s u m a) (a -> r)
    | PwhiteSpace r
    | forall a. Pparens (ParsecDSL s u m a) (a -> r)
    | forall a. Pbraces (ParsecDSL s u m a) (a -> r)
    | forall a. Pangles (ParsecDSL s u m a) (a -> r)
    | forall a. Pbrackets (ParsecDSL s u m a) (a -> r)
    | forall a. Psquares (ParsecDSL s u m a) (a -> r)
    | Psemi (String -> r)
    | Pcomma (String -> r)
    | Pcolon (String -> r)
    | Pdot (String -> r)
    | forall a. PsemiSep (ParsecDSL s u m a) ([a] -> r)
    | forall a. PsemiSep1 (ParsecDSL s u m a) ([a] -> r)
    | forall a. PcommaSep (ParsecDSL s u m a) ([a] -> r)
    | forall a. PcommaSep1 (ParsecDSL s u m a) ([a] -> r)
-}

instance Functor (ParsecF s u m) where
    fmap f (Plifted p k)            = Plifted p (f . k)
    fmap f (Peffect m k)            = Peffect m (f . k)

    fmap f (PgetState k)            = PgetState (f . k)
    fmap f (PputState u r)          = PputState u (f r)
    fmap f (PmodifyState g r)       = PmodifyState g (f r)
    fmap f (PgetPosition k)         = PgetPosition (f . k)
    fmap f (PsetPosition p r)       = PsetPosition p (f r)
    fmap f (PgetInput k)            = PgetInput (f . k)
    fmap f (PsetInput s r)          = PsetInput s (f r)
    fmap f (PgetParserState k)      = PgetParserState (f . k)
    fmap f (PsetParserState s k)    = PsetParserState s (f . k)
    fmap f (PupdateParserState g k) = PupdateParserState g (f . k)

    fmap f (Ptokens a b c k)        = Ptokens a b c (f . k)
    fmap f (PtokenPrimEx a b c d k) = PtokenPrimEx a b c d (f . k)

    fmap f (PalphaNum k)            = PalphaNum (f . k)
    fmap f (PanyChar k)             = PanyChar (f . k)
    fmap f (PanyToken k)            = PanyToken (f . k)
    fmap f (Pchar x r)              = Pchar x (f r)
    fmap f (Pcrlf k)                = Pcrlf (f . k)
    fmap f (Pdigit k)               = Pdigit (f . k)
    fmap f (PendOfLine k)           = PendOfLine (f . k)
    fmap f (Peof r)                 = Peof (f r)
    fmap f (PhexDigit k)            = PhexDigit (f . k)
    fmap f (Pletter k)              = Pletter (f . k)
    fmap f (Plower k)               = Plower (f . k)
    fmap f (Pnewline k)             = Pnewline (f . k)
    fmap f (PnoneOf xs k)           = PnoneOf xs (f . k)
    fmap f (PoctDigit k)            = PoctDigit (f . k)
    fmap f (PoneOf xs k)            = PoneOf xs (f . k)
    fmap _ (PparserFail s)          = PparserFail s
    fmap _ PparserZero              = PparserZero
    fmap f (Psatisfy g k)           = Psatisfy g (f . k)
    fmap f (Pspace k)               = Pspace (f . k)
    fmap f (Pspaces r)              = Pspaces (f r)
    fmap f (Pstring x r)            = Pstring x (f r)
    fmap f (Ptab k)                 = Ptab (f . k)
    fmap f (Pupper k)               = Pupper (f . k)
    fmap _ (Punexpected s)          = Punexpected s

    fmap f (PparserPlus p q k)      = PparserPlus p q (f . k)
    fmap f (Plabel p a k)           = Plabel p a (f . k)
    fmap f (Plabels p a k)          = Plabels p a (f . k)
    fmap f (Ptry p k)               = Ptry p (f . k)
    fmap f (Pchainl p q a k)        = Pchainl p q a (f . k)
    fmap f (Pchainl1 p q k)         = Pchainl1 p q (f . k)
    fmap f (Pchainr p q a k)        = Pchainr p q a (f . k)
    fmap f (Pchainr1 p q k)         = Pchainr1 p q (f . k)
    fmap f (Pchoice xs k)           = Pchoice xs (f . k)
    fmap f (Pcount n p k)           = Pcount n p (f . k)
    fmap f (PlookAhead p k)         = PlookAhead p (f . k)
    fmap f (Pmany p k)              = Pmany p (f . k)
    fmap f (Pmany1 p k)             = Pmany1 p (f . k)
    fmap f (PmanyAccum g p k)       = PmanyAccum g p (f . k)
    fmap f (PnotFollowedBy p r)     = PnotFollowedBy p (f r)
    fmap f (Poption a p k)          = Poption a p (f . k)
    fmap f (PoptionMaybe p k)       = PoptionMaybe p (f . k)
    fmap f (Poptional p r)          = Poptional p (f r)
    fmap f (PskipMany p r)          = PskipMany p (f r)
    fmap f (PskipMany1 p r)         = PskipMany1 p (f r)
    fmap f (PmanyTill p e k)        = PmanyTill p e (f . k)
    fmap f (Pbetween o c p k)       = Pbetween o c p (f . k)
    fmap f (PendBy p s k)           = PendBy p s (f . k)
    fmap f (PendBy1 p s k)          = PendBy1 p s (f . k)
    fmap f (PsepBy p s k)           = PsepBy p s (f . k)
    fmap f (PsepBy1 p s k)          = PsepBy1 p s (f . k)
    fmap f (PsepEndBy p s k)        = PsepEndBy p s (f . k)
    fmap f (PsepEndBy1 p s k)       = PsepEndBy1 p s (f . k)

{-
    fmap f (Pidentifier k)          = Pidentifier (f . k)
    fmap f (Preserved s r)          = Preserved s (f r)
    fmap f (Poperator k)            = Poperator (f . k)
    fmap f (PreservedOp s r)        = PreservedOp s (f r)
    fmap f (PcharLiteral k)         = PcharLiteral (f . k)
    fmap f (PstringLiteral k)       = PstringLiteral (f . k)
    fmap f (Pnatural k)             = Pnatural (f . k)
    fmap f (Pinteger k)             = Pinteger (f . k)
    fmap f (Pfloat k)               = Pfloat (f . k)
    fmap f (PnaturalOrFloat k)      = PnaturalOrFloat (f . k)
    fmap f (Pdecimal k)             = Pdecimal (f . k)
    fmap f (Phexadecimal k)         = Phexadecimal (f . k)
    fmap f (Poctal k)               = Poctal (f . k)
    fmap f (Psymbol s k)            = Psymbol s (f . k)
    fmap f (Plexeme p k)            = Plexeme p (f . k)
    fmap f (PwhiteSpace r)          = PwhiteSpace (f r)
    fmap f (Pparens p k)            = Pparens p (f . k)
    fmap f (Pbraces p k)            = Pbraces p (f . k)
    fmap f (Pangles p k)            = Pangles p (f . k)
    fmap f (Pbrackets p k)          = Pbrackets p (f . k)
    fmap f (Psquares p k)           = Psquares p (f . k)
    fmap f (Psemi k)                = Psemi (f . k)
    fmap f (Pcomma k)               = Pcomma (f . k)
    fmap f (Pcolon k)               = Pcolon (f . k)
    fmap f (Pdot k)                 = Pdot (f . k)
    fmap f (PsemiSep p k)           = PsemiSep p (f . k)
    fmap f (PsemiSep1 p k)          = PsemiSep1 p (f . k)
    fmap f (PcommaSep p k)          = PcommaSep p (f . k)
    fmap f (PcommaSep1 p k)         = PcommaSep1 p (f . k)
-}

instance Show (ParsecF s u m r) where
    show (Plifted _ _)            = "lifted"
    show (Peffect _ _)            = "effect"

    show (PgetState _)            = "getState"
    show (PputState _ _)          = "putState"
    show (PmodifyState _ _)       = "modifyState"
    show (PgetPosition _)         = "getPosition"
    show (PsetPosition _ _)       = "setPosition"
    show (PgetInput _)            = "getInput"
    show (PsetInput _ _)          = "setInput"
    show (PgetParserState _)      = "getParserState"
    show (PsetParserState _ _)    = "setParserState"
    show (PupdateParserState _ _) = "updateParserState"

    show (Ptokens _ _ _ _)        = "tokens"
    show (PtokenPrimEx _ _ _ _ _) = "tokenPrim"

    show (PalphaNum _)            = "alphaNum"
    show (PanyChar _)             = "anyChar"
    show (PanyToken _)            = "anyToken"
    show (Pchar x _)              = "char " ++ show x
    show (Pcrlf _)                = "crlf"
    show (Pdigit _)               = "digit"
    show (PendOfLine _)           = "endOfLine"
    show (Peof _)                 = "eof"
    show (PhexDigit _)            = "hexDigit"
    show (Pletter _)              = "letter"
    show (Plower _)               = "lower"
    show (Pnewline _)             = "newline"
    show (PnoneOf xs _)           = "noneOf " ++ show xs
    show (PoctDigit _)            = "octDigit"
    show (PoneOf xs _)            = "oneOf " ++ show xs
    show (PparserFail s)          = "parserFail " ++ show s
    show PparserZero              = "parserZero"
    show (Psatisfy _ _)           = "satisfy"
    show (Pspace _)               = "space"
    show (Pspaces _)              = "spaces"
    show (Pstring x _)            = "string " ++ show x
    show (Ptab _)                 = "tab"
    show (Pupper _)               = "upper"
    show (Punexpected s)          = "unexpected " ++ show s

    show (PparserPlus _ _ _)      = "parserPlus"
    show (Plabel _ a _)           = "label " ++ show a
    show (Plabels _ a _)          = "labels " ++ show a
    show (Ptry _ _)               = "try"
    show (Pchainl _ _ _ _)        = "chainl"
    show (Pchainl1 _ _ _)         = "chainl1"
    show (Pchainr _ _ _ _)        = "chainr"
    show (Pchainr1 _ _ _)         = "chainr1"
    show (Pchoice _ _)            = "choice"
    show (Pcount n _ _)           = "count " ++ show n
    show (PlookAhead _ _)         = "lookAhead"
    show (Pmany _ _)              = "many"
    show (Pmany1 _ _)             = "many1"
    show (PmanyAccum _ _ _)       = "manyAccum"
    show (PnotFollowedBy _ _)     = "notFollowedBy"
    show (Poption _ _ _)          = "option"
    show (PoptionMaybe _ _)       = "optionMaybe"
    show (Poptional _ _)          = "optional"
    show (PskipMany _ _)          = "skipMany"
    show (PskipMany1 _ _)         = "skipMany1"
    show (PmanyTill _ _ _)        = "manyTill"
    show (Pbetween _ _ _ _)       = "between"
    show (PendBy _ _ _)           = "endBy"
    show (PendBy1 _ _ _)          = "endBy1"
    show (PsepBy _ _ _)           = "sepBy"
    show (PsepBy1 _ _ _)          = "sepBy1"
    show (PsepEndBy _ _ _)        = "sepEndBy"
    show (PsepEndBy1 _ _ _)       = "sepEndBy1"

{-
    show (Pidentifier _)          = "identifier"
    show (Preserved _ _)          = "reserved"
    show (Poperator _)            = "operator"
    show (PreservedOp _ _)        = "reservedOp"
    show (PcharLiteral _)         = "charLiteral"
    show (PstringLiteral _)       = "stringLiteral"
    show (Pnatural _)             = "natural"
    show (Pinteger _)             = "integer"
    show (Pfloat _)               = "float"
    show (PnaturalOrFloat _)      = "naturalOrFloat"
    show (Pdecimal _)             = "decimal"
    show (Phexadecimal _)         = "hexadecimal"
    show (Poctal _)               = "octal"
    show (Psymbol _ _)            = "symbol"
    show (Plexeme _ _)            = "lexeme"
    show (PwhiteSpace _)          = "whiteSpace"
    show (Pparens _ _)            = "parens"
    show (Pbraces _ _)            = "braces"
    show (Pangles _ _)            = "angles"
    show (Pbrackets _ _)          = "brackets"
    show (Psquares _ _)           = "squares"
    show (Psemi _)                = "semi"
    show (Pcomma _)               = "comma"
    show (Pcolon _)               = "colon"
    show (Pdot _)                 = "dot"
    show (PsemiSep _ _)           = "semiSep"
    show (PsemiSep1 _ _)          = "semiSep1"
    show (PcommaSep _ _)          = "commaSep"
    show (PcommaSep1 _ _)         = "commaSep1"
-}

liftF' :: ParsecF s u m a -> ParsecDSL s u m a
liftF' x = ParsecDSL $ Free (fmap pure x)

lifted :: P.ParsecT s u m a -> ParsecDSL s u m a
lifted p = liftF' $ Plifted p id

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

{-
identifier' :: ParsecDSL s u m String
identifier' = liftF' $ Pidentifier id

reserved' :: String -> ParsecDSL s u m ()
reserved' s = liftF' $ Preserved s ()

operator' :: ParsecDSL s u m String
operator' = liftF' $ Poperator id

reservedOp' :: String -> ParsecDSL s u m ()
reservedOp' s = liftF' $ PreservedOp s ()

charLiteral' :: ParsecDSL s u m Char
charLiteral' = liftF' $ PcharLiteral id

stringLiteral' :: ParsecDSL s u m String
stringLiteral' = liftF' $ PstringLiteral id

natural' :: ParsecDSL s u m Integer
natural' = liftF' $ Pnatural id

integer' :: ParsecDSL s u m Integer
integer' = liftF' $ Pinteger id

float' :: ParsecDSL s u m Double
float' = liftF' $ Pfloat id

naturalOrFloat' :: ParsecDSL s u m (Either Integer Double)
naturalOrFloat' = liftF' $ PnaturalOrFloat id

decimal' :: ParsecDSL s u m Integer
decimal' = liftF' $ Pdecimal id

hexadecimal' :: ParsecDSL s u m Integer
hexadecimal' = liftF' $ Phexadecimal id

octal' :: ParsecDSL s u m Integer
octal' = liftF' $ Poctal id

symbol' :: String -> ParsecDSL s u m String
symbol' s = liftF' $ Psymbol s id

lexeme' :: ParsecDSL s u m a -> ParsecDSL s u m a
lexeme' p = liftF' $ Plexeme p id

whiteSpace' :: ParsecDSL s u m ()
whiteSpace' = liftF' $ PwhiteSpace ()

parens' :: ParsecDSL s u m a -> ParsecDSL s u m a
parens' p = liftF' $ Pparens p id

braces' :: ParsecDSL s u m a -> ParsecDSL s u m a
braces' p = liftF' $ Pbraces p id

angles' :: ParsecDSL s u m a -> ParsecDSL s u m a
angles' p = liftF' $ Pangles p id

brackets' :: ParsecDSL s u m a -> ParsecDSL s u m a
brackets' p = liftF' $ Pbrackets p id

squares' :: ParsecDSL s u m a -> ParsecDSL s u m a
squares' p = liftF' $ Psquares p id

semi' :: ParsecDSL s u m String
semi' = liftF' $ Psemi id

comma' :: ParsecDSL s u m String
comma' = liftF' $ Pcomma id

colon' :: ParsecDSL s u m String
colon' = liftF' $ Pcolon id

dot' :: ParsecDSL s u m String
dot' = liftF' $ Pdot id

semiSep' :: ParsecDSL s u m a -> ParsecDSL s u m [a]
semiSep' p = liftF' $ PsemiSep p id

semiSep1' :: ParsecDSL s u m a -> ParsecDSL s u m [a]
semiSep1' p = liftF' $ PsemiSep1 p id

commaSep' :: ParsecDSL s u m a -> ParsecDSL s u m [a]
commaSep' p = liftF' $ PcommaSep p id

commaSep1' :: ParsecDSL s u m a -> ParsecDSL s u m [a]
commaSep1' p = liftF' $ PcommaSep1 p id
-}

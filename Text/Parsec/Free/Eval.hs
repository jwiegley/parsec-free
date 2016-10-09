{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Parsec.Free.Eval where

import                    Control.Monad.Free
import                    Control.Monad.Trans.Class
import qualified "parsec" Text.Parsec.Char as P
import qualified "parsec" Text.Parsec.Combinator as P
import                    Text.Parsec.Free
import qualified "parsec" Text.Parsec.Prim as P

eval :: forall s u m t a. P.Stream s m t
     => (forall u' b c. ParsecF s u' m c -> P.ParsecT s u m b -> P.ParsecT s u m b)
     -> (forall b. P.ParsecT s u m b -> P.ParsecT s u m b)
     -> ParsecDSL s u m a -> P.ParsecT s u m a
eval fpre fnd = go
  where
    go :: forall b. ParsecDSL s u m b -> P.ParsecT s u m b
    go = iterM (fpre <*> phi) . runParsecDSL

    phi :: forall b. ParsecF s u m (P.ParsecT s u m b) -> P.ParsecT s u m b
    phi (Plifted p k)            = p >>= k
    phi (Peffect m k)            = lift m >>= k

    phi (PgetState k)            = P.getState >>= k
    phi (PputState u k)          = P.putState u >> k
    phi (PmodifyState g k)       = P.modifyState g >> k
    phi (PgetPosition k)         = P.getPosition >>= k
    phi (PsetPosition p k)       = P.setPosition p >> k
    phi (PgetInput k)            = P.getInput >>= k
    phi (PsetInput s k)          = P.setInput s >> k
    phi (PgetParserState k)      = P.getParserState >>= k
    phi (PsetParserState s k)    = P.setParserState s >>= k
    phi (PupdateParserState g k) = P.updateParserState g >>= k

    phi (Ptokens a b c k)        = P.tokens a b c >>= k
    phi (PtokenPrimEx a b c d k) = P.tokenPrimEx a b c d >>= k

    phi (PalphaNum k)            = P.alphaNum >>= k
    phi (PanyChar k)             = P.anyChar >>= k
    phi (PanyToken k)            = P.anyToken >>= k
    phi (Pchar c k)              = P.char c >> k
    phi (Pcrlf k)                = P.crlf >>= k
    phi (Pdigit k)               = P.digit >>= k
    phi (PendOfLine k)           = P.endOfLine >>= k
    phi (Peof k)                 = P.eof >> k
    phi (PhexDigit k)            = P.hexDigit >>= k
    phi (Pletter k)              = P.letter >>= k
    phi (Plower k)               = P.lower >>= k
    phi (Pnewline k)             = P.newline >>= k
    phi (PnoneOf xs k)           = P.noneOf xs >>= k
    phi (PoctDigit k)            = P.octDigit >>= k
    phi (PoneOf xs k)            = P.oneOf xs >>= k
    phi (PparserFail s)          = P.parserFail s
    phi PparserZero              = P.parserZero
    phi (Psatisfy g k)           = P.satisfy g >>= k
    phi (Pspace k)               = P.space >>= k
    phi (Pspaces k)              = P.spaces >> k
    phi (Pstring s k)            = P.string s >> k
    phi (Ptab k)                 = P.tab >>= k
    phi (Pupper k)               = P.upper >>= k
    phi (Punexpected s)          = P.unexpected s

    phi (PparserPlus p q k)      = fnd (fnd (go p) P.<|> go q) >>= k
    phi (Plabel p a k)           = P.label (go p) a >>= k
    phi (Plabels p a k)          = P.labels (go p) a >>= k
    phi (Ptry p k)               = fnd (P.try $ go p) >>= k
    phi (Pchainl p q a k)        = P.chainl (go p) (go q) a >>= k
    phi (Pchainl1 p q k)         = P.chainl1 (go p) (go q) >>= k
    phi (Pchainr p q a k)        = P.chainr (go p) (go q) a >>= k
    phi (Pchainr1 p q k)         = P.chainr1 (go p) (go q) >>= k
    phi (Pchoice xs k)           = P.choice (map go xs) >>= k
    phi (Pcount n p k)           = P.count n (go p) >>= k
    phi (PlookAhead p k)         = P.lookAhead (go p) >>= k
    phi (Pmany p k)              = fnd (P.many (go p)) >>= k
    phi (Pmany1 p k)             = fnd (P.many1 (go p)) >>= k
    phi (PmanyAccum acc p k)     = fnd (P.manyAccum acc (go p)) >>= k
    phi (PnotFollowedBy p k)     = fnd (P.notFollowedBy (go p)) >> k
    phi (Poption a p k)          = fnd (P.option a (go p)) >>= k
    phi (PoptionMaybe p k)       = fnd (P.optionMaybe (go p)) >>= k
    phi (Poptional p k)          = fnd (P.optional (go p)) >> k
    phi (PskipMany p k)          = fnd (P.skipMany (go p)) >> k
    phi (PskipMany1 p k)         = fnd (P.skipMany1 (go p)) >> k
    phi (PmanyTill p e k)        = fnd (P.manyTill (go p) (go e)) >>= k
    phi (Pbetween o c p k)       = fnd (P.between (go o) (go c) (go p)) >>= k
    phi (PendBy p s k)           = fnd (P.endBy (go p) (go s)) >>= k
    phi (PendBy1 p s k)          = fnd (P.endBy1 (go p) (go s)) >>= k
    phi (PsepBy p s k)           = fnd (P.sepBy (go p) (go s)) >>= k
    phi (PsepBy1 p s k)          = fnd (P.sepBy1 (go p) (go s)) >>= k
    phi (PsepEndBy p s k)        = fnd (P.sepEndBy (go p) (go s)) >>= k
    phi (PsepEndBy1 p s k)       = fnd (P.sepEndBy1 (go p) (go s)) >>= k

{-
    phi (Pidentifier k)          = P.identifier lexr >>= k
    phi (Preserved s k)          = P.reserved lexr s >> k
    phi (Poperator k)            = P.operator lexr >>= k
    phi (PreservedOp s k)        = P.reservedOp lexr s >> k
    phi (PcharLiteral k)         = P.charLiteral lexr >>= k
    phi (PstringLiteral k)       = P.stringLiteral lexr >>= k
    phi (Pnatural k)             = P.natural lexr >>= k
    phi (Pinteger k)             = P.integer lexr >>= k
    phi (Pfloat k)               = P.float lexr >>= k
    phi (PnaturalOrFloat k)      = P.naturalOrFloat lexr >>= k
    phi (Pdecimal k)             = P.decimal lexr >>= k
    phi (Phexadecimal k)         = P.hexadecimal lexr >>= k
    phi (Poctal k)               = P.octal lexr >>= k
    phi (Psymbol s k)            = P.symbol lexr s >>= k
    phi (Plexeme p k)            = P.lexeme lexr (go p) >>= k
    phi (PwhiteSpace k)          = P.whiteSpace lexr >> k
    phi (Pparens p k)            = P.parens lexr (go p) >>= k
    phi (Pbraces p k)            = P.braces lexr (go p) >>= k
    phi (Pangles p k)            = P.angles lexr (go p) >>= k
    phi (Pbrackets p k)          = P.brackets lexr (go p) >>= k
    phi (Psquares p k)           = P.squares lexr (go p) >>= k
    phi (Psemi k)                = P.semi lexr >>= k
    phi (Pcomma k)               = P.comma lexr >>= k
    phi (Pcolon k)               = P.colon lexr >>= k
    phi (Pdot k)                 = P.dot lexr >>= k
    phi (PsemiSep p k)           = P.semiSep lexr (go p) >>= k
    phi (PsemiSep1 p k)          = P.semiSep1 lexr (go p) >>= k
    phi (PcommaSep p k)          = P.commaSep lexr (go p) >>= k
    phi (PcommaSep1 p k)         = P.commaSep1 lexr (go p) >>= k
-}

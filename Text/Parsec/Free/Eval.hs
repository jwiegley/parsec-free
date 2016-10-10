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
eval fpre fnd = go True
  where
    go :: forall b. Bool -> ParsecDSL s u m b -> P.ParsecT s u m b
    go b = iterM (if b then fpre <*> phi else phi) . runParsecDSL
      where
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

        phi (PparserPlus p q k)      = fnd (fnd (go b p) P.<|> go b q) >>= k
        phi (Plabel p a k)           = P.label (go b p) a >>= k
        phi (Plabels p a k)          = P.labels (go b p) a >>= k
        phi (Ptry p k)               = fnd (P.try $ go b p) >>= k
        phi (Pchainl p q a k)        = P.chainl (go b p) (go b q) a >>= k
        phi (Pchainl1 p q k)         = P.chainl1 (go b p) (go b q) >>= k
        phi (Pchainr p q a k)        = P.chainr (go b p) (go b q) a >>= k
        phi (Pchainr1 p q k)         = P.chainr1 (go b p) (go b q) >>= k
        phi (Pchoice xs k)           = P.choice (map (go b) xs) >>= k
        phi (Pcount n p k)           = P.count n (go b p) >>= k
        phi (PlookAhead p k)         = P.lookAhead (go b p) >>= k
        phi (Pmany p k)              = fnd (P.many (go b p)) >>= k
        phi (Pmany1 p k)             = fnd (P.many1 (go b p)) >>= k
        phi (PmanyAccum acc p k)     = fnd (P.manyAccum acc (go b p)) >>= k
        phi (PnotFollowedBy p k)     = fnd (P.notFollowedBy (go b p)) >> k
        phi (Poption a p k)          = fnd (P.option a (go b p)) >>= k
        phi (PoptionMaybe p k)       = fnd (P.optionMaybe (go b p)) >>= k
        phi (Poptional p k)          = fnd (P.optional (go b p)) >> k
        phi (PskipMany p k)          = fnd (P.skipMany (go b p)) >> k
        phi (PskipMany1 p k)         = fnd (P.skipMany1 (go b p)) >> k
        phi (PmanyTill p e k)        = fnd (P.manyTill (go b p) (go b e)) >>= k
        phi (Pbetween o c p k)       = fnd (P.between (go b o) (go b c) (go b p)) >>= k
        phi (PendBy p s k)           = fnd (P.endBy (go b p) (go b s)) >>= k
        phi (PendBy1 p s k)          = fnd (P.endBy1 (go b p) (go b s)) >>= k
        phi (PsepBy p s k)           = fnd (P.sepBy (go b p) (go b s)) >>= k
        phi (PsepBy1 p s k)          = fnd (P.sepBy1 (go b p) (go b s)) >>= k
        phi (PsepEndBy p s k)        = fnd (P.sepEndBy (go b p) (go b s)) >>= k
        phi (PsepEndBy1 p s k)       = fnd (P.sepEndBy1 (go b p) (go b s)) >>= k

        phi (Pidentifier d k)        = go False d >>= k
        phi (Preserved d _ k)        = go False d >> k
        phi (Poperator d k)          = go False d >>= k
        phi (PreservedOp d _ k)      = go False d >> k
        phi (PcharLiteral d k)       = go False d >>= k
        phi (PstringLiteral d k)     = go False d >>= k
        phi (Pnatural d k)           = go False d >>= k
        phi (Pinteger d k)           = go False d >>= k
        phi (Pfloat d k)             = go False d >>= k
        phi (PnaturalOrFloat d k)    = go False d >>= k
        phi (Pdecimal d k)           = go False d >>= k
        phi (Phexadecimal d k)       = go False d >>= k
        phi (Poctal d k)             = go False d >>= k
        phi (Psymbol d _ k)          = go False d >>= k
        phi (Plexeme p k)            = go False p >>= k
        phi (PwhiteSpace d k)        = go False d >> k
        phi (Pparens p k)            = fnd (go b p) >>= k
        phi (Pbraces p k)            = fnd (go b p) >>= k
        phi (Pangles p k)            = fnd (go b p) >>= k
        phi (Pbrackets p k)          = fnd (go b p) >>= k
        phi (Psquares p k)           = fnd (go b p) >>= k
        phi (Psemi p k)              = fnd (go b p) >>= k
        phi (Pcomma d k)             = fnd (go b d) >>= k
        phi (Pcolon d k)             = fnd (go b d) >>= k
        phi (Pdot d k)               = fnd (go b d) >>= k
        phi (PsemiSep p k)           = fnd (go b p) >>= k
        phi (PsemiSep1 p k)          = fnd (go b p) >>= k
        phi (PcommaSep p k)          = fnd (go b p) >>= k
        phi (PcommaSep1 p k)         = fnd (go b p) >>= k

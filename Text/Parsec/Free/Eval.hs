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

eval' :: forall s u m t a. P.Stream s m t
      => (forall u' b c. Bool -> ParsecF s u' m c -> P.ParsecT s u m b
              -> P.ParsecT s u m b)
      -> (forall u' b c. Show b => Bool -> ParsecF s u' m c -> P.ParsecT s u m b
              -> P.ParsecT s u m b)
      -> (forall b. P.ParsecT s u m b -> P.ParsecT s u m b)
      -> ParsecDSL s u m a -> P.ParsecT s u m a
eval' h hS ind = go True
  where
    go :: forall x. Bool -> ParsecDSL s u m x -> P.ParsecT s u m x
    go True (ParsecDSL (Pure x)) = h True (Preturn x) (return x)
    go b (ParsecDSL prs) = iterM phi prs
      where
        phi :: forall y. ParsecF s u m (P.ParsecT s u m y) -> P.ParsecT s u m y
        phi z = case z of
            Plifted p k            -> h False z p            >>= k
            Preturn k              -> h b z (return ())      >>  k
            Pbind k                -> h False z (return ())  >>  k
            Peffect m k            -> h b z (lift m)         >>= k
            Pquiet p k             -> h False z (go False p) >>= k

            PgetState k            -> h b z P.getState        >>= k
            PputState u k          -> h b z (P.putState u)    >>  k
            PmodifyState g k       -> h b z (P.modifyState g) >>  k

            PgetPosition k         -> h b z P.getPosition     >>= k
            PsetPosition p k       -> h b z (P.setPosition p) >>  k

            PgetInput k            -> h b z P.getInput     >>= k
            PsetInput s k          -> h b z (P.setInput s) >>  k

            PgetParserState k      -> h b z P.getParserState        >>= k
            PsetParserState s k    -> h b z (P.setParserState s)    >>= k
            PupdateParserState g k -> h b z (P.updateParserState g) >>= k

            Ptokens a e c k        -> h b z (P.tokens a e c)        >>= k
            PtokenPrimEx a e c d k -> h b z (P.tokenPrimEx a e c d) >>= k

            PalphaNum k            -> hS b z P.alphaNum    >>= k
            PanyChar k             -> hS b z P.anyChar     >>= k
            PanyToken k            -> hS b z P.anyToken    >>= k
            Pchar c k              -> h  b z (P.char c)    >>  k
            Pcrlf k                -> hS b z P.crlf        >>= k
            Pdigit k               -> hS b z P.digit       >>= k
            PendOfLine k           -> hS b z P.endOfLine   >>= k
            Peof k                 -> hS b z P.eof         >>  k
            PhexDigit k            -> hS b z P.hexDigit    >>= k
            Pletter k              -> hS b z P.letter      >>= k
            Plower k               -> hS b z P.lower       >>= k
            Pnewline k             -> hS b z P.newline     >>= k
            PnoneOf xs k           -> hS b z (P.noneOf xs) >>= k
            PoctDigit k            -> hS b z P.octDigit    >>= k
            PoneOf xs k            -> hS b z (P.oneOf xs)  >>= k
            Psatisfy g k           -> hS b z (P.satisfy g) >>= k
            Pspace k               -> hS b z P.space       >>= k
            Pspaces k              -> hS b z P.spaces      >>  k
            Pstring s k            -> h  b z (P.string s)  >>  k
            Ptab k                 -> hS b z P.tab         >>= k
            Pupper k               -> hS b z P.upper       >>= k

            PparserFail s          -> h b z (P.parserFail s)
            PparserZero            -> h b z P.parserZero
            Punexpected s          -> h b z (P.unexpected s)

            PparserPlus p q k      -> h b z (ind (P.parserPlus (ind (go b p)) (go b q))) >>= k
            Plabel p a k           -> h b z (ind (P.label (go b p) a))                   >>= k
            Plabels p a k          -> h b z (ind (P.labels (go b p) a))                  >>= k
            Ptry p k               -> h b z (ind (P.try (go b p)))                       >>= k
            Pchainl p q a k        -> h b z (ind (P.chainl (go b p) (go b q) a))         >>= k
            Pchainl1 p q k         -> h b z (ind (P.chainl1 (go b p) (go b q)))          >>= k
            Pchainr p q a k        -> h b z (ind (P.chainr (go b p) (go b q) a))         >>= k
            Pchainr1 p q k         -> h b z (ind (P.chainr1 (go b p) (go b q)))          >>= k
            Pchoice xs k           -> h b z (ind (P.choice (map (go b) xs)))             >>= k
            Pcount n p k           -> h b z (ind (P.count n (go b p)))                   >>= k
            PlookAhead p k         -> h b z (ind (P.lookAhead (go b p)))                 >>= k
            Pmany p k              -> h b z (ind (P.many (go b p)))                      >>= k
            Pmany1 p k             -> h b z (ind (P.many1 (go b p)))                     >>= k
            PmanyAccum acc p k     -> h b z (ind (P.manyAccum acc (go b p)))             >>= k
            PnotFollowedBy p k     -> h b z (ind (P.notFollowedBy (go b p)))             >>  k
            Poption a p k          -> h b z (ind (P.option a (go b p)))                  >>= k
            PoptionMaybe p k       -> h b z (ind (P.optionMaybe (go b p)))               >>= k
            Poptional p k          -> h b z (ind (P.optional (go b p)))                  >>  k
            PskipMany p k          -> h b z (ind (P.skipMany (go b p)))                  >>  k
            PskipMany1 p k         -> h b z (ind (P.skipMany1 (go b p)))                 >>  k
            PmanyTill p e k        -> h b z (ind (P.manyTill (go b p) (go b e)))         >>= k
            Pbetween o c p k       -> h b z (ind (P.between (go b o) (go b c) (go b p))) >>= k
            PendBy p s k           -> h b z (ind (P.endBy (go b p) (go b s)))            >>= k
            PendBy1 p s k          -> h b z (ind (P.endBy1 (go b p) (go b s)))           >>= k
            PsepBy p s k           -> h b z (ind (P.sepBy (go b p) (go b s)))            >>= k
            PsepBy1 p s k          -> h b z (ind (P.sepBy1 (go b p) (go b s)))           >>= k
            PsepEndBy p s k        -> h b z (ind (P.sepEndBy (go b p) (go b s)))         >>= k
            PsepEndBy1 p s k       -> h b z (ind (P.sepEndBy1 (go b p) (go b s)))        >>= k

            Pidentifier d k        -> hS b z (go False d) >>= k
            Preserved d _ k        -> h  b z (go False d) >>  k
            Poperator d k          -> hS b z (go False d) >>= k
            PreservedOp d _ k      -> h  b z (go False d) >>  k
            PcharLiteral d k       -> hS b z (go False d) >>= k
            PstringLiteral d k     -> hS b z (go False d) >>= k
            Pnatural d k           -> hS b z (go False d) >>= k
            Pinteger d k           -> hS b z (go False d) >>= k
            Pfloat d k             -> hS b z (go False d) >>= k
            PnaturalOrFloat d k    -> hS b z (go False d) >>= k
            Pdecimal d k           -> hS b z (go False d) >>= k
            Phexadecimal d k       -> hS b z (go False d) >>= k
            Poctal d k             -> hS b z (go False d) >>= k
            Psymbol d _ k          -> h  b z (go False d) >>= k
            Plexeme d k            -> h  b z (go False d) >>= k
            PwhiteSpace d k        -> hS b z (go False d) >>  k

            Pparens p k            -> h b z (ind (go b p)) >>= k
            Pbraces p k            -> h b z (ind (go b p)) >>= k
            Pangles p k            -> h b z (ind (go b p)) >>= k
            Pbrackets p k          -> h b z (ind (go b p)) >>= k
            Psquares p k           -> h b z (ind (go b p)) >>= k
            Psemi p k              -> h b z (ind (go b p)) >>= k
            Pcomma p k             -> h b z (ind (go b p)) >>= k
            Pcolon p k             -> h b z (ind (go b p)) >>= k
            Pdot p k               -> h b z (ind (go b p)) >>= k
            PsemiSep p k           -> h b z (ind (go b p)) >>= k
            PsemiSep1 p k          -> h b z (ind (go b p)) >>= k
            PcommaSep p k          -> h b z (ind (go b p)) >>= k
            PcommaSep1 p k         -> h b z (ind (go b p)) >>= k

eval :: forall s u m t a. P.Stream s m t => ParsecDSL s u m a -> P.ParsecT s u m a
eval = eval' (const (const id)) (const (const id)) id

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Expr
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-- A helper module to parse \"expressions\".
-- Builds a parser given a table of operators and associativities.
-- 
-----------------------------------------------------------------------------

module Text.Parsec.Expr
    ( Assoc(..), Operator(..), OperatorTable
    , buildExpressionParser
    ) where

import Data.Typeable ( Typeable )

import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Parsec.Free as F
import qualified Text.Parsec.Free.Eval as F
import qualified "parsec" Text.Parsec.Expr as P

-----------------------------------------------------------
-- Assoc and OperatorTable
-----------------------------------------------------------

-- |  This data type specifies the associativity of operators: left, right
-- or none.

data Assoc                = AssocNone
                          | AssocLeft
                          | AssocRight
   deriving ( Typeable )

-- | This data type specifies operators that work on values of type @a@.
-- An operator is either binary infix or unary prefix or postfix. A
-- binary operator has also an associated associativity.

data Operator s u m a   = Infix (ParsecT s u m (a -> a -> a)) Assoc
                        | Prefix (ParsecT s u m (a -> a))
                        | Postfix (ParsecT s u m (a -> a))
-- #if MIN_VERSION_base(4,7,0)
--     deriving ( Typeable )
-- #endif

-- | An @OperatorTable s u m a@ is a list of @Operator s u m a@
-- lists. The list is ordered in descending
-- precedence. All operators in one list have the same precedence (but
-- may have a different associativity).

type OperatorTable s u m a = [[Operator s u m a]]

-----------------------------------------------------------
-- Convert an OperatorTable and basic term parser into
-- a full fledged expression parser
-----------------------------------------------------------

-- | @buildExpressionParser table term@ builds an expression parser for
-- terms @term@ with operators from @table@, taking the associativity
-- and precedence specified in @table@ into account. Prefix and postfix
-- operators of the same precedence can only occur once (i.e. @--2@ is
-- not allowed if @-@ is prefix negate). Prefix and postfix operators
-- of the same precedence associate to the left (i.e. if @++@ is
-- postfix increment, than @-2++@ equals @-1@, not @-3@).
--
-- The @buildExpressionParser@ takes care of all the complexity
-- involved in building expression parser. Here is an example of an
-- expression parser that handles prefix signs, postfix increment and
-- basic arithmetic.
--
-- >  expr    = buildExpressionParser table term
-- >          <?> "expression"
-- >
-- >  term    =  parens expr 
-- >          <|> natural
-- >          <?> "simple expression"
-- >
-- >  table   = [ [prefix "-" negate, prefix "+" id ]
-- >            , [postfix "++" (+1)]
-- >            , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
-- >            , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
-- >            ]
-- >          
-- >  binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
-- >  prefix  name fun       = Prefix (do{ reservedOp name; return fun })
-- >  postfix name fun       = Postfix (do{ reservedOp name; return fun })

buildExpressionParser :: (Show t, Stream s m t)
                      => OperatorTable s u m a
                      -> ParsecT s u m a
                      -> ParsecT s u m a
buildExpressionParser operators simpleExpr
    = flip F.label "buildExpressionParser"
    $ F.quiet
    $ F.lifted
    $ P.buildExpressionParser (map (map f) operators)
                              (F.eval simpleExpr)
  where
    f (Infix p AssocNone)  = P.Infix (F.eval p) P.AssocNone
    f (Infix p AssocLeft)  = P.Infix (F.eval p) P.AssocLeft
    f (Infix p AssocRight) = P.Infix (F.eval p) P.AssocRight
    f (Prefix p)           = P.Prefix (F.eval p)
    f (Postfix p)          = P.Postfix (F.eval p)

{- jww (2016-10-10): For whatever reason, the Free-ized version of
   'buildExpressionParser' fails with ambiguity problems, whereas simply
    lifting the original version does not. This implies to me either an
    associativity, or perhaps an order-of-evaluation problem, in Eval.hs. -}

{-
    = F.quiet $ foldl (makeParser) simpleExpr operators
    where
      makeParser term ops
        = let (rassoc,lassoc,nassoc
               ,prefix,postfix)      = foldr splitOp ([],[],[],[],[]) ops

              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc
              prefixOp   = choice prefix  <?> ""
              postfixOp  = choice postfix <?> ""

              ambigious assoc op= try $
                                  do{ op; fail ("ambiguous use of a " ++ assoc
                                                 ++ " associative operator")
                                    }

              ambigiousRight    = ambigious "right" rassocOp
              ambigiousLeft     = ambigious "left" lassocOp
              ambigiousNon      = ambigious "non" nassocOp

              termP      = do{ pre  <- prefixP
                             ; x    <- term
                             ; post <- postfixP
                             ; return (post (pre x))
                             }

              postfixP   = postfixOp <|> return id

              prefixP    = prefixOp <|> return id

              rassocP x  = do{ f <- rassocOp
                             ; y  <- do{ z <- termP; rassocP1 z }
                             ; return (f x y)
                             }
                           <|> ambigiousLeft
                           <|> ambigiousNon
                           -- <|> return x

              rassocP1 x = rassocP x  <|> return x

              lassocP x  = do{ f <- lassocOp
                             ; y <- termP
                             ; lassocP1 (f x y)
                             }
                           <|> ambigiousRight
                           <|> ambigiousNon
                           -- <|> return x

              lassocP1 x = lassocP x <|> return x

              nassocP x  = do{ f <- nassocOp
                             ; y <- termP
                             ;    ambigiousRight
                              <|> ambigiousLeft
                              <|> ambigiousNon
                              <|> return (f x y)
                             }
                           -- <|> return x

           in  do{ x <- termP
                 ; rassocP x <|> lassocP  x <|> nassocP x <|> return x
                   <?> "operator"
                 }


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)
-}

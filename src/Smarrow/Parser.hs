{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Smarrow.Parser where

import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Functor

import FlatParse.Basic hiding (Parser, char, cut, runParser, string)
import qualified FlatParse.Basic as FP

import Smarrow.Lexer
import Smarrow.Syntax
import Smarrow.Value

--------------------------------------------------------------------------------

runParser :: Parser a -> B.ByteString -> Result Error a
runParser = FP.runParser

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser a -> String -> Either String a
testParser p str = case strToUtf8 str of
  b -> case runParser p b of
    Err e  -> Left $ prettyError b e
    OK a _ -> Right a
    Fail   -> Left "uncaught parse error"

testParserIO :: Show a => Parser a -> String -> IO ()
testParserIO p str = case strToUtf8 str of
  b -> case runParser p b of
    Err e  -> putStrLn $ prettyError b e
    OK a _ -> print a
    Fail   -> putStrLn "uncaught parse error"

--------------------------------------------------------------------------------

-- A lot of this code is stolen from:
-- https://github.com/AndrasKovacs/flatparse/blob/main/src/FlatParse/Examples/BasicLambda/Parser.hs

type Name = B.ByteString

-- | Parse an identifier. This parser uses `isKeyword` to check that an identifier is not a
--   keyword.
ident :: Parser Name
ident = token $ byteStringOf $
  withSpan (identStartChar *> skipMany identChar) (\_ span' -> fails (isKeyword span'))

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser Name
ident' = ident `cut'` (Msg "identifier")

conName :: Parser Name
conName = token $ byteStringOf $
  withSpan (conNameStartChar *> skipMany conNameChar) (\_ span' -> fails (isKeyword span'))

digit :: Parser Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

int :: Parser Int
int = token $ do
  (place, n) <- chainr (\n (!place, !acc) -> (place*10,acc+place*n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

------------------------------------------------------------------------

pSrc' :: Parser Expr
pSrc' = ws *> pExpr' <* eof `cut` [Msg "end of input (lexical error)"]

pExpr' :: Parser Expr
pExpr' = pProc <|> eqLt' `cut` ["expr"]
  where
    pProc :: Parser Expr
    pProc = do
      $(keyword "proc")
      pat <- pPat'
      $(symbol' "->")
      cmd <- pCmd'
      return (Proc pat cmd)

    eqLt' :: Parser Expr
    eqLt' =
      pair >>= \e1 ->
      branch $(symbol "==") (BinOp Eq e1 <$> pair) $
      branch $(symbol "<")  (BinOp Lt e1 <$> pair) $
      pure e1

    pair :: Parser Expr
    pair = pair' <|> add'

    pair' :: Parser Expr
    pair' = do
      $(symbol "(")
      l <- pair
      $(symbol' ",")
      r <- pair
      $(symbol' ")")
      return (PairE l r)

    add' :: Parser Expr
    add' = chainl (BinOp Add) mul' ($(symbol "+") *> mul')

    mul' :: Parser Expr
    mul' = chainl (BinOp Mult) atom' ($(symbol "*") *> atom')

    atom' :: Parser Expr
    atom' = atom
      `cut` [Msg "identifier", "consturctor", Msg "parenthesised expression", Msg "integer literal"]

    atom :: Parser Expr
    atom = pVarE <|> pCon <|> pInt <|> pReturn <|> pUnit <|> pParenExpr
      where
        pVarE :: Parser Expr
        pVarE = VarE <$> pVar

        pCon :: Parser Expr
        pCon = Con . ConName <$> conName

        pInt :: Parser Expr
        pInt = LitE . Int <$> int

        pReturn :: Parser Expr
        pReturn = $(keyword "return") $> ReturnE

        pUnit :: Parser Expr
        pUnit = do
          $(symbol "(")
          $(symbol ")") -- XXX: '?
          return UnitE

        pParenExpr :: Parser Expr
        pParenExpr = $(symbol "(") *> pExpr' <* $(symbol' ")")

pStmt :: Parser Stmt
pStmt = do
  pat <- pPat
  $(symbol' "<-")
  cmd <- pCmd'
  return (pat :<- cmd)

pPat' :: Parser Pat
pPat' = pPat `cut` ["pattern"]

pPat :: Parser Pat
pPat = pVarP <|> pConNameP <|> pTupleP <|> pWildP <|> pUnitP
  where
    pVarP :: Parser Pat
    pVarP = VarP <$> pVar

    pConNameP :: Parser Pat
    pConNameP = ConNameP . ConName <$> conName

    pTupleP :: Parser Pat
    pTupleP = do
      $(symbol "(")
      l <- pPat'
      $(symbol' ",")
      r <- pPat'
      $(symbol' ")")
      return (TupleP l r)

    pWildP :: Parser Pat
    pWildP = $(symbol "_") $> WildP

    pUnitP :: Parser Pat
    pUnitP = $(symbol "()") $> UnitP

pVar :: Parser Var
pVar = Var <$> ident

pCmd' :: Parser Cmd
pCmd' = (pDo <|> pCase <|> pIf <|> pInput') `cut` ["cmd"]
  where
    -- do { stmt; ...; stmt; cmd }
    pDo :: Parser Cmd
    pDo = do
      $(keyword "do")
      $(symbol' "{")
      stmts <- many (pStmt <* $(symbol ";"))
      cmd <- pCmd'
      $(symbol' "}")
      return (Do stmts cmd)

    -- case expr of { pat -> cmd; ...; pat -> cmd }
    pCase = do
      $(keyword "case")
      scrut <- pExpr'
      $(keyword' "of")
      $(symbol' "{")
      alts <- many (pAlt <* $(symbol ";"))
      alt <- pAlt
      $(symbol' "}")
      return (Case scrut (alts ++ [alt]))
      where
        pAlt :: Parser Alt
        pAlt = do
          pat <- pPat
          $(symbol' "->")
          cmd <- pCmd'
          return (Alt pat (UnguardedAlt cmd) [])

    pIf :: Parser Cmd
    pIf = do
      $(keyword "if")
      b <- pExpr'
      $(keyword' "then")
      t <- pCmd'
      $(keyword' "else")
      f <- pCmd'
      return (If b t f)

    pInput' :: Parser Cmd
    pInput' = do
      f <- pExpr'
      $(symbol' "-<")
      a <- pExpr'
      return (f :-< a)

------------------------------------------------------------------------

pValue' :: Parser Value
pValue' = pValue `cut` ["tuple", "unit", "integer", "constructor"]

pValue :: Parser Value
pValue = pPair <|> pUnit <|> pInt <|> pCon
  where
    pPair :: Parser Value
    pPair = do
      $(symbol "(")
      l <- pValue
      $(symbol' ",")
      r <- pValue'
      $(symbol' ")")
      return (PairV l r)

    pUnit :: Parser Value
    pUnit = do
      $(symbol "(")
      $(symbol' ")")
      return UnitV

    pInt :: Parser Value
    pInt = IntV <$> int

    pCon :: Parser Value
    pCon = ConV . ConName <$> conName

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Smarrow.Parser where

import Control.Exception (catch, displayException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Functor (($>))
import System.Exit

import FlatParse.Basic hiding
       (Parser, char, cut, err, runParser, string)
import qualified FlatParse.Basic as FP

import Smarrow.Lexer
import Smarrow.AST

--------------------------------------------------------------------------------

parseFile_ :: FilePath -> IO Machine
parseFile_ fp = do
  r <- parseFile fp
  case r of
    Left err -> do
      putStrLn err
      exitFailure
    Right sm -> return sm

parseFile :: FilePath -> IO (Either String Machine)
parseFile fp = fmap (runParser_ pSrc') (BS.readFile fp)
  `catch` \(err :: IOError) -> return (Left (displayException err))

parseValue_ :: ByteString -> IO Value
parseValue_ bs = do
  case parseValue bs of
    Left err -> do
      putStrLn err
      exitFailure
    Right value -> return value

parseValue :: ByteString -> Either String Value
parseValue = runParser_ pValue'

runParser :: Parser a -> ByteString -> Result Error a
runParser = FP.runParser

runParser_ :: Parser a -> ByteString -> Either String a
runParser_ p bs =
  case runParser p bs of
    Err e  -> Left $ prettyError bs e
    OK a _ -> Right a
    Fail   -> Left "Uncaught parse error"

-- | Run parser, print pretty error on failure.
testParser :: Show a => Parser a -> String -> Either String a
testParser p str = runParser_ p (strToUtf8 str)

testParserIO :: Show a => Parser a -> String -> IO ()
testParserIO p str = case strToUtf8 str of
  b -> case runParser p b of
    Err e  -> putStrLn $ prettyError b e
    OK a _ -> print a
    Fail   -> putStrLn "uncaught parse error"

--------------------------------------------------------------------------------

-- A lot of this code is stolen from:
-- https://github.com/AndrasKovacs/flatparse/blob/main/src/FlatParse/Examples/BasicLambda/Parser.hs

-- Also see: https://www.haskell.org/onlinereport/haskell2010/haskellch10.html

type Name = ByteString

-- | Parse an identifier. This parser uses `isKeyword` to check that an identifier is not a
--   keyword.
ident :: Parser Name
ident = token $ byteStringOf $
  withSpan (identStartChar *> skipMany identChar) (\_ span' -> fails (isKeyword span'))

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser Name
ident' = ident `cut'` (Msg "identifier")

name :: Parser Name
name = token $ byteStringOf $
  withSpan (nameStartChar *> skipMany nameChar) (\_ span' -> fails (isKeyword span'))

digit :: Parser Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

int :: Parser Int
int = token $ do
  (place, n) <- chainr (\n (!place, !acc) -> (place*10,acc+place*n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

------------------------------------------------------------------------

pSrc' :: Parser Machine
pSrc' = ws *> pMachine' <* eof `cut` [Msg "end of input (lexical error)"]

pMachine' :: Parser Machine
pMachine' = do
  $(keyword' "machine")
  smName <- MachineName <$> name
  smRefines <- branch $(keyword "refines") (Just . MachineName <$> name) (pure Nothing)
  $(keyword' "where")
  smState <- pStateDecl'
  smLang <- pLangDecl'
  smFun <- pExpr'
  return (Machine smName smRefines smState smLang smFun)

pStateDecl' :: Parser StateDecl
pStateDecl' = do
  $(keyword' "state")
  $(symbol' ":")
  ty <- pType'
  return (StateDecl ty)

pType' :: Parser Type
pType' = pType `cut` ["type"]

pType :: Parser Type
pType = pDefined <|> pProduct
  where
    pDefined = Defined . TypeName <$> name

    pProduct = pProductG2 pType' pExpr' UnitT PairT RecordT

pLangDecl' :: Parser LangDecl
pLangDecl' = do
  $(keyword' "language")
  $(symbol' ":")
  ops <- many (pOp <* $(symbol "|"))
  lastOp <- pOp
  return (LangDecl (ops ++ [lastOp]))
  where
    pOp :: Parser (Type, Type)
    pOp = do
      a <- pType
      $(symbol' "->")
      b <- pType'
      return (a, b)

pExpr' :: Parser Expr
pExpr' = pFun <|> eqLt' `cut` ["expr"]
  where
    pFun :: Parser Expr
    pFun = do
      $(keyword "function")
      pat <- pPat'
      $(symbol' "->")
      cmd <- pCmd'
      return (Proc pat cmd)

    eqLt' :: Parser Expr
    eqLt' =
      add' >>= \e1 ->
      branch $(symbol "==") (BinOp Eq e1 <$> add') $
      branch $(symbol "<")  (BinOp Lt e1 <$> add') $
      pure e1

    add' :: Parser Expr
    add' = chainl (BinOp Add) mul' ($(symbol "+") *> mul')

    mul' :: Parser Expr
    mul' = chainl (BinOp Mult) proj' ($(symbol "*") *> proj')

    proj' :: Parser Expr
    proj' = chainl ProjectE update' ($(symbol ".") *> (FieldName <$> ident))

    update' :: Parser Expr
    update' = chainl UpdateE atom' ($(symbol "//") *> atom)

    atom' :: Parser Expr
    atom' = atom
      `cut` map Msg ["variable", "consturctor", "integer literal", "return",
                     "get", "put", "record", "unit", "parenthesised expression" ]

    atom :: Parser Expr
    atom = pVarE <|> pCon <|> pInt <|> pReturn <|> pGet <|> pPut <|> pProduct <|> pParenExpr
      where
        pVarE :: Parser Expr
        pVarE = VarE <$> pVar

        pCon :: Parser Expr
        pCon = Con . ConName <$> name

        pInt :: Parser Expr
        pInt = LitE . Int <$> int

        pReturn :: Parser Expr
        pReturn = $(keyword "return") $> ReturnE

        pGet :: Parser Expr
        pGet = $(keyword "get") $> GetE

        pPut :: Parser Expr
        pPut = $(keyword "put") $> PutE

        pProduct :: Parser Expr
        pProduct = pProductG eqLt' UnitE PairE RecordE

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
pPat = pVarP <|> pConNameP <|> pUnitP <|> pTupleP <|> pWildP
  where
    pVarP :: Parser Pat
    pVarP = VarP <$> pVar

    pConNameP :: Parser Pat
    pConNameP = ConNameP . ConName <$> name

    pUnitP :: Parser Pat
    pUnitP = $(symbol "{}") $> UnitP

    pTupleP :: Parser Pat
    pTupleP = do
      $(symbol "{")
      l <- pPat'
      $(symbol' ",")
      r <- pPat'
      $(symbol' "}")
      return (TupleP l r)

    pWildP :: Parser Pat
    pWildP = $(symbol "_") $> WildP


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
          return (Alt pat (UnguardedAlt cmd))

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
pValue = pProduct <|> pInt <|> pCon
  where
    pProduct :: Parser Value
    pProduct = pProductG pValue' UnitV PairV RecordV

    pInt :: Parser Value
    pInt = IntV <$> int

    pCon :: Parser Value
    pCon = ConV . ConName <$> name

pProductG2 :: Parser a -> Parser b
           -> a -> (a -> a -> a) -> ([(FieldName, Maybe Type, Maybe b)] -> a)
           -> Parser a
pProductG2 pa pb unit pair record = pUnit <|> pPair <|> pRecord
  where
    pUnit = $(symbol "{}") $> unit

    pPair = do
      $(symbol "{")
      -- XXX: can we be more strict here?
      l <- try pa
      $(symbol ",")
      r <- pa
      $(symbol' "}")
      return (pair l r)

    pRecord = do
      $(symbol "{")
      es <- many (pEntry <* $(symbol ","))
      e  <- pEntry'
      $(symbol' "}")
      return (record (es ++ [e]))
        where
          pEntry' = pEntry `cut` ["entry"]
          pEntry = do
            field <- ident
            mTypeAnn <- optional ($(symbol ":") *> pType')
            mValue   <- optional ($(symbol "=") *> pb)
            return (FieldName field, mTypeAnn, mValue)

pProductG :: Parser a -> a -> (a -> a -> a) -> ([(FieldName, Maybe Type, Maybe a)] -> a) -> Parser a
pProductG p unit pair record = pProductG2 p p unit pair record

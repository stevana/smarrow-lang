{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Smarrow.Lexer where

import Data.Char (isLower, isUpper)
import FlatParse.Basic hiding (Parser, runParser, string, char, cut)

import qualified FlatParse.Basic as FP
import qualified Data.ByteString as B
import Language.Haskell.TH

import Data.String
import qualified Data.Set as S

--------------------------------------------------------------------------------

-- Mostly stolen from:
-- https://github.com/AndrasKovacs/flatparse/blob/main/src/FlatParse/Examples/BasicLambda/Lexer.hs

-- | An expected item which is displayed in error messages.
data Expected
  = Msg String  -- ^ An error message.
  | Lit String  -- ^ A literal expected thing.
  deriving (Eq, Show, Ord)

instance IsString Expected where fromString = Lit

-- | A parsing error.
data Error
  = Precise Pos Expected     -- ^ A precisely known error, like leaving out "in" from "let".
  | Imprecise Pos [Expected] -- ^ An imprecise error, when we expect a number of different things,
                             --   but parse something else.
  deriving Show

errorPos :: Error -> Pos
errorPos (Precise p _)   = p
errorPos (Imprecise p _) = p

-- | Merge two errors. Inner errors (which were thrown at points with more consumed inputs)
--   are preferred. If errors are thrown at identical input positions, we prefer precise errors
--   to imprecise ones.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
merge :: Error -> Error -> Error
merge e e' = case (errorPos e, errorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (p, _p')         -> case (e, e') of
    (Precise{}      , _               ) -> e
    (_              , Precise{}       ) -> e'
    (Imprecise _ es , Imprecise _ es' ) -> Imprecise p (es ++ es')
{-# noinline merge #-} -- merge is "cold" code, so we shouldn't inline it.

type Parser = FP.Parser Error

-- | Pretty print an error. The `B.ByteString` input is the source file. The offending line from the
--   source is displayed in the output.
prettyError :: B.ByteString -> Error -> String
prettyError b e0 =

  let pos :: Pos
      pos      = case e0 of Imprecise pos' _e -> pos'
                            Precise pos' _e   -> pos'
      ls       = FP.linesUtf8 b
      (l, c)   = head $ FP.posLineCols b [pos]
      line     = if l < length ls then ls !! l else ""
      linum    = show l
      lpad     = map (const ' ') linum

      expected (Lit s) = show s
      expected (Msg s) = s

      err' (Precise _ e)    = expected e
      err' (Imprecise _ es) = imprec $ S.toList $ S.fromList es

      imprec :: [Expected] -> String
      imprec []     = error "impossible"
      imprec [e]    = expected e
      imprec (e:es) = expected e ++ go es where
        go []       = ""
        go [e']     = " or " ++ expected e'
        go (e':es') = ", " ++ expected e' ++ go es'

  in show l ++ ":" ++ show c ++ ":\n" ++
     lpad   ++ "|\n" ++
     linum  ++ "| " ++ line ++ "\n" ++
     lpad   ++ "| " ++ replicate c ' ' ++ "^\n" ++
     "parse error: expected " ++
     err' e0

-- | Imprecise cut: we slap a list of items on inner errors.
cut :: Parser a -> [Expected] -> Parser a
cut p es = do
  pos <- getPos
  FP.cutting p (Imprecise pos es) merge

-- | Precise cut: we propagate at most a single error.
cut' :: Parser a -> Expected -> Parser a
cut' p e = do
  pos <- getPos
  FP.cutting p (Precise pos e) merge

-- | Parse a line comment.
lineComment :: Parser ()
lineComment =
  withOption anyWord8
    (\case 10 -> ws
           _  -> lineComment)
    (pure ())

-- | Parse a potentially nested multiline comment.
multilineComment :: Parser ()
multilineComment = go (1 :: Int) where
  go 0 = ws
  go n = $(switch [| case _ of
    "-}" -> go (n - 1)
    "{-" -> go (n + 1)
    _    -> branch anyWord8 (go n) (pure ()) |])

-- | Consume whitespace.
ws :: Parser ()
ws = $(switch [| case _ of
  " "  -> ws
  "\n" -> ws
  "\t" -> ws
  "\r" -> ws
  "--" -> lineComment
  "{-" -> multilineComment
  _    -> pure () |])

-- | Consume whitespace after running a parser.
token :: Parser a -> Parser a
token p = p <* ws
{-# INLINE token #-}

-- | Read a starting character of an identifier.
identStartChar :: Parser Char
identStartChar = satisfyAscii (\c -> isLatinLetter c && isLower c)
{-# INLINE identStartChar #-}

-- | Read a non-starting character of an identifier.
identChar :: Parser Char
identChar = satisfyAscii (\c -> isLatinLetter c || isDigit c)
{-# INLINE identChar #-}

conNameStartChar :: Parser Char
conNameStartChar = satisfyAscii (\c -> isLatinLetter c && isUpper c)
{-# INLINE conNameStartChar #-}

conNameChar :: Parser Char
conNameChar = satisfyAscii (\c -> isLatinLetter c || isDigit c)
{-# INLINE conNameChar #-}

-- | Check whether a `Span` contains exactly a keyword. Does not change parsing state.
isKeyword :: Span -> Parser ()
isKeyword span0 = inSpan span0 $ do
  $(switch [| case _ of
      "proc"   -> pure ()
      "return" -> pure ()
      -- "let"    -> pure ()
      -- "in"     -> pure () -- XXX: causes: This binding for ‘c’ shadows the existing binding
      "do"     -> pure ()
      "if"     -> pure ()
      "then"   -> pure ()
      "else"   -> pure () |])
  eof

-- | Parse a non-keyword string.
symbol :: String -> Q Exp
symbol str = [| token $(FP.string str) |]

-- | Parser a non-keyword string, throw precise error on failure.
symbol' :: String -> Q Exp
symbol' str = [| $(symbol str) `cut'` Lit str |]

-- | Parse a keyword string.
keyword :: String -> Q Exp
keyword str = [| token ($(FP.string str) `notFollowedBy` identChar) |]

-- | Parse a keyword string, throw precise error on failure.
keyword' :: String -> Q Exp
keyword' str = [| $(keyword str) `cut'` Lit str |]

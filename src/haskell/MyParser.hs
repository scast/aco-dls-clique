{-

    parse-dimacs is free software: it is released under the BSD3 open source
    license.  You can find details of this license in the file LICENSE at the
    root of the source tree.

    Copyright 2008 Denis Bueno
-}

-- | A simple module for parsing CNF files in DIMACS format.
module MyParser
    ( parseByteString
    , parseFile
    , GraphEdges(..)
    , Edge )
    where

import Control.Monad
import Data.Array.Unboxed
import Data.ByteString.Lazy( ByteString , readFile)
import Prelude hiding (readFile, map)
import Text.Parsec( ParseError, SourceName )
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim( many, try, unexpected, runParser )
import qualified Text.Parsec.Token as T


data GraphEdges = GraphEdges
    { numVertex   :: !Int
    -- ^ The number of variables in the problem as reported by the cnf header.

    , numEdges :: !Int
    -- ^ The number of clauses in the problem as reported by the cnf header.

    , edges    :: ![Edge] } deriving Show
type Edge = (Int, Int)

-- | Parse a file containing DIMACS GraphEdges data.
parseFile :: FilePath -> IO (Either ParseError GraphEdges)
parseFile = parseFromFile cnf

-- | Parse a byte string containing DIMACS GraphEdges data.  The source name is only
-- | used in error messages and may be the empty string.
parseByteString :: SourceName -> ByteString -> Either ParseError GraphEdges
parseByteString = runParser cnf ()

-- A DIMACS GraphEdges file contains a header of the form "p cnf <numVars>
-- <numEdges>" and then a bunch of 0-terminated edges.
cnf :: Parser GraphEdges
cnf = uncurry GraphEdges `fmap` cnfHeader `ap` lexeme (many edge)

-- Parses into `(numVars, numEdges)'.
cnfHeader :: Parser (Int, Int)
cnfHeader = do
    whiteSpace
    char 'p' >> many1 space -- Can't use symbol here because it uses
                            -- whiteSpace, which will treat the following
                            -- "cnf" as a comment.
    symbol "edge"
    (,) `fmap` natural `ap` natural

edge :: Parser (Int, Int)
edge = do
  char 'e' >> many1 space
  (,) `fmap` natural `ap` natural


-- token parser
tp = T.makeTokenParser $ T.LanguageDef
   { T.commentStart = ""
   , T.commentEnd = ""
   , T.commentLine = "c"
   , T.nestedComments = False
   , T.identStart = unexpected "ParseDIMACS bug: shouldn't be parsing identifiers..."
   , T.identLetter = unexpected "ParseDIMACS bug: shouldn't be parsing identifiers..."
   , T.opStart = unexpected "ParseDIMACS bug: shouldn't be parsing operators..."
   , T.opLetter = unexpected "ParseDIMACS bug: shouldn't be parsing operators..."
   , T.reservedNames = ["p", "cnf"]
   , T.reservedOpNames = []
   , T.caseSensitive = True
   }

natural :: Parser Int
natural = fromIntegral `fmap` T.natural tp
int :: Parser Int
int = fromIntegral `fmap` T.integer tp
symbol = T.symbol tp
whiteSpace = T.whiteSpace tp
lexeme = T.lexeme tp

module NgLint.Parser where

import NgLint.Position
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

data Decl =
    Comment SourcePos String
    | Block SourcePos String [String] [Decl]
    | Directive SourcePos String [String]
    | IfDecl SourcePos String [Decl]
    deriving (Show)
data Config = Config [Decl] deriving (Show)

instance Position Decl where
    getPos (Comment pos _) = pos
    getPos (Block pos _ _ _) = pos
    getPos (Directive pos _ _) = pos
    getPos (IfDecl pos _ _) = pos

configFile :: Parser Config
configFile = do
    spaces
    lst <- decl `sepBy` spaces
    spaces
    eof
    return $ Config lst

nginxDef = emptyDef
    { P.identStart     = letter <|> char '_'
    , P.identLetter    = alphaNum <|> char '_' 
    , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
    }

lexer = P.makeTokenParser nginxDef

parens      = P.parens lexer
braces      = P.braces lexer
identifier  = P.identifier lexer

decl :: Parser Decl
decl = choice $ map try [comment, ifDecl, directive, block]

arg = many1 (alphaNum <|> oneOf "\"*_-+/.'$[]~\\:^()|=?!")

-- http://stackoverflow.com/questions/34342911/parsec-parse-nested-code-blocks
sepBy1Try :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1Try p sep = do
  x <- p
  xs <- many (try $ sep *> p)
  return (x:xs)

sepByTry p sep = sepBy1Try p sep <|> return []

block :: Parser Decl
block = do
    pos <- getPosition
    name <- identifier
    spaces
    args <- arg `sepByTry` spaces
    spaces
    decls <- braces (decl `sepEndBy` spaces)
    spaces
    return $ Block pos name args decls
    <?> "block"

comment :: Parser Decl
comment = do
    pos <- getPosition
    char '#'
    msg <- manyTill anyChar endOfLine
    return $ Comment pos msg
    <?> "comment"

ifDecl :: Parser Decl
ifDecl = do
    pos <- getPosition
    string "if" <?> "if"
    spaces
    expr <- parens identifier
    spaces
    decls <- braces (decl `sepEndBy` spaces)
    spaces
    return $ IfDecl pos expr decls

directive :: Parser Decl
directive = do
    pos <- getPosition
    name <- identifier
    spaces
    args <- arg `sepEndBy` spaces
    char ';'
    return $ Directive pos name args
    <?> "directive"

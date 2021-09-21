module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import Text.ParserCombinators.Parsec.Token (GenTokenParser(natural))

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = do e <- eassgn
            do reservedOp lis ","
               ESeq e <$> intexp
              <|> return e

eassgn :: Parser (Exp Int)
eassgn = try (do {v <- identifier lis;
                  reservedOp lis "=";
                  EAssgn v <$> eassgn})
             <|> expr

expr :: Parser (Exp Int)
expr = try (do {t <- term;
                do reservedOp lis "+"
                   Plus t <$> intexp
                  <|> do reservedOp lis "-"
                         Minus t <$> intexp
                  <|> return t})
            <|> eassgn

term :: Parser (Exp Int)
term = do f <- factor
          do reservedOp lis "*"
             Times f <$> term
            <|> do reservedOp lis "/"
                   Div f <$> term
            <|> return f

factor :: Parser (Exp Int)
factor = do reservedOp lis "-"
            UMinus <$> factor
           <|> do Const . fromIntegral <$> natural lis
           <|> do parens lis intexp
           <|> do Var <$> identifier lis

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = do b <- bool1
             do reservedOp lis "||"
                Or b <$> boolexp
               <|> return b

bool1 :: Parser (Exp Bool)
bool1 = do b <- bool2
           do reservedOp lis "&&"
              And b <$> bool1
             <|> return b

bool2 :: Parser (Exp Bool)
bool2 = do reservedOp lis "!"
           Not <$> bool3
          <|> do bool3

bool3 :: Parser (Exp Bool)
bool3 = do reserved lis "true"
           return BTrue
          <|> do reserved lis "false"
                 return BFalse
          <|> do parens lis boolexp
          <|> do i <- intexp
                 do reservedOp lis "=="
                    Eq i <$> intexp
                   <|> do reservedOp lis "!="
                          NEq i <$> intexp
                   <|> do reservedOp lis "<"
                          Lt i <$> intexp
                   <|> do reservedOp lis ">"
                          Gt i <$> intexp

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = do c <- comm'
          do reservedOp lis ";"
             Seq c <$> comm
            <|> return c

comm' :: Parser Comm
comm' = do reserved lis "if"
           b <- boolexp
           reservedOp lis "{"
           ct <- comm
           reservedOp lis "}"
           reserved lis "else"
           reservedOp lis "{"
           cf <- comm
           reservedOp lis "}"
           return (IfThenElse b ct cf)
          <|> do reserved lis "repeat"
                 c <- comm
                 reserved lis "until"
                 b <- boolexp
                 reserved lis "end"
                 return (Repeat c b)
          <|> do reserved lis "skip"
                 return Skip
          <|> do v <- identifier lis
                 reservedOp lis "="
                 Let v <$> intexp

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

parsex :: Parser a -> String -> Either ParseError a
parsex p = parse (totParser p) ""

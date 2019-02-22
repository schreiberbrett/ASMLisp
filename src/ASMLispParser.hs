module ASMLispParser (parseProgram) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec hiding (Label, label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Instruction (Instruction (..), Argument (..), Identifier, Parameters)

type Parser = Parsec Void String

parseProgram :: String -> [Instruction]
parseProgram s = case parse program "" s of
    Left e -> error $ show e
    Right instructions -> instructions

program :: Parser [Instruction]
program = between spaceConsumer eof $ many definition

instruction :: Parser Instruction
instruction = parens (definition <|> label <|> call)

definition :: Parser Instruction
definition = Define <$> symbol "define" <> identifier <*> argument

label :: Parser Instruction
label = Label <$> symbol "label" <* identifier

call :: Parser Instruction
call = Call <$> identifier <*> many argument

argument :: Parser Argument
argument = referenced <|> parens lambda <|> immediate

referenced :: Parser Argument
referenced = Referenced <$> identifier

lambda :: Parser Argument
-- lambda = Lambda <$> lambdaKeyword <*> parameters <*> many instruction
lambda = do
    _ <- lambdaKeyword
    ps <- parameters
    inst <- many instruction
    return (Lambda ps inst)


lambdaKeyword :: Parser ()
lambdaKeyword = do
    _ <- (symbol "lambda") <|> (symbol "λ") 
    return ()

parameters :: Parser Parameters
parameters = many identifier

immediate :: Parser Argument
immediate = Immediate <$> int

identifier :: Parser Identifier
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '-')

-- Helper functions
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") (L.skipBlockComment "(;" ";)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

int :: Parser Int
int = lexeme (fmap fromInteger L.decimal)


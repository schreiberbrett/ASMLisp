module ASMLispParser (parseASMLisp) where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Control.Applicative (liftA2)
import Data.Void
import Text.Megaparsec hiding (Label, label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Instruction (Instruction (..), Argument (..), Identifier, Parameters)

type Parser = Parsec Void String

parseASMLisp :: String -> [Instruction]
parseASMLisp str = case (parse program "" str) of
    (Left parseErrorBundle) -> error $ errorBundlePretty parseErrorBundle
    (Right instructions) -> instructions

program :: Parser [Instruction]
program = between spaceConsumer eof $ many instruction

instruction :: Parser Instruction
instruction = parens (definition <|> label <|> call)

definition :: Parser Instruction
definition = do
    symbol "define"
    id <- identifier
    arg <- argument
    return (Define id arg)

label :: Parser Instruction
label = Label <$> symbol "label" <* identifier

call :: Parser Instruction
call = liftA2 Call identifier $ many argument

argument :: Parser Argument
argument = referenced <|> parens lambda <|> immediate

referenced :: Parser Argument
referenced = Referenced <$> identifier

lambda :: Parser Argument
lambda = do
    symbol "lambda" <|> symbol "λ" <|> symbol "\\"
    ps <- parameters
    inst <- many instruction
    return $ Lambda ps inst

parameters :: Parser Parameters
parameters = many identifier

immediate :: Parser Argument
immediate = Immediate <$> int

identifier :: Parser Identifier
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '-')

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

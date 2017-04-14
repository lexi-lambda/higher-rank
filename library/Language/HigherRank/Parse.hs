module Language.HigherRank.Parse (parseExpr) where

import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.String

import Language.HigherRank.Typecheck

identifierP :: Parser String
identifierP = (:) <$> letterChar <*> many alphaNumChar

eunitP :: Parser Expr
eunitP = string "()" $> EUnit <?> "unit"

evarP :: Parser EVar
evarP = MkEVar <$> identifierP <?> "variable"

eannP :: Parser Expr
eannP = EAnn <$> (char '(' *> space *> exprP <* space <* char ':')
             <*> (space *> typeP <* space <* char ')')
             <?> "annotated expression"

elamP :: Parser Expr
elamP = ELam <$> (char '(' *> space *> char '\\' *> space *> evarP)
             <*> (space *> string "->" *> space *> exprP <* space <* char ')')
             <?> "function"

eappP :: Parser Expr
eappP = EApp <$> (char '(' *> space *> exprP)
             <*> (space *> exprP <* space <* char ')')
             <?> "function application"

exprP :: Parser Expr
exprP = (EVar <$> evarP)
    <|> try eunitP
    <|> try eannP
    <|> try elamP
    <|> eappP
    <?> "expression"

tunitP :: Parser Type
tunitP = string "()" $> TUnit <?> "unit"

tvarP :: Parser TVar
tvarP = MkTVar <$> identifierP <?> "type variable"

tarrP :: Parser Type
tarrP = TArr <$> (char '(' *> space *> typeP <* space <* string "->")
             <*> (space *> typeP <* space <* char ')')
             <?> "function type"

tallP :: Parser Type
tallP = TAll <$> (char '(' *> space *> string "forall" *> space *> tvarP <* space <* char '.')
             <*> (space *> typeP <* space <* char ')')
             <?> "forall type"

typeP :: Parser Type
typeP = (TVar <$> tvarP)
    <|> try tunitP
    <|> try tarrP
    <|> try tallP
    <?> "type"

parseExpr :: String -> Either String Expr
parseExpr str = case parse (exprP <* eof) "" str of
  Right expr -> Right expr
  Left err -> Left $ parseErrorPretty err

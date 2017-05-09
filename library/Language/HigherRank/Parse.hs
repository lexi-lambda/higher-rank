module Language.HigherRank.Parse (parseExpr, parseType) where

import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.String

import Language.HigherRank.Types

identifierP :: Parser String
identifierP = (:) <$> letterChar <*> many alphaNumChar

eunitP :: Parser SugaredExpr
eunitP = string "()" $> SEUnit <?> "unit"

etupleP :: Parser SugaredExpr
etupleP = SETuple <$> (char '(' *> space *> exprP <* space <* char ',')
                  <*> (space *> exprP <* space <* char ')')
                  <?> "tuple"

eleftP :: Parser SugaredExpr
eleftP = SELeft <$> (char '(' *> space *> exprP <* space <* string "|)")
                <?> "left sum"

erightP :: Parser SugaredExpr
erightP = SERight <$> (string "(|" *> space *> exprP <* space <* char ')')
                  <?> "right sum"

evarP :: Parser EVar
evarP = MkEVar <$> identifierP <?> "variable"

eannP :: Parser SugaredExpr
eannP = SEAnn <$> (char '(' *> space *> exprP <* space <* char ':')
              <*> (space *> typeP <* space <* char ')')
              <?> "annotated expression"

elamP :: Parser SugaredExpr
elamP = SELam <$> (char '(' *> space *> char '\\' *> space *> evarP)
              <*> (space *> string "->" *> space *> exprP <* space <* char ')')
              <?> "function"

eappP :: Parser SugaredExpr
eappP = SEApp <$> (char '(' *> space *> exprP)
              <*> (space *> exprP <* space <* char ')')
              <?> "function application"

exprP :: Parser SugaredExpr
exprP = (SEVar <$> evarP)
    <|> try eunitP
    <|> try etupleP
    <|> try eleftP
    <|> try erightP
    <|> try eannP
    <|> try elamP
    <|> eappP
    <?> "expression"

tunitP :: Parser Type
tunitP = string "()" $> TUnit <?> "unit"

tproductP :: Parser Type
tproductP = TProduct <$> (char '(' *> space *> typeP <* space <* char ',')
                     <*> (space *> typeP <* space <* char ')')
                     <?> "product"

tsumP :: Parser Type
tsumP = TSum <$> (char '(' *> space *> typeP <* space <* char '|')
             <*> (space *> typeP <* space <* char ')')
             <?> "sum"

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
    <|> try tproductP
    <|> try tsumP
    <|> try tarrP
    <|> try tallP
    <?> "type"

execParser :: Parser a -> String -> Either String a
execParser p str = case parse (p <* eof) "" str of
  Right expr -> Right expr
  Left err -> Left $ parseErrorPretty err

parseExpr :: String -> Either String SugaredExpr
parseExpr = execParser exprP

parseType :: String -> Either String Type
parseType = execParser typeP

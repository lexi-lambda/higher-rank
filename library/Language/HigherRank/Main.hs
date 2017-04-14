module Language.HigherRank.Main (main) where

import System.Console.Haskeline

import Language.HigherRank.Interpret (runInterpret)
import Language.HigherRank.Parse (parseExpr)
import Language.HigherRank.Print (printReducedExpr, printType)
import Language.HigherRank.Typecheck (runInfer)

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

repl :: (String -> String) -> IO ()
repl f = runInputT defaultSettings loop
  where loop = getInputLine "> " >>= \case
          Nothing -> return ()
          Just l -> outputStrLn (f l) >> loop

main :: IO ()
main = repl $ \input -> fromEither $ do
  e <- parseExpr input
  t <- runInfer e
  r <- runInterpret e
  return $ printReducedExpr r ++ " : " ++ printType t

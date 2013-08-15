{-# Language FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics
import Data.List
import Data.Maybe
import Language.CalDims.Action
import Language.CalDims.Misc
import Language.CalDims.Types
import Text.Pandoc hiding (Writer)
import Text.ParserCombinators.Parsec (runParser)
import qualified Language.CalDims.State as S

evalCodeBlock :: Block -> Block
evalCodeBlock (CodeBlock attrs s) = CodeBlock attrs (concat . intersperse "\n" . execWriter . evalCodeBlock' S.start $ s)
  where
    evalCodeBlock' state = foldM evalCodeLine state . lines
evalCodeBlock block = block

evalCodeLine state line = let (state', line') = case runParser parseLine state "" line of
                                                  Left err -> (state, line)
                                                  Right Nothing -> (state, line)
                                                  Right (Just cmd) -> case run state cmd of
                                                                        (Left err, _) -> (state, line)
                                                                        (Right res, state') -> case res of
                                                                                                 Ok Nothing -> (state', line)
                                                                                                 _          -> (state', line ++ " => " ++ pretty res)
                          in do tell [line']
                                return state'

readDoc :: String -> Pandoc
readDoc = readMarkdown def

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

transformDoc :: Pandoc -> Pandoc
transformDoc = bottomUp evalCodeBlock

transformDocM :: Pandoc -> Pandoc
transformDocM = flip evalState S.start . everywhereM (mkM evalCodeBlockM)

evalCodeBlockM :: MonadState S.State m => Block -> m Block
evalCodeBlockM (CodeBlock attrs s) = do state <- get
                                        let (state', codelines') = runWriter . foldM evalCodeLine state . lines $ s
                                            s' = concat . intersperse "\n" $ codelines'
                                        put state'
                                        return (CodeBlock attrs s')
evalCodeBlockM block = return block

main = interact (writeDoc . transformDocM . readDoc)

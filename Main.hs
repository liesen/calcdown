module Main where

import Control.Monad
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Language.CalDims.Action
import Language.CalDims.Misc
import Language.CalDims.State
import Language.CalDims.Types
import Text.Pandoc hiding (Writer)
import Text.ParserCombinators.Parsec (runParser)

evalCodeBlock :: Block -> Block
evalCodeBlock (CodeBlock attrs s) = CodeBlock attrs (concat . intersperse "\n" . execWriter . evalCodeBlock' start $ s)
  where
    evalCodeBlock' state = foldM evalCodeLine state . lines
    evalCodeLine state line = let (state', line') = case runParser parseLine state "" line of
                                                      Left err -> (state, line)
                                                      Right Nothing -> (state, line)
                                                      Right (Just cmd) -> case run state cmd of
                                                                            (Left err, _) -> (state, line)
                                                                            (Right res, state') -> case res of
                                                                                                     Ok Nothing -> (state', line)
                                                                                                     _          -> (state', pretty res)
                              in do tell [line']
                                    return state'
evalCodeBlock block = block

readDoc :: String -> Pandoc
readDoc = readMarkdown def

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

transformDoc :: Pandoc -> Pandoc
transformDoc = bottomUp evalCodeBlock

main = interact (writeDoc . transformDoc . readDoc)


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

type HeaderLevel = Int

instance Monoid S.State where
  mempty = S.start
  state1 `mappend` state2 = let scope = getScope state1 `mappend` getScope state2
                                args = getArgs state1 `mappend` getArgs state2
                                argValues = getArgValues state1 `mappend` getArgValues state2
                            in S.State scope args argValues

transformDocM :: Pandoc -> Pandoc
transformDocM = flip evalState (0, [S.start]) . everywhereM (mkM evalCodeBlockM)

evalCodeBlockM :: MonadState (HeaderLevel, [S.State]) m => Block -> m Block
evalCodeBlockM (CodeBlock attrs s) = do (depth, state:states) <- get
                                        let (state', codelines') = runWriter . foldM evalCodeLine state . lines $ s
                                            s' = concat . intersperse "\n" $ codelines'
                                        put (depth, state':states)
                                        return (CodeBlock attrs s')
evalCodeBlockM header@(Header k _ _) = do (depth, state:states) <- get
                                          when (k < depth) $ put (k, drop (depth - k) states)
                                          when (k > depth) $ put (k, replicate (k - depth) state ++ states)
                                          return header
evalCodeBlockM block = return block

main = interact (writeDoc . transformDocM . readDoc)

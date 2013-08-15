{-# Language FlexibleContexts #-}
module Main where

import Control.Monad
import Control.Monad.State
import Data.Generics
import Data.List
import Data.Maybe
import Data.Monoid
import Language.CalDims.Action
import Language.CalDims.Misc
import Language.CalDims.Types
import Text.Pandoc hiding (Writer)
import Text.ParserCombinators.Parsec (runParser)
import qualified Language.CalDims.State as S

readDoc :: String -> Pandoc
readDoc = readMarkdown def

writeDoc :: Pandoc -> String
writeDoc = writeMarkdown def

type HeaderLevel = Int

instance Monoid S.State where
  mempty = S.start
  state1 `mappend` state2 = let scope = getScope state1 `mappend` getScope state2
                                args = getArgs state1 `mappend` getArgs state2
                                argValues = getArgValues state1 `mappend` getArgValues state2
                            in S.State scope args argValues

transformDoc :: Pandoc -> Pandoc
transformDoc = flip evalState (0, [S.start]) . everywhereM (mkM evalCodeBlock)

evalCodeBlock :: MonadState (HeaderLevel, [S.State]) m => Block -> m Block
evalCodeBlock (CodeBlock attrs s) = do (depth, state:states) <- get
                                       let (state', codelines') = mapAccumL evalCodeLine state (lines s)
                                           s' = unlines codelines'
                                       put (depth, state':states)
                                       return (CodeBlock attrs s')
evalCodeBlock header@(Header k _ _) = do (depth, state:states) <- get
                                         when (k < depth) $ put (k, drop (depth - k) states)
                                         when (k > depth) $ put (k, replicate (k - depth) state ++ (state:states))
                                         return header
evalCodeBlock block = return block

evalCodeLine state line = let (state', line') = case runParser parseLine state "" line of
                                                  Left err -> (state, line)
                                                  Right Nothing -> (state, line)
                                                  Right (Just cmd) -> case run state cmd of
                                                                        (Left err, _) -> (state, line)
                                                                        (Right res, state') -> case res of
                                                                                                 Ok Nothing -> (state', line)
                                                                                                 _          -> (state', line ++ " => " ++ pretty res)
                          in do (state', line')

main = interact (writeDoc . transformDoc . readDoc)

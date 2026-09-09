{-# LANGUAGE TypeFamilies #-}

module ApplicativeTRS.TokenStream
  ( TokenStream (..),
    tokenStream,
    bundleErrorPos,
  )
where

import ApplicativeTRS.Lexer (PosToken (..), showToken, tokenWidth)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Lexer
import Text.Megaparsec

data TokenStream = TokenStream
  { tsSource :: String,
    tsTokens :: [PosToken]
  }
  deriving (Eq, Ord, Show)

tokenStream :: String -> [PosToken] -> TokenStream
tokenStream = TokenStream

instance Stream TokenStream where
  type Token TokenStream = PosToken
  type Tokens TokenStream = [PosToken]

  tokenToChunk Proxy t = [t]
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null

  take1_ (TokenStream src ts) = case ts of
    [] -> Nothing
    (t : ts') -> Just (t, TokenStream src ts')

  takeN_ n s@(TokenStream src ts)
    | n <= 0 = Just ([], s)
    | null ts = Nothing
    | otherwise = let (xs, ts') = splitAt n ts in Just (xs, TokenStream src ts')

  takeWhile_ f (TokenStream src ts) =
    let (xs, ts') = span f ts in (xs, TokenStream src ts')

instance VisualStream TokenStream where
  showTokens Proxy = unwords . NE.toList . NE.map (showToken . ptTok)
  tokensLength Proxy = max 1 . sum . NE.map (tokenWidth . ptTok)

instance TraversableStream TokenStream where
  reachOffset o pst =
    ( lineAt (unPos (sourceLine pos)) (tsSource stream),
      pst
        { pstateInput = TokenStream (tsSource stream) after,
          pstateOffset = max (pstateOffset pst) o,
          pstateSourcePos = pos,
          pstateLinePrefix = ""
        }
    )
    where
      stream = pstateInput pst
      (before, after) = splitAt (max 0 (o - pstateOffset pst)) (tsTokens stream)
      name = sourceName (pstateSourcePos pst)
      pos = case after of
        (t : _) -> toSourcePos name (ptPos t)
        [] -> case reverse before of
          (t : _) -> toSourcePos name (afterToken t)
          [] -> pstateSourcePos pst

toSourcePos :: FilePath -> SrcPos -> SourcePos
toSourcePos name (SrcPos l c) = SourcePos name (mkPos (max 1 l)) (mkPos (max 1 c))

afterToken :: PosToken -> SrcPos
afterToken pt = SrcPos l (c + tokenWidth (ptTok pt))
  where
    SrcPos l c = ptPos pt

lineAt :: Int -> String -> Maybe String
lineAt l src = case drop (l - 1) (lines src) of
  (x : _) -> Just x
  [] -> Nothing

bundleErrorPos :: (TraversableStream s) => ParseErrorBundle s e -> SrcPos
bundleErrorPos b = SrcPos (unPos (sourceLine p)) (unPos (sourceColumn p))
  where
    p =
      pstateSourcePos . snd $
        reachOffset (errorOffset (NE.head (bundleErrors b))) (bundlePosState b)

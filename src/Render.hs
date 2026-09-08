{-# LANGUAGE OverloadedStrings #-}

module Render
  ( Ann (..),
    annStyle,
    keyword,
    conName,
    varName,
    funName,
    literal,
    operator,
    punct,
    faint,
    filepath,
    prompt,
    number,
    errorDoc,
    warnDoc,
    infoDoc,
    okDoc,
    ColorMode (..),
    Out (..),
    mkOut,
    hPutDocLn,
    hPutDoc,
    renderPlain,
    renderOneLine,
    parenthesized,
    bracketed,
    commaSep,
    commaSepWrap,
  )
where

import Prettyprinter
import Prettyprinter.Render.String (renderString)
import qualified Prettyprinter.Render.Terminal as Term
import qualified Prettyprinter.Render.Text as Text
import System.Console.ANSI (hSupportsANSIColor)
import qualified System.Console.Terminal.Size as TSize
import System.Environment (lookupEnv)
import System.IO (Handle, hFlush, hPutStrLn)

data Ann
  = AKeyword
  | ACon
  | AVar
  | AFun
  | ALiteral
  | AOperator
  | APunct
  | AFaint
  | APath
  | ANumber
  | APrompt
  | AError
  | AWarn
  | AInfo
  | AOk
  deriving (Eq, Show)

annStyle :: Ann -> Term.AnsiStyle
annStyle a = case a of
  AKeyword -> Term.bold <> Term.color Term.Magenta
  ACon -> Term.color Term.Cyan
  AVar -> mempty
  AFun -> Term.color Term.Blue
  ALiteral -> Term.color Term.Green
  AOperator -> Term.color Term.Yellow
  APunct -> Term.colorDull Term.White
  AFaint -> Term.colorDull Term.White
  APath -> Term.underlined <> Term.color Term.Cyan
  ANumber -> Term.color Term.Yellow
  APrompt -> Term.bold <> Term.color Term.Green
  AError -> Term.bold <> Term.color Term.Red
  AWarn -> Term.bold <> Term.color Term.Yellow
  AInfo -> Term.color Term.Blue
  AOk -> Term.color Term.Green

keyword, conName, varName, funName, literal, operator, punct, faint, filepath, prompt :: Doc Ann -> Doc Ann
keyword = annotate AKeyword
conName = annotate ACon
varName = annotate AVar
funName = annotate AFun
literal = annotate ALiteral
operator = annotate AOperator
punct = annotate APunct
faint = annotate AFaint
filepath = annotate APath
prompt = annotate APrompt

number :: (Show a) => a -> Doc Ann
number = annotate ANumber . pretty . show

errorDoc, warnDoc, infoDoc, okDoc :: Doc Ann -> Doc Ann
errorDoc d = annotate AError "error:" <+> d
warnDoc d = annotate AWarn "warning:" <+> d
infoDoc d = annotate AInfo "info:" <+> d
okDoc d = annotate AOk "ok:" <+> d

data ColorMode = ColorAuto | ColorAlways | ColorNever
  deriving (Eq, Show)

data Out = Out
  { outHandle :: Handle,
    outColor :: Bool,
    outWidth :: Int
  }

mkOut :: ColorMode -> Handle -> IO Out
mkOut mode h = do
  noColor <- lookupEnv "NO_COLOR"
  supported <- hSupportsANSIColor h
  size <- TSize.hSize h

  let colorful = case mode of
        ColorAlways -> True
        ColorNever -> False
        ColorAuto -> supported && maybe True null noColor

  return
    Out
      { outHandle = h,
        outColor = colorful,
        outWidth = maybe 80 (max 40 . TSize.width) size
      }

hPutDoc :: Out -> Doc Ann -> IO ()
hPutDoc o d
  | outColor o = Term.renderIO h (layout o (reAnnotate annStyle d))
  | otherwise = Text.renderIO h (layout o (unAnnotate d))
  where
    h = outHandle o

hPutDocLn :: Out -> Doc Ann -> IO ()
hPutDocLn o d = do
  hPutDoc o d
  hPutStrLn (outHandle o) ""
  hFlush (outHandle o)

parenthesized :: Doc Ann -> Doc Ann
parenthesized d = punct "(" <> d <> punct ")"

bracketed :: Doc Ann -> Doc Ann
bracketed d = punct "[" <> d <> punct "]"

commaSep :: [Doc Ann] -> Doc Ann
commaSep = hsep . punctuate (punct ",")

commaSepWrap :: [Doc Ann] -> Doc Ann
commaSepWrap = align . sep . punctuate (punct ",")

renderOneLine :: Doc Ann -> String
renderOneLine = renderWith Unbounded

renderWith :: PageWidth -> Doc Ann -> String
renderWith pw = renderString . layoutPretty (LayoutOptions pw) . unAnnotate

renderPlain :: Int -> Doc Ann -> String
renderPlain w =
  renderString . layoutPretty (LayoutOptions (AvailablePerLine w 1.0)) . unAnnotate

layout :: Out -> Doc ann -> SimpleDocStream ann
layout o = layoutPretty (LayoutOptions (AvailablePerLine (outWidth o) 1.0))

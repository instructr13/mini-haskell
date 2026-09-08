module Render (hPutDocLn) where

import Prettyprinter
import qualified Prettyprinter.Render.Text as Text
import System.IO

layout :: Doc ann -> SimpleDocStream ann
layout = layoutPretty (LayoutOptions (AvailablePerLine 80 1.0))

hPutDoc :: Handle -> Doc ann -> IO ()
hPutDoc h d = Text.renderIO h (layout (unAnnotate d))

hPutDocLn :: Handle -> Doc ann -> IO ()
hPutDocLn h d = do
  hPutDoc h d
  hPutStrLn h ""
  hFlush h

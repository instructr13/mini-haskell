{-# LANGUAGE TemplateHaskell #-}

module ApplicativeTRS.Builtin (preludePath, preludeSrc) where

import Data.FileEmbed (embedStringFile)

preludePath :: FilePath
preludePath = "<prelude>"

preludeSrc :: String
preludeSrc = $(embedStringFile "lib/prelude.trst")

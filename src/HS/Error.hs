module HS.Error (SrcPos (..), CompileError (..)) where

import HS.Name

data SrcPos = SrcPos {spLine :: !Int, spCol :: !Int}
  deriving (Eq, Ord, Show)

data CompileError
  = SyntaxError SrcPos String
  | UnknownConstructor ConName
  | UnknownVariable VarName
  | ArityError String Int Int
  | OverlappingClauses VarName
  | DuplicateDefinition VarName
  | FixityConflict String String
  | UnknownKnownName String
  | SymbolCollision String
  | UnsupportedFeature String
  deriving (Show)

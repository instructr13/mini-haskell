module HS.Error (SrcPos (..), Feature (..), CompileError (..), featureName) where

import HS.Name

data SrcPos = SrcPos {spLine :: !Int, spCol :: !Int}
  deriving (Eq, Ord, Show)

data Feature
  = FGuards
  | FWhereClause
  | FLet
  | FLambda
  | FIf
  | FCase
  | FSection
  | FAsPattern
  | FHigherOrder
  | FNumericLiteral
  | FCharLiteral
  | FStringLiteral
  | FNegativeLiteral
  | FTypeClass
  | FModuleHeader
  deriving (Eq, Show)

featureName :: Feature -> String
featureName f = case f of
  FGuards -> "guards"
  FWhereClause -> "where clause"
  FLet -> "let"
  FLambda -> "lambda"
  FIf -> "if"
  FCase -> "case"
  FSection -> "operator section"
  FAsPattern -> "as pattern (x@p)"
  FHigherOrder -> "higher-order application"
  FNumericLiteral -> "numeric literal"
  FCharLiteral -> "character literal"
  FStringLiteral -> "string literal"
  FNegativeLiteral -> "negative integer literal"
  FTypeClass -> "type class"
  FModuleHeader -> "module header"

data CompileError
  = SyntaxError SrcPos String
  | UnknownConstructor ConName
  | UnknownVariable VarName
  | ArityError String Int Int
  | ClauseArityMismatch VarName Int Int
  | OverlappingClauses VarName
  | DuplicateDefinition VarName
  | FixityConflict String String
  | FixityLevelOutOfRange String Int
  | MissingKnownName KnownName String Int
  | KnownNameMismatch KnownName String Int Int
  | NoLiteralRepresentation NumericRep String
  | SymbolCollision String
  | Unsupported Feature
  | InternalError String
  deriving (Eq, Show)

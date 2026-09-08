module HS.Layout
  ( layout,
    layoutTrace,
    LayoutEvent (..),
    layoutTriggers,
  )
where

import HS.Lexer

data LayoutEvent
  = OpenImplicit SrcPos Int
  | CloseImplicit SrcPos Int
  | InsertSemi SrcPos Int
  | OpenExplicit SrcPos
  | CloseExplicit SrcPos
  | EmptyBlock SrcPos
  | CloseForIn SrcPos
  | Underflow SrcPos Int
  deriving (Eq, Show)

-- The keywords after which an implicit block may open.
layoutTriggers :: [String]
layoutTriggers = ["where", "let", "do", "of"]

-- An implicit context records the keyword that opened it, because (L3) may
-- only close one that a 'let' opened -- otherwise `let { .. } in` closes the
-- enclosing module block instead.
data Opener = ByLet | ByOther
  deriving (Eq, Show)

data Context = Implicit !Int !Opener | Explicit
  deriving (Eq, Show)

layout :: [PosToken] -> [PosToken]
layout = fst . layoutTrace

layoutTrace :: [PosToken] -> ([PosToken], [LayoutEvent])
layoutTrace [] = ([], [])
layoutTrace (t0 : ts)
  -- (L0) the module body is an implicit block unless it is written out.
  | isOpenBrace t0 = start [Explicit] (t0 : ts) [OpenExplicit (ptPos t0)]
  | otherwise =
      let n = col t0
       in prepend
            (virtual t0 TVLBrace)
            (go [Implicit n ByOther] (line t0) (t0 : ts))
            [OpenImplicit (ptPos t0) n]
  where
    start ctx rest evs = prepend' (go ctx 0 rest) evs
    prepend x (out, evs) evs0 = (x : out, evs0 ++ evs)
    prepend' (out, evs) evs0 = (out, evs0 ++ evs)

    go :: [Context] -> Int -> [PosToken] -> ([PosToken], [LayoutEvent])
    -- (L4) close every implicit context still open at the end of input.
    go ctx _ [] = (closers ctx, [CloseImplicit eofPos n | Implicit n _ <- ctx])
      where
        eofPos = SrcPos (maxBound `div` 2) 1
        closers cs = [PosToken eofPos TVRBrace | Implicit _ _ <- cs]
    go ctx prevLine (t : rest)
      -- (L2) a token that starts a line is measured against the top context.
      -- m < n closes the context and re-applies; if no enclosing context can
      -- accept the column, the source is mis-indented and the event says so.
      | line t /= prevLine,
        Implicit n _ : outer <- ctx,
        col t < n =
          emit
            (virtual t TVRBrace)
            (go outer prevLine (t : rest))
            (CloseImplicit (ptPos t) n : underflow outer)
      | line t /= prevLine,
        Implicit n _ : _ <- ctx,
        col t == n =
          emit (virtual t TVSemi) (continue ctx (t : rest)) [InsertSemi (ptPos t) n]
      | otherwise = continue ctx (t : rest)
      where
        underflow outer
          | null [() | Implicit m _ <- outer, col t >= m] = [Underflow (ptPos t) (col t)]
          | otherwise = []

    -- Past (L2): the token is emitted, then the rules that look at what the
    -- token *is* apply.
    continue ctx (t : rest)
      | isOpenBrace t = emit t (go (Explicit : ctx) (line t) rest) [OpenExplicit (ptPos t)]
      | isCloseBrace t = case ctx of
          Explicit : outer -> emit t (go outer (line t) rest) [CloseExplicit (ptPos t)]
          _ -> emit t (go ctx (line t) rest) []
      -- (L3) `in` closes one implicit block. The Report gets this from its
      -- parse-error rule; checking for Implicit keeps `let { .. } in` right.
      | isKeyword "in" t,
        Implicit n ByLet : outer <- ctx =
          emit (virtual t TVRBrace) (emit t (go outer (line t) rest) []) [CloseForIn (ptPos t), CloseImplicit (ptPos t) n]
      | Just o <- triggerOf t = openBlock o ctx t rest
      | otherwise = emit t (go ctx (line t) rest) []
    continue ctx [] = go ctx 0 []

    -- (L1) after a trigger keyword, open an implicit block at the next
    -- token's column, unless the block is written out explicitly.
    -- (L1') if that column is not further right than the enclosing context,
    -- the block is empty: emit "{ }" and carry on.
    openBlock o ctx t rest = case rest of
      [] ->
        emit
          t
          (emit (virtual t TVLBrace) (emit (virtual t TVRBrace) (go ctx (line t) []) []) [])
          [EmptyBlock (ptPos t)]
      (u : _)
        | isOpenBrace u -> emit t (go ctx (line t) rest) []
        | enclosing ctx >= col u ->
            emit
              t
              (emit (virtual u TVLBrace) (emit (virtual u TVRBrace) (go ctx (line t) rest) []) [])
              [EmptyBlock (ptPos u)]
        -- The token whose column opened the block must not then be measured
        -- against it, or a spurious ';' lands right after the '{'.
        | otherwise ->
            emit
              t
              (emit (virtual u TVLBrace) (go (Implicit (col u) o : ctx) (line u) rest) [])
              [OpenImplicit (ptPos u) (col u)]

    emit x (out, evs) evs0 = (x : out, evs0 ++ evs)

    enclosing cs = case [n | Implicit n _ <- cs] of
      (n : _) -> n
      [] -> 0

col :: PosToken -> Int
col = spCol . ptPos

line :: PosToken -> Int
line = spLine . ptPos

-- An inserted token carries the position of the token that caused it.
virtual :: PosToken -> Token -> PosToken
virtual t tok = PosToken (ptPos t) tok

isOpenBrace :: PosToken -> Bool
isOpenBrace t = ptTok t == TSpecial '{'

isCloseBrace :: PosToken -> Bool
isCloseBrace t = ptTok t == TSpecial '}'

isKeyword :: String -> PosToken -> Bool
isKeyword k t = ptTok t == TKeyword k

triggerOf :: PosToken -> Maybe Opener
triggerOf t = case ptTok t of
  TKeyword "let" -> Just ByLet
  TKeyword k | k `elem` layoutTriggers -> Just ByOther
  _ -> Nothing

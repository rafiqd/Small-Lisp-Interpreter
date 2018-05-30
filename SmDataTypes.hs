module SmDataTypes where

-- Parsing Functions & Types
data SExpression = NumAtom Int | SymAtom [Char] | List [SExpression] deriving (Show, Eq)

type Identifier = [Char]

data SmLispExpr = SExpr SExpression |
                  Variable Identifier |
                  FnCall Identifier [SmLispExpr] |
                  CondExpr [CondClause] |
                  LetExpr [LocalDef] SmLispExpr deriving (Show, Eq)

type CondClause = (SmLispExpr, SmLispExpr)
type LocalDef = (Identifier, SmLispExpr)

data Definition = ConstantDef [Comment] Identifier SmLispExpr |
                  FunctionDef [Comment] Identifier [Identifier] SmLispExpr deriving (Show, Eq)

type SmLispProgram = [Definition]

type Comment = [Char]

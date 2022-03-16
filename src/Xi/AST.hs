module Xi.AST where

import Data.Text

newtype Ident = Ident Text
              deriving (Show, Eq)

data PrimType = IntType
              | BoolType
              deriving (Show, Eq)

data Type = Prim PrimType
          | Array PrimType Int
          deriving (Show, Eq)

data Literal = IntLiteral Int
             | BoolLiteral Bool
             | ArrayLiteral [Literal]
             deriving (Show, Eq)

data Argument = Argument Ident Type
              deriving (Show, Eq)

data Function = Function
              { name :: Ident
              , args :: [Argument]
              , ret  :: [Type]
              , body :: Statement
              }
              deriving (Show, Eq)

data IntBinOp = Add
              | Sub
              | Mul
              | Div
              | Mod
              | HighMul
              deriving (Show, Eq)

data ComparisionOp = Lt
                   | Gt
                   | Leq
                   | Geq
                   | Eq
                   | Neq
                   deriving (Show, Eq)

data BoolBinOp = And
               | Or
               deriving (Show, Eq)

data IntUnOp = Neg
             deriving (Show, Eq)

data BoolUnOp = Not
              deriving (Show, Eq)

data BinOp = IntBinOp IntBinOp
           | BoolBinOp BoolBinOp
           | ComparisionOp ComparisionOp
           deriving (Show, Eq)

data UnOp = IntUnOp IntUnOp
          | BoolUnOp BoolUnOp
          deriving (Show, Eq)

data Expression = BinOp BinOp Expression Expression
                | UnOp UnOp Expression
                | FunctionCall Expression [Expression]
                | Literal Literal
                | Variable Ident
                | Indexing Expression Expression
                deriving (Show, Eq)

data Statement = Assignment [Maybe Expression] Expression
               | Block [Statement]
               | Expression Expression
               | If Expression Statement (Maybe Statement)
               | While Expression Statement
               | Return [Expression]
               | VarDecl [Argument] (Maybe Expression)
               | ArrayDecl Ident PrimType [Maybe Expression]
               deriving (Show, Eq)

newtype Use = Use Ident
            deriving (Show, Eq)

data GlobalVar = GlobalVar Ident PrimType (Maybe Expression)
               | GlobalArr Ident PrimType Int
               deriving (Show, Eq)

data Global = Using Use
            | GlobalDecl GlobalVar
            | FuncDecl Function
            deriving (Show, Eq)

newtype Program = Program [Global]
                deriving (Show, Eq)

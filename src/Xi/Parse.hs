{-# LANGUAGE OverloadedStrings #-}

module Xi.Parse (program) where

import           Control.Monad.Combinators
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Xi.AST
import qualified Data.Text                       as T
import qualified Text.Megaparsec.Char.Lexer      as L

-- | the type of the parser
type Parser = Parsec Void T.Text

-- | this section is for parsing basic stuff like
--   spaces, types and literals

-- | various symbols
symbol    = L.symbol scn
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
brackets  = between (symbol "[") (symbol "]")
quotes    = between (symbol "'") (symbol "'")
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
assign    = symbol "="

-- | reserved keywords
keyword :: T.Text -> Parser ()
keyword w = lexeme . try $ string w *> notFollowedBy (alphaNumChar <|> char '_')
if_       = keyword "if"
else_     = keyword "else"
return_   = keyword "return"
while     = keyword "while"
true      = keyword "true"
false     = keyword "false"
int       = keyword "int"
bool      = keyword "bool"
use       = keyword "use"

-- | reserved keywords
reserved = Ident <$> ["if", "else", "return", "while", "true", "false", "int", "bool", "use"]

-- | parse line comments
lineComment = L.skipLineComment "//"
blockComment = L.skipBlockComment "/*" "*/"

-- | consume spaces
scn = L.space space1 lineComment blockComment

-- | convert a parser into a lexeme
lexeme = L.lexeme scn

-- | parse an identifier
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
ident_ = lexeme $ Ident . T.pack <$> identifier

-- | filter the reserved keywords
notReserved id = if id `elem` reserved
                 then fail "using a reserved keyword"
                 else return id

-- | filter that an identifier is not reserved
ident = notReserved =<< ident_

-- | parse an integer
integer = L.signed scn $ lexeme L.decimal

-- | parse a boolean
boolean = True <$ true <|> False <$ false

-- | parse a char
char_ = lexeme $ ord <$> quotes L.charLiteral

-- | parse a string
string_ = lexeme $ (ord <$>) <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- | parse a comma seperated list of literals
array = braces $ sepEndBy literal comma

-- | parse a literal
literal :: Parser Literal
literal = IntLiteral                      <$> integer
      <|> IntLiteral                      <$> char_
      <|> BoolLiteral                     <$> boolean
      <|> ArrayLiteral                    <$> array
      <|> ArrayLiteral . (IntLiteral <$>) <$> string_

-- | parse a primitive type
primType = lexeme $ (IntType <$ int) <|> (BoolType <$ bool)

-- | parse a type
getTyp typ [] = Prim typ
getTyp typ xs = Array typ $ length xs
type_ = lexeme $ getTyp <$> primType <*> many (brackets scn)

-- | parse an argument to a function
argument = Argument <$> ident
                    <*> (colon *> type_)

-- | this section is for parsing expressions in
--   decreasing order of preference

-- | utility for parsing call and indexing
indexOrCallExpr = foldr1 (flip (.)) <$> some (singleIndex <|> singleCall)

-- | parse a single call expression
singleCall = flip FunctionCall <$> parens (sepEndBy expression comma)

-- | parse a single indexing
singleIndex = flip Indexing <$> brackets expression

-- | the leaf term in an expression
term = parens expression
   <|> try (Literal   <$> literal)
   <|>      ArrayExpr <$> braces (sepEndBy expression comma)
   <|>      Variable  <$> ident

-- | operator table
table = [ [ Postfix   indexOrCallExpr                               ]
        , [ Prefix  $ UnOp  (IntUnOp       Neg    ) <$ symbol "-"
          , Prefix  $ UnOp  (BoolUnOp      Not    ) <$ symbol "!"   ]
        , [ InfixL  $ BinOp (IntBinOp      HighMul) <$ symbol "*>>"
          , InfixL  $ BinOp (IntBinOp      Div    ) <$ symbol "/"
          , InfixL  $ BinOp (IntBinOp      Mod    ) <$ symbol "%"
          , InfixL  $ BinOp (IntBinOp      Mul    ) <$ symbol "*"   ]
        , [ InfixL  $ BinOp (IntBinOp      Add    ) <$ symbol "+"
          , InfixL  $ BinOp (IntBinOp      Sub    ) <$ symbol "-"   ]
        , [ InfixL  $ BinOp (ComparisionOp Leq    ) <$ symbol "<="
          , InfixL  $ BinOp (ComparisionOp Geq    ) <$ symbol ">="
          , InfixL  $ BinOp (ComparisionOp Lt     ) <$ symbol "<"
          , InfixL  $ BinOp (ComparisionOp Gt     ) <$ symbol ">"   ]
        , [ InfixL  $ BinOp (ComparisionOp Eq     ) <$ symbol "=="
          , InfixL  $ BinOp (ComparisionOp Neq    ) <$ symbol "!="  ]
        , [ InfixL  $ BinOp (BoolBinOp     And    ) <$ symbol "&"   ]
        , [ InfixL  $ BinOp (BoolBinOp     Or     ) <$ symbol "|"   ]
        ]

-- | parse an expression
expression = makeExprParser term table

-- | this section is for parsing statements

-- | parse a variable declaration
varDecl = VarDecl <$> sepBy1 argument comma
                  <*> optional (assign *> expression)

-- | parse an array declaration
arrayDecl = ArrayDecl <$> ident
                      <*> (colon *> primType)
                      <*> some (brackets $ optional expression)

-- | parse a single lvalue
lvalue = Nothing <$  symbol "_"
     <|> Just    <$> expression

-- | parse an assignment
assignment = Assignment <$> sepBy1 lvalue comma
                        <*> (assign *> expression)

-- | parse a block
block =  Block <$> braces (many statement)

-- | parse a procedure call
exprStmt = Expression <$> expression

-- | parse an if statement
elseStmt = else_ *> statement
ifStmt = If <$> (if_ *> expression)
            <*> statement
            <*> optional elseStmt

-- | parse a while statement
whileStmt = While <$> (while *> expression)
                  <*> statement

-- | parse a return statement
retStmt = Return <$> (return_ *> sepBy expression comma)

-- | parse all the statements
statements = ifStmt
         <|> whileStmt
         <|> retStmt
         <|> block
         <|> try varDecl
         <|> try arrayDecl
         <|> try assignment
         <|> exprStmt

-- | parse a statement optionally ened by a semi-colon
statement = statements <* optional semicolon

-- | this section is for parsing global declarations
--   like functions, global variables, and use

-- | parse a use function
use_ = use *> (Use <$> ident) <* optional semicolon

-- | parse a global variable declaration
globalVarDecl = GlobalVar <$> ident
                          <*> (colon *> primType)
                          <*> optional (assign *> literal)

-- | parse a global array declaration
globalArr x y z = GlobalArr x y $ length z
globalArrDecl = globalArr <$> ident
                          <*> (colon *> primType)
                          <*> some (brackets scn)

-- | parse a global declaration
globalDecl = (try globalArrDecl <|> globalVarDecl) <* optional semicolon

-- | parse a function declaration
function x y = Function x y . fromMaybe []
functionDecl = function <$> ident
                        <*> parens (sepBy argument comma)
                        <*> optional (colon *> sepBy1 type_ comma)
                        <*> statement

-- | parse a global declaration
global =      Using      <$> use_
     <|> try (GlobalDecl <$> globalDecl)
     <|>      FuncDecl   <$> functionDecl

-- | parse the program
program = scn *> (Program <$> many global) <* eof

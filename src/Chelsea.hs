module Chelsea where

import Text.Parsec.Char
import Text.Parsec.String

data Program = Program String [ProgramLine]
  deriving (Eq, Show)
  
data ProgramLine = ProgramLine Int String Statement
  deriving (Eq, Show)

data Statement = StatementLet ChVar ChExpr
               | StatementRead [ChVar]
               | StatementData [ChNum]
               | StatementPrint [PrintElement]
               | StatementGoto Int
               | StatementIf ChExpr ChRel ChExpr Int
               | StatementFor ChUVar ChExpr ChExpr (Maybe ChExpr)
               | StatementNext ChUVar
               | StatementEnd 
               | StatementStop
               | StatementDef Char ChUVar ChExpr
               | StatementGosub Int
               | StatementReturn
               | StatementDim Char Int (Maybe Int)
               | StatementRem
               | StatementSyntaxError
  deriving (Eq, Show)
               
data PrintElement = PrintLabel String
                  | PrintExpr ChExpr
                  | PrintLabelExpr String ChExpr
  deriving (Eq, Show)
                                 
data ChUVar = Var1 Char | Var2 Char Char
  deriving (Eq, Show)

data ChVar = Scalar ChUVar | ListRef Char ChExpr | ArrayRef Char ChExpr ChExpr
  deriving (Eq, Show)

data ChExpr = ExpLit ChNum
            | ExpVar ChVar
            | ExpBinOp ChBinOp ChExpr ChExpr
            | ExpStdFn ChFunc ChExpr
            | ExpUDF Char ChExpr
  deriving (Eq, Show)

data ChNum = Number Float 
  deriving (Eq, Show)
            
data ChBinOp = OpAdd | OpSub | OpMul | OpDiv | OpExp
  deriving (Eq, Show)

data ChFunc = FnSqr | FnSin | FnCos | FnTan | FnAtn | FnExp | FnAbs | FnLog | FnInt | FnRnd
  deriving (Eq, Show)
            
data ChRel = RelLt | RelLe | RelEq | RelNe | RelGt | RelGe
  deriving (Eq, Show)


addLine :: Program -> String -> Program
addLine p l = p'
  where pl = parseLine l
        p' = insertOrReplace p pl
        
insertOrReplace :: Program -> ProgramLine -> Program
insertOrReplace (Program nm p) pl@(ProgramLine ln _ _) = Program nm (before ++ [pl] ++ after)
  where before = filter (\pl' -> lineNumber pl' < ln) p
        after  = filter (\pl' -> lineNumber pl' > ln) p
        lineNumber (ProgramLine ln' _ _) = ln'

parseLine :: String -> ProgramLine
parseLine l = ProgramLine 0 l StatementSyntaxError


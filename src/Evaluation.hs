module Evaluation where

import Data.List
import qualified Data.Map as M
import Prelude hiding (seq)
import Data.Map (Map)

import Syntax
import Parser
import Typing


data Instruction = Assign Assignment | Eval Expr deriving (Show,Eq)
uninstruction (Assign (name,val)) = Lam name val (Just $ Type "assignment")
uninstruction (Eval expr)         = expr

type Binding = (Name,Expr)
type Bindings = [Binding]
type Bindings' = (Bindings, Map Name Int)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen pred = groupBy (const (not . pred))

binding :: Name -> Bindings -> Maybe Expr
binding c [] = Nothing
binding c ((var,val):binds) = if c == var then Just val else binding c binds

nameOccruances :: Name -> Map Name Int -> Int
nameOccruances = M.findWithDefault 0 

newBinding :: Name -> Bindings' -> Name
newBinding v counts | occur == 0 = v
                    | otherwise  = v ++ show occur
  where occur = nameOccruances v m
        m = snd counts

incrementOccruance :: Name -> Map Name Int -> Map Name Int
incrementOccruance v m = M.insert v ((+1) $ nameOccruances v m) m

builtinType = Type "primfun"

isBuiltin (App l r _) = isBuiltin l
isBuiltin e           = typeof e == builtinType

listType x = Type ("List(" ++ show (typeof x) ++ ")")

startype = Type "*"

seq :: Expr -> Expr
seq (App (Lit "seq" (Just builtinType)) r t) = App (App (Lit "cons" (Just $ Fun startype (listType r))) r (Just $ listType r)) (Lit "nil" (Just startype)) (Just $ listType r)
seq (App l r t)             = App (App (Lit "cons" (Just $ Fun startype (listType r))) r (Just $ listType r)) (seq l) (Just $ listType r)
seq x                       = App (App (Lit "cons" (Just $ Fun startype (listType x))) x (Just $ listType x)) (Lit "nil" (Just startype)) (Just $ listType x)
builtinBindings = [
    ("seq",Lit "seq" (Just builtinType))
  ]
builtin = flip (M.findWithDefault id) builtins
  where builtins = M.fromList [
            ("seq",seq)
          ]
builtinEvaluate :: Name -> Expr -> Expr
builtinEvaluate = builtin

findBuiltinName (Lit v (Just builtinType)) = v
findBuiltinName (App l _ _)                = findBuiltinName l
findBuiltinName (Lam _ b _)                = findBuiltinName b 

eval' :: Expr -> Bindings -> Expr
eval' expr bind = if isBuiltin expr 
                    then flip eval' bind $ builtinEvaluate (findBuiltinName expr) expr
                    else eval'' expr bind
  where eval'' (App f x t) binds = case eval' f binds of
          (Lam var body t) -> eval' body ((var,x):binds)
          other -> App other (eval' x binds) t
        eval'' (Lam var body t) binds =  Lam var (eval' body ((var,Lit var (Just $ domain $ unjust t)):binds)) t
        eval'' (Lit var t) binds = case binding var binds of
          Just (Lit var' t') -> if not $ var == var' then eval' (Lit var' t') binds else (Lit var' t')
          Just other         -> eval' other binds
          Nothing            -> (Lit var t)-- Lit var t

eval = flip eval' []

reduce expr = parse expr >>= typeCheck >>= return . flip eval' []

parseOk (Left _) = False
parseOk (Right _) = True
right (Right x) = x

dropComment []          = []
dropComment ('-':'-':_) = []
dropComment (x:xs)      = x : dropComment xs

removeNewlines = filter (/='\n')
cleanSource = filter (any (/=' ')) . map (filter (/=';')) . splitWhen (==';') . removeNewlines . unlines . map dropComment . lines

parseProgram :: String -> Maybe [Instruction]
parseProgram source = mapM parseLine progLines
  where progLines = (cleanSource source)
        parseLine line | parseOk assignmentParse = Just $ Assign $ right assignmentParse
                       | parseOk expressionPase  = Just $ Eval $ right expressionPase
                       | otherwise = Nothing
          where assignmentParse = parseAssignment line
                expressionPase  = parseExpr line


evaluateInstruction :: Instruction -> Bindings -> (Expr,Bindings)
evaluateInstruction (Assign (var,val)) binds = (expr,(var,expr):binds)
  where expr = eval' val binds
evaluateInstruction (Eval expr) binds = (eval' expr binds,binds)

validateInstruction :: Instruction -> Maybe Instruction
validateInstruction (Assign (var,val)) = typeCheck val >>= return . Assign . (,) var
validateInstruction (Eval expr) = typeCheck expr >>= return . Eval

validateProgram :: [Instruction] -> Maybe [Instruction]
validateProgram = mapM validateInstruction



runProgram :: [Instruction] -> Maybe Expr
runProgram = Just . fst . last . stackTrace

runProgram' [] binds = [(snd $ head $ binds,binds)]
runProgram' (i:[]) binds = [(fst $ evaluateInstruction i binds,binds)]
runProgram' (i:is) binds = (fst res, snd res) : runProgram' is (snd res)
  where res = evaluateInstruction i binds

runProgram'' = fst . last . stackTrace

stackTrace :: [Instruction] -> [(Expr,Bindings)]
stackTrace = flip runProgram' builtinBindings


programAssignments :: [Instruction] -> Bindings
programAssignments = snd . last . stackTrace
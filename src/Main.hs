module Main where

import Syntax
import Parser
import Typing
import Evaluation

import Data.List (intercalate, intersperse)

x = "x"
t = "t"

church :: Int -> String
church 0 = "x"
church 1 = "f x"
church n = "f (" ++ church (n-1) ++ ")"
church' n = "(\\f.\\x." ++ church n ++ ")"

ycom = "(\\f.(\\x.f (x x))(\\x.f (x x)))"

tt = "((\\x.x(\\z.z))(\\y.y))"
tt' = "(\\x.x x)(\\y.y y)"
main :: IO ()
main = prog
main' = do
  content <- readFile "expr.lc"
  let fileLines = cleanSource content
  let parseTrees = map parse fileLines
  let typeChecks = map (>>= typeCheck) parseTrees
  let types = map (>>= return . typeof) typeChecks
  let reductions = map (>>= return . eval) typeChecks
  mapM_ putStrLn $ map show $ zip reductions types

  -- putStrLn $ show $ eval (app (lam x (lit x)) (lit t))
prog :: IO ()
prog = do
  content <- readFile "expr.lc"
  -- putStrLn $ show $ unjust' $ parseProgram content
  let res = unjust' $ (parseProgram content >>= runProgram)
  putStrLn $ showTypeless res
  putStrLn $ show res

cenc n = Lam "f" (Lam "x" (cenc' n) cet) cet
  where cenc' 0 = Lit "x" cet
        cenc' n = App f (cenc' (n-1)) cet
        f = Lit "f" cet
        cet = Just $ Type "CE"
debug :: String -> IO ()
debug file = do
  content <- readFile file
  let source = content
  let cleaned = cleanSource source
  mapM_ putStrLn $ intersperse "\n" $ map show $ zip cleaned (map parse' cleaned)
  putStrLn $ show $ parseProgram content
  let parsed = unjust' $ parseProgram content
  let typeChecked = validateProgram parsed
  let isTypeCorrect = not $ null typeChecked
  let instructions = if isTypeCorrect
                       then unjust' typeChecked 
                       else parsed
  let st  = stackTrace $ unjust' typeChecked
  let assigns = programAssignments $ unjust' typeChecked
  let result = fst $ last st


  putStrLn $ "Debugging " ++ file

  putStrLn $ "Type correctness: " ++ show isTypeCorrect

  putStrLn "-- Program Source --"
  putStrLn source

  putStrLn "-- Program Assignments --"
  putStrLn $ unlines $ map (\p -> fst p ++" | "++ (showTypeless $ snd p) ++ " : " ++ (show $ typeof $ snd p)) assigns

  putStrLn "-- Program Scope --"
  let boundVarFrames = map (map fst . snd) st
  let redexValFrames = map fst st
  let prettyVarFrames = map (intercalate ", ") boundVarFrames
  let prettyValFrames = map showTypeless redexValFrames
  let prettyValTypeFrames = map (show . typeof) redexValFrames
  -- putStrLn $ unlines $ zipWith ((++) . (++" | ")) prettyVarFrames prettyValFrames
  putStrLn $ unlines $ prettyVarFrames
  
  putStrLn "-- Program Stack Frames --"
  putStrLn $ unlines $ zipWith ((++) . (++" | ")) prettyValFrames prettyValTypeFrames

  putStrLn "-- Program Output --"
  putStrLn $ showTypeless result
  putStrLn $ "Result Type: " ++ (show $ typeof result)
  putStrLn ""
  putStrLn $ "Output Length: " ++ (show $ length (showTypeless result))

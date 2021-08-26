module Main where

import Syntax
import Parser
import Typing
import Evaluation
import Serialize

import Data.Graph 
import Data.Graph.DGraph
import Data.Graph.UGraph
import Data.Graph.Types
import Data.Graph.Visualize
import qualified IainVisualize as IV
-- import Data.Graph.Visualize (toDirectedDot)

import Data.Function

import Data.List (intercalate, intersperse)

indexify list = zip [0..length list] list

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
main = makeGraph
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

  let vals = map snd assigns
  -- let connectedList = foldl (++) [] $ map (consConnectedList . uninstruction) instructions
  let connectedLists = map (consConnectedList . uninstruction) instructions
  -- mapM_ (\p -> saveGraphFromEdgeList (snd p) (show $ fst p)) $ indexify connectedLists
  -- mapM_ (putStrLn . showTypeless) $ map uninstruction instructions
  -- mapM_ (putStrLn . show) connectedLists
  return () 


-- https://hackage.haskell.org/package/graphviz-2999.20.1.0/docs/Data-GraphViz-Types-Graph.html#t:DotEdge
saveGraphFromEdgeList :: [(String,String)] -> String -> IO ()
saveGraphFromEdgeList pairs title = do
  let valGraph = insertEdgePairs pairs empty -- (empty :: DGraph String ())
  let graphOutput = "./graphs/" ++ title
  IV.plotDGraphPng valGraph graphOutput
  putStrLn $ IV.printGraph $ IV.toDirectedDot False valGraph
  putStrLn $ "Graph saved to " ++ graphOutput ++ ".png"

exprGraphLabel (Lit v t)   = showTypeless (Lit v t)
exprGraphLabel abs@(Lam v b t) | typeof abs == Type "assignment" = v
                               | otherwise                       = "λ" ++ v ++ " : "++ show (domain $ unjust' t)
exprGraphLabel (App l r t) = "App" -- "(" ++ exprGraphLabel l ++ " " ++ exprGraphLabel r ++ ")"

consConnectedList (Lit v t) = []
consConnectedList (Lam v b t) = (exprGraphLabel (Lam v b t),exprGraphLabel b) : consConnectedList b
consConnectedList (App l r t) = (++) [(selfLabel,exprGraphLabel l),(selfLabel,exprGraphLabel r)] $ on (++) consConnectedList l r
  where selfLabel = exprGraphLabel (App l r t)

foundationUniverse :: DGraph String ()
foundationUniverse = fromArcsList
    [ "Helicon" --> "Nishaya"
    , "Helicon" --> "Wencory"
    , "Nishaya" --> "Wencory"
    , "Wencory" --> "Getorin"
    , "Wencory" --> "Cinna"
    , "Wencory" --> "Lystena"
    , "Lystena" --> "Helicon"
    , "Cinna"   --> "Florina"
    , "Florina" --> "Nishaya"
    , "Florina" --> "Derowd"
    , "Derowd"  --> "Cinna"
    , "Derowd"  --> "Lystena"
    ]
  
makeGraph :: IO ()
makeGraph = do
  plotUGraphPng (toUndirected foundationUniverse) "./foundation"
  return ()
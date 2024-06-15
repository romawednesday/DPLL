import Formula
import DPLL

p, q, r :: Formula
p = Var "P"
q = Var "Q"
r = Var "R"

testCases :: [(String, Formula, Formula)]
testCases = [
    ("Test 1: Single variable entailment", p, p),
    ("Test 2: Negation", Not p, p),
    ("Test 3: Conjunction", And p q, p),
    ("Test 4: Disjunction", Or p q, p),
    ("Test 5: Implication", Impl p q, p),
    ("Test 6: Biconditional", BiCond p q, p),
    ("Test 7: Tautology", Or p (Not p), p),
    ("Test 8: Nested formulas", And (Or p q) (Not r), p),
    ("Test 9: Complex nested formulas", And (Or p (And q r)) (Impl r (Not p)), p)
  ]

main :: IO ()
main = mapM_ runTest testCases

runTest :: (String, Formula, Formula) -> IO ()
runTest (description, knowledge, query) = do
  putStrLn description
  putStrLn ("KnowledgeBase: " ++ show knowledge)
  putStrLn ("Query: " ++ show query)
  let lSAT = checkWithDPLL knowledge query
  putStrLn ("Result: " ++ show lSAT)
  putStrLn ""

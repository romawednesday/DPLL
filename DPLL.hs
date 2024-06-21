module DPLL where
import Formula
import Data.Tuple (swap)
import Data.List (sort, group, nub, (\\), concatMap)

type CNF = Formula
type NNF = Formula
type InCNF = [[Int]]
type IdMap = [(Id, Int)]

idMap :: Formula -> IdMap
idMap = flip zip [1..] . vars

convert :: CNF -> CNF -> CNF
convert a (And b c) = And (convert a b) (convert a c)
convert (And a b) c = And (convert a c) (convert b c)
convert a b = Or a b

toNNF :: Formula -> NNF
toNNF (Not (And f f')) = Or (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (Or f f')) = And (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (Not f)) = toNNF f
toNNF (BiCond f f') = toNNF (Or (And f f') (And (Not f) (Not f')))
toNNF (Impl f f') = toNNF (Or (Not f) (f'))
toNNF (Or f f') = Or (toNNF f) (toNNF f')
toNNF (And f f') = And (toNNF f) (toNNF f')
toNNF (Not f) = Not (toNNF f)
toNNF f = f

toCNF :: NNF -> CNF
toCNF = toCNF' . toNNF
    where
      toCNF' :: NNF -> CNF
      toCNF' (Or f f') = convert f f'
      toCNF' (And f f') = And (toCNF' f) (toCNF' f')
      toCNF' (Not f) = Not (toCNF' f)
      toCNF' f = f

convertToInCNF :: CNF -> InCNF
convertToInCNF f = convertToInCNF' f
    where
      idF = idMap f
      convertToInCNF' :: CNF -> InCNF
      convertToInCNF' (Var x) = [[lookUp x idF]]
      convertToInCNF' (Not (Var x)) = [[-(lookUp x idF)]]
      convertToInCNF' (And f f') = convertToInCNF' f ++ convertToInCNF' f'
      convertToInCNF' (Or f f') = (f1 ++ f2) : fs ++ fs'
          where
            (f1 : fs)  = convertToInCNF' f
            (f2 : fs')  = convertToInCNF' f'
      convertToInCNF' _ = error "Current formula is not in CNF"

pureLiteralElimination :: InCNF -> (InCNF, [Int])
pureLiteralElimination f = (f', pureLits)
  where
    literals = concat f
    groupedLiterals = group . sort $ literals
    pureLiterals = filter (\g -> notElem (-fst g) literals) . map (\g -> (head g, length g)) $ groupedLiterals
    pureLits = map fst pureLiterals
    pureClauses = [c | c <- f, any (`elem` pureLits) c]
    f' = f \\ pureClauses

eliminateAllPureLiterals :: InCNF -> ([Int], InCNF)
eliminateAllPureLiterals f = eliminateAllPureLiterals' f []
  where
    eliminateAllPureLiterals' f acc
      | null pureLits = (acc, f')
      | otherwise = eliminateAllPureLiterals' f' (acc ++ pureLits)
      where
        (f', pureLits) = pureLiteralElimination f

unitProp :: InCNF -> (InCNF, [Int])
unitProp f
  | null pure = (f, [])
  | otherwise = (f', u : us)
  where
    pure = concat.filter isUnitClause $ f
    u = head pure
    (f', us) = unitProp . unitProp' $ f
    unitProp' = map (filter (/= -u)) . filter (notElem u)
    isUnitClause [_] = True
    isUnitClause _ = False

dpll :: InCNF -> [[Int]]
dpll f
  | f'' == [] = [us ++ ps]
  | [] `elem` f'' = []
  | otherwise = (map (\solution -> us ++ ps ++ solution) (dpll ([u] : f'') ++ dpll ([-u] : f'')))
    where
      (f', us) = unitProp f
      (ps, f'') = eliminateAllPureLiterals f'
      u = head.head $ f''

checkSAT :: Formula -> [[(Id, Bool)]]
checkSAT f = (map (map toPair).listAll.toDP) f
    where
      toDP = dpll . convertToInCNF . toCNF
      listAll = map reverse.concatMap (getElems fVars [])
      fVars = [1..length (vars f)]
      fMap = (map swap . idMap) f
      getElems :: [Int] -> [Int] -> [Int] -> [[Int]]
      getElems [] acc _ = [acc]
      getElems (x : xs) acc s
        | x `elem` s = getElems xs (x : acc) s
        | -x `elem` s = getElems xs (-x : acc) s
        | otherwise = getElems xs (x : acc) s ++ getElems xs (-x : acc) s
      toPair :: Int -> (Id, Bool)
      toPair i = (lookUp (abs i) fMap, i > 0)

checkWithDPLL :: Formula -> Formula -> Bool
checkWithDPLL kb query = checkSAT (kb `And` Not query) == []

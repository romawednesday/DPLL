module FormulaParser where

import System.IO (hFlush, stdout)
import DPLL
import Formula
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Data.Functor (($>))

lexer = Token.makeTokenParser emptyDef

parens = Token.parens lexer
reservedOp = Token.reservedOp lexer
identifier = Token.identifier lexer
whiteSpace = Token.whiteSpace lexer

table = [ [Prefix (reservedOp "!" $> Not) ]
        , [Infix  (reservedOp "&" $> And) AssocLeft]
        , [Infix  (reservedOp "|" $> Or) AssocLeft]
        , [Infix  (reservedOp "->" $> Impl) AssocRight]
        , [Infix  (reservedOp "<->" $> BiCond) AssocRight]
        ]

expr :: Parser Formula
expr = buildExpressionParser table term

term = parens expr
    <|> fmap Var identifier

parseFormula :: String -> Either ParseError Formula
parseFormula input = parse (whiteSpace >> expr) "" input

main :: IO ()
main = do
    putStr "Enter a formula: "
    hFlush stdout
    formulaInput <- getLine
    putStr "Enter a query: "
    hFlush stdout
    queryInput <- getLine
    case parseFormula formulaInput of
        Left err -> print err
        Right formula -> 
            case parseFormula queryInput of
                Left err -> print err
                Right query -> do
                    let cnfQuery = toCNF $ toNNF query
                    let result = checkWithDPLL formula cnfQuery
                    putStrLn $ "Result: " ++ show result

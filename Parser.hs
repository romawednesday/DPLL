module FormulaParser where

import System.IO (hFlush, stdout)
import DPLL
import Formula
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer = Token.makeTokenParser emptyDef

parens = Token.parens lexer
reservedOp = Token.reservedOp lexer
identifier = Token.identifier lexer
whiteSpace = Token.whiteSpace lexer

table = [ [Prefix (reservedOp "!" >> return Not) ]
        , [Infix  (reservedOp "&" >> return And) AssocLeft]
        , [Infix  (reservedOp "|" >> return Or) AssocLeft]
        , [Infix  (reservedOp "->" >> return Impl) AssocRight]
        , [Infix  (reservedOp "<->" >> return BiCond) AssocRight]
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
                    let result = checkWithDPLL formula query
                    putStrLn $ "Result: " ++ show result

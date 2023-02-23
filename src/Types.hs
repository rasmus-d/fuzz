module Types (Pretty (..) , BinOperator ( .. ) , Expr ( .. ), noOfBinOperators ) where
    
    data BinOperator 
        = Plus 
        | Minus 
        | Times 
        | Divide 
        | Modulo 
        | Equal 
        | NotEqual 
        | Less 
        | Greater 
        | LessOrEq 
        | GreaterOrEq
        deriving (Eq, Show, Enum)
    noOfBinOperators :: Int
    noOfBinOperators = 11
    data Expr 
        = BinOperation BinOperator Expr Expr
        | EVar String
        | EInt Int 
        deriving (Eq, Show)

    -- Prettyprinting
    class Pretty a where
        pretty :: a -> String
        pretty = pretty

    instance Pretty BinOperator where
        pretty :: BinOperator -> String
        pretty Plus = "+"
        pretty Minus = "-" 
        pretty Times = "*" 
        pretty Divide = "/" 
        pretty Modulo = "%" 
        pretty Equal = "==" 
        pretty NotEqual = "!=" 
        pretty Less = "<" 
        pretty Greater = ">" 
        pretty LessOrEq = "<=" 
        pretty GreaterOrEq = ">="

    instance Pretty Expr where
        pretty :: Expr -> String
        pretty (BinOperation op e1 e2) = '(' : pretty e1 ++ pretty op ++ pretty e2 ++ ")" 
        pretty (EVar x) = x
        pretty (EInt i) = show i

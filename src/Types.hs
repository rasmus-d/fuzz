module Types 
    ( GenState ( .. )
    , Pretty ( .. ) 
    , BinOperator ( .. ) 
    , Expr ( .. )
    , Stmt ( .. )
    , Type ( .. )
    , noOfBinOperators
    , noOfTypes ) 
    where
    
    import System.Random ( StdGen )

    data GenState = GenState 
        { stmtAlpha :: Double
        , exprAlpha :: Double
        , temperature :: Double
        , gen :: StdGen
        , env :: [String]
        , cnt :: Int }

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
    data Type 
        = TInt 
        | TChar 
        deriving (Eq, Show, Enum)
    noOfTypes :: Int
    noOfTypes = 2
    data Stmt 
        = VarDecl Type String (Maybe Expr)
        | VarAssign String Expr
        | Scope [Stmt]
        | If Expr Stmt (Maybe Stmt)
        | While Expr Stmt
        | Return (Maybe Expr)
        deriving (Eq, Show)


    -- Prettyprinting
    class Pretty a where
        pretty :: a -> String
        pretty = pretty

    instance Pretty BinOperator where
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
        pretty (BinOperation op e1 e2) = '(' : pretty e1 ++ pretty op ++ pretty e2 ++ ")" 
        pretty (EVar x) = x
        pretty (EInt i) 
            | i < 0 = '(' : show i ++ ")"
            | otherwise = show i

    instance Pretty Type where
        pretty TInt = "int"
        pretty TChar = "char"

    instance Pretty Stmt where
        pretty (VarDecl ty str (Just e)) = pretty ty ++ ' ' : str ++ " = " ++ pretty e ++ ";\n"
        pretty (VarDecl ty str Nothing) = pretty ty ++ ' ' : str ++ ";\n"
        pretty (VarAssign str e) = str ++ " = " ++ pretty e ++ ";\n"
        pretty (Scope stmts) = "{\n" ++ concatMap pretty stmts ++ "\n}\n"
        pretty (If e s1 (Just s2)) = "if(" ++ pretty e ++ ")\n" ++ pretty s1 ++ "else " ++ pretty s2
        pretty (If e s Nothing) = "if(" ++ pretty e ++ ")\n" ++ pretty s
        pretty (While expr stmt) = "while(" ++ pretty expr ++ ")\n" ++ pretty stmt
        pretty (Return (Just e)) = "return " ++ pretty e ++ ";\n"
        pretty (Return Nothing) = "return;"

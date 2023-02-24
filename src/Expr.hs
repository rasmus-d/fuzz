module Expr (GenState (..), genExpr, genExprIO, genExpr') where
    import Types ( Expr(..), noOfBinOperators, GenState(..) )
    import Common
    import System.Random ( getStdGen, mkStdGen, Random(randomR) )
    import Control.Monad.State ( MonadState(get, put), modify, evalState, State )


    newLit :: State GenState Expr
    newLit = do 
        r <- rnd
        s <- get
        rv <- rndVar
        case rv of
            Just x | r < 0.5 -> return $ EVar x
            _ -> do
                let (i,g') = randomR (-100, 100) (gen s) --(minBound :: Int, maxBound :: Int) (gen s)
                put s {gen = g'}
                return $ EInt i

    newOperation :: State GenState Expr
    newOperation = do 
        e1 <- genExpr'
        e2 <- genExpr'
        s <- get
        let (r,g') = randomR (0, noOfBinOperators - 1) (gen s)
        put s {gen = g'}
        return $ BinOperation (toEnum r) e1 e2

    genExpr' :: State GenState Expr
    genExpr' = do
        r <- rnd
        s <- get
        if r > temperature s 
        then updateTemp (exprAlpha s) >> newLit 
        else updateTemp (exprAlpha s) >> newOperation

    genExpr :: Int -> Double -> [String] -> Expr
    genExpr seed alpha environment = evalState genExpr' s
        where s = GenState 
                { stmtAlpha = 0
                , exprAlpha = alpha
                , temperature = 1.0
                , gen = mkStdGen seed
                , env = environment
                , cnt = 0 }

    genExprIO :: Double -> [String] -> IO Expr
    genExprIO alpha environment = do
        seed <- getStdGen
        let s = GenState 
                { stmtAlpha = 0
                , exprAlpha = alpha
                , temperature = 1.0
                , gen = seed
                , env = environment
                , cnt = 0 }
        return $ evalState genExpr' s
        


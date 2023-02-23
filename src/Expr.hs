module Expr (GenState (..), genExpr) where
    import Types
    import System.Random
    import Control.Monad.State

    data GenState = GenState 
        { alpha :: Double
        , temperature :: Double
        , gen :: StdGen
        , cnt :: Int }
    
    rnd :: State GenState Double
    rnd = do
        s <- get
        let (r,g') = randomR (0,1.0) (gen s)
        put s {gen = g'}
        return r
    
    updateTemp :: State GenState ()
    updateTemp = do
        s <- get
        put s {temperature = temperature s - alpha s}

    newVar :: State GenState Expr
    newVar = do
        s <- get
        put s {cnt = cnt s + 1}
        return $ EVar ("tmp" ++ show (cnt s))

    newOperation :: State GenState Expr
    newOperation = do 
        s <- get
        e1 <- genExpr'
        e2 <- genExpr'
        case randomR (0, noOfBinOperators - 1) (gen s) of
            (r,g') -> do
                put s {gen = g'}
                return $ BinOperation (toEnum r) e1 e2

    genExpr' :: State GenState Expr
    genExpr' = do
        r <- rnd
        updateTemp
        s <- get
        if r > temperature s 
        then newVar 
        else newOperation

    genExpr :: Int -> Double -> Expr
    genExpr seed alpha = evalState genExpr' s
        where s = GenState {alpha = alpha, temperature = 1.0, gen = mkStdGen seed, cnt = 0}
        


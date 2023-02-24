module Stmt where
    import Types
    import Expr
    import Control.Monad.State
    import System.Random
    import Common


    nonReturnStmts = 
        [ newVarDecl
        , newVarAssign ]
        --, newScope
        --, newIf
        --, newWhile ]

    newType :: State GenState Type
    newType = do
        s <- get
        let (r,g') = randomR (0, noOfTypes - 1) (gen s)
        put s {gen = g'}
        return $ toEnum r

    newExpr :: State GenState Expr
    newExpr = do
        s <- get
        t <- gets temperature
        put s {temperature = 1.0}
        e <- genExpr'
        put s {temperature = t}
        return e

    newMaybe :: a -> State GenState (Maybe a)
    newMaybe x = do
        s <- get
        let (r,g') = randomR (0 :: Int, 1) (gen s)
        put s {gen = g'}
        case r of
            0 -> return Nothing
            _ -> return $ Just x


    newVarDecl :: State GenState Stmt
    newVarDecl = do
        t <- newType
        x <- newVar
        e <- newExpr
        me <- newMaybe e
        s <- get
        put s {env = x: env s}
        return $ VarDecl t x me


    newVarAssign :: State GenState Stmt
    newVarAssign = do 
        mx <- rndVar
        case mx of
            Just x -> VarAssign x <$> newExpr
            Nothing -> newVarDecl

    --newScope :: State GenState Stmt
    --newScope = do

    --newIf :: State GenState Stmt
    --newWhile :: State GenState Stmt
    newReturn :: State GenState Stmt
    newReturn = do
        e <- newExpr
        me <- newMaybe e
        return $ Return me

    newStmt :: State GenState Stmt
    newStmt = do
        r <- rnd
        s <- get
        if r > temperature s 
        then updateTemp (stmtAlpha s) >> newReturn
        else do 
            updateTemp (stmtAlpha s)
            let (r',g') = randomR (0, length nonReturnStmts -1) (gen s)
            put s {gen = g'}
            nonReturnStmts !! r'

    newIf :: State GenState Stmt
    newIf = do
        e1 <- newExpr
        s1 <- newStmt
        s2 <- newStmt
        s2' <- newMaybe s2
        return $ If e1 s1 s2'

    newScope :: State GenState Stmt
    newScope = do
        s <- get 
        let (r,g') = randomR (0 :: Int, 20) (gen s) 
        stmts <- mapM (const newStmt) [0..r]
        put s {gen = g'}
        return $ Scope stmts



    genStmts :: Double -> Int -> Stmt
    genStmts alpha seed = 
        let s = GenState 
                { stmtAlpha = alpha
                , exprAlpha = 0.5
                , temperature = 1.0
                , gen = mkStdGen seed
                , env = []
                , cnt = 1 } in
            evalState newScope s


    



module Common where
    import Types
    import Control.Monad.State
    import System.Random

    inc :: State GenState ()
    inc = do
        s <- get
        put s {cnt = cnt s + 1}

    updateTemp :: Double -> State GenState ()
    updateTemp a = 
        modify $ \s -> s {temperature = temperature s - a}

    rnd :: State GenState Double
    rnd = do
        s <- get
        let (r,g') = randomR (0,1.0) (gen s)
        put s {gen = g'}
        return r

    newVar :: State GenState String
    newVar = do
        c <- gets cnt
        inc
        return $ "tmp" ++ show c

    rndVar :: State GenState (Maybe String)
    rndVar = do
        s <- get
        if null (env s) 
        then do
            return Nothing
        else do
            let (i,g') = randomR (0, length (env s) - 1) (gen s)
            put s {gen = g'}
            return $ Just (env s !! i)

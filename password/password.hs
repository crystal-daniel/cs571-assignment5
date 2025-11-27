module Password where

newtype PwdOp a = PwdOp (String -> (a, String))

instance Monad PwdOp where
    return x = PwdOp (\pwd -> (x, pwd))
    (PwdOp m) >>= f = PwdOp (\pwd -> 
        let (a, pwd') = m pwd
            (PwdOp m') = f a
        in m' pwd')

instance Applicative PwdOp where
    pure = return
    mf <*> mx = do
        f <- mf
        x <- mx
        return (f x)

instance Functor PwdOp where
    fmap f m = do
        x <- m
        return (f x)

setPassword :: String -> PwdOp ()
setPassword newPwd = PwdOp (\_ -> ((), newPwd))

checkPassword :: String -> PwdOp Bool
checkPassword guess = PwdOp (\pwd -> (guess == pwd, pwd))

runPwdOp :: PwdOp a -> a
runPwdOp (PwdOp m) = fst (m "")






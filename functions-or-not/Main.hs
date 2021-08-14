import Control.Monad.State.Lazy
import Data.Map.Strict as M
import Control.Applicative
import Control.Monad.Trans.Except

addPair :: (Int, Int) -> State (M.Map Int Int) ()
addPair (a, b) = do
    m <- get
    put $ M.insert a b m

addPair' :: (Int, Int) -> State (M.Map Int Int) Bool
addPair' (a, b) = do
    m <- get
    if member a m then
        let v = m ! a
        in
        return (v == b)
    else
        do
            put $ M.insert a b m
            return True

addPairT :: (Int, Int) -> StateT (M.Map Int Int) (Either String) ()
addPairT (a, b) = do
    m <- get
    if member a m then
        let v = m ! a
        in if (v == b) then return () else lift $ Left "No"
    else
        do
            put $ M.insert a b m
            return ()

addPairT' :: (Int, Int) -> ExceptT String (State (M.Map Int Int)) ()
addPairT' (a, b) = do
    m <- lift $ get
    if member a m then
        let v = m ! a
        in if (v == b) then return () else throwE "Not Function"
    else
        lift $ put $ M.insert a b m

addPairT'' :: (Int, Int) -> ExceptT String (StateT (M.Map Int Int) IO) ()
addPairT'' (a, b) = do
    m <- lift get
    if member a m then
        let v = m ! a
        in if (v == b) then return () else throwE "Not Function"
    else
        lift $ put $ M.insert a b m

addPairT''' :: (Int, Int) -> ExceptT String (StateT (M.Map Int Int) IO) ()
addPairT''' (a, b) = do
    m <- lift get
    if member a m then
        let v = m ! a
        in if (v == b) then return () else throwE "Not Function"
    else
        lift $ put $ M.insert a b m

readInput = do
    n <- readLn
    pairs <- replicateM n $ do
        (a:b:[]) <- words <$> getLine
        return (read a :: Int, read b :: Int)
    result <- (`evalStateT` M.empty) $ runExceptT $ forM pairs addPairT''
    case result of
        Left _ -> putStrLn "NO"
        Right _ -> putStrLn "YES"

main = do
    t <- readLn
    forM_ [1..t] $ \_ -> readInput
    -- let result = (`runStateT` M.empty) $ do
    --     addPairT (1,1)
    --     addPairT (2,1)
    --     addPairT (1,2)
    -- print result
    -- let result' = (`runState` M.empty) $ runExceptT $ do
    --     addPairT' (1,1)
    --     addPairT' (2,1)
    --     addPairT' (1,2)
    -- print result'
    -- result'' <- (`runStateT` M.empty) $ runExceptT $ do
    --     addPairT'' (1,1)
    --     addPairT'' (2,1)
    --     addPairT'' (1,2)
    --     addPairT'' (3,2)
    -- print result''
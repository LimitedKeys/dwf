
module Dwf.Dll.Access where

import Foreign 
import Foreign.C.Types
import Foreign.C.String

import Data.Coerce (coerce)

data DwfResult a = DwfResult a
    | DwfError Int
    | DwfNone 

instance Functor DwfResult where
    fmap f (DwfResult x) = DwfResult (f x)
    fmap _ (DwfError x) = DwfError x
    fmap _ DwfNone = DwfNone

instance Applicative DwfResult where
    pure = DwfResult

    DwfNone  <*> _ = DwfNone
    DwfError x <*> _ = DwfError x
    DwfResult f <*> DwfResult x = DwfResult (f x)
    _ <*> DwfError x = DwfError x
    _ <*> DwfNone = DwfNone

-- Get only the errros from the list of DwfResults (kinda like lefts in either)
errors :: [DwfResult a] -> [Int]
errors = foldr f [] 
    where f x acc = case x of
            DwfNone -> acc
            DwfResult _ -> acc
            DwfError v -> v:acc

-- Get only the results from the list of DwfResults (kinda like rights in
-- either)
results :: [DwfResult a] -> [a]
results = foldr f []
    where f x acc = case x of
            DwfNone -> acc
            DwfResult v -> v:acc
            DwfError _ -> acc

fromResult :: DwfResult a -> a
fromResult DwfNone = error "Cannot decode DwfNone into a value"
fromResult (DwfError x) = error $ "Cannot decode DwfError (" <> show x <> ") into a value"
fromResult (DwfResult x) = x 

check :: (Int, a) -> DwfResult a
check (result, value) = if result /= 0 
    then DwfError result
    else DwfResult value

check' :: Int -> DwfResult ()
check' result = if result /= 0 
    then DwfError result
    else DwfNone

fCall :: IO CInt -> IO (DwfResult ())
fCall f = do check' . fromIntegral <$> f

fToInt :: (Ptr CInt -> IO CInt) -> IO (DwfResult Int)
fToInt f = alloca (\result -> do
    errorCode <- f result
    cValue <- peek result
    return $ check (fromIntegral errorCode, fromIntegral cValue)
    )

fToUChar :: (Ptr CUChar -> IO CInt) -> IO (DwfResult Int)
fToUChar f = alloca (\result -> do
    errorCode <- f result
    cValue <- peek result
    return $ check (fromIntegral errorCode, fromIntegral cValue)
    )

_coerce :: CDouble -> Double
_coerce = coerce

fToDouble :: (Ptr CDouble -> IO CInt) -> IO (DwfResult Double)
fToDouble f = alloca (\result -> do
    errorCode <- f result
    cValue <- _coerce <$> peek result
    return $ check (fromIntegral errorCode, cValue)
    )

fToDoubleDouble :: (Ptr CDouble -> Ptr CDouble -> IO CInt) -> IO (DwfResult (Double, Double))
fToDoubleDouble f = alloca (\a -> alloca (\b -> do
    errorCode <- fromIntegral <$> f a b
    cA <- _coerce <$> peek a
    cB <- _coerce <$> peek b
    return $ check (errorCode, (cA, cB))
    ))

fToDoubleDoubleDouble :: (Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt) -> IO (DwfResult (Double, Double, Double))
fToDoubleDoubleDouble f = alloca (\a -> alloca (\b -> alloca (\c -> do
    errorCode <- fromIntegral <$> f a b c
    cA <- _coerce <$> peek a
    cB <- _coerce <$> peek b
    cC <- _coerce <$> peek c
    return $ check (errorCode, (cA, cB, cC))
    )))

fToDoubleDoubleInt :: (Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt) -> IO (DwfResult (Double, Double, Int))
fToDoubleDoubleInt f = alloca (\a -> alloca (\b -> alloca (\c -> do
    errorCode <- fromIntegral <$> f a b c
    cA <- _coerce <$> peek a
    cB <- _coerce <$> peek b
    cC <- fromIntegral <$> peek c
    return $ check (errorCode, (cA, cB, cC))
    )))

fToDoubleArrayN :: Int -> (Ptr CDouble -> CInt -> IO CInt) -> IO (DwfResult [Double])
fToDoubleArrayN n f = allocaArray n (\a -> do
    let n' = (fromIntegral n) :: CInt 
    errorCode <- fromIntegral <$> f a n' 
    raw <- peekArray n a 
    let samples = map _coerce raw
    return $ check (errorCode, samples))

fToDoubleArrayIN :: Int -> Int -> (Ptr CDouble -> CInt -> CInt -> IO CInt) -> IO (DwfResult [Double])
fToDoubleArrayIN i n f = allocaArray n (\a -> do
    let i' = (fromIntegral i) :: CInt 
    let n' = (fromIntegral n) :: CInt 
    errorCode <- fromIntegral <$> f a i' n' 
    raw <- peekArray n a 
    let samples = map _coerce raw
    return $ check (errorCode, samples))

fTo2DoubleArrayN :: Int -> (Ptr CDouble -> Ptr CDouble -> CInt -> IO CInt) -> IO (DwfResult [(Double, Double)])
fTo2DoubleArrayN n f = allocaArray n (\a -> allocaArray n (\b -> do
    let n' = fromIntegral n
    errorCode <- fromIntegral <$> f a b n'
    rawA <- peekArray n a
    let samplesA = map _coerce rawA
    rawB <- peekArray n a
    let samplesB = map _coerce rawB
    return $ check (errorCode, zip samplesA samplesB)
    ))

fTo2DoubleArrayIN :: Int -> Int -> (Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO CInt) -> IO (DwfResult [(Double, Double)])
fTo2DoubleArrayIN i n f = allocaArray n (\a -> allocaArray n (\b -> do
    let n' = fromIntegral n
    let i' = fromIntegral i
    errorCode <- fromIntegral <$> f a b i' n'
    rawA <- peekArray n a
    let samplesA = map _coerce rawA
    rawB <- peekArray n a
    let samplesB = map _coerce rawB
    return $ check (errorCode, zip samplesA samplesB)
    ))

fToStringN :: Int -> (CString -> IO CInt) -> IO (DwfResult String)
fToStringN n f = allocaBytes n (\raw -> do
    errorCode <- f raw
    cMsg <- peekCString raw
    return $ check (fromIntegral errorCode, cMsg)
    )

fTo2StringN :: Int -> Int -> (CString -> CString -> IO CInt) -> IO (DwfResult (String, String))
fTo2StringN n m f = allocaBytes n (\rawA -> allocaBytes m (\rawB -> do
    errorCode <- f rawA rawB
    cMsg1 <- peekCString rawA
    cMsg2 <- peekCString rawB
    return $ check (fromIntegral errorCode, (cMsg1, cMsg2))
    ))

fToIntInt :: (Ptr CInt -> Ptr CInt -> IO CInt) -> IO (DwfResult (Int, Int))
fToIntInt f = alloca (\a -> alloca (\b -> do
    errorCode <- fromIntegral <$> f a b
    cA <- fromIntegral <$> peek a
    cB <- fromIntegral <$> peek b
    return $ check (errorCode, (cA, cB))
    ))

fToIntIntInt :: (Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt) -> IO (DwfResult (Int, Int, Int))
fToIntIntInt f = alloca (\a -> alloca (\b -> alloca (\c -> do
    errorCode <- fromIntegral <$> f a b c
    cA <- fromIntegral <$> peek a
    cB <- fromIntegral <$> peek b
    cC <- fromIntegral <$> peek c
    return $ check (errorCode, (cA, cB, cC))
    )))

fToIntArrayIN :: Int -> Int -> (Ptr CShort -> CInt -> CInt -> IO CInt) -> IO (DwfResult [Int])
fToIntArrayIN i n f = allocaArray n (\a -> do 
    let i' = fromIntegral i
    let n' = fromIntegral n
    errorCode <- fromIntegral <$> f a i' n'
    raw <- peekArray n a 
    let samples = map fromIntegral raw
    return $ check (errorCode, samples))

fToIntDoubleArray32 :: (Ptr CDouble -> Ptr CInt -> IO CInt) -> IO (DwfResult [Double])
fToIntDoubleArray32 f = alloca (\n -> allocaArray 32 (\a -> do
    errorCode <- fromIntegral <$> f a n 
    n' <- fromIntegral <$> peek n
    raw <- peekArray n' a
    let samples = map _coerce raw
    return $ check (errorCode, samples)))

fToBool :: (Ptr CInt -> IO CInt) -> IO (DwfResult Bool)
fToBool f = alloca (\a -> do
    errorCode <- fromIntegral <$> f a
    cA <- toB . fromIntegral <$> peek a
    return $ check (errorCode, cA))
    where toB :: Int -> Bool
          toB = (== 0)

infoD3 :: (CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO (CInt)) -> Int -> IO (DwfResult (Double, Double, Double))
infoD3 f p = fToDoubleDoubleDouble (f (fromIntegral p))

setD1 :: (CInt -> CDouble -> IO (CInt)) -> Int -> Double -> IO (DwfResult ())
setD1 f p q = fCall (f (fromIntegral p) (coerce q))

getD1 :: (CInt -> Ptr CDouble -> IO (CInt)) -> Int -> IO (DwfResult Double)
getD1 f p = fToDouble (f (fromIntegral p))

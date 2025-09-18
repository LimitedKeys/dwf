
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
check (result, value) = if result /= 1 
    then DwfError result
    else DwfResult value

check' :: Int -> DwfResult ()
check' result = if result /= 1
    then DwfError result
    else DwfNone

fCall :: IO CInt -> IO (DwfResult ())
fCall f = do check' . fromIntegral <$> f

fToInt :: Storable a => Integral a => (Ptr a -> IO CInt) -> IO (DwfResult Int)
fToInt f = alloca (\result -> do
    errorCode <- f result
    cValue <- peek result
    return $ check (fromIntegral errorCode, fromIntegral cValue)
    )

getI1X :: Storable a => Integral a => (CInt -> Ptr a -> IO CInt) -> Int -> IO (DwfResult Int)
getI1X f p = fToInt (f (fromIntegral p))

setI1X :: Storable a => Integral a => (CInt -> a -> IO CInt) -> Int -> Int -> IO (DwfResult ())
setI1X f p q = fCall (f (fromIntegral p) (fromIntegral q))

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
    let n' = fromIntegral n :: CInt 
    errorCode <- fromIntegral <$> f a n' 
    raw <- peekArray n a 
    let samples = map _coerce raw
    return $ check (errorCode, samples))

fToDoubleArrayIN :: Int -> Int -> (Ptr CDouble -> CInt -> CInt -> IO CInt) -> IO (DwfResult [Double])
fToDoubleArrayIN i n f = allocaArray n (\a -> do
    let i' = fromIntegral i :: CInt 
    let n' = fromIntegral n :: CInt 
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

fToIntInt :: (Storable a, Storable b)
          => (Integral a, Integral b)
          => (Ptr a -> Ptr b -> IO CInt) -> IO (DwfResult (Int, Int))
fToIntInt f = alloca (\a -> alloca (\b -> do
    errorCode <- fromIntegral <$> f a b
    cA <- fromIntegral <$> peek a
    cB <- fromIntegral <$> peek b
    return $ check (errorCode, (cA, cB))
    ))

fToIntIntInt :: (Storable a, Storable b, Storable c) 
             => (Integral a, Integral b, Integral c) 
             => (Ptr a -> Ptr b -> Ptr c -> IO CInt) -> IO (DwfResult (Int, Int, Int))
fToIntIntInt f = alloca (\a -> alloca (\b -> alloca (\c -> do
    errorCode <- fromIntegral <$> f a b c
    cA <- fromIntegral <$> peek a
    cB <- fromIntegral <$> peek b
    cC <- fromIntegral <$> peek c
    return $ check (errorCode, (cA, cB, cC))
    )))

fToIntIntIntInt :: (Storable a, Storable b, Storable c, Storable d) 
             => (Integral a, Integral b, Integral c, Integral d) 
             => (Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO CInt) -> IO (DwfResult (Int, Int, Int, Int))
fToIntIntIntInt f = alloca (\a -> alloca (\b -> alloca (\c -> alloca (\d -> do
    errorCode <- fromIntegral <$> f a b c d
    cA <- fromIntegral <$> peek a
    cB <- fromIntegral <$> peek b
    cC <- fromIntegral <$> peek c
    cD <- fromIntegral <$> peek d
    return $ check (errorCode, (cA, cB, cC, cD))
    ))))

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

-- Basic stuffs (single parameter)

setD1 :: (CInt -> CDouble -> IO CInt) -> Int -> Double -> IO (DwfResult ())
setD1 f p q = fCall (f (fromIntegral p) (coerce q))

setI1 :: Storable a => Integral a => (CInt -> a -> IO CInt) -> Int -> Int -> IO (DwfResult ())
setI1 = setI1X

setUI1 :: (CInt -> CUInt -> IO CInt) -> Int -> Int -> IO (DwfResult ())
setUI1 f p q = fCall (f (fromIntegral p) (fromIntegral q))

setI2 :: (CInt -> CInt -> CInt -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult ())
setI2 f p q r = fCall (f p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

setI4 :: (Storable a, Storable b, Storable c, Storable d)
      => (Integral a, Integral b, Integral c, Integral d)
      => (CInt -> a -> b -> c -> d -> IO CInt) -> Int -> Int -> Int -> Int -> Int -> IO (DwfResult ())
setI4 f p q r s t = fCall (f p' q' r' s' t')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r
          s' = fromIntegral s
          t' = fromIntegral t

getD1 :: (CInt -> Ptr CDouble -> IO CInt) -> Int -> IO (DwfResult Double)
getD1 f p = fToDouble (f (fromIntegral p))

getI1 :: Storable a => Integral a => (CInt -> Ptr a -> IO CInt) -> Int -> IO (DwfResult Int)
getI1 = getI1X

getU1 :: (CInt -> Ptr CUChar -> IO CInt) -> Int -> IO (DwfResult Int)
getU1 f p = fToInt (f (fromIntegral p))

getUI1 :: (CInt -> Ptr CUInt -> IO CInt) -> Int -> IO (DwfResult Int)
getUI1 f p = fToInt (f (fromIntegral p))

getUL1 :: (CInt -> Ptr CULong -> IO CInt) -> Int -> IO (DwfResult Int)
getUL1 f p = fToInt (f (fromIntegral p))

getD2 :: (CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt) -> Int -> IO (DwfResult (Double, Double))
getD2 f p = fToDoubleDouble (f (fromIntegral p))

getI2 :: (Storable a, Storable b, Storable c) 
      => (Integral a, Integral b, Integral c)
      => (a -> Ptr b -> Ptr c -> IO CInt) -> Int -> IO (DwfResult (Int, Int))
getI2 f p = fToIntInt (f (fromIntegral p))

getD3 :: (CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CInt) -> Int -> IO (DwfResult (Double, Double, Double))
getD3 f p = fToDoubleDoubleDouble (f (fromIntegral p))

getI3 :: (CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt) -> Int -> IO (DwfResult (Int, Int, Int))
getI3 f p = fToIntIntInt (f (fromIntegral p))

getI4 :: (Storable a, Storable b, Storable c, Storable d) 
      => (Integral a, Integral b, Integral c, Integral d) 
      => (CInt -> Ptr a -> Ptr b -> Ptr c -> Ptr d -> IO CInt) -> Int -> IO (DwfResult (Int, Int, Int, Int))
getI4 f p = fToIntIntIntInt (f (fromIntegral p))

-- Double parameter (hdwf, channel)

setChanI1 :: (Storable a, Integral a)
          => (Storable b, Integral b)
          => (Storable c, Integral c)
          => (a -> b -> c -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult ())
setChanI1 f p q r = fCall (f (fromIntegral p) (fromIntegral q) (fromIntegral r))

setChanU1 :: (CInt -> CInt -> CUChar -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult ())
setChanU1 f p q r = fCall (f (fromIntegral p) (fromIntegral q) (fromIntegral r))

setChanD1 :: (CInt -> CInt -> CDouble -> IO CInt) -> Int -> Int -> Double -> IO (DwfResult ())
setChanD1 f p q r = fCall (f (fromIntegral p) (fromIntegral q) (coerce r))

getChanI1 :: (Storable a, Integral a) 
          => (Storable b, Integral b)
          => (Storable c, Integral c)
          => (a -> b -> Ptr c -> IO CInt) -> Int -> Int -> IO (DwfResult Int)
getChanI1 f p q = fToInt (f (fromIntegral p) (fromIntegral q))

getChanD1 :: (CInt -> CInt -> Ptr CDouble -> IO CInt) -> Int -> Int -> IO (DwfResult Double)
getChanD1 f p q = fToDouble (f (fromIntegral p) (fromIntegral q))

getChanI2 :: (Storable a, Integral a)
          => (Storable b, Integral b)
          => (Storable c, Integral c)
          => (Storable d, Integral d)
          => (a -> b -> Ptr c -> Ptr d -> IO CInt) -> Int -> Int -> IO (DwfResult (Int, Int))
getChanI2 f p q = fToIntInt (f (fromIntegral p) (fromIntegral q))

getChanD2 :: (CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt) -> Int -> Int -> IO (DwfResult (Double, Double))
getChanD2 f p q = fToDoubleDouble (f (fromIntegral p) (fromIntegral q))

getChanU1 :: (CInt -> CInt -> Ptr CInt -> IO CInt) -> Int -> Int -> IO (DwfResult Int)
getChanU1 f p q = fToInt (f (fromIntegral p) (fromIntegral q))

getChanI3 :: (CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt) -> Int -> Int -> IO (DwfResult (Int, Int, Int))
getChanI3 f p q = fToIntIntInt (f (fromIntegral p) (fromIntegral q))

-- Channel Node (hdwf, channel, node)

setNodeI1 :: (CInt -> CInt -> CInt -> CInt -> IO CInt) -> Int -> Int -> Int -> Int -> IO (DwfResult ())
setNodeI1 f p q r s = fCall (f p' q' r' s')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r
          s' = fromIntegral s

setNodeU1 :: (CInt -> CInt -> CInt -> CUChar -> IO CInt) -> Int -> Int -> Int -> Int -> IO (DwfResult ())
setNodeU1 f p q r s = fCall (f p' q' r' s')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r
          s' = fromIntegral s

setNodeD1 :: (CInt -> CInt -> CInt -> CDouble -> IO CInt) -> Int -> Int -> Int -> Double -> IO (DwfResult ())
setNodeD1 f p q r s = fCall (f p' q' r' s')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r
          s' = coerce s

getNodeI1 :: Storable a => Integral a => (CInt -> CInt -> CInt -> Ptr a -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult Int)
getNodeI1 f p q r = fToInt (f p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

getNodeD1 :: (CInt -> CInt -> CInt -> Ptr CDouble -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult Double)
getNodeD1 f p q r = fToDouble (f p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

getNodeD2 :: (CInt -> CInt -> CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult (Double, Double))
getNodeD2 f p q r = fToDoubleDouble (f p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

getNodeI2 :: (CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult (Int, Int))
getNodeI2 f p q r = fToIntInt (f p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

getNodeI3 :: (CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult (Int, Int, Int))
getNodeI3 f p q r = fToIntIntInt (f p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

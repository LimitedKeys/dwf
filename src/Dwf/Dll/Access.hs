
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

fToDoubleDoubleInt :: (Ptr CDouble -> Ptr CDouble -> Ptr CInt -> IO CInt) -> IO (DwfResult (Double, Double, Int))
fToDoubleDoubleInt f = alloca (\a -> alloca (\b -> alloca (\c -> do
    errorCode <- fromIntegral <$> f a b c
    cA <- _coerce <$> peek a
    cB <- _coerce <$> peek b
    cC <- fromIntegral <$> peek c
    return $ check (errorCode, (cA, cB, cC))
    )))

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

fToBool :: (Ptr CInt -> IO CInt) -> IO (DwfResult Bool)
fToBool f = alloca (\a -> do
    errorCode <- fromIntegral <$> f a
    cA <- toB . fromIntegral <$> peek a
    return $ check (errorCode, cA))
    where toB :: Int -> Bool
          toB = (== 0)

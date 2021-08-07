module Week04.MonadNotes where

-- (>>=)      :: IO a            -> (a -> IO b)            -> IO b
-- bindMaybe  :: Maybe a         -> (a -> Maybe b)         -> Maybe b
-- bindEither :: Either String a -> (a -> Either String b) -> Either String b
-- bindWriter :: Writer a        -> (a -> Writer b)        -> Writer b
-- 
-- return              :: a -> IO a
-- Just                :: a -> Maybe a
-- Right               :: a -> Either String a
-- (\a -> Writer a []) :: a -> Writer a

--now that we've defined Monad instances for WriterNotes, lets use Monad functions that can be used for any Monad
threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
  mx >>= \k ->
  my >>= \l ->
  mz >>= \m ->
  let s = k + l + m in return s -- or can just be return k + l + m

--we can use do notation as syntactic sugar, allows us to bind computations and name them without using the bind operator followed by lambda notation
 
threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
  k <- mx
  l <- my
  m <- mz
  return $ k + l + m
--or
--let s = k + l + m -- we don't need to use `in` during a do block!
--return s
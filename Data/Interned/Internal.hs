{-# LANGUAGE TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , BangPatterns
           , GeneralizedNewtypeDeriving #-}

module Data.Interned.Internal
  ( Interned(..)
  , Uninternable(..)
  , mkCache
  , Cache(..)
  , CacheState(..)
  , cacheSize
  , Id
  , intern
  , recover
  ) where

import Data.Array
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Foldable
import Data.Traversable
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)

-- tuning parameter
defaultCacheWidth :: Int
defaultCacheWidth = 1024

data CacheState t = CacheState
   { fresh :: {-# UNPACK #-} !Id
   , content :: !(HashMap (Description t) t)
   }

-- | Map containing interned values. Only use it through 'cacheSize' or to
-- implement 'cache' through 'mkCache'.
newtype Cache t = Cache { getCache :: Array Int (IORef (CacheState t)) }

{- | Measure size of a cache. -}
cacheSize :: Cache t -> IO Int
cacheSize (Cache t) = foldrM
   (\a b -> do
       v <- readIORef a
       return $! HashMap.size (content v) + b
   ) 0 t

{- | Creates a 'Cache' 't' using 'unsafePerformIO'. You should only use this to
   implement 'cache' as described in its documentation.-}
mkCache :: Interned t => Cache t
mkCache   = result where
  element = CacheState (seedIdentity result) HashMap.empty
  w       = cacheWidth result
  result  = Cache
          $ unsafePerformIO
          $ traverse newIORef
          $ listArray (0,w - 1)
          $ replicate w element

type Id = Int

{- | t is your main data structure, where each constructor is typically tagged with an unboxed 'Id' member to identify the cell.
   Thanks to hash consing, you can implement 'Eq' instances by just comparing 'Id's. The same applies for 'Ord' instances, as long
   as you are not interested in a specific ordering but in using your 't' for lookups in search trees.
   (???) -}
class ( Eq (Description t)
      , Hashable (Description t)
      ) => Interned t where
  -- | 'Description' 't' should be isomorphic to t, but more efficient to compare.
  --   Recursive occurrences of your data structure.
  --   A variant of your type where the members are replaced by IDs (???).
  --
  -- Equality and hashing are both supposed to be fast (possibly O(1)) for good
  -- performance (as in Term.hs, where the datatype is recursive and contained
  -- values are already interned), but not necessarily (see String.hs).
  data Description t
  {- | A standard version of your data type, with the 'Id' members dropped. -}
  type Uninterned t

  {- | Compute the description of your uninterned data type.
       Law:

       > describe u = describe v ==> u = v
  -}
  describe :: Uninterned t -> Description t
  -- | Tag the 'Uninterned' value with the passed 'Id'.
  --
  -- Your application can then compare instances of 't' by comparing their IDs.
  -- See documentation of 'intern' for the guarantee which makes this legal.
  --
  identify :: Id -> Uninterned t -> t
  -- identity :: t -> Id

  {- | 'seedIdentity' returns the initial ID value for initializing the cache.
     It is not supposed to use its argument (and by parametricity it can't): it is
     just provided to ensure 't' appears in the signature of seedIdentity, so that
     the correct instance will be found.

     The default implementation is usually sufficient. -}

  seedIdentity :: p t -> Id
  seedIdentity _ = 0

  cacheWidth :: p t -> Int
  cacheWidth _ = defaultCacheWidth
  modifyAdvice :: IO t -> IO t
  modifyAdvice = id

  -- | Return a reference to a global per-type cache.
  --
  -- Two calls to cache are supposed to return the same copy of the cache, which
  -- is then modified with `unsafePerformIO`. Therefore, the definition must never be
  -- inlined: you can inform GHC via the NOINLINE pragma.
  --
  -- Recommended definition template:
  --
  -- @
  -- instance Interned Foo where
  --   -- ...
  --   cache = fooCache
  -- fooCache :: Cache Foo
  -- fooCache = mkCache
  -- {-\# NOINLINE fooCache #-}
  -- @
  --
  cache        :: Cache t

class Interned t => Uninternable t where
  -- | Drop ids.
  -- Law:
  --
  -- prop> unintern (identify id v) = v.
  unintern :: t -> Uninterned t

-- | Produce an interned version of your data structure.
-- Laws:
--
-- > a == b => intern a === intern b
-- > intern a = identify someId b && describe b == describe a.
-- > if intern a = identify idA a, and intern b = identify idB b, then idA == idB <==> describe a == describe b.
--
-- The above uses === to indicate that 'intern' is guaranteed to return the
-- *same* value for equal inputs.
--
-- Thanks to laws on describe, the latter condition implies 'b == a'.
intern :: Interned t => Uninterned t -> t
intern !bt = unsafeDupablePerformIO $ modifyAdvice $ atomicModifyIORef slot go
  where
  slot = getCache cache ! r
  !dt = describe bt
  !hdt = hash dt
  !wid = cacheWidth dt
  r = hdt `mod` wid
  go (CacheState i m) = case HashMap.lookup dt m of
    Nothing -> let t = identify (wid * i + r) bt in (CacheState (i + 1) (HashMap.insert dt t m), t)
    Just t -> (CacheState i m, t)
--
-- Two entries only get the same ID if they have equal descriptions, but the
-- proof is tricky, and is modulo overflow.
--
-- The ID encodes a pair of 'i' and 'r' (because 'r' is in Z_wid, and 'i' is
-- multiplied by 'wid'). So two IDs are equal if they share both the 'r' and the
-- 'i' components. However, if they share 'r', the 'i' was created by the fresh
-- member of the same CacheState, which is then incremented and never reused.
--
-- However, if 'cacheWidth * i' overflows an Int, this guarantee is lost.

-- | Given a description, hunt for the corresponding entry in the cache.
recover :: Interned t => Description t -> IO (Maybe t)
recover !dt = do
  CacheState _ m <- readIORef $ getCache cache ! (hash dt `mod` cacheWidth dt)
  return $ HashMap.lookup dt m

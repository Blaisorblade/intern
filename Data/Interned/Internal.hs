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

newtype Cache t = Cache { getCache :: Array Int (IORef (CacheState t)) }

cacheSize :: Cache t -> IO Int
cacheSize (Cache t) = foldrM
   (\a b -> do
       v <- readIORef a
       return $! HashMap.size (content v) + b
   ) 0 t

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

{- t is your main data structure, where each constructor is typically tagged with an unboxed Id member to identify the cell. -}
class ( Eq (Description t)
      , Hashable (Description t)
      ) => Interned t where
  {- Description t needs to be isomorphic to t, but more efficient to compare.
  A variant of your type where the members are replaced by IDs (?). -}
  data Description t
  {- A "standard" version of your data type, with the Id members dropped. -}
  type Uninterned t

  {- | Compute the description of your uninterned data type.
       Law: describe u == describe v => u = v.
  -}
  describe :: Uninterned t -> Description t
  {- Tag the Uninterned datatype with the Id. -}
  identify :: Id -> Uninterned t -> t
  -- identity :: t -> Id
  {- | seedIdentity returns the initial ID value for initializing the cache. It is supposed to ignore its argument, which is  -}
  seedIdentity :: p t -> Id

  seedIdentity _ = 0
  cacheWidth :: p t -> Int
  cacheWidth _ = defaultCacheWidth
  modifyAdvice :: IO t -> IO t
  modifyAdvice = id
  {- | Return a reference to a global per-type cache.

     Two calls to cache are supposed to return the same copy of the cache, which
     is then modified with unsafePerformIO. Therefore, the definition must never be
     inlined: you can inform GHC via the NOINLINE pragma.

     Recommended definition template:

     instance Interned Foo where
       cache = fooCache
     fooCache :: Cache Foo
     fooCache = mkCache
     {-# NOINLINE fooCache #-}
   -}
  cache        :: Cache t

class Interned t => Uninternable t where
  {- Drop ids.
     Law: unintern (identify id v) = v. -}
  unintern :: t -> Uninterned t

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

-- | Given a description, hunt for an entry in the cache
recover :: Interned t => Description t -> IO (Maybe t)
recover !dt = do
  CacheState _ m <- readIORef $ getCache cache ! (hash dt `mod` cacheWidth dt)
  return $ HashMap.lookup dt m

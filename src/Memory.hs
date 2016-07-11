{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, GADTs, BangPatterns #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Emulation of first-class memory:
-- memory that stores values of arbitrary types and can look them
-- up by their (integer) addresses.
-- The emulation must necessarily use unsafeCoerce:
-- the safety invariant -- distinct nodes have distinct addresses
-- and hence if two nodes have the same address, they have the same type --
-- cannot be expressed in Haskell.
-- It is a good puzzle to express this invariant in, say, Agda, and
-- enforce it during the generation of new addresses.

module Memory (
  Address,
  Ref, dref, addr, modify_ref,         -- data constructor is not exported
  Memory, 
  empty, size,
  new,
  refresh, locate, modify, store,
  )
       where

import qualified Data.IntMap.Strict as M

-- The following is inevitable if we emulate memory
import Unsafe.Coerce (unsafeCoerce)


-- Address in memory
type Address = Int
-- A reference box -- what is stored in memory
-- The data constructor is not exported!
data Ref a = Ref{dref :: a, addr :: !Address}
modify_ref :: a -> Ref a -> Ref a
modify_ref x r = r{dref = x}

instance Show a => Show (Ref a) where
  show Ref{..} = "@"++show addr++" "++show dref
  
-- Actually, Memory stores ARefs, existentialized references
-- ARef is parameterized by some constructor, so we can have
-- some information about the stored value.
data ARef c where ARef :: Ref (c a) -> ARef c

data Memory c = Memory {
  refs    :: M.IntMap (ARef c),
  count   :: !Address              -- for generating new addresses
  }

empty :: Memory c
empty = Memory M.empty 0

size :: Memory c -> Int
size Memory{refs} = M.size refs

-- Allocate a new reference
new :: c a -> Memory c -> (Ref (c a), Memory c)
new x Memory{..} =
  let count' = count + 1
      ref = Ref x count'
  in (ref, Memory (M.insert count' (ARef ref) refs) count')

-- The lookup function, which checks the memory invariants

locate :: Address -> Memory c -> (forall a. Ref (c a) -> w) -> w 
locate addr Memory{refs} k =
  case M.lookup addr refs of
    Just (ARef r@(Ref _ addr')) | addr == addr' -> k r
    Just _ -> error $ "memory corruption! Stored address is not equal"++
                      "to ref address, for address: " ++ show addr
    Nothing -> error $ "Unable to locate reference at address: " ++
                       show addr

-- Given a (Ref a), update its contents from the one
-- stored in memory.
-- unsafeCoerce is unescapable: we emulate mutable cells
-- Refs with the same address have the same type
-- The coersion is not totally unsafe: addresses must match

refresh :: Ref (c a) -> Memory c -> Ref (c a)
refresh (Ref _ addr) mem =
  locate addr mem (\(x:: Ref (c x)) -> unsafeCoerce x)

-- Modify a reference cell
modify :: Address -> Memory c -> (forall a. c a -> c a) -> Memory c
modify addr mem@Memory{refs} f = mem{refs = M.adjust modif addr refs}
 where
   modif (ARef (Ref x addr)) = ARef (Ref (f x) addr)

-- Returns the previous contents of (Ref c a) in memory, which
-- must exist
store :: Ref (c a) -> Memory c -> (Ref (c a), Memory c)
store r@(Ref _ addr) mem@Memory{refs} =
  case M.insertLookupWithKey (\_ x _ -> x) addr (ARef r) refs of
    (Just (ARef rold@(Ref _ addr')),refs') | addr == addr' ->
      (unsafeCoerce rold, mem{refs=refs'})


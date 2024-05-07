{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CNGen where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Fail
import Test.QuickCheck

-------------------------------------------------------------------
-- Concrete Heaps

newtype Heap = Heap (Map Int Int)
               deriving Show

-- The C monad (for manipulating concrete heaps)

newtype C a = C (MaybeT (State Heap) a)
              deriving (Functor, Applicative, Monad)

runC :: C a -> Heap -> (Maybe a, Heap)
runC (C c) h = runState (runMaybeT c) h

failC :: C a
failC = C $ MaybeT $ do
  return Nothing

deref :: Int -> C Int
deref 0 = failC
deref p = C $ do
  Heap h <- get
  return (h Map.! p)

store :: Int -> Int -> C ()
store 0 _ = failC
store p n = C $ do
  Heap h <- get
  put (Heap (Map.insert p n h))
  return ()

-- find a place to allocate n words and return a pointer to it
-- (simple version!)
findSpaceFor :: Int -> C Int
findSpaceFor n = C $ do
  Heap h <- get
  return (maxKey h + 1)

  where maxKey h = safeHead $ reverse $ Map.keys h
        safeHead l = case uncons l of
                       Nothing -> (0 :: Int)
                       Just (p,_) -> p

-- allocate n words, initialize, and return a pointer to the first one
malloc :: Int -> C Int
malloc n = do
  p <- findSpaceFor n
  mapM_ (\p' -> store p' 0) [p .. p+n-1]
  return p


-------------------------------------------------------------------
-- The CN monad (for evaluating SL predicates on concrete heaps)

newtype CNRes a = CNRes {unCNRes :: StateT (Set Int) Maybe a}
        deriving (Functor, Applicative, Monad)

instance Show a => Show (CNRes a) where
  show = show . runCNRes

runCNRes :: CNRes a -> Maybe (a, Set Int)
runCNRes (CNRes m) = runStateT m Set.empty

own :: Int -> CNRes ()
own l = CNRes $ do
  mine <- get
  if Set.member l mine then lift Nothing
                       else put $ Set.insert l mine

newtype CN a = CN {unCN :: ReaderT Heap CNRes a}
  deriving (Functor, Applicative, Monad)

runCN :: CN a -> Heap -> Maybe (Set Int, a)
runCN (CN r) (Heap h) =
  case runCNRes (runReaderT r (Heap h)) of
    Nothing -> Nothing
    Just (a,locs) | Map.keysSet h == locs -> Just (locs,a)
                  | otherwise -> Nothing

failCN :: CN a
failCN = CN (lift (CNRes (lift Nothing)))

owned :: Int -> CN Int
owned l = CN $ do
  Heap heap <- ask
  case Map.lookup l heap of
    Just v -> lift $ do own l
                        return v
    Nothing -> unCN failCN

-------------------------------------------------------------------
-- Symbolic heaps

newtype SAddr = SAddr Int
               deriving (Eq, Ord, Show)

newSAddr (SAddr a) = SAddr (a+1)

data SLoc = SLoc {base :: SAddr,
                  offset :: Int,
                  validOffsets :: Int} -- do we need this?
            deriving (Show, Eq, Ord)

type SVal = Either SLoc Int

data SHeap = SHeap {contents :: Map SLoc SVal,
                    freePtr :: SAddr}
            deriving (Show, Eq, Ord)

smalloc :: [SVal] -> SHeap -> (SVal,SHeap)
smalloc svs s = (Left (SLoc p 0 (length svs)), newSHeap)
  where p = freePtr s
        newSLocs = zip [ SLoc p i (length svs) | i <- [0..] ] svs
        newMap = Map.union (contents s) (Map.fromList newSLocs)
        newSHeap = SHeap newMap (newSAddr p)

-- Generating symbolic heaps

newtype SHeapBuilder a = SHeapBuilder (State SHeap a)
                         deriving (Functor, Applicative, Monad)

runSHeapBuilder :: SHeapBuilder a -> (a, SHeap)
runSHeapBuilder (SHeapBuilder m) =
  runState m (SHeap Map.empty (SAddr 1))

alloc svs = SHeapBuilder . state $ smalloc svs

intFromSLoc :: Map SAddr Int -> SLoc -> Int
intFromSLoc am r = am Map.! (base r) + offset r

intFromSVal :: Map SAddr Int -> SVal -> Int
intFromSVal _ (Right n) = n
intFromSVal am (Left p) = intFromSLoc am p

-- (Plus a specific sheap builder for each datatype used in specs)

-----------------------------------------------------------------------
-- Translating symbolic heaps to (generators for) concrete heaps

{-
  The concretize function takes a symbolic heap (and a list of
  symbolic locations, which is currently not used and whose purpose is
  not 100% clear to BCP: it was something to do with the possibility
  of pointers from the "current" heap to other parts of memory that
  are not in the domain of the current heap).

  It produces a generator for random concrete heaps (plus associated
  maps from symbolic to concrete addresses).
-}
concretize :: SHeap -> [SLoc] -> Gen (Heap, Map SAddr Int)
concretize s slocs = do
  sAddrsAndSizes <- allSAddrsAndSizes
  gaps <- sequence [ oneof [return 0, choose (1,10)] | _ <- sAddrsAndSizes ]
  let sAddrMap = addresses 1 gaps sAddrsAndSizes
  -- slocMap :: SLoc -> Int
  let slocMap sloc =  (sAddrMap Map.! base sloc) + offset sloc
  let SHeap h _ = s
  let  h' = Map.fromList
              [ (slocMap k, either slocMap id v) | (k,v) <- Map.toList h ]
  return (Heap h', sAddrMap)

  where
    allSAddrsAndSizes =
      shuffle $
      nubOrd . map baseAndSize $
      slocs ++ [sl | Left sl <- Map.elems (contents s)]
            ++ Map.keys (contents s)

    baseAndSize sl = (base sl, validOffsets sl)

    addresses _ [] [] = Map.empty
    addresses p (g:gaps) ((a,n):rest) =
      Map.insert a (p+g) (addresses (p+g+n) gaps rest)

    nubOrd = map head . group . sort

-------------------------------------------------------------------
-- Testing

-- Pointer fiddling

swapC :: Int -> Int -> C ()
swapC p q = do
  pv <- deref p
  qv <- deref q
  store p qv
  store q pv

prop_swapC :: Int -> Int -> Property
prop_swapC m n =
  forAll (concretize sh []) $ \(h, am) ->
    let pn = intFromSVal am spn
        qn = intFromSVal am sqn
        (Just (), Heap h') = runC (swapC pn qn) h in
    runCN (liftM2 (,) (owned pn) (owned qn)) (Heap h') == Just (Map.keysSet h', (n,m))

  where ((spn,sqn), sh) = runSHeapBuilder $ do
          p <- alloc [Right m]; q <- alloc [Right n]; return (p,q)

incr2C :: Int -> Int -> C ()
incr2C p q = do
  pv <- deref p
  store p (pv+1)
  qv <- deref q
  store q (qv+1)

bothOwned :: Int -> Int -> CN (Int,Int)
bothOwned p q | p == q = do pv <- owned p; return (pv,pv)
              | otherwise = do pv <- owned p; qv <- owned q; return (pv,qv)

prop_incr2C :: (Either Int (Int,Int)) -> Property
prop_incr2C tc =
  forAll (concretize sh []) $ \(h, am) ->
    let pn = intFromSVal am spn
        qn = intFromSVal am sqn
        (Just (), Heap h') = runC (do incr2C pn qn; incr2C pn pn) h in
    counterexample (show (Heap h')) $
    runCN (bothOwned pn qn) (Heap h') == Just (Map.keysSet h', result)

  where ((spn,sqn), sh) =
           runSHeapBuilder $
           case tc of
             Left n -> do p <- alloc [Right n]; return (p,p)
             Right (m,n) -> do p <- alloc [Right m]; q <- alloc [Right n]; return (p,q)
        result =
           case tc of
             Left n -> (n+4,n+4)
             Right (m,n) -> (m+3,n+1)

-- Lists

intList :: Int -> CN [Int]
intList 0 = return []
intList p = do
  hd <- owned p
  q <- owned (p+1)
  tl <- intList q
  return (hd:tl)

intListSHeapBuilder :: [Int] -> SHeapBuilder SVal
intListSHeapBuilder [] = return (Right 0)
intListSHeapBuilder (n:ns) = do
  p <- intListSHeapBuilder ns
  alloc [Right n, p]

prop_intListSHeapBuilder :: [Int] -> Property
prop_intListSHeapBuilder ns =
  forAll (concretize sh []) $ \(Heap h, am) ->
    let p = intFromSVal am sv in
    runCN (intList p) (Heap h) == Just (Map.keysSet h, ns)
  where (sv, sh) = runSHeapBuilder (intListSHeapBuilder ns)

revC :: Int -> C Int
revC 0 = pure 0
revC p = revC' 0 p
  where revC' left 0 = pure left
        revC' left q = do
          q' <- deref (q+1)
          store (q+1) left
          revC' q q'

prop_revC :: [Int] -> Property
prop_revC ns =
  forAll (concretize sh []) $ \(h, am) ->
    let p = intFromSVal am sv
        (Just p', Heap h') = runC (revC p) h in
    runCN (intList p') (Heap h') == Just (Map.keysSet h', reverse ns)

  where (sv, sh) = runSHeapBuilder (intListSHeapBuilder ns)

appendC :: Int -> Int -> C Int
appendC 0 q = pure q
appendC p q = do
  p' <- deref (p+1)
  if p' == 0 then store (p+1) q
             else void $ appendC p' q
  return p

prop_appendC :: [Int] -> [Int] -> Property
prop_appendC ms ns =
  forAll (concretize sh []) $ \(h, am) ->
    let pm = intFromSVal am svm
        pn = intFromSVal am svn
        (Just p', Heap h') = runC (appendC pm pn) h in
    runCN (intList p') (Heap h') == Just (Map.keysSet h', ms ++ ns)

  where ((svm,svn), sh) = runSHeapBuilder (liftM2 (,) (intListSHeapBuilder ms) (intListSHeapBuilder ns))

prop_appendC_aliased :: [Int] -> Property
prop_appendC_aliased ns =
  forAll (concretize sh []) $ \(h, am) ->
    let pn = intFromSVal am svn
        (Just p', Heap h') = runC (appendC pn pn) h in
    runCN (intList p') (Heap h') == Just (Map.keysSet h', ns ++ ns)

  where (svn, sh) = runSHeapBuilder (intListSHeapBuilder ns)

tailC :: Int -> C Int
tailC 0 = undefined
tailC p = deref (p+1)

prop_tailC :: [Int] -> Property
prop_tailC ns =
  not (null ns) ==>
  forAll (concretize sh []) $ \(h, am) ->
    let pn = intFromSVal am svn
        (Just p', Heap h') = runC (tailC pn) h in
    runCN (intList p') (Heap h') == Just (Map.keysSet h', tail ns)

  where (svn, sh) = runSHeapBuilder (intListSHeapBuilder ns)

-- Queues

-- Every queue is represented by a pointer to a two-word structure.
-- If the queue is empty, then both words in this structure contain
-- null.  If non-empty, then the first word points to the head cell of
-- the queue, second to the tail.  Each cell contains a number and a
-- pointer to the next cell (or null).

intQueue :: Int -> CN [Int]
intQueue 0 = failCN
intQueue p = do
  hd <- owned p
  tl <- owned (p+1)
  case (hd,tl) of
    (0,0) -> return []
    (_,0) -> failCN
    (0,_) -> failCN
    _ -> intQueueFromTo hd tl

intQueueFromTo :: Int -> Int -> CN [Int]
intQueueFromTo h last = do
  hv <- owned h
  next <- owned (h+1)
  if h == last then do
    if next /= 0 then failCN
                 else return [hv]
  else do
    tv <- intQueueFromTo next last
    return (hv:tv)

intQueueSHeapBuilder :: [Int] -> SHeapBuilder SVal
intQueueSHeapBuilder [] = do
  p' <- alloc [Right 0, Right 0]
  return p'
intQueueSHeapBuilder (n:ns) = do
  (p,q) <- intQueueFromToSHeapBuilder (n:ns)
  p' <- alloc [p, q]
  return p'

intQueueFromToSHeapBuilder :: [Int] -> SHeapBuilder (SVal,SVal)
intQueueFromToSHeapBuilder (n:ns) = do
  if ns /= [] then do
    (p,q) <- intQueueFromToSHeapBuilder ns
    p' <- alloc [Right n, p]
    return (p',q)
  else do
    p' <- alloc [Right n, Right 0]
    return (p',p')

prop_intQueueSHeapBuilder :: [Int] -> Property
prop_intQueueSHeapBuilder ns =
  forAll (concretize sh []) $ \(Heap h, am) ->
    let p = intFromSVal am sv in
    runCN (intQueue p) (Heap h) === Just (Map.keysSet h, ns)
  where (sv, sh) = runSHeapBuilder (intQueueSHeapBuilder ns)

pushC :: Int -> Int -> C ()
pushC 0 _ = failC
pushC p i = do
  h <- deref p
  t <- deref $ p+1
  c <- malloc 2
  store c i
  store (c+1) h
  store p c
  if t == 0
    then store (p+1) c
    else return ()

prop_pushC :: [Int] -> Int -> Property
prop_pushC ns i =
  forAll (concretize sh []) $ \(h, am) ->
    let pn = intFromSVal am svn
        (r, Heap h') = runC (pushC pn i) h in
    counterexample ("h = " ++ show h) $
    counterexample ("pn = " ++ show pn) $
    counterexample ("r = " ++ show r) $
    counterexample ("h' = " ++ show h') $
    case r of
      Nothing -> False
      Just () -> runCN (intQueue pn) (Heap h')
                 == Just (Map.keysSet h', [i] ++ ns)

  where (svn, sh) = runSHeapBuilder (intQueueSHeapBuilder ns)

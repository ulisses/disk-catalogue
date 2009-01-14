--{-#OPTIONS -fglasgow-exts -O2 -fvia-C#-}
--{-#OPTIONS -fwarn-orphans -fwarn-unused-matches -fwarn-unused-imports#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Cd2
-- Copyright   :  (c) Ulisses Araujo Costa 2007
-- License     :  a ver ainda...
--
-- Maintainer  :  ulissesmonhecosta@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module illustrates & tests most of the features of Haddock.
-- Testing references from the description: 'T', 'f', 'g', 'Visible.visible'.
--
-----------------------------------------------------------------------------
module Cd2 where

import Mpi
import System.Time
import System.IO
import NewBinary.Binary as Bin

data Disco a = Ficheiro a
             | Pasta    a [Disco a]
    deriving (Eq , Ord , Read)

instance Functor Disco where
    fmap f = catadisco $ indisco . recdisco f id

instance Show a => Show (Disco a) where
    show = showDisco

-- | Converts a 'Disco' a to a 'String'.
--
-- Na integra o que estava na biblioteca "Data.Tree"
showDisco :: Show a => Disco a -> String
{-# SPECIALIZE INLINE showDisco :: Disco Data   -> String #-}
{-# SPECIALIZE INLINE showDisco :: Disco String -> String #-}
{-# SPECIALIZE INLINE showDisco :: Disco Int -> String #-}
{-# SPECIALIZE INLINE showDisco :: Disco (String,Int) -> String #-}
showDisco  = unlines . drawDisco . fmap show
    where
    drawDisco :: Disco String -> [String]
    drawDisco (Ficheiro a) = [a]
    drawDisco (Pasta p ts0) = grp this (space (Prelude.length this)) (stLoop ts0)
        where this          = s1 ++ p ++ " "

              space n       = replicate n ' '

              stLoop []     = [""]
              stLoop [t]    = grp s2 "  " (drawDisco t)
              stLoop (t:ts) = grp s3 s4 (drawDisco t) ++ [s4] ++ rsLoop ts

              rsLoop []     = error "rsLoop:Unexpected empty list."
              rsLoop [t]    = grp s5 "  " (drawDisco t)
              rsLoop (t:ts) = grp s6 s4 (drawDisco t) ++ [s4] ++ rsLoop ts

              grp fst0 rst  = zipWith (++) (fst0:repeat rst)

              [s1,s2,s3,s4,s5,s6] = ["- ", "--", "-+", " |", " `", " +"]

instance Binary a => Binary (Disco a) where
    put_ hb (Ficheiro a) = do put_ hb (1 :: Int) ; put_ hb a
    put_ hb (Pasta a l) = do put_ hb (2 :: Int) ; put_ hb a ; put_ hb (3 :: Int) ;  put_ hb l

    get bh = do
        control <- (Bin.get bh :: IO Int)
        case control of
            1 -> do
                f <- Bin.get bh
                return $ Ficheiro f
            2 -> do
                p <- Bin.get bh
                3 <- (Bin.get bh :: IO Int)
                l <- Bin.get bh
                return $ Pasta p l

data Data = Data { name         :: String
                 , size         :: Integer
                 , creationTime :: DataSec
                 , tipo         :: String
                 }
          | DataNull
    deriving (Eq , Ord , Show , Read)

instance Binary Data where
    put_ bh (Data {name = n, Cd2.size = s, creationTime = c, tipo = t})
        = do put_ bh (1 :: Int) ; put_ bh n ; put_ bh s ; put_ bh c ; put_ bh t
    put_ bh DataNull = put_ bh (0 :: Int)
    get  bh = do
        control <- (Bin.get bh :: IO Int)
        case control of
            1 -> do
                n <- Bin.get bh
                s <- Bin.get bh
                c <- Bin.get bh
                t <- Bin.get bh
                return $ Data n s c t
            0 -> return DataNull

type DataSec = Int

getIntTime :: ClockTime -> DataSec
getIntTime fileTime = tdSec $ diffClockTimes fileTime $ TOD 0 0

putTime :: DataSec -> ClockTime
putTime fileClockTime = TOD (toInteger fileClockTime) 0

----------------------------------------------------------------------
-- material para listas
----------------------------------------------------------------------

catalist :: (Either () (a,c) -> c) -> [a] -> c
catalist g   = g . reclist (catalist g) . outlist

analist :: (a -> Either b (c,a)) -> a -> [c]
analist h = inlist . reclist (analist h) . h

hylolist :: (Either () (c,c) -> c) -> (a -> Either b (c,a)) -> a -> c
hylolist f g = catalist f . analist g

reclist :: (c -> d) -> Either a (b,c) -> Either a (b,d)
reclist g = id -|- id >< g

outlist :: [a] -> Either () (a,[a])
outlist []    = i1 ()
outlist (a:x) = i2(a,x)

inlist :: Either a (b,[b]) -> [b]
inlist = either (const []) (uncurry (:))

----------------------------------------------------------------------
-- material para o tipo @Disco a@
----------------------------------------------------------------------
catadisco :: (Either a (a,[c]) -> c) -> Disco a -> c
{-# SPECIALIZE INLINE catadisco :: (Either Data (Data,[Disco Data]) -> Disco Data) -> Disco Data -> Disco Data #-}
catadisco f = f . recdisco id (catadisco f) . outdisco

anadisco :: (a -> Either a (a,[a])) -> a -> Disco a
anadisco h = indisco . (recdisco id (anadisco h)) . h

hylodisco :: (Either a (a,[c]) -> c ) -> (a ->  Either a (a,[a])) -> a -> c
hylodisco f g = catadisco f . anadisco g

recdisco :: (a -> b) -> (c -> d) -> Either a (a,[c]) -> Either b (b,[d])
recdisco f g = f -|- f >< map g

outdisco :: Disco a -> Either a (a,[Disco a])
{-# SPECIALIZE INLINE outdisco :: Disco Data -> Either Data (Data,[Disco Data]) #-}
outdisco (Ficheiro a) = i1 a
outdisco (Pasta a l)  = i2 (a,l)

indisco :: Either a (a,[Disco a]) -> Disco a
{-# SPECIALIZE INLINE indisco :: Either Data (Data,[Disco Data]) -> Disco Data #-}
indisco = either Ficheiro (uncurry Pasta)

----------------------------------------------------------------------
-- funcoes de estatistica
----------------------------------------------------------------------
-- consta o numero de ficheiros
nrfiles :: Disco a -> Integer
{-# SPECIALIZE INLINE nrfiles :: Disco Data -> Integer #-}
{-# SPECIALIZE INLINE nrfiles :: Disco String -> Integer #-}
{-# SPECIALIZE INLINE nrfiles :: Disco Int -> Integer #-}
nrfiles = catadisco $ either (const 1) (sum . p2)

-- conta o numero de pastas
nrpastas :: Disco a -> Integer
{-# SPECIALIZE INLINE nrpastas :: Disco Data -> Integer #-}
{-# SPECIALIZE INLINE nrpastas :: Disco String -> Integer #-}
{-# SPECIALIZE INLINE nrpastas :: Disco Int -> Integer #-}
nrpastas = catadisco $ either (const 0) (\(_,l) -> 1 + sum l)

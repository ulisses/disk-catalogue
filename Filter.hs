{-#OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
--{-#OPTIONS -fwarn-orphans -fwarn-unused-matches -fwarn-unused-imports#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Filter
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
module Filter ( SubString
              , filterDisco
              , contain
              ) where

import List (elemIndices)
import Cd2 (Disco(..), Data(..))

--
-- | Type SubString
--
-- 'SubString' is used in /SPECIALIZATION/ of function 'contain'.
--
type SubString = String

------------------------------------------------------------------------
--
-- | > SPECIALIZE filterDisco :: [Data -> Bool] -> [Disco Data] -> [Disco Data]
--
filterDisco :: [a -> Bool] -- ^ List of functions
            -> [Disco a]   -- ^ to apply to 'Disco'
            -> [Disco a]   -- ^ return the \"filtered\" 'Disco'.
{-# SPECIALIZE INLINE filterDisco :: [Data -> Bool] -> [Disco Data] -> [Disco Data] #-}
filterDisco _ []               = []
filterDisco f ((Ficheiro a):t) | res f a   = (Ficheiro a) : filterDisco f t
                               | otherwise = filterDisco f t
filterDisco f ((Pasta a l):t)  = let fl    = filterDisco f l
                                     ft    = filterDisco f t
                                     res_  = (Pasta a fl) : ft
                                 in if res f a then res_
                                    else if (null fl) then ft
                                         else res_

res :: [a -> Bool] -> a -> Bool
{-# SPECIALIZE INLINE res :: [Data -> Bool] -> Data -> Bool #-}
res l = and . zipWith ($) l . iterate id

--
-- | > SPECIALIZE contain :: SubString -> String -> Bool
--
-- Verify if a 'SubString' is in a String:
--
-- > contain "my" "hello my friend!" = True
--
-- > contain "kell" "must love haskell ;)" = True
--
-- It's an important function used by 'Parser.Search'
--
contain :: (Eq [a] , Eq a) => [a] -> [a] -> Bool
{-# SPECIALIZE INLINE contain :: SubString -> String -> Bool #-}
contain s1 s2 | null s1 || null s2 = False
contain s1@(h:_) s2 =
    let len   = length s1
        elems = elemIndices h s2
    in  (or . map (==s1)) [take len $ drop n s2 | n <- elems ]

------------------------------------------------------------------------

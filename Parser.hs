--{-#OPTIONS -fglasgow-exts -fallow-undecidable-instances -O2 -fvia-C#-}
--{-#OPTIONS -fwarn-orphans -fwarn-unused-matches -fwarn-unused-imports#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Parser
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
module Parser where

import List
import Char
import Cd2
import Mpi

-- | um comando... podemos dar / enfase / a uma palavra...
type Comando = String

-- | comentario de um data...
data Flag = Scan FilePath
          | Save FilePath
          | Search String [S]
          | ShowTreeNames
          | ShowTreeSizes
          | ShowTreeCreationTime
          | ShowTreeAll
          | Exit
          | Version
          | Help
    deriving (Show, Eq , Ord, Read)

--
-- make parsing of all expressions
--
-- parseTot :: ReadS Flag
-- parseTot s =
--        [(Scan fp , res) | (flag,p1) <- lex s , tolower flag == "scan" , (fp,res) <- lex p1 ]
--     ++ [(Save fp , res) | (flag,p1) <- lex s , tolower flag == "save" , (fp,res) <- lex p1 ]
--     ++ [(Search nome (nub $ sort $ opt) , res)
--            | (flag,p1)    <- lex s
--            , tolower flag == "search"
--            , (nome,p2)    <- lex p1
--            , (opt , res)  <- readS p2
--        ]
--     ++ [(ShowTreeNames , res) | (flag,res) <- lex s , tolower flag == "showtreenames" ]
--     ++ [(ShowTreeNames , res) | (flag,res) <- lex s , tolower flag == "showtreesizes" ]
--     ++ [(ShowTreeNames , res) | (flag,res) <- lex s , tolower flag == "showtreecreationtime" ]
--     ++ [(ShowTreeNames , res) | (flag,res) <- lex s , tolower flag == "showtreeall" ]
--     ++ [(ShowTreeNames , res) | (flag,res) <- lex s , tolower flag == "exit" ]
--     ++ [(ShowTreeNames , res) | (flag,res) <- lex s , tolower flag == "quit" ]
--     ++ [(ShowTreeNames , res) | (flag,res) <- lex s , tolower flag == "showtreenames" ]
--     ++ [(ShowTreeNames , res) | (flag,res) <- lex s , tolower flag == "showtreenames" ]
--

tolower = map toLower


parseSearch :: ReadS Flag
parseSearch s =
    [(Search nome (nub $ sort $ opt) , res)
        | (flag,p1)    <- lex s
        , tolower flag == "search"
        , (nome,p2)    <- lex p1
        , (opt , res)  <- readS p2
    ]
    where tolower = map toLower

--
-- save file -- Save String
-- search name [Ms tamanho] [ms tamanho] [t type] [Md date] [md date]
--                 ^               ^         ^          ^        ^
--                >=              <=       tipo      date max date min
--

type FlagSearch = String

data S = S :^: S
       | MsMaior String
       | MsMenor String
       | T String
       | MdMaior String
       | MdMenor String

instance Read S where
    readsPrec _ s = let [( l , res)] = readS s
                    in  [(foldr1 (:^:) l,res)]
    readList s    = readS s

readS :: ReadS [S]
readS [] = [([],[])]
readS s  = [ ( format flag opt tot, rrr )
                       | (flag , p1)  <- lex s
                       , flag `elem`  notMd
                       , (opt , res)  <- lex p1
                       , (tot , rrr)  <- readS res
           ]
        ++ [ ( format flag (concat $ intersperse " " [dia,mes,ano]) tot, rr)
                       | (flag , p1)  <- lex s
                       , flag `elem`  yesMd
                       , (dia , res)  <- lex p1
                       , (mes , res1) <- lex res
                       , (ano , res2) <- lex res1
                       , (tot , rr)   <- readS res2
           ]
    where
    notMd , yesMd :: [FlagSearch]

    notMd = filter (uncurry (&&) . split (/="md") (/="Md")) flags

    yesMd = flags \\ notMd

    format :: String -> String -> [S] -> [S]
    format flag s res =
        (case flag of
            "Ms" -> MsMaior
            "ms" -> MsMenor
            "t"  -> T
            "Md" -> MdMaior
            "md" -> MdMenor
        ) s : res

instance Eq S where
    (==) a b = procura a == procura b

instance Ord S where
    compare a b = procura a `compare` procura b

procura :: Show a => a -> [Int]
{-# SPECIALIZE INLINE procura :: S -> [Int] #-}
procura = flip elemIndices flags . head . words . show

flags :: [FlagSearch]
flags = ["Ms" , "ms" , "t" , "Md" , "md"]

instance Show S where
    show (a :^: b) = show a ++ " :^: " ++ show b

    show (MsMaior m) = "Ms " ++ show m
    show (MsMenor m) = "ms " ++ show m

    show (T t) = "t " ++ show t

    show (MdMaior d) = "Md " ++ show d
    show (MdMenor d) = "md " ++ show d

--{-#OPTIONS -fglasgow-exts -fallow-undecidable-instances -O2 -fvia-C#-}
--{-#OPTIONS -fwarn-orphans -fwarn-unused-matches -fwarn-unused-imports#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
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
module Main where

import Filter
import Cd2 --(size)
import Parser
import Mpi (split,cond,(!))
import System.Directory ( doesFileExist, getDirectoryContents, getModificationTime , getHomeDirectory)
import System.IO as IO ( openBinaryFile , hFileSize , hClose , IOMode(..) , openFile)
import System.IO.Error as Error (try , ioeGetFileName , ioeGetErrorType)
import GHC.IOBase
import System
import System.Time (getClockTime)
import Maybe (fromJust , fromMaybe)
import List as L (elemIndices)
import System.Console.Readline (readline , initialize , setCompletionEntryFunction , setCompletionAppendCharacter , restorePrompt ,
                                completionMatches , addHistory
                               )
import System.Exit (exitWith)
import Data.Set as Set (Set(..) , toList , map , empty , insert , fromList)
import Control.Monad.State as ST (evalStateT , StateT(..) , get , put , lift)
import qualified NewBinary.Binary as Bin (openBinIO_ , put_ , get)

-- | Um atalho para /following/ declaration
-- and it continues until the next non-comment line
type Type = String

database , errorFile :: FilePath
database  = (unsafePerformIO getHomeDirectory) ++ "/.cd/.dados"
errorFile = (unsafePerformIO getHomeDirectory) ++ "/.cd/info.log"

writeDataBase :: Set (Disco Data) -> IO()
writeDataBase set = do
    let listaSet = Set.toList set
    h  <- openFile database WriteMode
    hb <- Bin.openBinIO_ h
    Bin.put_ hb listaSet
    hClose h

readDataBase :: IO (Set (Disco Data))
readDataBase = do
    h          <- openFile database ReadMode
    hb         <- Bin.openBinIO_ h
    lDiscoData <- Bin.get hb
    hClose h
    if lDiscoData ==  []
      then return Set.empty
      else return $ Set.fromList lDiscoData

exitDCWithSuccess :: IO a
exitDCWithSuccess = putStrLn "Leaving DC." >> exitWith ExitSuccess

putStrLnTreeFunction :: (Ord a , Show a) => (Data -> a) -> Set (Disco Data) -> IO()
{-# SPECIALIZE putStrLnTreeFunction :: (Data -> String) -> Set (Disco Data) -> IO() #-}
{-# SPECIALIZE putStrLnTreeFunction :: (Data -> Int) -> Set (Disco Data) -> IO() #-}
putStrLnTreeFunction f = putStrLn . unwords . toList . Set.map (show . fmap f)

main :: IO()
main = do
    t <- doesFileExist database
    putStrLn ("a ler o ficheiro " ++ database ++ " ...")
    let run = readDataBase >>= evalStateT kernel
    (case t of
        True  -> run
        False -> writeDataBase Set.empty >> run)

functionCompletion :: String -> IO [String]
functionCompletion s = do
    let tkn = Prelude.take $ Prelude.length s
    let d   = Prelude.map tkn commandsList
    return [ commandsList !! n | n <- elemIndices s d ]

--
-- | Type of a SearchCommand
--
-- 'SearchCommand' means:
--
-- * \"search hello t hs\" /(finding all files that the name include the word \"hello\" and the type is \".hs\")/
--
-- * \"search hello t lhs ms 40\" /(finding all files that the name include the word \"hello\", the type is \".lhs\" and
-- the size is lesser or equal than 40bytes)/
--
-- * etc...
--
type SearchCommand = String

--
-- | Type of a SingleCommand
--
-- 'SingleCommand' means:
--
-- * \"showTreeNames\"
--
-- * \"showTreeSizes\"
--
-- * etc...
--
type SingleCommand = String

--
-- | The /relation/ between 'SingleCommand' and the result
-- of that's commands
--
cf :: SearchCommand -> Set (Disco Data) -> [(SingleCommand , IO())]
cf command tree =
    [ ("showTreeNames"        , putStrLnTreeFunction name tree)
    , ("showTreeSizes"        , putStrLnTreeFunction (Mpi.split name Cd2.size) tree)
    , ("showTreeCreationTime" , putStrLnTreeFunction (Mpi.split name (putTime . creationTime)) tree)
    , ("showTreeAll"          , putStrLnTreeFunction id tree)
    , ("search"               , putStrLnTreeSearch command tree)
    , ("add"                  , return())
    , ("exit"                 , exitDCWithSuccess)
    , ("quit"                 , exitDCWithSuccess)
    , ("version"              , exitDCWithSuccess)
    , ("help"                 , exitDCWithSuccess)
    ]

--
-- | Show /all/ the tree in a tuple form ('name','tipo').
--
-- This function use 'showDisco', see that for more information
-- about the output.
--
putStrLnTreeSearch :: SearchCommand -> Set (Disco Data) -> IO()
putStrLnTreeSearch command =
      putStrLn
    . unlines
    . Prelude.map show
    . toList
    . (Set.map . fmap) (Mpi.split name tipo)
    . Set.map (execSearch command)

--
-- | The list of available commands
--
commandsList :: [SingleCommand]
commandsList = Prelude.map fst $ cf [] Set.empty

--
-- | The list of available functions
--
functionsList :: Set (Disco Data) -> [IO()]
functionsList = Prelude.map snd . cf []

--
-- | Parse and execute a 'Search' command for a 'Disco' 'Data'.
--
-- Returns a filtered 'Disco' 'Data' cf. the given command
--
-- If the command does not respect the syntax then return:
-- Pasta a [], the emptyed root directory
--
execSearch :: SearchCommand -> Disco Data -> Disco Data
execSearch c (Pasta n l) = Pasta n $ execSearch_ c l
    where
    execSearch_ :: SearchCommand -> [Disco Data] -> [Disco Data]
    execSearch_ s d =
        case parseSearch s of
            [] -> []
            [(Search nome l , _)] -> filterDisco (contain nome . name : getFuns l) d
        where
        getFuns :: [S] -> [Data -> Bool]
        getFuns []               = []
        getFuns ((T t):t')       = ((==t) . tipo)                       : getFuns t'
        getFuns ((MsMaior o):t') = ((>=(read o :: Integer)) . Cd2.size) : getFuns t'
        getFuns ((MsMenor o):t') = ((<=(read o :: Integer)) . Cd2.size) : getFuns t'
        --getFuns ((MdMaior o):t') = ((>=(read o :: ClockTime)) . Cd2.creationTime) : getFuns t'
        --getFuns ((MdMenor o):t') = ((<=(read o :: ClockTime)) . Cd2.creationTime) : getFuns t'

--
-- | Execute a 'Comando' from the stdin.
--
-- This function find the command from 'commandsList' and pick what function
-- showld execute from 'functionsList' using the 'cf' function
--
executeSingleCommand :: SearchCommand -> SingleCommand -> Set (Disco Data) -> IO()
executeSingleCommand cmd str fp = sequence_ [ c | (s,c) <- cf cmd fp , str == s ]

--
-- | Our kernel!
--
-- With 'Monad' 'StateT' where our state is 'Set' ('Disco' 'Data').
--
-- Using the 'readline' and other's from "System.Console.Readline"
--
kernel :: StateT (Set (Disco Data)) IO ()
kernel = do
    lift $ initialize
    lift $ setCompletionEntryFunction $ Just $ functionCompletion
    lift $ setCompletionAppendCharacter Nothing
    r <- lift $ readline "[DiskCataloger]: "
    case r of
        {- ControlD -}
        Nothing  -> lift $ (restorePrompt >> exitDCWithSuccess)
        (Just s) -> do
            c <- lift $ completionMatches s functionCompletion
            case c of
                Nothing      -> do let (a:b) = words s
                                   tree <- ST.get
                                   case a `elem` commandsList of
                                       False -> do lift $ putStrLn ("funcao " ++ a ++ " nao existe!")
                                                   kernel
                                       True  -> do
                                           case a of
                                               "add" -> do
                                                   path <- lift $ scan_ (b !! 0) -- possible error... (!!) only at 0...
                                                   case path of
                                                       (Pasta DataNull _) -> do kernel -- BUGBUG never happends...
                                                       _                  -> do let newSet = Set.insert path tree
                                                                                ST.put $ newSet
                                                                                lift $ writeDataBase newSet
                                                                                lift $ putStrLn
                                                                                     $ uncurry (++)
                                                                                     $ Mpi.split (("Ficheiros: "++) . show . nrfiles)
                                                                                                 ((" | Pastas: "++) . show . nrpastas)
                                                                                                 path
                                                                                lift $ addHistory s
                                                                                kernel
                                               _     -> do lift $ executeSingleCommand s a tree
                                                           lift $ addHistory s
                                                           kernel
                (Just (p,l)) -> case Prelude.null l of
                    {- Enter -}
                    False -> kernel
                    {- we find the desired command-}
                    True  -> do tree <- ST.get
                                lift $ executeSingleCommand s p tree
                                lift $ addHistory p
                                kernel
    lift $ restorePrompt
    return()

-----------------------------------------------------------------
--
-- | Is the responsable for find files recursively in any
-- valid directory @d@ and assemble the information in data
-- 'Disco' 'Data', our type...
--
scan_ :: FilePath -> IO (Disco Data)
scan_ f = scan f >>= return . fmap (fromMaybe DataNull)

--
-- | Is the responsable for find files recursively in any
-- valid directory @d@ and assemble the information in data
-- 'Disco' ('Maybe' 'Data'), a intermidian type...
-- because of the errors...
--
scan :: FilePath -> IO (Disco (Maybe Data))
scan s = do
    exist <- doesFileExist s
    case exist of
        True  -> (getInfoFile s $ Left()) >>= return . Ficheiro
        False -> do info      <- getInfoFile s $ Right()
                    teste_try <- Error.try $ getDirectoryContentsDiskCatalog (s++"/")
                    case teste_try of
                        Left err ->   getClockTime
                                  >>= \_ -> reportError err
                                  >>  (return $ Pasta info [])
                        Right contents -> do
                            case contents of
                                [] -> return $ Pasta info []
                                l  -> mapM scan l >>= \rest -> return $ Pasta info rest


type FileName = String

{-|
    Get the file name from a path.

    * Dyagram of function 'fileName', in a anamorphism:

@                               genAna_s
                 ['Disco' a] ---------------> 1 + ['Char'] * ['Char']
                     |                              |
                     |                              |
    'analist' genAna_s |                              | id '-|-' id '><' 'analist' genAna_s
                     |                              |
                     V                              V
                 [['Char']] <------------------- 1 + ['Char'] * [['Char']]
                               'inlist' (matrix)
@

    > where genAna_s is the auxiliar function of the anamorphism.
-}
fileName :: FilePath -- ^ 'FilePath' of the file
         -> FileName -- ^ Return just the name of the file
fileName = tail . last . analist genAna
    where
    genAna = cond Prelude.null (Left . (!)) (cond (elem '/') (Right . Mpi.split id tail) (Left . (!)))

--
-- | Get directory contents and drop the '..' and '.' dir's.
--
-- * /getDirectoryContentsDiskCatalog/ in 'do' notation:
--
-- > getDirectoryContentsDiskCatalog s =
-- >    do l <- getDirectoryContents s
-- >       return $ (drop 2) $ map (s++) l
--
getDirectoryContentsDiskCatalog :: FilePath -> IO [FilePath]
getDirectoryContentsDiskCatalog f =
    getDirectoryContents f >>= return . (Prelude.drop 2) . Prelude.map (f++)

--
-- | Assemble some file information into 'Data'.
--
getInfoFile :: FilePath        -- ^ 'FilePath' of file
            -> Either () ()    -- ^ 'Left'() if 'FilePath' variable points to a file or 'Right'() if it's a directory
            -> IO (Maybe Data) -- ^ return 'Nothing' if accours an error (report it to log.err file) see: 'reportError'
getInfoFile f e = do let newFileName = fileName f
                     case e of
                         Left()  -> do
                             teste_try <- Error.try $ openBinaryFile f ReadMode
                             case teste_try of
                                 Left err      -> do reportError err ; return Nothing
                                 Right handle  -> do
                                     teste_try_size <- Error.try $ hFileSize handle
                                     case teste_try_size of
                                         Left err -> do reportError err ; return Nothing
                                         Right size -> do
                                             teste_try' <- Error.try $ getModificationTime f
                                             case teste_try' of
                                                 Left err -> do reportError err ; return Nothing
                                                 Right time' -> do
                                                     let time = getIntTime time'
                                                     hClose handle
                                                     return $ Just $ Data newFileName size time (tipe f)
                         Right() -> do
                             teste_try' <- Error.try $ getModificationTime f
                             case teste_try' of
                                 Left err    -> do reportError err ; return Nothing
                                 Right time' -> (return $ getIntTime time') >>= \time -> return $ Just $ Data newFileName 0 time ""

--
-- | Report an error into file log.err.
--
reportError :: IOError -> IO ()
reportError err = do
    time_file <- getClockTime
    let tipo          = ioeGetErrorType err
    let (Just file)   = ioeGetFileName err
    appendFile errorFile ((logError (tipo,file) $ show time_file) ++ "\n")
    where logError :: (IOErrorType , String) -> String -> String
          logError (tipo,file) time =
               case tipo of
                  AlreadyExists -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": ja existe"
                  NoSuchThing -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": ficheiro nao existe"
                  ResourceBusy -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": resource ocupado"
                  ResourceExhausted -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": resource exausted"
                  EOF -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": end of file"
                  IllegalOperation -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": operacao ilegal"
                  PermissionDenied -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": permissao negada"
                  UserError -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": erro de utilizador"
                  UnsatisfiedConstraints -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": InsatisfiedConstraints"
                  SystemError -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": erro de sistema"
                  ProtocolError -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": erro de protocolo"
                  OtherError -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": outro erro"
                  InvalidArgument -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": argumento invalido"
                  InappropriateType -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": tipo inapropriado"
                  HardwareFault -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": ups! hardware fault"
                  UnsupportedOperation -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": operacao nao sopurtada"
                  TimeExpired -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": tempo expirou"
                  ResourceVanished -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": ResourceVanished"
                  Interrupted -> "(EE) " ++ time ++ ": erro no ficheiro " ++ file ++ ": interrompido"

--
-- | Get the type of a file.
--
-- *Examples:
--
-- > |--------------------|
-- > | filename  | result |
-- > +--------------------+
-- > | abc.def   | def    |
-- > | abc.de.fg | fg     |
-- > | abc.def.g | g      |
--
tipe :: FilePath -> Type
tipe s = let elems = elemIndices '.' s
         in case elems of
             [] -> []
             _  -> drop ((last $ elems)+1) s

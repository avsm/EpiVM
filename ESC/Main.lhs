> module Main where

> import System
> import System.Directory
> import System.Environment
> import System.IO
> import Monad

> import EMachine.Compiler

> import Prefix

> libdir = prefix ++ "/lib/evm"

> main = do args <- getArgs
>           (fn, opts) <- usage args
>           (tmpn,tmph) <- tempfile
>           compile fn tmph
>           let cmd = "gcc -g -x c " ++ tmpn ++ " -o " ++ (mkExecname fn) ++
>                     " -L"++libdir++" -I"++libdir ++ " -levm -lgc"
>           exit <- system cmd
>           when (KeepC `elem` opts) $ do
>              rawc <- readFile tmpn
>              writeFile ((getRoot fn)++".c") rawc
>           removeFile tmpn
>           if (exit /= ExitSuccess) 
>              then exitWith exit
>              else return ()

> mkExecname fn = case span (/='.') fn of
>     (stem,".e") -> stem
>     (stem,_) -> fn ++ ".exe"

> getRoot fn = case span (/='.') fn of
>     (stem,_) -> stem

> usage (fn:args) = do let opts = parseArgs args
>                      return (fn,opts)
> usage _ = do putStrLn "Epigram Supercombinator Compiler version 0.1"
>              putStrLn "Usage:\n\tesc <input file>"
>              exitWith (ExitFailure 1)

> data Option = KeepC -- Don't delete intermediate file
>   deriving Eq

> parseArgs [] = []
> parseArgs ("-keepc":args) = KeepC:(parseArgs args)

> tempfile :: IO (FilePath, Handle)
> tempfile = do env <- environment "TMPDIR"
>               let dir = case env of
>                               Nothing -> "/tmp"
>                               (Just d) -> d
>               openTempFile dir "esc"

> environment :: String -> IO (Maybe String)
> environment x = catch (do e <- getEnv x
>                           return (Just e))
>                       (\_ -> return Nothing)


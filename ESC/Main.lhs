> module Main where

> import System
> import System.Directory
> import System.Environment
> import System.IO

> import EMachine.Compiler

> import Prefix

> libdir = prefix ++ "/lib/evm"

> main = do args <- getArgs
>           (fn, opts) <- usage args
>           (tmpn,tmph) <- tempfile
>           compile fn tmph
>           let cmd = "gcc -O2 -x c " ++ tmpn ++ " -o " ++ (mkExecname fn) ++
>                     " -L"++libdir++" -I"++libdir ++ " -levm -lgc"
>           exit <- system cmd
>           removeFile tmpn
>           if (exit /= ExitSuccess) 
>              then exitWith exit
>              else return ()

> mkExecname fn = case span (/='.') fn of
>     (stem,".e") -> stem
>     (stem,_) -> fn ++ ".exe"

> usage [fn] = return (fn, [])
> usage _ = do putStrLn "Epigram Supercombinator Compiler version 0.1"
>              putStrLn "Usage:\n\tesc <input file>"
>              exitWith (ExitFailure 1)

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


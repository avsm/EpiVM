> module Main where

> import System
> import System.Directory
> import System.Environment
> import System.IO
> import Monad

> import EMachine.Compiler

> main = do args <- getArgs
>           (fns, opts) <- getInput args
>           outfile <- getOutput opts
>           ofiles <- compileFiles fns (mkOpts opts)
>           if ((length ofiles) > 0 && (not (elem Obj opts)))
>              then link ofiles outfile
>              else return ()
>   where mkOpts (KeepInt:xs) = KeepC:(mkOpts xs)
>         mkOpts (_:xs) = mkOpts xs
>         mkOpts [] = []

> compileFiles [] _ = return []
> compileFiles (fn:xs) opts
>     | isDotE fn = do
>        let ofile = getRoot fn ++ ".o"
>        compileOpts fn ofile (Just (getRoot fn ++ ".ei")) opts
>        rest <- compileFiles xs opts
>        return (ofile:rest)
>     | isDotO fn = do
>        rest <- compileFiles xs opts
>        return (fn:rest)

> isDotE ('.':'e':[]) = True
> isDotE (_:xs) = isDotE xs
> isDotE [] = False

> isDotO ('.':'o':[]) = True
> isDotO (_:xs) = isDotO xs
> isDotO [] = False

> mkExecname fn = case span (/='.') fn of
>     (stem,".e") -> stem
>     (stem,_) -> fn ++ ".exe"

> getRoot fn = case span (/='.') fn of
>     (stem,_) -> stem

> getInput :: [String] -> IO ([FilePath],[Option])
> getInput args = do let opts = parseArgs args
>                    fns <- getFile opts
>                    if (length fns == 0) 
>                       then do showUsage
>                               return (fns,opts)
>                       else return (fns,opts)

> showUsage = do putStrLn "Epigram Supercombinator Compiler version 0.1"
>                putStrLn "Usage:\n\tesc <input file> [options]"
>                exitWith (ExitFailure 1)

> data Option = KeepInt -- Don't delete intermediate file
>             | Obj -- Just make the .o, don't link
>             | File String -- File to send the compiler
>             | Output String -- Output filename
>   deriving Eq

> parseArgs :: [String] -> [Option]
> parseArgs [] = []
> parseArgs ("-keepc":args) = KeepInt:(parseArgs args)
> parseArgs ("-c":args) = Obj:(parseArgs args)
> parseArgs ("-o":name:args) = (Output name):(parseArgs args)
> parseArgs (x:args) = (File x):(parseArgs args)

> getFile :: [Option] -> IO [FilePath]
> getFile ((File x):xs) = do fns <- getFile xs
>                            return (x:fns)
> getFile (_:xs) = getFile xs
> getFile [] = return []

> getOutput :: [Option] -> IO FilePath
> getOutput ((Output fn):xs) = return fn
> getOutput (_:xs) = getOutput xs
> getOutput [] = return "a.out"

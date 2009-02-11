> -- | 
> -- Module      : EMachine.Compiler
> -- Copyright   : Edwin Brady
> -- Licence     : BSD-style (see LICENSE in the distribution)
> --
> -- Maintainer  : eb@dcs.st-and.ac.uk
> -- Stability   : experimental
> -- Portability : portable
> -- 
> -- Public interface for Epigram Supercombinator Compiler

> module Epic.Compiler(CompileOptions(..),
>                      compile, 
>                      compileOpts, 
>                      link, 
>                      libdir) where

Brings everything together; parsing, checking, code generation

> import System
> import System.IO
> import System.Directory
> import System.Environment

> import Epic.Language
> import Epic.Parser
> import Epic.Scopecheck
> import Epic.CodegenC
> import Epic.Prefix

> -- | (Debugging) options to give to compiler
> data CompileOptions = KeepC -- ^ Keep intermediate C file
>                     | ShowBytecode -- ^ Show generated code
>                     | ShowParseTree -- ^ Show parse tree
>                     | GCCOpt String -- ^ Extra GCC option
>   deriving Eq

> addGCC :: [CompileOptions] -> String
> addGCC [] = ""
> addGCC ((GCCOpt s):xs) = s ++ " " ++ addGCC xs
> addGCC (_:xs) = addGCC xs

> -- |Compile a source file in supercombinator language to a .o
> compile :: FilePath -- ^ Input file name
>            -> FilePath -- ^ Output file name
>            -> Maybe FilePath -- ^ Interface (.ei) file name, if desired
>            -> IO ()
> compile fn outf iface
>     = compileOpts fn outf iface []

> compileOpts :: FilePath -- ^ Input file name
>            -> FilePath -- ^ Output file name
>            -> Maybe FilePath -- ^ Interface (.ei) file name, if desired
>            -> [CompileOptions] -- Keep the C file
>            -> IO ()
> compileOpts fn outf iface opts
>     = do input <- readFile fn
>          -- prelude <- readFile (libdir ++ "/Prelude.e")
>          let s = parse input fn
>          case s of
>              Failure err _ _ -> fail err
>              Success ds -> do
>                 (tmpn,tmph) <- tempfile
>                 checked <- compileDecls (checkAll ds) tmph
>                 let cmd = "gcc -c -foptimize-sibling-calls -x c " ++ tmpn ++ " -I" ++ libdir ++ " -o " ++ outf ++ " " ++ addGCC opts
>                 -- putStrLn $ cmd
>                 exit <- system cmd
>                 if (elem KeepC opts)
>                    then do system $ "cp " ++ tmpn ++ " " ++ 
>                                       (getRoot fn) ++ ".c"
>                            return ()
>                    else return ()
>                 -- removeFile tmpn
>                 if (exit /= ExitSuccess) 
>                    then fail $ "gcc failed"
>                    else return ()
>                 case iface of
>                    Nothing -> return ()
>                    (Just fn) -> do writeFile fn (writeIFace checked)

> getRoot fn = case span (/='.') fn of
>     (stem,_) -> stem


> compileDecls (Success (ctxt, decls)) outh
>     = do hPutStr outh $ codegenC ctxt decls
>          hFlush outh
>          hClose outh
>          return decls
> compileDecls (Failure err _ _) _ = fail err

> -- |Link a collection of .o files into an executable
> link :: [FilePath] -- ^ Object files
>         -> [FilePath] -- ^ Extra include files for main program
>         -> FilePath -- ^ Executable filename
>         -> IO ()
> link infs extraIncs outf = do
>     mainprog <- mkMain extraIncs 
>     let cmd = "gcc -x c " ++ mainprog ++ " -x none -L" ++
>               libdir++" -I"++libdir ++ " " ++
>               (concat (map (++" ") infs)) ++ 
>               " -levm -lgc -lpthread -lgmp -o "++outf
>     -- putStrLn $ cmd
>     exit <- system cmd
>     if (exit /= ExitSuccess)
>        then fail $ "Linking failed"
>        else return ()

Output the main progam, adding any extra includes needed. 
(Some libraries need the extra includes, notably SDL, to compile correctly.
Grr.)

> mkMain :: [FilePath] -> IO FilePath
> mkMain extra = 
>    do mp <- readFile (libdir ++ "/mainprog.c")
>       (tmp, tmpH) <- tempfile
>       hPutStr tmpH (concat (map (\x -> "#include <" ++ x ++ ">\n") extra))
>       hPutStr tmpH mp
>       hClose tmpH
>       return tmp

> -- |Get the path where the required C libraries and include files are stored
> libdir :: FilePath
> libdir = libprefix ++ "/lib/evm"

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

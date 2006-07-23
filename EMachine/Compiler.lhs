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

> module EMachine.Compiler(compile, libdir) where

Brings everything together; parsing, checking, code generation

> import System.IO

> import EMachine.Language
> import EMachine.Parser
> import EMachine.Scopecheck
> import EMachine.CodegenC
> import EMachine.Prefix

> -- |Compile a source file in supercombinator language to C
> compile :: FilePath -- ^ Input file
>            -> Handle -- ^ Output C filehandle
>            -> IO ()
> compile fn outh
>     = do prelude <- readFile $ libdir ++ "/Prelude.e"
>          input <- readFile fn
>          let s = parse (prelude ++ input) fn
>          case s of
>              Failure err _ _ -> fail err
>              Success ds -> compileDecls (checkAll ds) outh

> compileDecls (Success (ctxt, decls)) outh
>     = do hPutStr outh $ codegenC ctxt decls
>          hFlush outh
>          hClose outh

> compileDecls (Failure err _ _) _ = putStrLn err

> -- |Get the path where the required C libraries and include files are stored
> libdir :: FilePath
> libdir = libprefix ++ "/lib/evm"

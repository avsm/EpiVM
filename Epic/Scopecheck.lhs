> module Epic.Scopecheck where

Check that an expression has all its names in scope. This is the only
checking we do (for now).

> import Control.Monad.State

> import Epic.Language
> import Epic.Parser

> checkAll :: Monad m => [Decl] -> m (Context, [Decl])
> checkAll xs = do let ctxt = mkContext xs
>                  ds <- ca (mkContext xs) xs
>                  return (ctxt,ds)
>    where ca ctxt [] = return []
>          ca ctxt ((Decl nm rt fn):xs) = 
>              do fn' <- scopecheck ctxt fn
>                 xs' <- ca ctxt xs
>                 return $ (Decl nm rt fn'):xs'
>          ca ctxt (x:xs) =
>              do xs' <- ca ctxt xs
>                 return (x:xs')

>          mkContext [] = []
>          mkContext ((Decl nm rt (Bind args _ _)):xs) =
>              (nm,(map snd args, rt)):(mkContext xs)
>          mkContext ((Extern nm rt args):xs) =
>              (nm,(args, rt)):(mkContext xs)
>          mkContext (_:xs) = mkContext xs

> scopecheck :: Monad m => Context -> Func -> m Func
> scopecheck ctxt (Bind args locs exp) = do
>        (exp', locs') <- runStateT (tc (v_ise args 0) exp) (length args)
>        return $ Bind args locs' exp'
>  where
>    tc env (R n) = case lookup n env of
>                      Nothing -> case lookup n ctxt of
>                         Nothing -> return $ Const (MkInt 1234567890)
> -- lift $ fail $ 
>    --                                  "Unknown name " ++ showuser n
>                         (Just _) -> return $ R n
>                      (Just i) -> return $ V i
>    tc env (Let n ty v sc) = do
>                v' <- tc env v
>                sc' <- tc ((n,length env):env) sc
>                maxlen <- get
>                put (if (length env + 1)>maxlen 
>                        then (length env + 1) 
>                        else maxlen)
>                return $ Let n ty v' sc'
>    tc env (Case v alts) = do
>                v' <- tc env v
>                alts' <- tcalts env alts
>                return $ Case v' alts'
>    tc env (If a t e) = do
>                a' <- tc env a
>                t' <- tc env t
>                e' <- tc env e
>                return $ If a' t' e'
>    tc env (App f as) = do
>                f' <- tc env f
>                as' <- mapM (tc env) as
>                return $ App f' as'
>    tc env (Lazy e) = do
>                e' <- tc env e
>                return $ Lazy e'
>    tc env (Con t as) = do
>                as' <- mapM (tc env) as
>                return $ Con t as'
>    tc env (Proj e i) = do
>                e' <- tc env e
>                return $ Proj e' i
>    tc env (Op op l r) = do
>                l' <- tc env l
>                r' <- tc env r
>                return $ Op op l' r'
>    tc env (ForeignCall ty fn args) = do
>                argexps' <- mapM (tc env) (map fst args)
>                return $ ForeignCall ty fn (zip argexps' (map snd args))
>    tc env (LazyForeignCall ty fn args) = do
>                argexps' <- mapM (tc env) (map fst args)
>                return $ LazyForeignCall ty fn (zip argexps' (map snd args))
>    tc env x = return x

>    tcalts env [] = return []
>    tcalts env ((Alt tag args expr):alts) = do
>                let env' = (v_ise args (length env))++env
>                expr' <- tc env' expr
>                maxlen <- get
>                put (if (length env')>maxlen 
>                        then (length env')
>                        else maxlen)
>                alts' <- tcalts env alts
>                return $ (Alt tag args expr'):alts'
>    tcalts env ((ConstAlt tag expr):alts) = do
>                expr' <- tc env expr
>                alts' <- tcalts env alts
>                return $ (ConstAlt tag expr'):alts'
>    tcalts env ((DefaultCase expr):alts) = do
>                expr' <- tc env expr
>                alts' <- tcalts env alts
>                return $ (DefaultCase expr'):alts'

Turn the argument list into a mapping from names to argument position

>    v_ise [] _ = []
>    v_ise ((n,ty):args) i = (n,i):(v_ise args (i+1))
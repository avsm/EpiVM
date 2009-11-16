> module Epic.Scopecheck where

Check that an expression has all its names in scope. This is the only
checking we do (for now).

> import Control.Monad.State

> import Epic.Language
> import Epic.Parser

> import Debug.Trace

> checkAll :: Monad m => [Decl] -> m (Context, [Decl])
> checkAll xs = do let ctxt = mkContext xs
>                  ds <- ca (mkContext xs) xs
>                  return (mkContext ds,ds)
>    where ca ctxt [] = return []
>          ca ctxt ((Decl nm rt fn exp fl):xs) = 
>              do (fn', newds) <- scopecheck ctxt nm fn
>                 xs' <- ca ctxt (newds ++ xs)
>                 return $ (Decl nm rt fn' exp fl):xs'
>          ca ctxt (x:xs) =
>              do xs' <- ca ctxt xs
>                 return (x:xs')

>          mkContext [] = []
>          mkContext ((Decl nm rt (Bind args _ _ _) _ _):xs) =
>              (nm,(map snd args, rt)):(mkContext xs)
>          mkContext ((Extern nm rt args):xs) =
>              (nm,(args, rt)):(mkContext xs)
>          mkContext (_:xs) = mkContext xs

Check all names are in scope in a function, and convert global references (R) to local names
(V). Also, if any lazy expressions are not already applications, lift them out and make
a new function. Returns the modified function, and a list of new declarations. The new 
declarations will *not* have been scopechecked.

> scopecheck :: Monad m => Context -> Name -> Func -> m (Func, [Decl])
> scopecheck ctxt nm (Bind args locs exp fl) = do
>        (exp', (locs', _, ds)) <- runStateT (tc (v_ise args 0) exp) (length args, 0, [])
>        return $ (Bind args locs' exp' fl, ds)
>  where
>    getRoot (UN nm) = nm
>    getRoot (MN nm i) = "_" ++ nm ++ "_" ++ show i
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
>                (maxlen, nextn, decls) <- get
>                put ((if (length env + 1)>maxlen 
>                         then (length env + 1) 
>                         else maxlen), nextn, decls)
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
>    tc env (While t b) = do
>                t' <- tc env t
>                b' <- tc env b
>                return $ While t' b'
>    tc env (WhileAcc t a b) = do
>                t' <- tc env t
>                a' <- tc env a
>                b' <- tc env b
>                return $ WhileAcc t' a' b'
>    tc env (App f as) = do
>                f' <- tc env f
>                as' <- mapM (tc env) as
>                return $ App f' as'
>    tc env (Lazy e) | appForm e = do
>                e' <- tc env e
>                return $ Lazy e'

Make a new function, with current env as arguments, and add as a decl

>    tc env (Lazy e) = 
>         do (maxlen, nextn, decls) <- get
>            let newname = MN (getRoot nm) nextn
>            let newargs = zip (map fst env) (repeat TyAny)
>            let newfn = Bind newargs 0 e []
>            let newd = Decl newname TyAny newfn Nothing []
>            put (maxlen, nextn+1, newd:decls)
>            return $ Lazy (App (R newname) (map V (map snd env)))

>    tc env (Effect e) = do
>                e' <- tc env e
>                return $ Effect e'
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
>                (maxlen, nextn, decls) <- get
>                put ((if (length env')>maxlen 
>                         then (length env')
>                         else maxlen), nextn, decls)
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
If any names appear more than once, use the last one.

We're being very tolerant of input here... 

>    v_ise [] _ = []
>    v_ise ((n,ty):args) i = let rest = v_ise args (i+1) in
>                                case lookup n rest of
>                                  Nothing -> (n,i):rest
>                                  _ -> rest

       where dropArg n [] = []
             dropArg n ((x,i):xs) | x == n = dropArg n xs
                                  | otherwise = (x,i):(dropArg n xs)

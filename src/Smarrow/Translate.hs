module Smarrow.Translate where

import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.List (sortBy)
import Data.Set (Set)
import qualified Data.Set as Set

import Smarrow.Environment
import Smarrow.AST

------------------------------------------------------------------------

-- See Ross Paterson's [Arrows and
-- Computation](http://www.staff.city.ac.uk/~ross/papers/fop.html) (p. 14, 2003)

-- https://github.com/phadej/overloaded/blob/master/src/Overloaded/Plugin/Categories.hs#L416

translate :: Env -> Expr -> CCC
translate e = simplify . tr e

tr :: Env -> Expr -> CCC
tr env (Proc pat body) = case body of
  f :-< a | fvP pat `Set.intersection` fvE f == Set.empty ->
              tr (extendEnv env pat) a :>>> tr emptyEnv f
          | otherwise -> error "tr, not implemented yet" -- undefined :>>> App
  Do []                      cmd -> tr env (Proc pat cmd)
  Do (pat' :<- cmd' : stmts) cmd ->
    (tr env (Proc pat cmd') :&&& Id) :>>> tr env (Proc (TupleP pat' pat) (Do stmts cmd))

  If cond true false ->
    tr (extendEnv env pat) cond :>>> (tr env (Proc pat true) :||| tr env (Proc pat false))
  Case scrut alts -> tr (extendEnv env pat) scrut :>>> FanIn (trAlts env pat alts)
tr env  (VarE v)        = lookupVar env v
tr env  (PairE l r)     = tr env l :&&& tr env r
tr _env ReturnE         = Id
tr _env IdE             = Id
tr _env (LitE x)        = LitA x
tr env  (FstE p)        = tr env p :>>> Fst
tr env  (SndE p)        = tr env p :>>> Snd
tr env  (BinOp op x y)  = tr env x :&&& tr env y :>>> BinOpA op
tr env  (Con conName)   = InjectA conName (conNameIndex env conName)
tr _env GetE            = Get
tr _env PutE            = Put
tr _env UnitE           = Unit
tr env  (RecordE es)    = FanOut (map (\(_field, _mType, mValue) ->
                                         tr env (fromMaybe (error "tr: impossible, due to typechecking") mValue)) es)
tr env  (ProjectE e f)  = tr env e :>>> Project f (fieldNameIndex env f)
tr env  (UpdateE r r')  = tr env r :&&& tr env r' :>>> UpdateA
tr _env e = error (show e)

trAlts :: Env -> Pat -> [Alt] -> [CCC]
trAlts env pat alts = map snd (sortBy (compare `on` fst) (map (trAlt env pat) alts))

trAlt :: Env -> Pat -> Alt -> (Int, CCC)
trAlt env pat (Alt (ConNameP conName) (UnguardedAlt cmd)) =
  (conNameIndex env conName, tr env (Proc pat cmd))
trAlt _env _pat alt = error (show alt)
-- trAlt env (Alt pat            (UnguardedAlt cmd) _decls) = tr env (Proc pat cmd)

translateValueCons :: Env -> Value -> Value
translateValueCons env (ConV conName) = Inject conName (conNameIndex env conName) UnitV
-- ^ XXX: constructor arguments
translateValueCons _env v = v

------------------------------------------------------------------------

-- XXX: Move? Only used here.

fvP :: Pat -> Set Var
fvP (VarP v)     = Set.singleton v
fvP (TupleP p q) = fvP p `Set.union` fvP q
fvP WildP        = Set.empty
fvP (ConNameP _) = Set.empty
fvP UnitP        = Set.empty

fvE :: Expr -> Set Var
fvE (Proc pat body) = fvC body `Set.difference` fvP pat
fvE (VarE v)        = Set.singleton v
fvE ReturnE         = Set.empty
fvE (PairE l r)     = fvE l `Set.union` fvE r
fvE IdE             = Set.empty
fvE GetE            = Set.empty
fvE PutE            = Set.empty
fvE e = error (show e)

fvC :: Cmd -> Set Var
fvC (f :-< a)        = fvE f `Set.union` fvE a
fvC (If b t f)       = fvE b `Set.union` fvC t `Set.union` fvC f
fvC (Case _e _as)    = undefined
fvC (Do _stmts _cmd) = undefined

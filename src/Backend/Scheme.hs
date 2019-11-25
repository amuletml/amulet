{-# LANGUAGE ViewPatterns #-}
module Backend.Scheme (genScheme) where

import Control.Lens

import qualified Data.VarMap as M
import Data.VarMap (Map)

import Core.Types
import Core.Core
import Core.Var

import Text.Pretty.Semantic

genScheme :: [Stmt CoVar] -> Doc
genScheme sts =
      parens (keyword "import" <+> string "scheme")
  <#> parens (keyword "import" <+> parens (string "chicken base"))
  <#> vsep (map genOne sts)

genOne :: Stmt CoVar -> Doc
genOne (Foreign v _ e) = parens $
  keyword "define" <+> var v <+> text e
genOne (Type _ cs) = vsep (map genConstructor cs)
genOne (StmtLet bs) = genTopBinds bs

genTopBinds :: Binding CoVar -> Doc
genTopBinds (One (v, _, e)) = parens $
  keyword "define" <+> var v <#> indent 2 (genTerm mempty e)

genTopBinds (Many binds) =
  let vars = map (view _1) binds
      binding (v, _, e) = parens $ var v <+> align (genTerm mempty e)
   in parens $
     keyword "define-values" <+> parens (hsep (map var vars)) <#>
       indent 2 (parens
         (keyword "letrec"
           <+> parens (align (vsep (map binding binds)))
           <#> indent 2 (parens (keyword "values" <+> hsep (map var vars)))))

-- All of the Scheme generation functions that build up a Scheme
-- expression take an "unpack" map, to represent variables that are
-- actually multiple variables pretending to be a single variable.

genTerm :: Map [Doc] -> Term CoVar -> Doc
genTerm unpack (Atom a) = genAtom unpack a

genTerm unpack (App f x) = parens $ genAtom unpack f <+> genAtom unpack x
genTerm unpack (Lam c b) =
  case c of
    TypeArgument _ _ -> genTerm unpack b
    TermArgument v (ValuesTy xs) ->
      let vars = map (\i -> char '|' <> var v <+> int i <> char '|') [1..length xs]
          unpack' = M.insert v vars unpack
       in parens $
            keyword "lambda" <+> parens (hsep vars) <#> indent 2 (genTerm unpack' b)
    TermArgument v _ -> parens $
      keyword "lambda" <+> parens (var v) <#> indent 2 (genTerm unpack b)

genTerm unpack (Let (One (v, _, erase -> Atom (Ref v' _))) b)
  | v' `M.member` unpack = genTerm (M.insert v (unpack M.! v') unpack) b
  | otherwise = genTerm (M.insert v [var v'] unpack) b

genTerm unpack (Let (One (v, t, e)) b) =
  let binding = genTerm unpack e
      vars = case t of
        ValuesTy xs ->
          map (\i -> char '|' <> var v <+> int i <> char '|') [1..length xs]
        _ -> [var v]
      unpack' =
        case vars of
          [_] -> unpack
          _ -> M.insert v vars unpack
      (let_kw, let_vars) =
        case vars of
          [x] -> (keyword "let", x)
          xs -> (keyword "let-values", parens (hsep xs))
   in parens $
     let_kw
       <+> parens (parens (let_vars <+> align binding))
       <#> indent 2 (genTerm unpack' b)

genTerm unpack (Let (Many vars) b) =
  let binding (v, _, e) = parens $ var v <+> align (genTerm unpack e)
   in parens $
     keyword "letrec"
       <+> parens (align (vsep (map binding vars)))
       <#> indent 2 (genTerm unpack b)

genTerm unpack (Match t bs) =
  parens $
    keyword "cond"
      <#> vsep (map (indent 2 . genBranch unpack t) bs)

-- Erased terms:
genTerm u (TyApp a _) = genAtom u a
genTerm u (Cast a _ _) = genAtom u a

genTerm u (Extend atom rows) =
  let new = parens $ keyword "copy-record-storage"
                 <+> genAtom u atom
                 <+> sliteral (int (length rows))
      ext (k, _, a) = parens $ keyword "record-storage-set!"
                           <+> string "new-record"
                           <+> shown k
                           <+> genAtom u a
   in parens $
     keyword "let"
       <+> parens (parens (string "new-record" <+> new))
       <#> vsep (map (indent 2 . ext) rows)
       <#> indent 2 (string "new-record")

genTerm u (Values xs) = parens $ keyword "values" <+> hsep (map (genAtom u) xs)

erase :: Term a -> Term a
erase (Cast a _ _) = Atom a
erase (TyApp a _) = Atom a
erase a = a

genBranch :: Map [Doc] -> Atom CoVar -> Arm CoVar -> Doc
genBranch unpack a (Arm p _ t _ _) =
  let rhs = align $ genTerm unpack t
   in case p of
     Constr p -> parens . align $
           parens (keyword "eq?" <+> quote p <+> parens (keyword "vector-ref" <+> genAtom unpack a <+> sliteral (int 0)))
       <#> rhs
     Destr p vs ->
       let captures = zipWith capture vs [1..]
           capture (Capture v _) i = parens $
             var v <+> parens (keyword "vector-ref"
                                   <+> genAtom unpack a
                                   <+> sliteral (int i))
        in parens . align $
          parens (keyword "eq?" <+> quote p
                   <+> parens (keyword "vector-ref"
                                <+> genAtom unpack a
                                <+> sliteral (int 0)))
          <#> parens (keyword "let" <+> parens (vsep captures) <#> indent 2 rhs)
     PatWildcard -> parens $ keyword "else" <+> rhs
     PatLit l -> parens . align $
           parens (keyword "eq?" <+> genAtom unpack a <+> genLit l)
       <#> rhs
     PatRecord vs ->
       let captures = map capture vs
           capture (k, Capture v _) = parens $
             var v <+> parens (keyword "record-storage-ref"
                                   <+> genAtom unpack a
                                   <+> genLit (Str k))
        in parens . align $
                sliteral (string "#t")
            <#> parens (keyword "let" <+> parens (vsep captures) <#> indent 2 rhs)
     PatValues xs ->
       let capture (Capture v _) = var v
        in parens . align $
               sliteral (string "#t")
           <#> parens (keyword "let-values"
                        <+> parens (parens (parens (hsep (map capture xs))
                                        <+> parens (keyword "values" <+> genAtom unpack a)))
                  <#> indent 2 rhs)

genAtom :: Map [Doc] -> Atom CoVar -> Doc
genAtom _ (Lit l) = genLit l
genAtom unpack (Ref v _) =
  case M.lookup v unpack of
    Just d -> hsep d
    Nothing -> var v

var :: CoVar -> Doc
var (CoVar id (Just t) _) = char '|' <> text t <> char '#' <> int id <> char '|'
var (CoVar id Nothing _)  = string "_#" <> int id

genLit :: Literal -> Doc
genLit (Int i)   = sliteral (integer i)
genLit (Str t)   = sliteral (shown t)
genLit (Float d) = sliteral (double d)
genLit LitTrue   = sliteral (string "#t")
genLit LitFalse  = sliteral (string "#f")
genLit Unit      = parens $ keyword "void"
genLit RecNil    = parens $ keyword "make-record-storage" <+> sliteral (int 0)

quote :: CoVar -> Doc
quote v = parens $ keyword "quote" <+> var v

genConstructor :: (CoVar, Type CoVar) -> Doc
genConstructor (v, t) =
  let ar = arity t in case ar of
    0 -> parens $ keyword "define" <+> var v <+> parens (keyword "vector" <+> quote v)
    n ->
      let l = sliteral (squote <> var v):vs
          vs = map (\i -> string ('x':'#':show i)) [0 .. n - 1]
          lambda =
            foldr (\v b -> parens $
                      keyword "lambda"
                  <+> parens v
                  <+> b)
                  (parens (keyword "vector" <+> hsep l))
                  vs
       in parens $ keyword "define" <+> var v <+> lambda

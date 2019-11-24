module Backend.Scheme (genScheme) where

import Control.Lens

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
  keyword "define" <+> var v <#> indent 2 (genTerm e)
genTopBinds (Many binds) =
  let vars = map (view _1) binds
      binding (v, _, e) = parens $ var v <+> align (genTerm e)
   in parens $
     keyword "define-values" <+> parens (hsep (map var vars)) <#>
       indent 2 (parens
         (keyword "letrec"
           <+> parens (align (vsep (map binding binds)))
           <#> indent 2 (parens (keyword "values" <+> hsep (map var vars)))))

genTerm :: Term CoVar -> Doc
genTerm (Atom a) = genAtom a

genTerm (App f x) = parens $ genAtom f <+> genAtom x
genTerm (Lam c b) =
  case c of
    TypeArgument _ _ -> genTerm b
    TermArgument v _ -> parens $
      keyword "lambda" <+> parens (var v) <#> indent 2 (genTerm b)

genTerm (Let (One (v, _, e)) b) =
  let binding = genTerm e
   in parens $
     keyword "let"
       <+> parens (parens (var v <+> align binding))
       <#> indent 2 (genTerm b)

genTerm (Let (Many vars) b) =
  let binding (v, _, e) = parens $ var v <+> align (genTerm e)
   in parens $
     keyword "let"
       <+> parens (align (vsep (map binding vars)))
       <#> indent 2 (genTerm b)

genTerm (Match t bs) =
  parens $
    keyword "cond"
      <#> vsep (map (indent 2 . genBranch t) bs)

-- Erased terms:
genTerm (TyApp a _) = genAtom a
genTerm (Cast a _ _) = genAtom a

genTerm (Extend atom rows) =
  let new = parens $ keyword "copy-record-storage" <+> genAtom atom
      ext (k, _, a) = parens $ keyword "record-storage-insert!"
                           <+> string "new-record"
                           <+> shown k
                           <+> genAtom a
   in parens $
     keyword "let"
       <+> parens (parens (string "new-record" <+> new))
       <#> vsep (map (indent 2 . ext) rows)
       <#> indent 2 (string "new-record")

genTerm Values{} = error "todo genTerm Values"

genBranch :: Atom CoVar -> Arm CoVar -> Doc
genBranch a (Arm p _ t _ _) =
  let rhs = align $ genTerm t
   in case p of
     Constr p -> parens . align $
           parens (keyword "eq?" <+> quote p <+> genAtom a)
       <#> rhs
     Destr p vs ->
       let captures = zipWith capture vs [1..]
           capture (Capture v _) i = parens $
             var v <+> parens (keyword "vector-ref"
                                   <+> genAtom a
                                   <+> sliteral (int i))
        in parens . align $
          parens (keyword "eq?" <+> quote p
                   <+> parens (keyword "vector-ref"
                                <+> genAtom a
                                <+> sliteral (int 0)))
          <#> parens (keyword "let" <+> parens (vsep captures) <#> indent 2 rhs)
     PatWildcard -> parens $ keyword "else" <+> rhs
     PatLit l -> parens . align $
           parens (keyword "eq?" <+> genAtom a <+> genLit l)
       <#> rhs
     PatRecord vs ->
       let captures = map capture vs
           capture (k, Capture v _) = parens $
             var v <+> parens (keyword "record-storage-ref"
                                   <+> genAtom a
                                   <+> genLit (Str k))
        in parens . align $
                sliteral (string "#t")
            <#> parens (keyword "let" <+> parens (vsep captures) <#> indent 2 rhs)
     PatValues{} -> error "todo: genBranch PatValues"

genAtom :: Atom CoVar -> Doc
genAtom (Lit l) = genLit l
genAtom (Ref v _) = var v

var :: CoVar -> Doc
var (CoVar id (Just t) _) = text t <> char '#' <> int id
var (CoVar id Nothing _)  = string "_#" <> int id

genLit :: Literal -> Doc
genLit (Int i)   = sliteral (integer i)
genLit (Str t)   = sliteral (shown t)
genLit (Float d) = sliteral (double d)
genLit LitTrue   = sliteral (string "#t")
genLit LitFalse  = sliteral (string "#f")
genLit Unit      = parens $ keyword "void"
genLit RecNil    = parens $ keyword "make-record-storage"

quote :: CoVar -> Doc
quote v = parens $ keyword "quote" <+> var v

genConstructor :: (CoVar, Type CoVar) -> Doc
genConstructor (v, t) =
  let ar = arity t in case ar of
    0 -> parens $ keyword "define" <+> var v <+> quote v
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

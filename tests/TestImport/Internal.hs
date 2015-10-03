module TestImport.Internal where

import Prelude hiding (exp)

import Language.Haskell.TH.Quote
import qualified Language.Haskell.Exts.Annotated.Syntax as Src
import qualified Language.Haskell.Exts.Parser as Src
import qualified Language.Haskell.Exts.Pretty as Src
import qualified Language.Haskell.Exts.SrcLoc as Src
import qualified Language.Haskell.Meta.Parse as Exp
import qualified Language.Haskell.TH as TH

marked :: QuasiQuoter
marked = QuasiQuoter
    { quoteExp = decorate
    , quotePat = fail "no pattern for marked"
    , quoteType = fail "no type for marked"
    , quoteDec = fail "no declaration for marked"
    }
  where
    decorate input = do
        loc <- TH.location
        let file = TH.loc_filename loc
            (line, _) = TH.loc_start loc

            fixup 1 = 0
            fixup x = x - 2

            onException_ l = Src.QVarOp l $ Src.Qual l (Src.ModuleName l "TestImport") (Src.Ident l "onException")
            report l =
                let str = file ++ ":" ++ show (line + fixup (Src.srcLine l)) ++ ": exception raised here"
                 in Src.App l
                        (Src.Var l $ Src.Qual l (Src.ModuleName l "TestImport") (Src.Ident l "liftIO"))
                      $ Src.App l
                            (Src.Var l $ Src.Qual l (Src.ModuleName l "Prelude") (Src.Ident l "putStrLn"))
                            (Src.Lit l $ Src.String l str str)

            mark l e = Src.InfixApp l (Src.Paren l e) (onException_ l) (report l)

            decorateExp :: Src.Exp Src.SrcLoc -> Src.Exp Src.SrcLoc
            decorateExp (Src.Do l stmts) = mark l $ Src.Do l $ map decorateStmt stmts
            decorateExp exp = mark (Src.ann exp) exp

            decorateStmt :: Src.Stmt Src.SrcLoc -> Src.Stmt Src.SrcLoc
            decorateStmt (Src.Generator l pat exp) = Src.Generator l pat $ decorateExp exp
            decorateStmt (Src.Qualifier l exp) = Src.Qualifier l $ decorateExp exp
            decorateStmt stmt = stmt

        case Src.parse ("do\n" ++ input) of
            Src.ParseOk a -> either fail return $ Exp.parseExp $ Src.prettyPrint $ decorateExp a
            Src.ParseFailed _ e -> fail e

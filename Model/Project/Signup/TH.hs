module Model.Project.Signup.TH where

import           Prelude
import           Data.Monoid
import qualified Data.Text            as Text
import           Database.Persist
import           Database.Persist.Sql
import           Language.Haskell.TH
import           Yesod.Markdown

derivePersistField' :: ExpQ -> ExpQ -> Name -> Q [Dec]
derivePersistField' make unmake name = do
    TyConI (NewtypeD _ _ _ (NormalC vcon _) _) <- reify name
    x <- newName "x"
    y <- newName "y"
    [d|instance PersistField $(conT name) where
           toPersistValue $(conP vcon [varP x]) =
               PersistText $(appE unmake (varE x))

           fromPersistValue $(conP 'PersistText [varP y]) =
               Right $ $(appE (conE vcon) (appE make (varE y)))
           fromPersistValue v =
               Left $ Text.pack $
                   "expected " <> $(stringE $ nameBase name) <>
                   ", received: " <> show v

       instance PersistFieldSql $(conT name) where
           sqlType _ = SqlString|]

idE :: ExpQ
idE = varE 'id

markdownE :: ExpQ
markdownE = conE 'Markdown

unMarkdownE :: ExpQ
unMarkdownE = varE 'unMarkdown

derivePersistFieldText :: Name -> Q [Dec]
derivePersistFieldText = derivePersistField' idE idE

derivePersistFieldMarkdown :: Name -> Q [Dec]
derivePersistFieldMarkdown = derivePersistField' markdownE unMarkdownE

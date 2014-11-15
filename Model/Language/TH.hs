module Model.Language.TH (makeLanguages) where

import Prelude

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Control.Arrow
import Data.Char

uc1 :: String -> String
uc1 "" = ""
uc1 (c:cs) = toUpper c : cs

makeLanguages :: QuasiQuoter
makeLanguages = QuasiQuoter
    { quoteExp  = fail "no expression expansion of langs"
    , quotePat  = fail "no pattern expansion of langs"
    , quoteType = fail "no type expansion of langs"
    , quoteDec  =
        words >>> \ langs -> return
            [ DataD [] language [] (map lang2conC langs) (map mkName $ words "Eq Ord Enum Bounded Show Read Data Typeable")

            , InstanceD [] (AppT (ConT $ mkName "PersistField") (ConT language))
                [ FunD (mkName "toPersistValue") $ map (\ lang -> Clause [lang2conP lang] (NormalB $ AppE (ConE persistText) $ LitE $ StringL lang) []) langs
                , FunD (mkName "fromPersistValue")
                    $ map (\ lang -> Clause [ConP persistText [LitP $ StringL lang]] (NormalB $ rightE $ lang2conE lang) []) langs
                        ++ [ Clause [WildP] (NormalB $ leftE $ LitE $ StringL "bad persistent type for Language") [] ]
                ]

            , InstanceD [] (AppT (ConT $ mkName "PathPiece") (ConT language))
                [ FunD (mkName "toPathPiece") $ map (\ lang -> Clause [lang2conP lang] (NormalB $ LitE $ StringL lang) []) langs
                , FunD (mkName "fromPathPiece")
                    $ map (\ lang -> Clause [LitP $ StringL lang] (NormalB $ justE $ lang2conE lang) []) langs
                        ++ [ Clause [WildP] (NormalB $ ConE (mkName "Nothing")) [] ]
                ]

            , InstanceD [] (AppT (ConT $ mkName "PersistFieldSql") (ConT language))
                [ FunD (mkName "sqlType") [Clause [WildP] (NormalB $ ConE (mkName "SqlString")) []] ]

            ]
    }
  where
    langName = uc1 >>> ("Lang" ++) >>> mkName
    lang2conE = langName >>> \ name -> ConE name
    lang2conP = langName >>> \ name -> ConP name []
    lang2conC = langName >>> \ name -> NormalC name []
    rightE = AppE (ConE $ mkName "Right")
    leftE = AppE (ConE $ mkName "Left")
    justE = AppE (ConE $ mkName "Just")
    language = mkName "Language"
    persistText = mkName "PersistText"




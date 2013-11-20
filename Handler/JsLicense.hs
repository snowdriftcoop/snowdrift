module Handler.JsLicense where

import Import

import qualified Data.Text as T

import Yesod.Form.Jquery

data Lib =
    Lib { libName :: Text
        , libRoute :: Text
        , libLicenseName :: Text
        , libLicenseRoute :: Text
        , libOrigName :: Text
        , libOrigRoute :: Text
        }

getJsLicenseR :: Handler Html
getJsLicenseR = do
    app <- getYesod
    render <- getUrlRender

    let jqueryUrl = either render id $ urlJqueryJs app
        unMin lib = maybe lib (`T.append` "js") $ T.stripSuffix "min.js" . fst . T.breakOnEnd "?" $ lib

        libs :: [Lib]
        libs =
            [ Lib "jquery.min.js" jqueryUrl "Expat License" "http://www.jclark.com/xml/copying.txt" "jquery.js" (unMin jqueryUrl)
            , Lib "bootstrap.min.js" (render $ StaticR js_bootstrap_min_js) "Apache License, Version 2.0" "http://www.apache.org/licenses/LICENSE-2.0" "bootstrap.js" (render $ StaticR js_bootstrap_js)
            , Lib "modernizr.js" (render $ StaticR js_modernizr_js) "Expat License" "http://www.jclark.com/xml/copying.txt" "modernizr.js" (render $ StaticR js_modernizr_js)
            , Lib "include.js" "https://browserid.org/include.js" "Mozilla Public License Version 2.0" "http://www.mozilla.org/MPL/2.0/" "include.orig.js" "https://login.persona.org/include.orig.js"
            , Lib "jquery.jqplot.min.js" (render $ StaticR js_jquery_jqplot_min_js) "Expat License" "http://www.jclark.com/xml/copying.txt" "jquery.jqplot.js" (render $ StaticR js_jquery_jqplot_js)
            , Lib "jqplot.logAxisRenderer.min.js" (render $ StaticR js_plugins_jqplot_logAxisRenderer_min_js) "Expat License" "http://www.jclark.com/xml/copying.txt" "jqplot.logAxisRenderer.js" (render $ StaticR js_plugins_jqplot_logAxisRenderer_js)
            ]

    defaultLayout [whamlet|
        <table .table id="jslicense-labels1">
            $forall lib <- libs
                <tr>
                    <td>
                        <a href="#{libRoute lib}">
                            #{libName lib}

                    <td>
                        <a href="#{libLicenseRoute lib}">
                            #{libLicenseName lib}

                    <td>
                        <a href="#{libOrigRoute lib}">
                            #{libOrigName lib}
    |]

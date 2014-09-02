{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module TestImport
    ( runDB
    , Spec
    , Example
    , needsLogin
    , login
    , liftIO
    , extractLocation
    , statusIsResp
    , onException
    , module TestImport
    ) where

import Yesod (Yesod, RedirectUrl)
import Yesod.Test as TestImport
import Database.Persist as TestImport hiding (get)
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Network.URI (URI (uriPath), parseURI)
import Network.HTTP.Types (StdMethod (..), renderStdMethod)
import Network.Wai.Test (SResponse (..))
import qualified Test.HUnit as HUnit
import qualified Network.HTTP.Types as H

import qualified Data.ByteString as B

import qualified Data.Text as T
import Data.Text (Text)

import Data.Text.Encoding (decodeUtf8)
import Foundation as TestImport
import Model as TestImport

import Control.Monad (when)

import qualified Language.Haskell.Meta.Parse as Exp
import qualified Language.Haskell.Meta.Syntax.Translate as Exp
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Quote

import qualified Language.Haskell.Exts.Parser as Src
import qualified Language.Haskell.Exts.SrcLoc as Src
import qualified Language.Haskell.Exts.Pretty as Src
import qualified Language.Haskell.Exts.Annotated.Syntax as Src

import Control.Exception.Lifted



type Spec = YesodSpec App
type Example = YesodExample App

testDB :: SqlPersistM a -> Example a
testDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool


-- Adjust as necessary to the url prefix in the Testing configuration
testRoot :: Text
testRoot = "http://localhost:3000/"

-- Force failure by swearing that black is white, and pigs can fly...
assertFailure :: String -> YesodExample site ()
assertFailure msg = assertEqual msg True False

-- Convert an absolute URL (eg extracted from responses) to just the path
-- for use in test requests.
urlPath :: Text -> Text
urlPath = T.pack . maybe "" uriPath . parseURI . T.unpack

-- Stages in login process, used below
firstRedirect :: (Yesod site, RedirectUrl site url) => StdMethod -> url -> YesodExample site (Maybe B.ByteString)
firstRedirect method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url

    extractLocation  -- We should get redirected to the login page

assertLoginPage :: Yesod site => Text -> YesodExample site ()
assertLoginPage loc = do
    assertEqual "correct login redirection location"
                (testRoot `T.append` "/auth/login") loc

    get $ urlPath loc
    statusIsResp 200
    bodyContains "Login"


submitLogin :: Yesod site => Text -> Text -> YesodExample site ()
submitLogin user pass = do
    -- Ideally we would extract this url from the login form on the current page
    request $ do
        setMethod "POST"
        setUrl $ urlPath testRoot `T.append` "auth/page/hashdb/login"
        addPostParam "username" user
        addPostParam "password" pass


extractLocation :: YesodExample site (Maybe B.ByteString)
extractLocation = do
    statusIsResp 302
    withResponse ( \ SResponse { simpleHeaders = h } ->
                        return $ lookup "Location" h
                 )

-- Check that accessing the url with the given method requires login, and
-- that it redirects us to what looks like the login page.
--
needsLogin :: (RedirectUrl site url, Yesod site) => StdMethod -> url -> YesodExample site ()
needsLogin method url = do
    mbloc <- firstRedirect method url
    maybe (assertFailure "Should have location header") (assertLoginPage . decodeUtf8) mbloc

-- Do a login (using hashdb auth).  This just attempts to go to the home
-- url, and follows through the login process.  It should probably be the
-- first thing in each "it" spec.
--
login :: (Yesod site) => YesodExample site ()
login = do
    get $ urlPath $ testRoot `T.append` "/auth/login"
    statusIs 200
    submitLogin "test" "test"

-- Do a login (using hashdb auth).  This just attempts to go to the home
-- url, and follows through the login process.  It should probably be the
-- first thing in each "it" spec.
--
adminLogin :: (Yesod site) => YesodExample site ()
adminLogin = do
    get $ urlPath $ testRoot `T.append` "/auth/login"
    statusIs 200
    submitLogin "admin" "admin"


statusIsResp :: Int -> YesodExample site ()
statusIsResp number = withResponse $ \ SResponse { simpleStatus = s } -> do
  when (H.statusCode s /= number) printBody
  liftIO $ flip HUnit.assertBool (H.statusCode s == number) $ concat
    [ "Expected status was ", show number
    , " but received status was ", show $ H.statusCode s
    ]


marked :: QuasiQuoter
marked = QuasiQuoter { quoteExp = decorate }
  where
    decorate input = do
        loc <- TH.location
        let file = TH.loc_filename loc
            (line, _) = TH.loc_start loc

            fixup 1 = 0
            fixup x = x - 2

            onException l = Src.QVarOp l $ Src.Qual l (Src.ModuleName l "TestImport") (Src.Ident l "onException")
            report l =
                let str = file ++ ":" ++ show (line + fixup (Src.srcLine l)) ++ ": exception raised here"
                 in Src.App l
                        (Src.Var l $ Src.Qual l (Src.ModuleName l "TestImport") (Src.Ident l "liftIO"))
                      $ Src.App l
                            (Src.Var l $ Src.Qual l (Src.ModuleName l "Prelude") (Src.Ident l "putStrLn"))
                            (Src.Lit l $ Src.String l str str)

            mark l e = Src.InfixApp l (Src.Paren l e) (onException l) (report l)

            decorateExp :: Src.Exp Src.SrcLoc -> Src.Exp Src.SrcLoc
            decorateExp (Src.Do l stmts) = mark l $ Src.Do l $ map decorateStmt stmts
            decorateExp exp = mark (Src.ann exp) exp

            decorateStmt :: Src.Stmt Src.SrcLoc -> Src.Stmt Src.SrcLoc
            decorateStmt (Src.Generator l pat exp) = Src.Generator l pat $ decorateExp exp
            decorateStmt (Src.Qualifier l exp) = Src.Qualifier l $ decorateExp exp
            decorateStmt stmt = stmt

        case Src.parse ("do\n" ++ input) of
            Src.ParseOk a -> either fail return $ Exp.parseExp $ Src.prettyPrint $ decorateExp a
            Src.ParseFailed l e -> fail e


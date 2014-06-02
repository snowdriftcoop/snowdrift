{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Data.Text.PrettyHtml (unlinesHtml, prettyHtml, prettyThings) where

import Import

import Data.Attoparsec.Text

import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Text.Blaze.Html5 as Html

import Data.Either

import Data.List as L
import Data.String
import Data.Text as T

import Control.Applicative


unlinesHtml :: [Html] -> Html
unlinesHtml = sequence_ . L.intersperse Html.br

prettyHtml :: (Monad m, HasGithubRepo (HandlerT site m)) => [Parser Pretty] -> Text -> HandlerT site m Html
prettyHtml filters text =
    case parseOnly (many $ (Left <$> choice filters) <|> (Right . T.singleton <$> anyChar)) text of
        Right result -> do
            let regroup = L.concatMap $ \(a, b) -> L.map Left a ++ [Right b | T.length b > 0]
                splitUp = fmap (fmap T.concat . partitionEithers) . L.groupBy ((==) `on` isRight)
                pieces = regroup . splitUp $ result

            fmap sequence_ $ forM pieces $ either renderPretty (return . toHtml)

        Left err -> error err

renderPretty :: (Monad m, HasGithubRepo (HandlerT site m)) => Pretty -> HandlerT site m Html
renderPretty pretty = case pretty of
        RawHtml html -> return html
        GithubTicket int -> do
            maybe_github_repo_link <- getGithubRepo
            let github_issue = toHtml $ "Github issue " ++ show int
            return $ case maybe_github_repo_link of
                Just github_repo_link -> Html.a github_issue Html.! Attr.href
                    (fromString $ "https://github.com/" ++ T.unpack github_repo_link ++ "/issues/" ++ show int)
                Nothing -> github_issue


data Pretty = GithubTicket Int | RawHtml Html

githubTicketRef :: Parser Pretty
githubTicketRef = GithubTicket <$> (asciiCI "GH-" *> decimal)


prettyThings :: [Parser Pretty]
prettyThings = [githubTicketRef]



module Model.CollapseState (CollapseState (..), collapseState) where

import Import

import Model.CollapseState.Internal

-- import Data.Time.Units

collapseState :: Handler ()
collapseState = undefined -- TODO(mitchell, david)
-- collapseState :: UTCTime -> CommentClosure -> Handler CollapseState
-- collapseState now closure =
--     return $ case (fromIntegral :: Integer -> Second) $ round $ diffUTCTime now $ commentClosureTs closure of
--         -- TODO: make these user preference
--         x | convertUnit x < (24 :: Hour) -> FullyVisible
--         x | convertUnit x < (72 :: Hour) -> Collapsed
--         _ -> FullyHidden



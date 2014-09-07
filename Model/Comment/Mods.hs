module Model.Comment.Mods where

import Import

import Data.Default (Default, def)

-- | Data type used in makeCommentWidgetMod, containing modifications to comment-action-related
-- data structures.
data CommentMods = CommentMods
    { mod_earlier_closures :: [CommentClosing]                              -> [CommentClosing]
    , mod_earlier_retracts :: [CommentRetracting]                           -> [CommentRetracting]
    , mod_user_map         :: Map UserId User                               -> Map UserId User
    , mod_closure_map      :: Map CommentId CommentClosing                  -> Map CommentId CommentClosing
    , mod_retract_map      :: Map CommentId CommentRetracting               -> Map CommentId CommentRetracting
    , mod_ticket_map       :: Map CommentId (Entity Ticket)                 -> Map CommentId (Entity Ticket)
    , mod_claim_map        :: Map CommentId TicketClaiming                  -> Map CommentId TicketClaiming
    , mod_flag_map         :: Map CommentId (CommentFlagging, [FlagReason]) -> Map CommentId (CommentFlagging, [FlagReason])
    , mod_tag_map          :: Map TagId Tag                                 -> Map TagId Tag
    }

instance Default CommentMods where
    def = CommentMods id id id id id id id id id


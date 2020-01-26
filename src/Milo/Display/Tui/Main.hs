{-# LANGUAGE OverloadedStrings #-}

module Milo.Display.Tui.Main (main) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style


import Data.Text (Text, unpack)
import Control.Monad (void)
import Graphics.Vty.Attributes (defAttr)

import qualified Graphics.Vty.Input.Events as E
import qualified Milo.Model as M


main :: Either M.TweetRetrievalError [M.TweetOutput [] M.Tweet] -> IO ()
main tweets = void $ defaultMain myApp tweets

-- TODO: Adapt to include DMs.
type AppState = Either M.TweetRetrievalError [M.TweetOutput [] M.Tweet]
type ResourceName = AttrName

-- TODO: can we dump out errors here?
iterateTweets :: AppState -> BrickEvent ResourceName e -> EventM ResourceName (Next AppState)
iterateTweets l@(Left _) (VtyEvent (E.EvKey E.KEnter _))     = halt l
iterateTweets r@(Right []) (VtyEvent (E.EvKey E.KEnter _))   = halt r
iterateTweets (Right ((M.TweetOutput _ []):ys)) (VtyEvent (E.EvKey E.KEnter _)) = continue $ Right ys 
iterateTweets (Right (M.TweetOutput heading (_:xs):ys)) (VtyEvent (E.EvKey E.KEnter _)) = continue $ Right (M.TweetOutput heading xs:ys) 
iterateTweets xs _                                         = halt xs

drawTweet :: AppState -> [Widget n]
drawTweet (Left (M.TweetRetrievalError heading endpoint tweeterror)) = 
  [withTopLabel (topLabel $ headingText heading) (center $ txtWrap $ errorText heading endpoint tweeterror)]      
drawTweet (Right tweetOutputs) = concat $ fmap renderTweets tweetOutputs
      
renderTweets :: M.TweetOutput [] M.Tweet -> [Widget n]
renderTweets (M.TweetOutput heading tweets) = 
  fmap (\(M.Tweet _ _ _ _ tweet _ _ _) -> withTopLabel (topLabel $ headingText heading) (center $ txtWrap tweet)) tweets


errorText :: M.Heading -> M.TwitterEndpoint -> M.TwitterError -> Text
errorText heading (M.TwitterEndpoint endpoint) (M.TwitterError tweetError) =
  headingText heading <> ": " <> endpoint <> ", " <> tweetError

headingText :: M.Heading -> Text
headingText (M.Heading M.MentionHeading       heading) = heading
headingText (M.Heading M.UserTimelineHeading  heading) = heading 
headingText (M.Heading M.HomeTimelineHeading  heading) = heading 
headingText (M.Heading M.DirectMessageHeading heading) = heading 
headingText (M.Heading M.SearchHeading        heading) = "Search:" <> " " <> heading


myApp :: App AppState e ResourceName
myApp = App {
    appDraw = drawTweet
  , appChooseCursor = (\_ _ -> Nothing)
  , appHandleEvent = iterateTweets  
  , appStartEvent = pure 
  , appAttrMap = const $ attrMap defAttr []
}

withTopLabel :: Widget s -> Widget s -> Widget s
withTopLabel tl w = withBorderStyle unicode $ borderWithLabel tl $ w

topLabel :: Text -> Widget n
topLabel = str . unpack 
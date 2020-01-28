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

-- Either TweetRetrievalError [TweetOutput [] Tweet]
-- [IO (Either TweetRetrievalError (TweetOutput [] Tweet))]
main :: [M.TweetResultIOWithTweet] -> IO ()
main tweets = void $ defaultMain myApp (tweets, Nothing)

-- TODO: Adapt to include DMs.
type AppState = ([M.TweetResultIOWithTweet], Maybe M.TweetResultWithTweet)
type ResourceName = AttrName

-- TODO: can we dump out errors here?
iterateTweets :: AppState -> BrickEvent ResourceName e -> EventM ResourceName (Next AppState)
iterateTweets ((action1:actions), _) (VtyEvent (E.EvKey E.KEnter _)) = 
  let nextAction = action1 >>= (\trtNext -> pure (actions, Just trtNext)) in
  suspendAndResume nextAction
iterateTweets ([], _) (VtyEvent (E.EvKey E.KEnter _)) = halt ([], Nothing)
iterateTweets (_, _) _ = halt ([], Nothing)

drawTweet :: AppState -> [Widget n]
drawTweet ([], Nothing) = [goodbyeMessage "Goodbye"]
drawTweet (_:_, Nothing) = [welcomeMessage "Welcome to Milo"]
drawTweet (_, Just (Right tweetOutputs)) = renderTweets tweetOutputs
drawTweet (_, Just (Left tweetError)) = renderTweetError tweetError

centreMessage :: Text -> Widget n
centreMessage message = center . str . unpack $ message

goodbyeMessage :: Text -> Widget n
goodbyeMessage goodbye = centreMessage goodbye

welcomeMessage :: Text -> Widget n
welcomeMessage welcome = centreMessage welcome
      
renderTweets :: M.TweetOutputWithTweetList -> [Widget n]
renderTweets (M.TweetOutput heading []) = [withTopLabel (topLabel $ headingText heading) (center $ txtWrap "No Tweets")]
renderTweets (M.TweetOutput heading tweets) = 
  fmap (\(M.Tweet _ _ _ _ tweet _ _ _) -> withTopLabel (topLabel $ headingText heading) (center $ txtWrap tweet)) tweets

renderTweetError :: M.TweetRetrievalError -> [Widget n]
renderTweetError (M.TweetRetrievalError heading (M.TwitterEndpoint endpoint) (M.TwitterError tweetError)) =
  let header    = headingText heading
      errorText = header <> ": " <> endpoint <> ", " <> tweetError
  in [withTopLabel (topLabel header) (center $ txtWrap $ errorText)]

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
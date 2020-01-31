{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Milo.Display.Tui.Main (main) where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Data.Text               (Text, unpack, pack)
import Data.Functor            (($>))
import Control.Monad           (void)
import Control.Monad.IO.Class  (liftIO)
import Control.Exception       (catch)
import Control.Exception.Base  (IOException)
import Milo.Resolution         (resolveReTweets)
import Milo.HtmlEntity         (removeHtmlEntities)

import qualified Graphics.Vty.Attributes   as A
import qualified Graphics.Vty              as V
import qualified Graphics.Vty.Input.Events as E
import qualified System.Process            as P
import qualified Milo.Model                as M


-- Either TweetRetrievalError [TweetOutput [] Tweet]
-- [IO (Either TweetRetrievalError (TweetOutput [] Tweet))]
main :: [M.TweetResultIOWithTweet] -> IO ()
main tweets = void $ defaultMain myApp (tweets, Nothing)

-- TODO: Adapt to include DMs.
type AppState = ([M.TweetResultIOWithTweet], Maybe M.TweetResultWithTweet)
type ResourceName = AttrName

-- TODO: can we dump out errors here?
iterateTweets :: AppState -> BrickEvent ResourceName e -> EventM ResourceName (Next AppState)
iterateTweets ([], tweetResults) (VtyEvent (E.EvKey E.KEnter _)) =
  case tweetResults of
    Nothing                             -> halt ([], Nothing)     
    Just (Left _)                       -> halt ([], Nothing)
    Just (Right (M.TweetOutput _ []))   -> halt ([], Nothing)
    Just (Right (M.TweetOutput _ (_:[])))  -> halt ([], Nothing)     
    Just (Right (M.TweetOutput heading (_:x:xs))) -> continue ([], Just (Right (M.TweetOutput heading (x:xs))))

iterateTweets (remActions@(action1:actions), tweetResults) (VtyEvent (E.EvKey E.KEnter _)) =
  case tweetResults of
    Nothing                               -> performNextAction
    Just (Left _)                         -> performNextAction
    Just (Right (M.TweetOutput _ []))     -> performNextAction
    Just (Right (M.TweetOutput _ (_:[]))) -> performNextAction
    Just (Right (M.TweetOutput heading (_:x:xs))) -> continue (remActions, Just (Right (M.TweetOutput heading (x:xs))))
  where 
      performNextAction :: EventM ResourceName (Next AppState)
      performNextAction  = 
        let nextAction = action1 >>= (\trtNext -> pure (actions, Just trtNext)) in
        liftIO nextAction >>= continue

iterateTweets (actions, tweetResults) (VtyEvent (E.EvKey (E.KChar 'o') _)) =
  case tweetResults of
    Just (Right (M.TweetOutput _ ((M.Tweet _ _ url _ _ _ _ _):_))) -> 
      -- TODO: Find a way to pass through the config. Maybe store it with state?
      let openTweet :: IO ()
          openTweet = P.callCommand $ "open https://twitter.com/i/web/status/" <> show url 

          fallback :: IOException -> IO ()
          fallback e = putStrLn $ "Could not launch browser: " <> show e
          
          nextAction :: IO ([M.TweetResultIOWithTweet], Maybe M.TweetResultWithTweet)
          nextAction = (openTweet `catch` \(e :: IOException) -> fallback e) $> (actions, tweetResults)

      in suspendAndResume nextAction
    _ -> continue (actions, tweetResults)

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
renderTweets (M.TweetOutput heading []) = [withTopLabel (topLabel $ headingText heading) (center $ txtWrap "End of tweets")]
renderTweets (M.TweetOutput heading (firstTweet:_)) = 
  -- let (M.Tweet createdAt (TweetedBy _ screenName) idStr _ tweet _ retCount favCount) = removeHtmlEntities . resolveReTweets $ firstTweet
  let (M.Tweet createdAt (M.TweetedBy _ screenName) _ _ tweet _ reweetCount favCount) = removeHtmlEntities . resolveReTweets $ firstTweet
  in [withTopLabel (topLabel $ headingText heading) (center $ (tweetTextW tweet) <=> ((tweetOwnerW screenName) <+> (tweetCreatedAtW createdAt)) <=> ((tweetFavCountW favCount) <+> (tweetRetweetCountW reweetCount)))]

tweetTextW :: Text -> Widget n
tweetTextW = hLimitPercent 80 . withAttr tweetTextAttr . txtWrap

tweetOwnerW :: Text -> Widget n
tweetOwnerW screenName = withAttr tweetUserScreenNameAttr $ txtWrap $ "- " <> "@" <> screenName

tweetCreatedAtW :: Text -> Widget n
tweetCreatedAtW = padLeft (Pad 2) . withAttr tweetCreatedAtAttr . txtWrap 

heart :: Text
heart = "\x02665"

doubleArrows :: Text
doubleArrows = "\x02939\x02938"

intToText :: Int -> Text
intToText = pack . show

tweetFavCountW :: Int -> Widget n
tweetFavCountW count = withAttr tweetFavouriteCountAttr . txtWrap $ (heart <> (intToText count))

tweetRetweetCountW :: Int -> Widget n
tweetRetweetCountW count = padLeft (Pad 2) . withAttr tweetFavouriteCountAttr . txtWrap $ (doubleArrows <> (intToText count))

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
  , appAttrMap = const $ miloAttrMap
}

miloAttrMap :: AttrMap
miloAttrMap = 
  attrMap A.defAttr 
    [
        (tweetTextAttr, fg V.yellow)
      , (tweetUserScreenNameAttr, fg V.green)
      , (tweetCreatedAtAttr, fg V.white)
      , (tweetFavouriteCountAttr, fg V.red)
      , (tweetRetweetCountAttr, fg V.green)

    ]

tweetTextAttr, tweetUserScreenNameAttr, tweetCreatedAtAttr, tweetFavouriteCountAttr, tweetRetweetCountAttr :: AttrName
tweetTextAttr           = "milo" <> "tweet" <> "full_text"
tweetUserScreenNameAttr = "milo" <> "tweet" <> "user" <> "screen_name"
tweetCreatedAtAttr = "milo" <> "tweet" <> "created_at"
tweetFavouriteCountAttr = "milo" <> "tweet" <> "favouriteCount"
tweetRetweetCountAttr = "milo" <> "tweet" <> "retweetCount"


withTopLabel :: Widget s -> Widget s -> Widget s
withTopLabel tl w = withBorderStyle unicode $ borderWithLabel tl $ w

topLabel :: Text -> Widget n
topLabel = str . unpack 
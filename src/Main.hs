{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Main where

import           Control.Monad   (guard, void, (<=<))
import           Data.Foldable   (for_)

import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock (getCurrentTime)
import           Reflex.Dom

import           Rule

-- TODO
-- 1. Speed range
-- 3. Click events delete rows underneath and repopulate ? Maybe?
main :: IO ()
main = do
  firstRow <- randomRow 80
  now <- getCurrentTime
  mainWidgetWithHead headWidget $ do
    tick <- tickLossy 0.05 now
    el "body" $
      divClass "center" $ do
        elClass "h2" "header" $ text "Rule 110"
        (play) <- divClass "controls" $ do
          play <- playButton False
          return (play)
        divClass "rows-container center" $ do
          let tick' = gate (current play) tick -- Filter out ticks while paused
          rowsD <- reverse <$$> foldDynMaybe (const stepRow) [firstRow] tick'
          rowsWidget rowsD

playButton :: (MonadWidget t m) => Bool -> m (Dynamic t Bool)
playButton initState = do
  rec isOnD <- toggle initState btnE
      btnE <- do
        (e, _) <- element "button" def $ do
          let iconAttrs = ffor isOnD $ \case
                True -> "class" =: "fa fa-pause"
                False -> "class" =: "fa fa-play"
          elDynAttr "i" iconAttrs blank
        return $ domEvent Click e
  return isOnD

rowsWidget :: MonadWidget t m => Dynamic t [Row Bit] -> m ()
rowsWidget rowsD =
  void $ simpleList rowsD
    (drawRow <=< sample . current)

stepRow :: [Row Bit] -> Maybe [Row Bit]
stepRow rs@(r:_) = do
  guard . not $ ruleFinished r
  return $ step r : rs

drawRow :: MonadWidget t m => Row Bit -> m ()
drawRow r =
  divClass "row-container" $
    for_ r $ \case
      Zero -> divClass "zero cell" $ blank
      One  -> divClass "one cell"  $ blank

headWidget :: MonadWidget t m => m ()
headWidget = do
  el "style" $ text css
  elAttr "meta" [("http-equiv", "Content-type"), ("content", "text/html; charset=UTF-8")] blank
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
  where
    styleSheet href = elAttr "link" [("rel", "stylesheet"), ("type", "text/css"), ("href", href)] blank

css :: Text
css =
  T.unwords [ ".zero {color: #666; background-color: #666;}"
             , ".one {color: white; background-color: white;}"
             , ".cell {width: 10px; height: 10px; border: 0.5px solid rgba(0, 0, 0, .2); margin: 0px;}"
             , ".row-container {display: flex; justify-content: center;}"
             , ".rows-container {display: flex; flex-direction: column;}"
             , ".text-center {margin: 0 auto;}"
             , ".header {padding: 10px;}"
             , ".controls {padding: 10px;}"
             , ".center {margin: 0 auto; text-align: center;}"
             ]

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

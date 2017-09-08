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
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime,
                                  getCurrentTime)
import           Reflex.Dom

import           Rule

-- TODO
-- 1. Style controls, pin to top of screen, bootstrap it or something
-- 2. Maybe autoscroll so bottom is always visible
-- 3. Click events delete rows underneath and repopulate ? Maybe?
-- 4. Number input to control length of row...
main :: IO ()
main = do
  firstRow <- randomRow 80
  now <- getCurrentTime
  mainWidgetWithHead headWidget $ do
    tick <- tickLossy 0.01 now
    el "body" $
      divClass "center" $ do
        elClass "h2" "header" $ text "Rule 110"
        (play, delay') <- divClass "controls" $ do
          play <- playButton False
          delay' <- delayRange initialDelay
          return (play, delay')
        divClass "rows-container center" $ do
          let tick' = (gate $ current play)      -- Filter out ticks while paused
                     . attachPromptlyDyn delay'  -- Tag event with delay info
                     $ tick
          rowsD <- reverse . fmap fst <$$> foldDynMaybe stepRow [(firstRow, now)] tick'
          rowsWidget rowsD

playButton :: (MonadWidget t m) => Bool -> m (Dynamic t Bool)
playButton initVal = do
  rec isOnD <- toggle initVal btnE
      btnE <- do
        (e, _) <- element "button" def $ do
          let iconAttrs = ffor isOnD $ \case
                True -> "class" =: "fa fa-pause"
                False -> "class" =: "fa fa-play"
          elDynAttr "i" iconAttrs blank
        return $ domEvent Click e
  return isOnD

delayRange :: (MonadWidget t m) => NominalDiffTime -> m (Dynamic t NominalDiffTime)
delayRange initVal = do
  -- TODO dynamically change value from green to yellow or something
  r <- rangeInput $
    def & rangeInputConfig_attributes .~ constDyn [("min", ".04"), ("max", "2.0"), ("step", "0.01")]
        & rangeInputConfig_initialValue .~ toFloat initVal
  return $ fromFloat <$> value r
  where
    toFloat = realToFrac
    fromFloat = realToFrac


rowsWidget :: MonadWidget t m => Dynamic t [Row Bit] -> m ()
rowsWidget rowsD =
  void $ simpleList rowsD
    (drawRow <=< sample . current)

stepRow
  :: (NominalDiffTime, TickInfo)
  -> [(Row Bit, UTCTime)]
  -> Maybe [(Row Bit, UTCTime)]
stepRow _ [] = Nothing -- this shouldn't happen
stepRow (d, ti) rowInfos@((row, lastUpdate):_) = do
  let tickTime = _tickInfo_lastUTC ti
  guard $ (tickTime `diffUTCTime` lastUpdate) >= d -- Nothing if delay has not elapsed
  guard . not $ ruleFinished row                   -- Nothing if computation finished
  return $ (step row, tickTime) : rowInfos

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

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

delayD :: (MonadWidget t m) => Dynamic t NominalDiffTime -> Event t a -> m (Event t a)
delayD dynD e = do
  d <- sample . current $ dynD
  delay d e

initialDelay :: NominalDiffTime
initialDelay = 1

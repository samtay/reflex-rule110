{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Main where

import           Control.Monad   (guard, void, (<=<))
import           Data.Foldable   (for_)

import           Data.Map        (Map)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime,
                                  getCurrentTime)
import           Reflex.Dom

import           Rule

-- TODO
-- 1. Click events delete rows underneath and repopulate ? Maybe?
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
          delay' <- delayRange
          return (play, delay')
        el "hr" blank
        divClass "rows-container center" $ do
          let tick' = (gate $ current play)      -- Filter out ticks while paused
                     . attachPromptlyDyn delay'  -- Tag event with delay info
                     $ tick
          row <- foldDynMaybe stepRow (firstRow, 0, now) tick'
          let (rowMap, rowTotal) = splitDynPure . ffor row $ \(r, i, _) -> (i =: Just r, i + 1)
          void $ rowsWidget rowTotal (updated rowMap)

rowsWidget :: MonadWidget t m
  => Dynamic t Int -- ^ A 'Dynamic' of the total number of items
  -> Event t (Map Int (Maybe (Row Bit))) -- ^ The update 'Event'.
  -> m (Dynamic t (Int, Int), Dynamic t (Map Int (Row Bit)))
  -- ^ A tuple containing: a 'Dynamic' of the (based on the current scroll position)
  -- and number of items currently being rendered, and the 'Dynamic' list result
rowsWidget rowTotal newRow = do
  let lastIndex = subtract 1 <$> updated rowTotal
  virtualList (constDyn 500) 10 rowTotal 1 lastIndex id mempty newRow $ \_ v _ -> drawRow v

stepRow
  :: (NominalDiffTime, TickInfo)
  -> (Row Bit, Int, UTCTime)
  -> Maybe (Row Bit, Int, UTCTime)
stepRow (d, ti) (row, ix, lastUpdate) = do
  let tickTime = _tickInfo_lastUTC ti
  guard $ (tickTime `diffUTCTime` lastUpdate) >= d -- Nothing if delay has not elapsed
  guard . not $ ruleFinished row                   -- Nothing if computation finished
  return $ (step row, ix + 1, tickTime)

drawRow :: MonadWidget t m => Row Bit -> m (Row Bit)
drawRow r = do
  divClass "row-container" $
    for_ r $ \case
      Zero -> divClass "zero cell" $ blank
      One  -> divClass "one cell"  $ blank
  return r

playButton :: (MonadWidget t m) => Bool -> m (Dynamic t Bool)
playButton initVal = divClass "control" $ do
  rec isOnD <- toggle initVal btnE
      btnE <- do
        (e, _) <- element "button" defBtn $ do
          let iconAttrs = ffor isOnD $ \case
                True -> "class" =: "fa fa-pause"
                False -> "class" =: "fa fa-play"
          elDynAttr "i" iconAttrs blank
        return $ domEvent Click e
  return isOnD
  where
    defBtn = def & elementConfig_initialAttributes .~ "class" =: "btn"

delayRange :: (MonadWidget t m) => m (Dynamic t NominalDiffTime)
delayRange = divClass "control range-container" $ do
  el "label" $ text "Speed"
  r <- rangeInput $
    def & rangeInputConfig_attributes .~ constDyn [("min", ".04"), ("max", "2.0"), ("step", "0.01")]
        & rangeInputConfig_initialValue .~ toFloat 1
  return $ (2.04 -) . fromFloat <$> value r
  where
    toFloat = realToFrac
    fromFloat = realToFrac

headWidget :: MonadWidget t m => m ()
headWidget = do
  el "style" $ text css
  elAttr "meta" [("http-equiv", "Content-type"), ("content", "text/html; charset=UTF-8")] blank
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
  styleSheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
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
             , ".controls {padding: 10px; display: flex; flex-direction: row; justify-content: center;}"
             , ".control {padding: 8px;}"
             , ".range-container {margin-top: -12px;}"
             , ".center {margin: 0 auto; text-align: center;}"
             ]

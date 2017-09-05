{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Monad         (guard, void, (<=<))
import           Data.Foldable         (for_)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock       (getCurrentTime)
import           Reflex.Dom

import           Rule

-- TODO
-- 1. Speed range
-- 2. Scrollable?
-- 3. Click events delete rows underneath and repopulate ? Maybe?
main :: IO ()
main = do
  firstRow <- randomRow 80
  now <- getCurrentTime
  mainWidgetWithCss css $ do
    tick <- tickLossy 0.05 now
    el "body" $
      divClass "center" $ do
        elClass "h2" "header" $ text "Rule 110"
        divClass "rows-container center" $ do
          rowsD <- reverse <$$> foldDynMaybe (const stepRow) [firstRow] tick
          rowsWidget rowsD

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

css :: ByteString
css =
  BS.unwords [ ".zero {color: #666; background-color: #666;}"
             , ".one {color: white; background-color: white;}"
             , ".cell {width: 10px; height: 10px; border: 0.5px solid rgba(0, 0, 0, .2); margin: 0px;}"
             , ".row-container {display: flex; justify-content: center;}"
             , ".rows-container {display: flex; flex-direction: column;}"
             , ".text-center {margin: 0 auto;}"
             , ".header {padding: 10px;}"
             , ".center {margin: 0 auto; text-align: center;}"
             ]

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

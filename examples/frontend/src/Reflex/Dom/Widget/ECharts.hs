{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.Widget.ECharts where

import Prelude hiding ((!!))
import Reflex.Dom.Core
import ECharts hiding (ffor)

import Language.Javascript.JSaddle

import Data.Time
import Data.Some (Some)
import qualified Data.Some as Some
import Control.Monad (forM, void, foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Fix (MonadFix)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens

type XAxisData = Text

data LineChartConfig t k = LineChartConfig
  -- XXX Can be made a Dynamic
  -- and use this API to adjust size
  -- https://ecomfe.github.io/echarts-doc/public/en/api.html#echartsInstance.resize
  { _lineChartConfig_size :: (Int, Int)
  -- We will re-create the whole chart if the options change
  , _lineChartConfig_options :: Dynamic t ChartOptions
  , _lineChartConfig_series :: Map k
    ( Series SeriesLine
    , Dynamic t (Map XAxisData (Data SeriesLine))
    , Dynamic t [XAxisData]
    )
  }

-- Currently nothing interesting here
data Chart = Chart

lineChart
  :: forall t m k .
     ( Ord k
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadSample t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => LineChartConfig t k
  -> m Chart
lineChart c = do
  let attr = (_lineChartConfig_size c) & \(w, h) ->
        "style" =: ("width:" <> tshow w <> "px; height:" <> tshow h <> "px;")
  e <- fst <$> elAttr' "div" attr blank

  -- The initialization is done using PostBuild because the element need
  -- to be present in the DOM before calling echarts APIs
  p <- getPostBuild
  opt0 <- sample (current $ _lineChartConfig_options c)
  optV <- liftJSM $ toJSVal opt0
  optVObj <- liftJSM $ makeObject optV

  -- Init the chart
  chart <- performEvent $ ffor p $ \_ -> liftJSM $ do
    c <- ECharts.initECharts $ _element_raw e
    toJSVal optVObj >>= setOptionWithCatch c
    return c

  cDyn <- holdDyn Nothing (Just <$> chart)

  -- Convert the user specified "series" options to JSVal
  -- and modify it according to the Dynamic values
  -- (series, (series.xAxisIndex, xAxis[i].data))
  -- The (xAxisIndex, xAxis[i].data) are later used to modify the "xAxis" object
  dataJSVals :: [Dynamic t (Object, (Int, JSVal))] <-
    forM (Map.elems $ _lineChartConfig_series c) $ \(s, dd, xd) -> do
      -- series options without the data
      sVal <- liftJSM (makeObject =<< toJSVal (Some.This $ SeriesT_Line s))

      let
        i = maybe 0 id (s ^. series_xAxisIndex)
        makeJSVals m xs = do
          -- The ordering of elements is determined by xs
          -- XXX default value of 0 might be wrong here
          dv <- toJSVal (map (\x -> Map.findWithDefault (DataInt 0) x m) xs)
          setProp "data" dv sVal
          xv <- toJSVal xs
          return xv

      initV <- do
        m <- sample (current dd)
        xs <- sample (current xd)
        liftJSM $ makeJSVals m xs

      let yx = (,) <$> dd <*> xd
      ev <- performEvent $ ffor (updated yx) $ \(m, xs) ->
        liftJSM $ makeJSVals m xs

      foldDyn (\xv _ -> (sVal, (i, xv))) (sVal, (i, initV)) ev

  let
    dataDyn = sequenceA dataJSVals
    updEv = leftmost [(updated dataDyn), tag (current dataDyn) chart]
  m1 :: Event t [(JSString, JSVal)] <- performEvent $ ffor updEv $ \d -> liftJSM $ do
    series <- toJSVal $ map fst d
    xAxisObj <- getProp "xAxis" optVObj >>= makeObject

    let f (i, v) = do
          -- XXX This can throw exception if user did not specify
          -- xAxis in ChartOptions correctly
          a <- (xAxisObj !! i) >>= makeObject
          setProp "data" v a
    mapM f $ map snd d
    xAxis <- toJSVal xAxisObj
    return [("series", series), ("xAxis", xAxis)]

  performEvent $ ffor (attach (current cDyn) m1) $ \((Just chart), fvs) -> liftJSM $ do
    mapM (\(f, v) -> setProp f v optVObj) fvs
    toJSVal optVObj >>= setOptionWithCatch chart

  return Chart

data TimeLineChartConfig t k = TimeLineChartConfig
  { _timeLineChartConfig_size :: (Int, Int)
  -- We will re-create the whole chart
  , _timeLineChartConfig_options :: Dynamic t ChartOptions
  , _timeLineChartConfig_appendData :: Map k
    ( Series SeriesLine
    , Int
    , Event t [(UTCTime, Double)]
    )
  }

timeLineChart
  :: forall t m k .
     ( Ord k
     , PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadSample t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => TimeLineChartConfig t k
  -> m Chart
timeLineChart c = do
  let attr = (_timeLineChartConfig_size c) & \(w, h) ->
        "style" =: ("width:" <> tshow w <> "px; height:" <> tshow h <> "px;")
  e <- fst <$> elAttr' "div" attr blank
  p <- getPostBuild
  opt0 <- sample (current $ _timeLineChartConfig_options c)
  optV <- liftJSM $ toJSVal opt0
  optVObj <- liftJSM $ makeObject optV

  -- Init the chart
  chart <- performEvent $ ffor p $ \_ -> liftJSM $ do
    c <- ECharts.initECharts $ _element_raw e
    toJSVal optVObj >>= setOptionWithCatch c
    return c

  cDyn <- holdDyn Nothing (Just <$> chart)

  dataJSVals <- forM (Map.elems $ _timeLineChartConfig_appendData c) $ \(s, len, ev) -> do
    -- series object
    sVal <- liftJSM (makeObject =<< toJSVal (Some.This $ SeriesT_Line s))

    rec
      newArr <- performEvent $ ffor (attach (current arrDyn) ev) $ \(arr, vs) -> liftJSM $ do
        let
          -- The timeline needs special data object with name and value fields
          -- the value has to be a tuple like this to render properly
          f :: (UTCTime, Double) -> Data SeriesLine
          f (t, v) = def
            & data_name ?~ utcTimeToEpoch t
            & data_value ?~ (utcTimeToEpoch t, v)
        new <- mapM toJSVal (map f vs)
        return $ (drop ((length arr) + (length new) - len) arr) ++ new

      arrDyn <- holdDyn [] newArr

    ev <- performEvent $ ffor (updated arrDyn) $ \a -> liftJSM $ do
      v <- toJSVal a
      setProp "data" v sVal

    holdDyn sVal (sVal <$ ev)

  let
    dataDyn :: Dynamic t [Object]
    dataDyn = sequenceA dataJSVals

  performEvent $ ffor (attach (current cDyn) (updated dataDyn)) $ \((Just chart), d) -> liftJSM $ do
    dv <- toJSVal d
    setProp "series" dv optVObj
    toJSVal optVObj >>= setOptionWithCatch chart

  return Chart

setOptionWithCatch :: ECharts -> JSVal -> JSM ()
setOptionWithCatch c o = setOptionJSVal c o
  -- When doing development with jsaddle, the exceptions thrown by echarts are not shown
  -- This is a workaround to capture and show the exceptions
  -- `catch` \(JSException e) -> (valToText e) >>= (liftIO . putStrLn . show) >> return ()

tshow :: Show a => a -> Text
tshow = T.pack . show

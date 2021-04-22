{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.Widget.ECharts
  ( LineChartConfig(..)
  , Chart(..)
  , TimeLineChartConfig(..)
  , lineChart
  , timeLineChart
  , module X
  )
  where

import Prelude hiding ((!!))
import Reflex.Dom.Core
import ECharts as X hiding (ffor)

import Language.Javascript.JSaddle

import Data.Time
import qualified Data.Some as Some
import Control.Monad (forM, void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens
import Reflex.Network

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

data Chart t = Chart
  { _chart_rendered :: Event t ()
  , _chart_finished :: Event t ()
  }

lineChart
  :: forall t m k .
     ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadHold t m
     , TriggerEvent t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => LineChartConfig t k
  -> m (Chart t)
lineChart c = do
  let
    cDyn = _lineChartConfig_options c
    attr = (_lineChartConfig_size c) & \(w, h) ->
      "style" =: ("width:" <> tshow w <> "px; height:" <> tshow h <> "px;")
  e <- fst <$> elAttr' "div" attr blank

  -- The initialization is done using PostBuild because the element need
  -- to be present in the DOM before calling echarts APIs
  p <- getPostBuild
  (evR, onActionR) <- newTriggerEvent
  (evF, onActionF) <- newTriggerEvent

  -- Init the chart
  chartEv <- performEvent $ ffor p $ \_ -> liftJSM $ do
    chart <- X.initECharts $ _element_raw e
    X.onRenderedAction chart (liftIO $ onActionR ())
    X.onFinishedAction chart (liftIO $ onActionF ())
    return chart

  void $ widgetHold blank $ ffor chartEv $ \chart -> do
    void $ networkView $ ffor cDyn $ \opt -> do
      -- set first options
      optVObj <- liftJSM $ makeObject =<< toJSVal opt

      -- Convert the user specified "series" options to JSVal
      -- and modify it according to the Dynamic values
      -- (series, (series.xAxisIndex, xAxis[i].data))
      -- The (xAxisIndex, xAxis[i].data) are later used to modify the "xAxis" object
      vs :: [(Object, Event t (Int, JSVal))] <-
        forM (Map.elems $ _lineChartConfig_series c) $ \(s, dd, xd) -> do
          -- series options without the data
          sVal <- liftJSM (makeObject =<< toJSVal (Some.mkSome $ SeriesT_Line s))

          let
            i = maybe 0 id (s ^. series_xAxisIndex)
            yx = (,) <$> dd <*> xd

          ev <- networkView $ ffor yx $ \(m, xs) -> liftJSM $ do
            -- The ordering of elements is determined by xs
            -- XXX default value of 0 might be wrong here
            dv <- toJSVal (map (\x -> Map.findWithDefault (DataInt 0) x m) xs)
            setProp "data" dv sVal
            toJSVal xs

          return (sVal, (,) i <$> ev)

      let
        updEv = mergeList $ map snd vs
        seriesJSVals = map fst vs

      performEvent_ $ ffor updEv $ \xs -> liftJSM $ do
        series <- toJSVal seriesJSVals
        xAxisObj <- getProp "xAxis" optVObj >>= makeObject
        let
          f v = f' v
            `catch` \(JSException e) -> liftIO $ putStrLn
            "reflex-dom-echarts: Error in '_lineChartConfig_series' value.\
           \ The 'xAxis' for specified 'xAxisIndex' does not exist in 'ChartOptions'"
          f' (i, v) = do
              a <- (xAxisObj !! i) >>= makeObject
              setProp "data" v a
        mapM_ f xs
        xAxis <- toJSVal xAxisObj
        setProp "xAxis" xAxis optVObj
        setProp "series" series optVObj
        toJSVal optVObj >>= setOptionWithCatch chart

  return (Chart evR evF)

data TimeLineChartConfig t k = TimeLineChartConfig
  { _timeLineChartConfig_size :: (Int, Int)
  -- We will re-create the whole chart
  , _timeLineChartConfig_options :: Dynamic t ChartOptions
  , _timeLineChartConfig_appendData :: Map k
    ( Series SeriesLine
    , Int -- max number of data points
    , Event t [(UTCTime, Double)]
    )
  }

timeLineChart
  :: forall t m k .
     ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadHold t m
     , TriggerEvent t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     )
  => TimeLineChartConfig t k
  -> m (Chart t)
timeLineChart c = do
  let
    cDyn = _timeLineChartConfig_options c
    attr = (_timeLineChartConfig_size c) & \(w, h) ->
      "style" =: ("width:" <> tshow w <> "px; height:" <> tshow h <> "px;")
  e <- fst <$> elAttr' "div" attr blank
  p <- getPostBuild
  (evR, onActionR) <- newTriggerEvent
  (evF, onActionF) <- newTriggerEvent

  -- Init the chart
  chartEv <- performEvent $ ffor p $ \_ -> liftJSM $ do
    chart <- X.initECharts $ _element_raw e
    -- This causes a flickr in charts
    -- dont know what is the fix
    -- X.onRenderedAction chart (liftIO $ onActionR ())
    -- X.onFinishedAction chart (liftIO $ onActionF ())
    -- Meanwhile trigger these actions on setting the options below
    return chart

  void $ widgetHold blank $ ffor chartEv $ \chart -> do
    void $ networkView $ ffor cDyn $ \opt -> do
      -- set first options
      optVObj <- liftJSM $ makeObject =<< toJSVal opt

      vs <- forM (Map.elems $ _timeLineChartConfig_appendData c) $ \(s, len, ev) -> do
        -- series object
        sVal <- liftJSM (makeObject =<< toJSVal (Some.mkSome $ SeriesT_Line s))

        rec
          newArr <- performEvent $ ffor (attach (current arrDyn) ev) $ \(arr, vs) -> liftJSM $ do
            let
              -- The timeline needs special data object with name and value fields
              -- the value has to be a tuple like this to render properly
              f :: (UTCTime, Double) -> Data SeriesLine
              f (t, v) = def
                & data_name ?~ utcTimeToEpoch t
                & data_value ?~ (utcTimeToEpoch t, v)
            n <- mapM toJSVal (map f vs)

            let newArrV = (drop ((length arr) + (length n) - len) arr) ++ n
            v <- toJSVal newArrV
            setProp "data" v sVal
            return $ newArrV

          arrDyn <- holdDyn [] newArr

        return (sVal, () <$ newArr)

      let
        updEv = leftmost $ map snd vs
        seriesJSVals = map fst vs

      performEvent_ $ ffor updEv $ \_ -> liftJSM $ do
        dv <- toJSVal seriesJSVals
        setProp "series" dv optVObj
        toJSVal optVObj >>= setOptionWithCatch chart
        -- This is a workaround, see the comments above
        liftIO $ onActionR ()
        liftIO $ onActionF ()

  return (Chart evR evF)

setOptionWithCatch :: ECharts -> JSVal -> JSM ()
setOptionWithCatch c o = setOptionJSVal c o
  -- When doing development with jsaddle, the exceptions thrown by echarts are not shown
  -- This is a workaround to capture and show the exceptions
  -- `catch` \(JSException e) -> (valToText e) >>= (liftIO . putStrLn . show) >> return ()

tshow :: Show a => a -> Text
tshow = T.pack . show

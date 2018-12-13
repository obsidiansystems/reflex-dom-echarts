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
  -- Should be Dynamic
  { _lineChartConfig_size :: (Int, Int)
  -- We will re-create the whole chart
  , _lineChartConfig_options :: Dynamic t ChartOptions
  , _lineChartConfig_series ::
      Map k ( Series SeriesLine
            , Dynamic t (Map XAxisData (Data SeriesLine))
            , Dynamic t [XAxisData]
            )
  -- Easy to use interface, Later
  -- , _chartConfig_updateData :: Map k (Series seriesType
  --                                , (Event t [XData], Event t [(XData, YData)])
  --                                , Event t (Map XData (Maybe YData))
  -- -- only append update of Array
  -- , _chartConfig_appendData :: Map k (Int, Event t [Data seriesType])
  -- This will be necessary to modify axis
  -- , _lineChartConfig_xAxisData :: Dynamic t [XAxisData]
  }

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
  p <- getPostBuild
  opt0 <- sample (current $ _lineChartConfig_options c)
  optV <- liftJSM $ toJSVal opt0
  optVObj <- liftJSM $ makeObject optV

  -- Init the chart
  chart <- performEvent $ ffor p $ \_ -> liftJSM $ do
    liftIO $ putStrLn "hello"
    c <- ECharts.initECharts $ _element_raw e
    toJSVal optVObj >>= setOptionWithCatch c
    return c

  -- performEvent_ $ ffor chart $ \c -> liftJSM $ do
  --   (toJSVal opt0Obj >>= setOptionWithCatch c)
    -- w <- jsg ("console" :: Text)
    -- w ^. js1 ("log" :: Text) (o)
    -- return ()

  cDyn <- holdDyn Nothing (Just <$> chart)

  -- Convert the series options to
  -- (Series JSVal, (xAxisIndex, xAxis[i].data))
  -- [Dynamic t (JSVal, (Int, JSVal))]
  dataJSVals <- forM (Map.elems $ _lineChartConfig_series c) $ \(s, dd, xd) -> do
    -- series
    let
      i = maybe 0 id (s ^. series_xAxisIndex)
    sVal <- liftJSM (makeObject =<< toJSVal (Some.This $ SeriesT_Line s))

    let makeJSVals m xs = do
          dv <- toJSVal (map (\x -> Map.findWithDefault (DataInt 0) x m) xs)
          setProp "data" dv sVal
          xv <- toJSVal xs
          return xv

    -- xAxis data
    initV <- do
      m <- sample (current dd)
      xs <- sample (current xd)
      liftJSM $ makeJSVals m xs

    let yx = (,) <$> dd <*> xd
    ev <- performEvent $ ffor (updated yx) $ \(m, xs) -> liftJSM $ makeJSVals m xs

    foldDyn (\xv _ -> (sVal, (i, xv))) (sVal, (i, initV)) ev

  let dataDyn = sequenceA dataJSVals
  -- add to opt0 and set
  -- performEvent $ ffor (attach (current dataDyn) chart) $ \(d, chart) -> liftJSM $ do
  --   dv <- toJSVal d
  --   setProp "series" dv opt0Obj
  --   liftJSM $ do
  --     w <- jsg ("console" :: Text)
  --     w ^. js1 ("log" :: Text) dv
  --   toJSVal opt0Obj >>= setOptionWithCatch chart

  let updEv = leftmost [(updated dataDyn), tag (current dataDyn) chart]
  m1 <- performEvent $ ffor updEv $ \d -> liftJSM $ do
    dv <- toJSVal $ map fst d
    xAxis <- getProp "xAxis" optVObj >>= makeObject

    let f (i, v) = do
          a <- (xAxis !! i) >>= makeObject
          setProp "data" v a
    mapM f $ map snd d
    xv <- toJSVal xAxis
    return [("series", dv), ("xAxis", xv)]

  let
    modObjEv :: Event t [(JSString, JSVal)]
    modObjEv = m1

  performEvent $ ffor (attach (current cDyn) modObjEv) $ \((Just chart), fvs) -> liftJSM $ do
    mapM (\(f, v) -> setProp f v optVObj) fvs
    toJSVal optVObj >>= setOptionWithCatch chart

  return Chart

data TimeLineChartConfig t k = TimeLineChartConfig
  { _timeLineChartConfig_size :: (Int, Int)
  -- We will re-create the whole chart
  , _timeLineChartConfig_options :: Dynamic t ChartOptions
  , _timeLineChartConfig_appendData ::
      Map k ( Series SeriesLine
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
    liftIO $ putStrLn "hello"
    c <- ECharts.initECharts $ _element_raw e
    toJSVal optVObj >>= setOptionWithCatch c
    return c

  cDyn <- holdDyn Nothing (Just <$> chart)

  dataJSVals <- forM (Map.elems $ _timeLineChartConfig_appendData c) $ \(s, len, ev) -> do
    -- series
    sVal <- liftJSM (makeObject =<< toJSVal (Some.This $ SeriesT_Line s))

    rec
      newArr <- performEvent $ ffor (attach (current arrDyn) ev) $ \(arr, vs) -> liftJSM $ do
        let
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

  let dataDyn = sequenceA dataJSVals

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

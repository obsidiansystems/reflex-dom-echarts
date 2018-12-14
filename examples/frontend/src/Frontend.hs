{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend where

import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad (void)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Text.URI
import Data.Functor.Sum
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T

import Common.Types
import Common.Route
import Obelisk.Generated.Static
import qualified Obelisk.ExecutableConfig as Cfg

import Language.Javascript.JSaddle hiding ((!!))
import Control.Lens
import Reflex.Dom.Widget.ECharts

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "meta" ("charset" =: "utf-8") blank
      -- XXX is there a way to automatically add this?
      elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"echarts.min.js") blank

  , _frontend_body = do
      r <- liftIO $ Cfg.get "config/common/route"
      app r
  }

app
  :: forall t m js .
     ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js m
     )
  => Maybe Text
  -> m ()
app r = prerender blank $ do
  pb <- getPostBuild
  ev <- cpuStatWebSocket r
  void $ widgetHold blank $ ffor pb $ \_ -> do
    void $ basicLineChart
    void $ cpuStatTimeLineChart ev
    void $ multipleXAxes
    return ()
  return ()

tickWithSpeedSelector
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadFix m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadIO (Performable m)
     )
  => m (Event t TickInfo)
tickWithSpeedSelector = do
  r <- rangeInput $ def
    & rangeInputConfig_initialValue .~ 1
    & rangeInputConfig_attributes .~ constDyn (("min" =: "0.1") <> ("max" =: "2") <> ("step" =: "0.1"))
  dyn ((\v -> tickLossyFromPostBuildTime (fromRational $ toRational v)) <$> (value r))
    >>= switchHold never

basicLineChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart)
basicLineChart = do
  tick <- tickWithSpeedSelector
  let
    f _ m = Map.fromList $ zip xAxisData $ ls ++ [l]
      where (l:ls) = map (\x -> Map.findWithDefault (DataInt 0) x m) xAxisData
  dd <- do
    cb <- el "div" $ do
      el "label" $ text "Dynamic line 1"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn f yAxisData ev
  dd2 <- do
    cb <- el "div" $ do
      el "label" $ text "Dynamic line 2"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn f yAxisData2 ev

  xd <- do
    cb <- el "div" $ do
      el "label" $ text "X-axis"
      checkbox False def
    let ev = gate (current $ value cb) tick
    foldDyn (\_ (l:ls) -> ls ++ [l]) xAxisData ev

  let chartDataDyn = (0 =: (def, dd, xd)) <> (1 =: (dd2Series, dd2, xd))
      dd2Series = def
        & series_smooth ?~ Left True
        & series_areaStyle ?~ def

  lineChart (LineChartConfig (600, 400)
              (constDyn basicLineChartOpts)
              chartDataDyn
            )
  where
    yAxisData = Map.fromList $ zip xAxisData $ map DataInt $ reverse [820, 932, 901, 934, 1290, 1330, 1320]
    yAxisData2 = Map.fromList $ zip xAxisData $ map DataInt $ [820, 932, 901, 934, 1290, 1330, 1320]
    xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
    basicLineChartOpts :: ChartOptions
    basicLineChartOpts = def
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category
        & axis_data ?~ (zip xAxisData $ repeat Nothing)
        ) : []

multipleXAxes
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadFix m
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart)
multipleXAxes =
  lineChart $ LineChartConfig (600, 400) (constDyn multipleXAxesOpts)
    (chartDataDyn)
  where
    chartDataDyn = (0 =: (s1, constDyn y1, constDyn x1)) <> (1 =: (s2, constDyn y2, constDyn x2))
    s1 = def
      & series_smooth ?~ Left True
      & series_name ?~ xSeriesName1
      & series_xAxisIndex ?~ 1
    s2 = def
      & series_smooth ?~ Left True
      & series_name ?~ xSeriesName2
    xSeriesName1 = "2015"
    xSeriesName2 = "2016"
    colors = ["#5793f3", "#d14a61", "#675bba"]
    y1 = Map.fromList $ zip (months xSeriesName1) $
      map DataDouble [2.6, 5.9, 9.0, 26.4, 28.7, 70.7, 175.6, 182.2, 48.7, 18.8, 6.0, 2.3]
    y2 = Map.fromList $ zip (months xSeriesName2) $
      map DataDouble [3.9, 5.9, 11.1, 18.7, 48.3, 69.2, 231.6, 46.6, 55.4, 18.4, 10.3, 0.7]
    months y = map (\m -> y <> "-" <> tshow m) [1..12]
    x1 = months xSeriesName1
    x2 = months xSeriesName2

    multipleXAxesOpts :: ChartOptions
    multipleXAxesOpts = def
      & chartOptions_legend ?~ (def
        & legend_data ?~ [ (xSeriesName1, def)
                         , (xSeriesName2, def)
                         ])
      & chartOptions_tooltip ?~ (def
        & toolTip_trigger ?~ "none"
        & toolTip_axisPointer ?~ (def
          & axisPointer_type ?~ "Cross"))
      & chartOptions_grid .~ (def
        & grid_pos ?~ (def
          & pos_top ?~ PosAlign_Pixel 70
          & pos_bottom ?~ PosAlign_Pixel 50)) : []
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Category
        & axis_axisTick ?~ (def & axisTick_alignWithLabel ?~ True)
        & axis_axisLine ?~ (def
          & axisLine_onZero ?~ False
          & axisLine_lineStyle ?~ (def & lineStyle_color ?~ colors !! 1))
        -- TODO formatter
        -- & axis_axisPointer ?~ (def
        --   & axisPointer_label ?~ def)
        & axis_data ?~ zip x2 (repeat Nothing))
      : (def
        & axis_type ?~ AxisType_Category
        & axis_axisTick ?~ (def & axisTick_alignWithLabel ?~ True)
        & axis_axisLine ?~ (def
          & axisLine_onZero ?~ False
          & axisLine_lineStyle ?~ (def & lineStyle_color ?~ colors !! 0))
        -- TODO formatter
        -- & axis_axisPointer ?~ (def
        --   & axisPointer_label ?~ def)
        & axis_data ?~ zip x1 (repeat Nothing)) : []

cpuStatTimeLineChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadSample t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => Event t (UTCTime, CpuStat Double)
  -> m (Chart)
cpuStatTimeLineChart ev = do
  timeLineChart $ TimeLineChartConfig (600, 400) (constDyn opts)
    chartData
  where
    chartData = Map.fromList $ map (\(t, f) -> (t, (s t, len, g f))) sNames
    g f = ffor ev $ \(t, c) -> [(t, f c)]
    s n = def
      & series_smooth ?~ Left True
      & series_name ?~ n
    len = 50
    opts :: ChartOptions
    opts = def
      & chartOptions_title ?~ (def
        & title_text ?~ "CPU Stats")
      & chartOptions_yAxis .~ (def
        & axis_type ?~ AxisType_Value
        & axis_min ?~ Left 0
        & axis_max ?~ Left 101
                              ) : []
      & chartOptions_xAxis .~ (def
        & axis_type ?~ AxisType_Time) : []
    sNames =
      [ ("user", _cpuStat_user)
      , ("nice", _cpuStat_nice)
      , ("system", _cpuStat_system)
      , ("idle", _cpuStat_idle)
      , ("iowait", _cpuStat_iowait)
      , ("irq", _cpuStat_irq)
      , ("softirq", _cpuStat_softirq)
      , ("steal", _cpuStat_steal)
      , ("guest", _cpuStat_guest)
      , ("guestNice", _cpuStat_guestNice)
      ]

cpuStatWebSocket
  :: forall t m js .
     ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadSample t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , Prerender js m
     , TriggerEvent t m
     )
  => Maybe Text
  -> m (Event t (UTCTime, CpuStat Double))
cpuStatWebSocket r = do
  wsRespEv <- prerender (return never) $ do
    case checkEncoder backendRouteEncoder of
      Left err -> do
        el "div" $ text err
        return never
      Right encoder -> do
        let wsPath = fst $ encode encoder $ InL BackendRoute_EChartsCpuStats :/ ()
        let mUri = do
              uri' <- mkURI =<< r
              pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
              wsScheme <- case uriScheme uri' of
                rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
                rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
                _ -> Nothing
              return $ uri'
                { uriPath = Just (False, pathPiece)
                , uriScheme = Just wsScheme
                }
        case mUri of
          Nothing -> return never
          Just uri -> do
            ws <- webSocket (render uri) $ def
              & webSocketConfig_send .~ (never :: Event t [Text])
            return (_webSocket_recv ws)
  return $ fmapMaybe (Aeson.decode . LBS.fromStrict) wsRespEv

tshow :: Show a => a -> Text
tshow = T.pack . show

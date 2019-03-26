module Component (State, Query(..), ui) where

import Prelude (type (~>), Unit, Void, bind, const, discard, pure, show, ($), (<<<), (<>), (=<<))

import Data.Array (concat)
import Data.Maybe (Maybe(..), isNothing )
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Simple.JSON as JSON
import Web.Event.Event (type_, EventType(..), preventDefault)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)
import Control.Monad.Reader (class MonadAsk, asks)

import Model.Quote (Quote)

-- TODO - move this to some kind of supplemental library
inputR :: forall f a. (a -> f Unit) -> a -> Maybe (f Unit)
inputR f x = Just $ (f x)


-- TODO - result and error should just be captured as one Either value
type State =
  { loading :: Boolean
  , symbol :: String
  , result :: Maybe Quote
  , error :: Maybe String
  }

data Query a
  = SetSymbol String a
  | MakeRequest a
  | PreventDefault Event (Query a)

ui :: forall m r
    . MonadAff m
   => MonadAsk { apiUrl :: String | r } m
   => H.Component HH.HTML Query Unit Void m
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, symbol: "", result: Nothing, error: Nothing}

  -- TODO - break this up into smaller components
  render :: State -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ HH.h1_ [ HH.text "Lookup Stock Quote" ]
      , HH.label_
          [ HH.div_ [ HH.text "Stock Symbol: " ]
          , HH.input
              [ HP.value st.symbol
              , HE.onValueInput (HE.input SetSymbol)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick $ inputR \e ->
                          PreventDefault (toEvent e) $
                          H.action $ MakeRequest
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_ $
          concat [
                    case st.error of
                      Nothing -> []
                      Just err -> [ HH.p_ [ HH.text (err) ] ]
                 ,  case st.result of
                      Nothing -> []
                      Just (q) ->
                        [ HH.h2_ [ HH.text $  q.symbol <> " Quote:" ]
                        , HH.p_  [ HH.text $ "price: " <> q.price ]
                        , HH.p_  [ HH.text $ "open: " <> q.open ]
                        , HH.p_  [ HH.text $ "high: " <> q.high ]
                        , HH.p_  [ HH.text $ "low: " <> q.low ]
                        , HH.p_  [ HH.text $ "volume: " <> q.volume ]
                        , HH.p_  [ HH.text $ "latest trading day: " <> q.latestTradingDay ]
                        , HH.p_  [ HH.text $ "previous close: " <> q.previousClose ]
                        , HH.p_  [ HH.text $ "change: " <> q.change ]
                        , HH.p_  [ HH.text $ "change percent: " <> q.changePercent ]
                        ]
                 ]
      ]

  --  TODO: pull out pure parts into their own functions
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    SetSymbol symbol next -> do
      H.modify_ (_ { symbol = symbol
                   , result = Nothing :: Maybe Quote
                   , error = Nothing :: Maybe String 
                   })
      pure next
    MakeRequest next -> do
    --TODO - move  the API call here + parsing to a service file
      symbol <- H.gets _.symbol
      apiUrl <- asks _.apiUrl
      H.liftEffect $ log $ "api url in use: " <> apiUrl
      let reqUrl = apiUrl <> "/stocks/" <> symbol
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.get AXRF.string (reqUrl)
      let rb = hush response.body
      let (q :: Maybe Quote) = hush <<< JSON.readJSON =<< rb
      let e = if isNothing q then Just "Invalid Symbol" else Nothing
      H.modify_ (_ { loading = false, result = q, error = e })
      pure next
      -- TODO Move this out to a helper lib
    PreventDefault e q -> do
      let (EventType t) = type_ e
      H.liftEffect $ preventDefault e
      H.liftEffect $ log $ show t <> " default navigation prevented"
      eval q
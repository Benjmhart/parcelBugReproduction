module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Component as UI 
import Routing.Hash (getHash)

import Model.AppEnv (AppEnv, runAppM)

-- | Run the app.
main :: String -> String -> Effect Unit
main env apiUrl = HA.runHalogenAff do
  body <- HA.awaitBody
  -- mutable reference used to store user profile if there is one
  -- currentUser <- H.liftEffect $ Ref.new (Nothing :: Maybe User)
  -- landing page of the user - in case they did not navigate to home route
  -- this gets handed off to the router once we have one in place
  initialHash <- H.liftEffect $ getHash
  -- TODO - read a token from local storage and see if the user is logged in here

  let 
    appEnv :: AppEnv
    appEnv = 
      { environment: env
      , apiUrl:      apiUrl
      -- , user:        currentUser
      }
    rootComponent :: H.Component HH.HTML UI.Query Unit Void Aff
    rootComponent = H.hoist (runAppM appEnv) UI.ui
  --
  -- run ReaderT..build 
  halogenIO <- runUI rootComponent unit body
  pure unit
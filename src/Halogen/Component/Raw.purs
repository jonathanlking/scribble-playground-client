-- Taken from https://gist.github.com/prathje/7422e49b7c809fe8236bb2f213e7076e
module Halogen.Component.Raw where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Web.HTML.HTMLElement (HTMLElement)
import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import setHTML :: HTMLElement -> String -> Effect Unit

type RawState =
  { elRef :: String
  , html :: String
  }

data RawQuery a
  = Initialize a

type RawOutput = Void

type RawDSL = H.ComponentDSL RawState RawQuery RawOutput Aff

rawComponent :: H.Component HH.HTML RawQuery RawState RawOutput Aff
rawComponent =
  H.lifecycleComponent
  { render
  , eval
  , initialState : identity
  , initializer  : Just (H.action Initialize)
  , finalizer    : Nothing
  , receiver: const Nothing
  }
  where

    render :: RawState -> H.ComponentHTML RawQuery
    render s = HH.div [ HP.ref (H.RefLabel s.elRef) ] []

    eval :: RawQuery ~> RawDSL
    eval = case _ of
      Initialize next -> do
        updateHTML
        pure next

    updateHTML :: RawDSL Unit
    updateHTML = do
      elRef <- H.gets _.elRef
      H.getHTMLElementRef (H.RefLabel elRef) >>= case _ of
        Nothing -> pure unit
        Just el -> do
          html <- H.gets _.html
          liftEffect $ setHTML el html
          pure unit

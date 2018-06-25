-- Taken from https://gist.github.com/prathje/7422e49b7c809fe8236bb2f213e7076e
module Halogen.Component.Raw where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import DOM.HTML.Types (HTMLElement)
import Data.Maybe (Maybe(..))

foreign import data RAWHTML :: Effect

foreign import setHTML :: forall e. HTMLElement -> String -> Eff (rawhtml :: RAWHTML | e) Unit

type RawState =
  { elRef :: String
  , html :: String
  }

data RawQuery a
  = Initialize a

type RawOutput = Void

type RawMonad e = (Aff (rawhtml :: RAWHTML | e))
type RawDSL   e = H.ComponentDSL RawState RawQuery RawOutput (RawMonad e)

rawComponent :: forall e. H.Component HH.HTML RawQuery RawState RawOutput (RawMonad e)
rawComponent =
  H.lifecycleComponent
  { render
  , eval
  , initialState : id
  , initializer  : Just (H.action Initialize)
  , finalizer    : Nothing
  , receiver: const Nothing
  }
  where

    render :: RawState -> H.ComponentHTML RawQuery
    render s = HH.div [ HP.ref (H.RefLabel s.elRef) ] []

    eval :: RawQuery ~> RawDSL e
    eval = case _ of
      Initialize next -> do
        updateHTML
        pure next

    updateHTML :: RawDSL e Unit
    updateHTML = do
      elRef <- H.gets _.elRef
      H.getHTMLElementRef (H.RefLabel elRef) >>= case _ of
        Nothing -> pure unit
        Just el -> do
          html <- H.gets _.html
          liftEff $ setHTML el html
          pure unit

module Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Int (fromString)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

data Query a
  = Connect a
  | Connecting a
  | Connected a
  | Disconnect a
  | UpdateInput String a
  | QueryFib Int a
  | ResultFib Int a
  | Reset a
  | Errored String a

data Message = ConnectM | FibM Int

data PState
  = SDisconnected
  | SConnecting
  | SReady (Maybe Int)
  | SWorking
  | SResult Int
  | SErrored String

type State = { status :: PState }

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { status: SDisconnected }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Scribble Playground" ]
      , case state.status of
          SDisconnected ->
            HH.p_
              [ HH.text "Disconnected"
              , HH.button
                  [ HE.onClick (HE.input_ Connect) ]
                  [ HH.text "Connect" ]
              ]
          SConnecting ->
            HH.p_ [ HH.text "Connecting" ]
          -- TODO: Refactor into a single case
          SReady Nothing ->
            HH.p_
              [ HH.input
                  [ HP.type_ HP.InputNumber
                  , HP.placeholder "5"
                  , HP.autofocus true
                 , HE.onValueChange (HE.input UpdateInput)
                  ]
              , HH.button
                  [ HP.disabled true ]
                  [ HH.text "Calculate" ]
              ]
          SReady (Just val) ->
            HH.p_
              [ HH.input
                  [ HP.type_ HP.InputNumber
                  , HP.placeholder "5"
                  , HP.autofocus true
                  , HP.value (show val)
                 , HE.onValueChange (HE.input UpdateInput)
                  ]
              , HH.button
                  [ HE.onClick (HE.input_ (QueryFib val)) ]
                  [ HH.text "Calculate" ]
              ]
          SWorking ->
            HH.p_ [ HH.text "Waiting for result..." ]
          SResult res ->
            HH.p_
              [ HH.text ("The result was " <> show res)
              , HH.button
                  [ HE.onClick (HE.input_ Reset) ]
                  [ HH.text "Reset" ]
              ]
          SErrored e ->
            HH.p_ [ HH.text "Errored!"
                  , HH.text e
                  ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Connect next -> do
      H.raise ConnectM
      pure next
    Connecting next -> do
      H.modify_ (\state -> { status: SConnecting })
      pure next
    Connected next -> do
      H.modify_ (\state -> { status: SReady Nothing })
      pure next
    Disconnect next -> do
      H.modify_ (\state -> { status: SDisconnected })
      pure next
    UpdateInput i next -> do
      H.modify_ (\state -> { status: SReady (fromString i) })
      pure next
    QueryFib n next -> do
      H.modify_ (\state -> { status: SWorking })
      H.raise (FibM n)
      pure next
    ResultFib res next -> do
      H.modify_ (\state -> { status: SResult res })
      pure next
    Reset next -> do
      H.modify_ (\state -> { status: SReady Nothing})
      pure next
    Errored e next -> do
      H.modify_ (\state -> { status: SErrored e })
      pure next

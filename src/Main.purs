module Main where

import Prelude
import Effect (Effect)

import Control.Coroutine (await, Consumer)
import Effect.Aff (Aff, delay)

import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath as CP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Halogen.HTML.CSS as CSS

import AceComponent (AceOutput(..), AceQuery(..), aceComponent)

import Data.Either (Either(..), either)
import Data.Time.Duration (Milliseconds(..))

import Scribble.FSM
import Scribble.Core
import Scribble.WebSocket (WebSocket, URL(..))
import Type.Proxy (Proxy(..))
import Data.Symbol (SProxy(SProxy))
import Effect.Aff.AVar (AVar)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Partial.Unsafe (unsafeCrashWith)
import Control.Monad.Error.Class (throwError)
import Scribble.Halogen
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Control.Parallel

import Graphics.Viz as Viz
import Halogen.Component.Raw
import Data.Functor.Coproduct.Nested
import Data.Either.Nested

import CSS.Font (color)
import Color.Scheme.Clrs (red, green)

import Scribble.Protocol.Playground.LanguageServer as LS

type Scribble = String
type ProtocolName = String
type RoleName = String

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { text :: Scribble, protocol :: ProtocolName, role :: RoleName,  mode :: Mode }

data Mode
  = Connecting
  | Ready (Either String Result)
  | Working
  | Disconnected
derive instance eqMode :: Eq Mode

data Message
  = VerifyM Scribble
  | ProjectM Scribble ProtocolName RoleName
  | FSMM Scribble ProtocolName RoleName

data Result
  = FSM String
  | Projection Scribble
  | Verified
  | Connected   
derive instance eqResult :: Eq Result

-- | The query algebra for the app.
data Query a
  = ResultEvent (Either String Result) a
  | PerformQuery Message a
  | HandleAceUpdate String a
  | UpdateProtocolName String a
  | UpdateRoleName String a
  | Disconnect a

-- | The slot address type for the Ace component.
data AceSlot = AceSlot
derive instance eqAceSlot :: Eq AceSlot
derive instance ordAceSlot :: Ord AceSlot

type ChildQuery = Coproduct2 AceQuery RawQuery
type ChildSlot = Either2 AceSlot Unit 

-- | The main UI component definition.
ui :: forall eff. H.Component HH.HTML Query Unit Message Aff
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { text: "", protocol: "", role: "", mode: Connecting }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render { text: text, mode: mode, role: role, protocol: protocol} =
    HH.div_
      [ HH.h3_
          [ HH.text "Scribble Playground" ]
      , HH.div_
          [ HH.slot' CP.cp1 AceSlot aceComponent unit handleAceOutput ]
      , HH.div_
          [ HH.p_
              [ HH.text "Protocol:" 
              , HH.input [HE.onValueChange (HE.input UpdateProtocolName)]
              ]
          , HH.p_
              [ HH.text "Role:" 
              , HH.input [HE.onValueChange (HE.input UpdateRoleName)]
              ]
          , HH.p_
              [ HH.button
                  [ HE.onClick (HE.input_ $ PerformQuery (VerifyM text)), enabled ]
                  [ HH.text "Verify" ]
              , HH.button
                  [ HE.onClick (HE.input_ $ PerformQuery (ProjectM text role protocol)), enabled ]
                  [ HH.text "Project" ]
              , HH.button
                  [ HE.onClick (HE.input_ $ PerformQuery (FSMM text role protocol)), enabled ]
                  [ HH.text "Generate FSM" ]
              ]
          ]
      , result
      ]
    where
    result = case mode of
      Connecting -> HH.p_ [ HH.text "Connecting..." ]
      (Ready (Left e)) -> HH.p [ CSS.style (color red) ] [ HH.text $ "Error: " <> e ]
      (Ready (Right Connected)) -> HH.p_ [ HH.text "Connected" ]
      (Ready (Right Verified)) -> HH.h3 [ CSS.style (color green) ] [ HH.text "Verified!" ]
      (Ready (Right (FSM desc))) -> HH.div_ [HH.slot' CP.cp2 unit rawComponent {html: Viz.viz desc Viz.SVG, elRef: "fsm"} (const Nothing)] 
      (Ready (Right (Projection proj))) -> HH.p_ [ HH.text proj ]
      Working -> HH.p_ [ HH.text "Working..." ]
      Disconnected -> HH.p_ [ HH.text "The connection has been lost, please refresh the page." ]
    enabled = HP.enabled (mode /= Working)  

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Aff
  eval (ResultEvent result next) = do
    _ <- H.modify (_ { mode = Ready result })
--    _ <- H.query AceSlot $ H.action Enable
    pure next
  eval (PerformQuery m next) = do
    _ <- H.modify (_ { mode = Working })
    H.raise m
    pure next
  eval (HandleAceUpdate text next) = do
    _ <- H.modify (_ { text = text })
    pure next
  eval (UpdateProtocolName p next) = do
    _ <- H.modify (_ { protocol = p })
    pure next
  eval (UpdateRoleName r next) = do
    _ <- H.modify (_ { role = r })
    pure next
  eval (Disconnect next) = do
    _ <- H.modify (_ { mode = Disconnected })
    pure next
    

  handleAceOutput :: AceOutput -> Maybe (Query Unit)
  handleAceOutput (TextChanged text) = Just $ H.action $ HandleAceUpdate text

-- | Run the app!
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI ui unit body
--  delay (Milliseconds 1000.0)
--  io.query $ H.action $ ResultEvent (Right Connected)
  (io.subscribe (app io.query))

app :: forall q m a eff.
       (Query ~> Aff)
    -> Consumer Message Aff Unit
app query = do
  halogenSession
    (Proxy :: Proxy WebSocket)
    (Role :: Role LS.Client)
    (URL $ "ws://scribble-playground.jlk.co:9160")
    query
    (const $ query $ H.action $ Disconnect) -- No error handling
    (\c -> (lift $ query $ H.action $ ResultEvent (Right Connected)) *> loop c)
  where
  loop c = do
    msg <- await
    c <- case msg of
      VerifyM scr -> lift $ do
        c <- select c (SProxy :: SProxy "verify")
        c <- send c (LS.Verify scr)
        (Tuple (LS.QResult res) c) <- receive c
        query $ H.action $ ResultEvent $ either (Left) (Right <<< const Verified) res
        pure c
      ProjectM scr p r -> lift $ do
        c <- select c (SProxy :: SProxy "project")
        c <- send c (LS.Project scr p r)
        (Tuple (LS.QResult res) c) <- receive c
        query $ H.action $ ResultEvent $ either (Left) (Right <<< Projection) res
        pure c
      FSMM scr r p -> lift $ do
        c <- select c (SProxy :: SProxy "fsm")
        c <- send c (LS.FSM scr p r)
        (Tuple (LS.QResult res) c) <- receive c
        query $ H.action $ ResultEvent $ either (Left) (Right <<< FSM) res
        pure c
    loop c

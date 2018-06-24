module Main where

import Prelude
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Control.Coroutine (await, Consumer)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Class (liftEff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import DOM (DOM)

import Component (component)
import Component as Comp

import Scribble.FSM
import Scribble.Core
import Scribble.WebSocket (WebSocket)
import DOM.Websocket.WebSocket as WS
import Type.Proxy (Proxy(..))
import Data.Symbol (SProxy(SProxy))
import Control.Monad.Aff.AVar (AVAR, AVar)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Partial.Unsafe (unsafeCrashWith)
import Control.Monad.Error.Class (throwError)
import Scribble.Halogen
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Parallel

main :: forall eff. Eff (HA.HalogenEffects (eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  pure unit
--  io <- runUI component unit body
--  io.subscribe (stooge io.query)
--  (io.subscribe (app io.query))
--  sequential $
--    (\ _ _ -> unit)
--      <$> parallel runServer
--      <*> parallel (io.subscribe (app io.query))

-- | A function to fake the behaviour of Server role
-- stooge :: forall eff. (Comp.Query ~> Aff (HA.HalogenEffects (eff)))
--       -> Consumer Comp.Message (Aff (HA.HalogenEffects (eff))) Unit
-- stooge query = forever $ do
--     -- Await the PingE query message
--     msg <- await
--     lift $ do
--       case msg of
--         Comp.PingM -> pure unit
--         _ -> unsafeCrashWith "Expecting ping"
--       -- Delay before the Pong
--       delay (Milliseconds 1000.0)
--       -- Send the PongE query message
--       query $ H.action $ Comp.Pong

-- app :: forall q m a eff.
--        (Comp.Query ~> Aff (TransportEffects (eff)))
--     -> Consumer Comp.Message (Aff (TransportEffects (eff))) Unit
-- app query = do
--   msg <- await
--   c <- lift $ do
--     case msg of
--       Comp.ConnectM -> query $ H.action $ Comp.Connecting
--       _ -> unsafeCrashWith "Expecting connect"
--   halogenSession
--         (Proxy :: Proxy WebSocket)
--         (Role :: Role MS.Client)
--         (WS.URL $ "ws://localhost:9161")
--         query
--         handler
--         (\c -> (lift $ query $ H.action $ Comp.Connected) *> loop c)
--         where
--           handler e = do
--              query $ H.action $ Comp.Errored (show e)
--           loop c = do
--             -- Await the PingE query message
--             msg <- await
--             c <- lift $ do
--               x <- case msg of
--                 Comp.FibM x -> pure x
--                 _ -> unsafeCrashWith "Expecting FibM"
--               -- Perform the fibonacci computation
--               (Tuple res c) <- fib x c
--               -- Send the PongE query message
--               query $ H.action $ Comp.ResultFib res
--               pure c
--             loop c


-- app :: forall q m a eff.
--        (Comp.Query ~> Aff (TransportEffects (eff)))
--     -> Consumer Comp.Message (Aff (TransportEffects (eff))) Unit
-- app query = do
--   msg <- await
--   c <- lift $ do
--     case msg of
--       Comp.ConnectM -> query $ H.action $ Comp.Connecting
--       _ -> unsafeCrashWith "Expecting connect"
--   halogenMultiSession
--         (Proxy :: Proxy WebSocket)
--         (WS.URL $ "ws://127.0.0.1:9160")
--         (Protocol :: Protocol MS.MathServer)
--         (Tuple (Role :: Role MS.Client) (Identifier "Jonathan"))
--         {"Server": Identifier "Nick"}
--         query
--         handler
--         (\c -> (lift $ query $ H.action $ Comp.Connected) *> loop c)
--         where
--           handler e = do
--              query $ H.action $ Comp.Errored (show e)
--           loop c = do
--             -- Await the PingE query message
--             msg <- await
--             c <- lift $ do
--               x <- case msg of
--                 Comp.FibM x -> pure x
--                 _ -> unsafeCrashWith "Expecting FibM"
--               -- Perform the fibonacci computation
--               (Tuple res c) <- fib x c
--               -- Send the PongE query message
--               query $ H.action $ Comp.ResultFib res
--               pure c
--             loop c

-- prog :: forall eff. Int -> Aff (TransportEffects (console :: CONSOLE | eff)) Unit
-- prog n
--   = multiSession
--         (Proxy :: Proxy WebSocket)
--         (WS.URL $ "ws://127.0.0.1:9160")
--         (Protocol :: Protocol MS.MathServer)
--         (Tuple (Role :: Role MS.Client)
--         (Identifier "Jonathan"))
--         {"Server": Identifier "Nick"}
--         (\c -> do
--             (Tuple x c) <- fib n c
--             liftEff $ log $ show x
--             c <- select c (SProxy :: SProxy "quit")
--             send c MS.Quit)
-- 
-- fib :: forall eff. Int -> Channel WebSocket MS.S9 -> Aff (TransportEffects eff) (Tuple Int (Channel WebSocket MS.S9))
-- fib n c
--   | n <= 1    = pure (Tuple 1 c)
--   | otherwise = do
--      (Tuple x c) <- fib (n - 1) c
--      (Tuple y c) <- fib (n - 2) c
--      c <- select c (SProxy :: SProxy "add")
--      c <- send c (MS.Add x y)
--      (Tuple (MS.Sum s) c) <- receive c
--      pure (Tuple s c)
-- 
-- runServer :: forall eff. Aff (TransportEffects eff) Unit
-- runServer
--   = multiSession
--         (Proxy :: Proxy WebSocket)
--         (WS.URL $ "ws://127.0.0.1:9160")
--         (Protocol :: Protocol MS.MathServer)
--         (Tuple (Role :: Role MS.Server)
--         (Identifier "Nick"))
--         {"Client": Identifier "Jonathan"}
--         server
-- 
-- server :: forall eff. Channel WebSocket MS.S20 -> Aff (TransportEffects eff) (Channel WebSocket MS.S21)
-- server c
--     = choice c {
-- 	quit: (\c -> do
--             (Tuple _ c) <- receive c
--             pure c)
--         , add: (\c -> do
--             (Tuple (MS.Add x y) c) <- receive c
--             c <- send c (MS.Sum (x + y))
--             server c)
--         , multiply: (\c -> do
--             (Tuple (MS.Multiply x y) c) <- receive c
--             c <- send c (MS.Product (x * y))
--             server c)
--         }
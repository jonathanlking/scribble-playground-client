module Scribble.Protocol.Playground.LanguageServer where

import Scribble.FSM
import Scribble.Type.SList (type (:::), SLProxy(..), SNil, symbols)
import Type.Row (Cons, Nil)
import Data.Void (Void)

-- From purescript-argonaut-codecs
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Core (Json) -- From purescript-argonaut-core
import Data.Generic.Rep (class Generic) -- From purescript-generics-rep
-- From purescript-argonaut-generic
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)

import ScribblePlayground (Result)

data Project = Project String String String
derive instance genericProject :: Generic Project _
instance encodeJsonProject :: EncodeJson Project where
  encodeJson = genericEncodeJson
instance decodeJsonProject :: DecodeJson Project where
  decodeJson = genericDecodeJson
data FSM = FSM String String String
derive instance genericFSM :: Generic FSM _
instance encodeJsonFSM :: EncodeJson FSM where
  encodeJson = genericEncodeJson
instance decodeJsonFSM :: DecodeJson FSM where
  decodeJson = genericDecodeJson
data QResult = QResult Result
derive instance genericQResult :: Generic QResult _
instance encodeJsonQResult :: EncodeJson QResult where
  encodeJson = genericEncodeJson
instance decodeJsonQResult :: DecodeJson QResult where
  decodeJson = genericDecodeJson
data Verify = Verify String
derive instance genericVerify :: Generic Verify _
instance encodeJsonVerify :: EncodeJson Verify where
  encodeJson = genericEncodeJson
instance decodeJsonVerify :: DecodeJson Verify where
  decodeJson = genericDecodeJson

foreign import data LanguageServer :: Protocol

instance protocolNameLanguageServer :: ProtocolName LanguageServer "LanguageServer"

instance protocolRoleNamesLanguageServer :: ProtocolRoleNames LanguageServer ("Client" ::: "Server" ::: SNil)

foreign import data Client :: Role

instance roleNameClient :: RoleName Client "Client"

foreign import data S9 :: Type
foreign import data S9Verify :: Type
foreign import data S9Project :: Type
foreign import data S9FSM :: Type
foreign import data S11 :: Type
foreign import data S12 :: Type
foreign import data S13 :: Type

instance initialClient :: Initial Client S9
instance terminalClient :: Terminal Client Void
instance sendS9Verify :: Send Server S9Verify S11 Verify
instance sendS9Project :: Send Server S9Project S12 Project
instance sendS9FSM :: Send Server S9FSM S13 FSM
instance selectS9 :: Select Server S9 (Cons "project" S9Project (Cons "fsm" S9FSM (Cons "verify" S9Verify Nil)))
instance receiveS11 :: Receive Server S11 S9 QResult
instance receiveS12 :: Receive Server S12 S9 QResult
instance receiveS13 :: Receive Server S13 S9 QResult

foreign import data Server :: Role

instance roleNameServer :: RoleName Server "Server"

foreign import data S21 :: Type
foreign import data S21Verify :: Type
foreign import data S21Project :: Type
foreign import data S21FSM :: Type
foreign import data S23 :: Type
foreign import data S24 :: Type
foreign import data S25 :: Type

instance initialServer :: Initial Server S21
instance terminalServer :: Terminal Server Void
instance receiveS21Verify :: Receive Client S21Verify S23 Verify
instance receiveS21Project :: Receive Client S21Project S24 Project
instance receiveS21FSM :: Receive Client S21FSM S25 FSM
instance branchS21 :: Branch Server S21 (Cons "project" S21Project (Cons "fsm" S21FSM (Cons "verify" S21Verify Nil)))
instance sendS23 :: Send Client S23 S21 QResult
instance sendS24 :: Send Client S24 S21 QResult
instance sendS25 :: Send Client S25 S21 QResult


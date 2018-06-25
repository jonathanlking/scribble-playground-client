module Graphics.Viz where

import Prelude (class Show, show)
import Data.Function.Uncurried (Fn2, runFn2)

foreign import _viz :: Fn2 String String String

data Format
  = SVG -- We can add more later

instance showFormat :: Show Format where
  show SVG = "svg"

viz :: String -> Format -> String
viz d f = runFn2 _viz d (show f)

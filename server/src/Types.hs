module Types where

import Data.Text (Text)
import Data.HashMap.Lazy

type Token = Text

type MessageQueue = [Text]

type MessagePool = HashMap Token MessageQueue

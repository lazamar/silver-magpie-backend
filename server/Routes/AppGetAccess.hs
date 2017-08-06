module Routes.AppGetAccess (get) where

import Control.Monad.IO.Class (liftIO)
import Servant (Handler, err401, throwError)
import Types (InfoMsg)

get :: Handler InfoMsg
get =
    throwError err401

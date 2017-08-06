module Routes.AppGetAccess (get) where

import Servant (Handler, err401, throwError)
import Types (InfoMsg)

get :: Handler InfoMsg
get =
    throwError err401

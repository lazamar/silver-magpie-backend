module SafeHttp (safeRequest) where

import Control.Exception (catch)
import Network.HTTP.Client
    (HttpException (HttpExceptionRequest, InvalidUrlException))


safeRequest :: IO (Either String a) -> IO (Either String a)
safeRequest performRequest =
    catch performRequest errHandler


errHandler :: HttpException -> IO (Either String a)
errHandler (HttpExceptionRequest _ reason) = return $ Left $ show reason
errHandler (InvalidUrlException _ reason)  = return $ Left $ show reason

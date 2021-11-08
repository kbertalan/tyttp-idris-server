module Main

import Control.Monad.Maybe
import Control.Monad.Either
import Control.Monad.Trans
import Data.Buffer
import Data.IORef

-- TyTTP
import Node.Buffer
import Node.Error
import Node.HTTP.Server
import TyTTP
import TyTTP.HTTP
import TyTTP.HTTP.Combinators
import TyTTP.Adapter.Node.HTTP
import TyTTP.Adapter.Node.URI
import TyTTP.Support.Routing
import TyTTP.Support.Stream
import TyTTP.Support.Promise

-- Idris Server

sendError : Monad m
  => HasIO m
  => Status
  -> String
  -> Applicative m
  => Step me u h1 fn s h2 a b
  -> m $ Step me u h1 fn Status StringHeaders a (Publisher IO NodeError Buffer)
sendError status message step = do
  let payload = fromString message
      publisher : Publisher IO NodeError Buffer = singleton payload
  size <- rawSize payload

  pure $ 
    { response.headers := 
      [ ("Content-Type", "plain/text")
      , ("Content-Length", show size)
      ] 
    , response.status := status
    , response.body := publisher
    } step

adapter : Step Method String StringHeaders (HTTP.bodyOf { monad = IO, error = NodeError})  Status StringHeaders Buffer ()
  -> Promise NodeError IO $ Step Method String StringHeaders (HTTP.bodyOf { monad = IO, error = NodeError}) Status StringHeaders Buffer (Publisher IO NodeError Buffer)
adapter = do
  let uriError = sendError BAD_REQUEST "URI contains invalid characters"
  uri' uriError :> consumeBody $ respond

  where
    respond : HasIO m
      => Step Method u h1 Request.simpleBody s h2 String b
      -> m $ Step Method u h1 Request.simpleBody Status StringHeaders String $ Publisher IO NodeError Buffer
    respond step = do
      putStrLn "respond"
      let body = fromString step.request.body
      size <- rawSize body
      pure $ {
        response.body := singleton body
      , response.headers :=
        [ ("Content-Type", "plain/text")
        , ("Content-Length", show size)
        ]
      , response.status := OK
      } step

main : IO ()
main = do
  http <- require
  ignore $ listen adapter


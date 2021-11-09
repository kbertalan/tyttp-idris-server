module Main

import Control.Monad.Maybe
import Control.Monad.Either
import Control.Monad.Trans
import Data.Buffer
import Data.IORef
import Data.List1
import Data.String

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

-- TyTTP HTTP client
import Node
import Node.HTTP.Client as Client

-- Idris Server
import Requests
import Server
import Server.EDSL.Servant
import Server.Engine.TyTTP
import Server.Server

%hide Prelude.(/)

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

adapter : {state : Type}
  -> Show state
  => (logLevel : LogLevel)
  -> (initial : state)
  -> (path : Server.Path)
  -> (impl : Signature state path)
  -> (errHandler :
    ServerError
    -> Step Method String StringHeaders Request.simpleBody Status StringHeaders String ()
    -> IO $ Step Method String StringHeaders Request.simpleBody Status StringHeaders String(Publisher IO e Buffer)
  )
  -> Step Method String StringHeaders Request.simpleBody Status StringHeaders String ()
  -> IO $ Step Method String StringHeaders Request.simpleBody Status StringHeaders String(Publisher IO e Buffer)
adapter logLevel initial path impl errHandler step = do
  let server = newServer logLevel initial path impl
      req = convertReq step.request
  Right result <- server req
   | Left err => errHandler err step
  pure $ { response.body := singleton $ fromString result
         , response.status := OK
         , response.headers :=
           [ ("Content-Type", "plain/text")
           , ("Content-Length", show $ length result)
           ]
         } step

  where
    convertReq : Request Method String StringHeaders Request.simpleBody String -> Requests.Request
    convertReq request =
      let method = case (method request) of
                      GET => Get
                      POST => Post
                      _ => Get
          path = tail $ String.split (=='/') (url request)
          headers = Request.Request.headers request
          body = Request.Request.body request
      in MkReq method (1 ** 1 ** V1997) headers path body
  

-- Idris Server Calculator example
infixr 5 /

API : Path
API = Cap "left" Int / Cap "right" Int / Split [
   "add" / Returns Int Get,
   "min" / Returns Int Get,
   "mul" / Returns Int Get,
   "div" / Returns Int Get]

SimpleAPI : Signature () API
SimpleAPI = [\x, y, () => x + y
            ,\x, y, () => x - y
            ,\x, y, () => x * y
            ,\x, y, () => x `div` y]

main : IO ()
main = do
  let uriError = sendError BAD_REQUEST "URI contains invalid characters"
      serverError = \err => sendError INTERNAL_SERVER_ERROR "Idris server has failed"
      handler = uri' uriError :> consumeBody
        $ adapter Verbose () API SimpleAPI serverError

  http <- require
  server <- listen {port = 3000} handler 

  -- example client call over HTTP
  defer $ do
    ignore $ http.get "http://localhost:3000/1/2/add" $ \res => do
      putStrLn "\nClient call results"
      putStrLn res.statusCode
      onData res putStrLn

    ignore $ http.get "http://localhost:3000/1/2/min" $ \res => do
      putStrLn "\nClient call results"
      putStrLn res.statusCode
      onData res putStrLn

    ignore $ http.get "http://localhost:3000/1/2/mul" $ \res => do
      putStrLn "\nClient call results"
      putStrLn res.statusCode
      onData res putStrLn

    ignore $ http.get "http://localhost:3000/1/2/div" $ \res => do
      putStrLn "\nClient call results"
      putStrLn res.statusCode
      onData res putStrLn
      server.close

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

-- TyTTP HTTP client
import Node
import Node.HTTP.Client as Client

-- Idris Server
import Server.Engine.TyTTP

import Requests
import Server.Server
import Server
import Server.EDSL.Servant
import Optics
import Interfaces
import Data.Product
import Data.List
import Data.List.Optics
import Data.SortedMap
import Data.SortedMap.Optics
import Data.Vect
import Data.String.ParserInterface
import Data.String.Parser

%hide Prelude.(/)

||| sends a text body with a status code, used for error responses
sendError : Monad m
  => HasIO m
  => Status
  -> String
  -> Applicative m
  => Step me u h1 f s h2 a b
  -> m $ Step me u h1 f Status StringHeaders a (Publisher IO NodeError Buffer)
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

||| adapter between TyTTP and Idris Server
adapter : {state : Type}
  -> Show state
  => (logLevel : LogLevel)
  -> (initial : state)
  -> (path : Server.Path)
  -> (impl : Signature state path)
  -> (errHandler :
    ServerError
    -> Step Method String StringHeaders Request.simpleBody Status StringHeaders Buffer ()
    -> IO $ Step Method String StringHeaders Request.simpleBody Status StringHeaders Buffer (Publisher IO e Buffer)
  )
  -> Step Method String StringHeaders Request.simpleBody Status StringHeaders Buffer ()
  -> IO $ Step Method String StringHeaders Request.simpleBody Status StringHeaders Buffer(Publisher IO e Buffer)
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
    convertReq : Request Method String StringHeaders Request.simpleBody Buffer -> Requests.Request
    convertReq request =
      let method = case (method request) of
                      GET => Get
                      POST => Post
                      _ => Get
          path = tail $ String.split (=='/') (url request)
          headers = Request.Request.headers request
          body = toStringUTF8 $ Request.Request.body request
      in MkReq method (1 ** 1 ** V1997) headers path body

-- Idris Server Todo example
infixr 5 /

update : Monoid v => k -> (v -> v) -> SortedMap k v -> SortedMap k v
update k f m =
  let v = fromMaybe neutral $ lookup k m in
    insert k (f v) m

record Todo where
  constructor MkTodo
  title : String
  note : String

HasParser Todo where
  partialParse = MkTodo <$> str <* char ',' <*> str
      where str : Parser String
            str = pack <$> some alphaNum

Show Todo where
  show (MkTodo t n) = "Todo(title: \{t}, note: \{n})"

Display Todo where
  display = "Todo"

Default Todo where
  def = MkTodo "get milk" "get the one on reduction"

getTodo : Path
getTodo = "todo" / Cap "user" Int / Returns (List Todo) Get

setTodo : Path
setTodo = "todo" / Cap "user" Int / Returns (List Todo) (Post Todo)

TodoAPI : Path
TodoAPI = Split [getTodo, setTodo]

ServerState : Type
ServerState = SortedMap Int (List Todo)

initial : ServerState
initial = empty

todoImpl : Signature ServerState TodoAPI
todoImpl = [findTodo, updateTodo]
  where
    findTodo : Int -> ServerState -> List Todo
    findTodo id state = fromMaybe [] (lookup id state)

    updateTodo : Int -> Todo -> ServerState -> List Todo * ServerState
    updateTodo id todo state = (todo :: findTodo id state) && (update id (todo ::) state)

||| sets up server, includes URI decoding, consuming whole body from stream and pass it to idris server adapter
main : IO ()
main = do
  let uriError = sendError BAD_REQUEST "URI contains invalid characters"
      serverError = \err => sendError INTERNAL_SERVER_ERROR "Idris server has failed"
      handler = uri' uriError :> consumeBody
        $ adapter Normal initial TodoAPI todoImpl serverError

  http <- require
  server <- listen' handler 

  -- example client calls over HTTP
  defer $ do
    createTodo 1 "something" "eat"
    createTodo 1 "other" "tasks"
    listTodo 1 server

  where
    createTodo : { auto http : HTTP } -> Int -> String -> String -> IO ()
    createTodo {http} id title note = do
        req <- http.post "http://localhost:3000/todo/\{show id}" $ \res => do
          putStrLn "\nClient call results"
          putStrLn res.statusCode
          onData res putStrLn

        req.write "\{title},\{note}"
        req.end

    listTodo : { auto http : HTTP } -> Int -> Server -> IO ()
    listTodo {http} id server = do
        ignore $ http.get "http://localhost:3000/todo/\{show id}" $ \res => do
          putStrLn "\nClient call results"
          putStrLn res.statusCode
          onData res putStrLn

          server.close

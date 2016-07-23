module JQuery.Ajax
  ( Ajax()
  , C()
  , ErrCont
  , Response(..)
  , JQueryAjaxOptions()
  ) where

--   ( Ajax(), C(), ErrCont(..), Response(..), Header(..), DataType(..)
--   , Method(..), URL(), JQueryAjaxOptions()
--   , getWith, get, getJSON, postWith, putWith, delete
--   , url, method, dataType, body, contentType
--   ) where
--
import Prelude
import Control.Monad.Cont.Trans (ContT(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (ExceptT(..))
import Data.Either (Either(..))
import Data.Foreign (Foreign(), F())
import Data.Foreign.Class (class IsForeign, read)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Options (Options(), Option(), options, (:=))
--
foreign import data Ajax :: !

type C eff = ContT Unit (Eff (ajax :: Ajax | eff))

type ErrCont eff = ExceptT Response (C eff)

newtype Response = Response {status :: Number, responseText :: String}

-- Todo: Error is not a type class!!
-- instance errorResponse :: Error Response where
--   noMsg = makeResponse 0 ""
--   strMsg = makeResponse 0
--
instance showResponse :: Show Response where
  show (Response err) = (show $ err.status) <> " " <> err.responseText

makeResponse :: Number -> String -> Response
makeResponse status text = Response {status: status, responseText: text}

type URL = String

data Header = Header String String

type AjaxResponse = {status :: Number, responseText :: String}

foreign import data JQueryAjaxOptions :: *

foreign import jqueryAjaxImpl :: forall eff. Fn3 Foreign
                                    (AjaxResponse -> Eff (ajax :: Ajax | eff) Unit)
                                    (Foreign -> Eff (ajax :: Ajax | eff) Unit)
                                    (Eff (ajax :: Ajax | eff) Unit)

jqueryAjax :: forall eff. Options JQueryAjaxOptions
                       -> (Either Response Foreign -> Eff (ajax :: Ajax | eff) Unit)
                       -> Eff (ajax :: Ajax | eff) Unit
jqueryAjax s cb = runFn3 jqueryAjaxImpl (options s) (cb <<< Left <<< Response) (cb <<< Right)

jqueryAjaxCont :: forall eff. Options JQueryAjaxOptions -> ErrCont eff Foreign
jqueryAjaxCont s = ExceptT $ ContT $ jqueryAjax s

getWith :: forall eff. Options JQueryAjaxOptions -> URL -> ErrCont eff Foreign
getWith opts url' = jqueryAjaxCont $ url := url' <> opts

getJSON :: forall eff a. (IsForeign a) => URL -> ErrCont eff (F a)
getJSON url' = read <$> getWith (dataType := JSON) url'

get :: forall eff. URL -> ErrCont eff Foreign
get url' = jqueryAjaxCont $ url := url'

postWith :: forall eff. URL -> Options JQueryAjaxOptions -> ErrCont eff Foreign
postWith url' opts = jqueryAjaxCont $ method := POST <> url := url' <> opts

putWith :: forall eff. URL -> Options JQueryAjaxOptions -> ErrCont eff Foreign
putWith url' opts = jqueryAjaxCont $ method := PUT <> url := url' <> opts

delete :: forall eff. URL -> ErrCont eff Foreign
delete url' = jqueryAjaxCont $ method := DELETE <> url := url'


foreign import unsafeToOption :: forall a. String -> Option JQueryAjaxOptions a

url :: Option JQueryAjaxOptions String
url = unsafeToOption "url"

data Method = GET | POST | PUT | DELETE

instance showMethod :: Show Method where
  show GET = "GET"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"

-- Todo: Understand what this method does
-- instance methodIsOption :: Option Method where
--  options k m = (assoc k) := show m

method :: Option JQueryAjaxOptions Method
method = unsafeToOption "method"

data DataType = Text | JSON | HTML

instance showDataType :: Show DataType where
  show Text = "text"
  show JSON = "json"
  show HTML = "html"

-- Todo: Solve Option DataType issue where Option is not a dataType
-- instance dataTypeIsOption :: Option DataType where
--   assoc k a = ((:=) k) := show a

dataType :: Option JQueryAjaxOptions DataType
dataType = unsafeToOption "dataType"

body :: Option JQueryAjaxOptions String
body = unsafeToOption "data"

contentType :: Option JQueryAjaxOptions String
contentType = unsafeToOption "contentType"

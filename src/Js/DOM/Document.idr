module Js.DOM.Document

import Control.Monad.Syntax
import Js
import Js.Object
import Js.DOM

%default total
%access export



getElement : (id : String) -> JS_IO Element
getElement id = MkElement <$> js "document.getElementById(%0)" (String -> JS_IO Ptr) id

write : String -> JS_IO ()
write = js "document.write(%0)" (String -> JS_IO ())

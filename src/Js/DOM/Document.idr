module Js.DOM.Document

import Control.Monad.Syntax
import Js
import Js.DOM

%default total
%access export



getElement : (id : String) -> JS_IO Element
getElement = cast . js "document.getElementById(%0)" (String -> JS_IO Ptr)

write : String -> JS_IO ()
write = js "document.write(%0)" (String -> JS_IO ())

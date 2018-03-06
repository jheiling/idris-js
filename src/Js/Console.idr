module Js.Console

import Js

%default total



export
log : String -> JS_IO ()
log = js "console.log(%0)" (String -> JS_IO ())

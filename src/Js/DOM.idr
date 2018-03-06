module Js.DOM

%default total
%access export



public export
data Element = MkElement Ptr

Cast Element Ptr where
    cast (MkElement ptr) = ptr

Cast (JS_IO Ptr) (JS_IO Element) where
    cast x = pure $ MkElement !x

Cast (JS_IO Element) (JS_IO Ptr) where
    cast x = pure $ cast !x

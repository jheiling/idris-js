module Js

%default total
%access export



infix 6 ===

%inline
js : (fname : String) -> (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
js fname ty = foreign FFI_JS fname ty

%inline
null : JS_IO Ptr
null = js "null" (JS_IO Ptr)

%inline
undefined : JS_IO Ptr
undefined = js "undefined" (JS_IO Ptr)

%inline
(===) : Ptr -> Ptr -> JS_IO Bool
(===) a b = pure $ !(js "%0 === %1 + 0" (Ptr -> Ptr -> JS_IO Int) a b) /= 0

%inline
isNull : Ptr -> JS_IO Bool
isNull x = pure $ !(js "(%0 === null) + 0" (Ptr -> JS_IO Int) x) /= 0

%inline
isUndefined : Ptr -> JS_IO Bool
isUndefined x = pure $ !(js "(%0 === undefined) + 0" (Ptr -> JS_IO Int) x) /= 0

|||time in ms
%inline
setTimeout : (time : Nat) -> (action : JS_IO ()) -> JS_IO ()
setTimeout time action = assert_total $ js "setTimeout(%1, %0)" (Int -> JsFn (() -> JS_IO ()) -> JS_IO ()) (cast time) $ MkJsFn $ \() => action

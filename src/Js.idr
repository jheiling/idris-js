module Js

%default total
%access export



%inline
js : (fname : String) -> (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
js fname ty = foreign FFI_JS fname ty

%inline
null : JS_IO Ptr
null = js "null" (JS_IO Ptr)

%inline
isNull : Ptr -> JS_IO Bool
isNull x = pure $ !(js "(%0 == null) + 0" (Ptr -> JS_IO Int) x) /= 0

%inline
isUndefined : Ptr -> JS_IO Bool
isUndefined x = pure $ !(js "(%0 == undefined) + 0" (Ptr -> JS_IO Int) x) /= 0

%inline
setTimeout : (time : Nat) -> (action : JS_IO ()) -> JS_IO ()
setTimeout time action = assert_total $ js "setTimeout(%1, %0)" (Int -> JsFn (() -> JS_IO ()) -> JS_IO ()) (cast time) (MkJsFn (\() => action))

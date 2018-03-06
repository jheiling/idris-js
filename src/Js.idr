module Js

%default total
%access export



%inline
js : (fname : String) -> (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
js fname ty = foreign FFI_JS fname ty

%inline
null : JS_IO Ptr
null = js "null" (JS_IO Ptr)

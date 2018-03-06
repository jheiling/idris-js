module Js.Object

import Control.Monad.Syntax
import Js

%default total
%access export



public export
data Object = MkObject Ptr

public export
interface Member a where
    get : (member : String) -> (object : Object) -> JS_IO a
    set : (member : String) -> (value : a) -> (object : Object) -> JS_IO ()



Cast Object Ptr where
    cast (MkObject ptr) = ptr

Cast (JS_IO Ptr) (JS_IO Object) where
    cast x = pure $ MkObject !x

Cast (JS_IO Object) (JS_IO Ptr) where
    cast x = pure $ cast !x

%inline
jsGet : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsGet ty = js "%1[%0]" ty

%inline
jsSet : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsSet ty = js "%2[%0] = %1" ty

Member Nat where
    get member = jsGet (String -> Ptr -> JS_IO Int) member . cast >=> pure . cast
    set member value = jsSet (String -> Int -> Ptr -> JS_IO ()) member (cast value) . cast

Member Int where
    get member = jsGet (String -> Ptr -> JS_IO Int) member . cast
    set member value = jsSet (String -> Int -> Ptr -> JS_IO ()) member value . cast

Member Double where
    get member = jsGet (String -> Ptr -> JS_IO Double) member . cast
    set member value = jsSet (String -> Double -> Ptr -> JS_IO ()) member value . cast

Member String where
    get member = jsGet (String -> Ptr -> JS_IO String) member . cast
    set member value = jsSet (String -> String -> Ptr -> JS_IO ()) member value . cast



%inline
empty : JS_IO Object
empty = cast $ js "{}" (JS_IO Ptr)

%inline
wrap : Member a => (member : String) -> (value : a) -> JS_IO Object
wrap member value = do object <- empty
                       set member value object
                       pure object

%inline
setBool : (member : String) -> (value : Bool) -> (object : Object) -> JS_IO ()
setBool member False = js "%1[%0] = false" (String -> Ptr -> JS_IO ()) member . cast
setBool member True = js "%1[%0] = true" (String -> Ptr -> JS_IO ()) member . cast

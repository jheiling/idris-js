module Js.Object

import Control.Monad.Syntax
import Js

%default total
%access export



public export
interface Class c where
    ptr : (object : c) -> Ptr

public export
interface Member a where
    get : Class c => (member : String) -> (object : c) -> JS_IO a
    set : Class c => (member : String) -> (value : a) -> (object : c) -> JS_IO ()

%inline
jsGet : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsGet ty = js "%1[%0]" ty

%inline
jsSet : (ty : Type) -> {auto fty : FTy FFI_JS [] ty} -> ty
jsSet ty = js "%2[%0] = %1" ty



public export
data Object = MkObject Ptr

Class Object where
    ptr (MkObject p) = p

%inline
empty : JS_IO Object
empty = MkObject <$> js "{}" (JS_IO Ptr)

%inline
wrap : Member a => (member : String) -> (value : a) -> JS_IO Object
wrap member value = do object <- empty
                       set member value object
                       pure object



Member Bool where
    get member object = pure $ !(js "%1[%0] + 0" (String -> Ptr -> JS_IO Int) member $ ptr object) /= 0
    set member False = js "%1[%0] = false" (String -> Ptr -> JS_IO ()) member . ptr
    set member True = js "%1[%0] = true" (String -> Ptr -> JS_IO ()) member . ptr

Member Nat where
    get member = jsGet (String -> Ptr -> JS_IO Int) member . ptr >=> pure . cast
    set member value = jsSet (String -> Int -> Ptr -> JS_IO ()) member (cast value) . ptr

Member Int where
    get member = jsGet (String -> Ptr -> JS_IO Int) member . ptr
    set member value = jsSet (String -> Int -> Ptr -> JS_IO ()) member value . ptr

Member Double where
    get member = jsGet (String -> Ptr -> JS_IO Double) member . ptr
    set member value = jsSet (String -> Double -> Ptr -> JS_IO ()) member value . ptr

Member String where
    get member = jsGet (String -> Ptr -> JS_IO String) member . ptr
    set member value = jsSet (String -> String -> Ptr -> JS_IO ()) member value . ptr

Member Ptr where
    get member = jsGet (String -> Ptr -> JS_IO Ptr) member . ptr
    set member value = jsSet (String -> Ptr -> Ptr -> JS_IO ()) member value . ptr

Member Object where
    get member = jsGet (String -> Ptr -> JS_IO Ptr) member . ptr >=> pure . MkObject
    set member value = jsSet (String -> Ptr -> Ptr -> JS_IO ()) member (ptr value) . ptr

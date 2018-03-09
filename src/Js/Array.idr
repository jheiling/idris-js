module Js.Array

import Control.Monad.Syntax
import Data.Foldable.Extras
import Js
import Js.Object

%default total
%access export



public export
interface ArrayValue a where
    getAt : Class o => (index : Nat) -> (object : o) -> JS_IO a
    setAt : Class o => (index : Nat) -> (value : a) -> (object : o) -> JS_IO ()



public export
data Array = MkArray Ptr

Class Array where
    ptr (MkArray p) = p

%inline
empty : JS_IO Array
empty = MkArray <$> js "[]" (JS_IO Ptr)

%inline
length : (array : Array) -> JS_IO Nat
length = js "%0.length" (Ptr -> JS_IO Int) . ptr >=> pure . cast

%inline
append : ArrayValue a => (value : a) -> (array : Array) -> JS_IO ()
append value array = setAt !(length array) value array

createWith : Foldable f => (append' : a -> Array -> JS_IO ()) -> (source : f a) -> JS_IO Array
createWith append' source = do array <- empty
                               iter (flip append' array) source
                               pure array

%inline
create : (Foldable f, ArrayValue a) => (source : f a) -> JS_IO Array
create = createWith append



ArrayValue Bool where
    getAt index object = pure $ !(js "%1[%0] + 0" (Int -> Ptr -> JS_IO Int) (cast index) (ptr object)) /= 0
    setAt index False = js "%1[%0] = false" (Int -> Ptr -> JS_IO ()) (cast index) . ptr
    setAt index True = js "%1[%0] = true" (Int -> Ptr -> JS_IO ()) (cast index) . ptr

ArrayValue Nat where
    getAt index = jsGet (Int -> Ptr -> JS_IO Int) (cast index) . ptr >=> pure . cast
    setAt index value = jsSet (Int -> Int -> Ptr -> JS_IO ()) (cast index) (cast value) . ptr

ArrayValue Int where
    getAt index = jsGet (Int -> Ptr -> JS_IO Int) (cast index) . ptr
    setAt index value = jsSet (Int -> Int -> Ptr -> JS_IO ()) (cast index) value . ptr

ArrayValue Double where
    getAt index = jsGet (Int -> Ptr -> JS_IO Double) (cast index) . ptr
    setAt index value = jsSet (Int -> Double -> Ptr -> JS_IO ()) (cast index) value . ptr

ArrayValue String where
    getAt index = jsGet (Int -> Ptr -> JS_IO String) (cast index) . ptr
    setAt index value = jsSet (Int -> String -> Ptr -> JS_IO ()) (cast index) value . ptr

ArrayValue Ptr where
    getAt index = jsGet (Int -> Ptr -> JS_IO Ptr) (cast index) . ptr
    setAt index value = jsSet (Int -> Ptr -> Ptr -> JS_IO ()) (cast index) value . ptr

ArrayValue Object where
    getAt index = jsGet (String -> Ptr -> JS_IO Ptr) (cast index) . ptr >=> pure . MkObject
    setAt index value = jsSet (String -> Ptr -> Ptr -> JS_IO ()) (cast index) (ptr value) . ptr

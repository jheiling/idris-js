module Js.Array

import Control.Monad.Syntax

import Data.Foldable.Extras
import Js
import Js.Object

%default total
%access export



public export
data Array = MkArray Ptr

public export
interface Storable a where
    getAt : (index : Nat) -> (array : Array) -> JS_IO a
    setAt : (index : Nat) -> (value : a) -> (array : Array) -> JS_IO ()



Cast Array Ptr where
    cast (MkArray ptr) = ptr

Cast (JS_IO Ptr) (JS_IO Array) where
    cast x = pure $ MkArray !x

Cast (JS_IO Array) (JS_IO Ptr) where
    cast x = pure $ cast !x

Storable Bool where
    getAt index array = pure $ !(js "%1[%0] + 0" (Int -> Ptr -> JS_IO Int) (cast index) (cast array)) /= 0
    setAt index False = js "%1[%0] = false" (Int -> Ptr -> JS_IO ()) (cast index) . cast
    setAt index True = js "%1[%0] = true" (Int -> Ptr -> JS_IO ()) (cast index) . cast

Storable Nat where
    getAt index = jsGet (Int -> Ptr -> JS_IO Int) (cast index) . cast >=> pure . cast
    setAt index value = jsSet (Int -> Int -> Ptr -> JS_IO ()) (cast index) (cast value) . cast

Storable Int where
    getAt index = jsGet (Int -> Ptr -> JS_IO Int) (cast index) . cast
    setAt index value = jsSet (Int -> Int -> Ptr -> JS_IO ()) (cast index) value . cast

Storable Double where
    getAt index = jsGet (Int -> Ptr -> JS_IO Double) (cast index) . cast
    setAt index value = jsSet (Int -> Double -> Ptr -> JS_IO ()) (cast index) value . cast

Storable String where
    getAt index = jsGet (Int -> Ptr -> JS_IO String) (cast index) . cast
    setAt index value = jsSet (Int -> String -> Ptr -> JS_IO ()) (cast index) value . cast

Storable Ptr where
    getAt index = jsGet (Int -> Ptr -> JS_IO Ptr) (cast index) . cast
    setAt index value = jsSet (Int -> Ptr -> Ptr -> JS_IO ()) (cast index) value . cast



%inline
empty : JS_IO Array
empty = pure $ MkArray !(js "[]" (JS_IO Ptr))

%inline
length : (array : Array) -> JS_IO Nat
length = js "%0.length" (Ptr -> JS_IO Int) . cast >=> pure . cast

%inline
append : Storable a => (value : a) -> (array : Array) -> JS_IO ()
append value array = setAt !(length array) value array

createWith : (Foldable f) => (append' : a -> Array -> JS_IO ()) -> (source : f a) -> JS_IO Array
createWith append' source = do array <- empty
                               iter (flip append' array) source
                               pure array

%inline
create : (Foldable f, Storable a) => (source : f a) -> JS_IO Array
create = createWith append

module Js.DOM.Element

import Js.Object

%default total
%access export



public export
data Element = MkElement Ptr

Class Element where
    ptr (MkElement p) = p

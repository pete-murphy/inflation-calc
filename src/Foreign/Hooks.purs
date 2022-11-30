module Foreign.Hooks (useResizeObserver, UseResizeObserver(..), useWidth) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Effect.Uncurried as Uncurried
import React.Basic (Ref)
import React.Basic.Hooks (type (&), Hook, Render, UseState, (/\))
import React.Basic.Hooks as Hooks
import Web.DOM.Node (Node)

newtype UseResizeObserver hooks = UseUseResizeObserver hooks

derive instance Newtype (UseResizeObserver hooks) _

useWidth
  :: forall hooks
   . Ref (Nullable Node)
  -> Render hooks
       (hooks & UseState (Maybe Number) & UseResizeObserver)
       (Maybe Number)
useWidth ref = Hooks.do
  width /\ setWidth <- Hooks.useState' Nothing
  useResizeObserver ref \box -> setWidth (Just box.width)
  pure width

useResizeObserver
  :: Ref (Nullable Node)
  -> ({ width :: Number, height :: Number } -> Effect Unit)
  -> Hook UseResizeObserver Unit
useResizeObserver ref callback =
  Hooks.unsafeHook
    ( Uncurried.runEffectFn1
        _useResizeObserver
        { ref
        , callback: Uncurried.mkEffectFn1 callback
        }
    )

foreign import _useResizeObserver
  :: EffectFn1
       { ref :: Ref (Nullable Node)
       , callback :: EffectFn1 { width :: Number, height :: Number } Unit
       }
       Unit
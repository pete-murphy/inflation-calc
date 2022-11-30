module ResizeSection where

import Prelude

import Data.Foldable as Foldable
import Data.Nullable as Nullable
import Foreign.Hooks as Foreign.Hooks
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as Hooks

make
  :: Component
       ({ width :: Number, height :: Number } -> Array JSX)

make = do
  Hooks.component "ResizeSection" \handleResize -> Hooks.do
    ref <- Hooks.useRef Nullable.null
    maybeBounds <- Foreign.Hooks.useContentBox ref
    pure
      ( DOM.section { ref, children: maybeBounds # Foldable.foldMap handleResize }
      )


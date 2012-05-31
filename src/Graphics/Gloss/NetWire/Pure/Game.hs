{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}

module Graphics.Gloss.NetWire.Pure.Game (playNetwire,GlossWire,key,keyState,keyModifier,keyStateDown, keyStateUp, keyRight, keyLeft, keyDown, keyUp, keyModifierShift, keyModifierCtrl, keyModifierAlt,keyPosition,mouseMotion,Display(..),Key(..),SpecialKey(..),MouseButton(..),Modifiers(..),KeyState(..),module Graphics.Gloss.Data.Color, module Graphics.Gloss.Data.Picture) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Control.Arrow
import Control.Arrow.Unicode
import Prelude.Unicode
import Control.Wire hiding (lift)
import Control.Monad.Reader

playNetwire ∷ Display → Color → Int → GlossWire () Picture → IO ()
playNetwire display bg fps wire =
  play display bg fps
  (NW blank wire [])
  pic
  eventFunc
  stepFunc

data NetwireWorld =
  NW
  { pic ∷ Picture
  , wire ∷ GlossWire () Picture
  , pendingEvents ∷ [Event]
  }

newtype GlossWireM a = GWM (Reader (Float,Maybe Event) a)
                     deriving (Monad,MonadFix)

runGWM ∷ GlossWireM a → Float → Maybe Event → a
runGWM (GWM a) dt e = runReader a (dt,e)

instance WWithDT Float (Kleisli GlossWireM) where
  passDT = (⋙) $ constant (GWM $ asks fst) ⋙ arrM
  withDT = (⋙) $ identity &&& (passDT identity)

type GlossWire = WireM () GlossWireM

stepFunc ∷ Float → NetwireWorld → NetwireWorld
stepFunc dt (NW oldPic oldWire events) =
  case result of
    Right newPic → NW newPic newWire remainingEvents
    Left _ → NW oldPic newWire remainingEvents
  where (result,newWire) = runGWM (stepWireM oldWire ()) dt event
        (event,remainingEvents) = case events of [] → (Nothing,[])
                                                 (e:es) → (Just e,es)

eventFunc ∷ Event → NetwireWorld → NetwireWorld
eventFunc event oldWorld@(NW _ _ events) =
  oldWorld{pendingEvents=events⧺[event]}


currEvent ∷ GlossWire a Event
currEvent = constant (GWM $ asks snd) ⋙ arrM ⋙ injectEvent

keyEvent ∷ GlossWire a (Key,KeyState,Modifiers,(Float, Float))
keyEvent = currEvent ⋙ proc e →
  case e of EventKey k s m p → returnA -< (k,s,m,p)
            _ → zeroArrow -< ()

--

key ∷ Key → GlossWire a ()
key k = fmap (\(k',_,_,_) → k' ≡ k) keyEvent ⋙ require

keyState ∷ KeyState → GlossWire a ()
keyState s = fmap (\(_,s',_,_) → s' ≡ s) keyEvent ⋙ require

keyModifier ∷ (Modifiers → Bool) → GlossWire a ()
keyModifier pred = fmap (\(_,_,m,_) → pred m) keyEvent ⋙ require

keyStateDown, keyStateUp, keyRight, keyLeft, keyDown, keyUp, keyModifierShift, keyModifierCtrl, keyModifierAlt ∷ GlossWire a ()
[keyStateDown,keyStateUp] = map keyState [Down,Up]
[keyRight,keyLeft,keyDown,keyUp] =
  map (key ∘ SpecialKey) [KeyRight,KeyLeft,KeyDown,KeyUp]
[keyModifierShift,keyModifierCtrl,keyModifierAlt] =
  map (keyModifier ∘ ((≡ Down) ∘ )) [shift,ctrl,alt]

keyPosition ∷ GlossWire a (Float,Float)
keyPosition = fmap (\(_,_,_,p) → p) keyEvent

mouseMotion ∷ GlossWire a (Float,Float)
mouseMotion = currEvent ⋙ proc e →
  case e of EventMotion pos → returnA -< pos
            _ → zeroArrow -< ()

module Fire where

data FireState
  = Dead
  | Smouldering
  | Flickering
  | Burning
  | Roaring
  deriving (Eq, Show, Enum, Ord)

fireState :: FireState -> String
fireState Dead = "the fire is dead."
fireState Smouldering = "the fire is smouldering."
fireState Flickering = "the fire is flickering."
fireState Burning = "the fire is burning."
fireState Roaring = "the fire is roaring."

firePred :: FireState -> FireState
firePred Dead = Dead
firePred x = pred x

fireSucc :: FireState -> FireState
fireSucc Roaring = Roaring
fireSucc x = succ x

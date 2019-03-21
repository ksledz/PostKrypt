module Mon where

infixl 5 ><
class Mon m where
  m1 :: m              -- element neutralny operacji
  (><) :: m -> m -> m  -- >< musi być łączne
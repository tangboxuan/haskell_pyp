data Colour = Red | Green | Blue
    deriving (Show, Bounded, Enum)

data Time = Mil Int | Wall Int Flag
data Flag = Am | Pm
    deriving (Show)
to24 :: Time -> Time
to24 (Wall t Pm)
    = Mil (t + p)
    where
        p = if t < 1200 then 1200 else 0
to24 (Wall t Am)
    = Mil (t - p)
    where
        p = if t < 1200 then 0 else 1200
to24 t
    = t
equalTime :: Time -> Time -> Bool
equalTime t1 t2
    = t1' == t2'
    where
        Mil t1' = to24 t1
        Mil t2' = to24 t2
instance Eq Time where
    (==) = equalTime
instance Show Time where
    show (Mil t) = show t ++ "hrs"
    show (Wall 1200 Pm) = "Midday"
    show (Wall 1200 Am) = "Midnight"
    show (Wall t Am) = show (div t 100) ++ ":" ++ show (mod t 100) ++ "am" 
    show (Wall t Pm) = show (div t 100) ++ ":" ++ show (mod t 100) ++ "pm" 

type VarName = String
data Fun = Add | Sub | Mul
        deriving (Eq, Show)
data Exp = Val Int | Id VarName | App Fun Exp Exp
        deriving (Eq, Show)
type Assignment = (VarName, Exp)
type Program = [Statement]
data Statement = A Assignment | Loop Int Program
type Environment a = [(String, a)]
infixl 1 <--
(<--) :: VarName -> Exp -> Statement
(<--)
  = (A .) . (,)
loop :: Int -> Program -> Statement
loop = Loop
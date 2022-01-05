import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table and is unique
lookUp n xs
  = fromJust (lookup n xs)

states :: LTS -> [State]
states ls
  = nub (states' ls)
  where
    states' []
      = []
    states' (((s, t), _):ls)
      = s : t : states' ls

transitions :: State -> LTS -> [Transition]
transitions s ls
  = filter ((s==) . fst . fst) ls

alphabet :: LTS -> Alphabet
alphabet ls
  = nub (alphabet' ls)
  where
    alphabet' []
      = []
    alphabet' (((_, _), a):ls)
      = a : alphabet' ls

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions p
  = nub (actions' p)
  where
    actions' (STOP)
      = []
    actions' (Ref _)
      = []
    actions' (Prefix a p')
      = a : actions' p'
    actions' (Choice (p':ps))
      = actions' p' ++ actions' (Choice ps)
    actions' (Choice [])
      = []

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.

accepts [] _
  = True
accepts _ []
  = False
accepts ids po@((_, p):_)
  = accepts' ids p
  where
    accepts' :: [Id] -> Process -> Bool
    accepts' [] _
      = True
    accepts' _ STOP
      = False
    accepts' ids' (Ref id')
      = accepts' ids' (lookUp id' po)
    accepts' (id'': ids'') (Prefix id' p')
      | id' == id'' = accepts' ids'' p'
      | otherwise   = False
    accepts' ids (Choice (x:xs))
      = (accepts' ids x || accepts' ids (Choice xs)) 
    accepts' id (Choice [])
      = False

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                  -> Alphabet -> Alphabet 
                  -> StateMap 
                  -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
m :: StateMap
m = [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]

composeTransitions ((s, t), a) ((s', t'), a') a1 a2 sm
  | a == a'   = [((i s s', i t t'), a)]
  | e && e'   = []
  | e'        = [((i s s', i t s'), a)]
  | e         = [((i s s', i s t'), a')]
  | otherwise = [((i s s', i t s'), a), ((i s s', i s t'), a')]
  where
    i x y = lookUp (x, y) sm
    e = elem a a2
    e' = elem a' a1

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts 
  = visit 0 []
  where
    visit :: State -> [State] -> [Transition]
    visit x xs
      | elem x xs = []
      | otherwise = ts' ++ concat [visit x' (x:xs)| x' <- map (snd . fst) ts']
      where
        ts' = transitions x ts

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose 
  = undefined

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]


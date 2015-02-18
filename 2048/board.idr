import Effect.StdIO
import Effect.System
import Effect.Random

Board : Type
Board = List (Int, Int, Int)

lookup : Board -> (Int, Int) -> Int
lookup [] x = 0
lookup ((x, y, val) :: xs) (a, b) = if (x == a && y == b)
                                      then val
                                      else lookup xs (a, b)
                                      
getRow : Int -> Board -> List Int
getRow x [] = []
getRow x ((a, b, c) :: xs) = if (x == b)
                               then c :: getRow x xs
                               else getRow x xs

getCol : Int -> Board -> List Int
getCol x [] = []
getCol x ((a, b, c) :: xs) = if (x == a)
                                then c :: getCol x xs
                                else getCol x xs
                                      
getHighest : Board -> Int
getHighest [] = 0
getHighest b = nextHighest b 0 where
    nextHighest : Board -> Int -> Int
    nextHighest [] a = a
    nextHighest ((x, y, z) :: xs) a = if (z > a)
                                         then nextHighest xs z
                                         else nextHighest xs a

getSpaces : Board -> Nat
getSpaces [] = 16
getSpaces b = 16 - length b

getPairs : Board -> Nat
getPairs [] = 0
getPairs b = (colPairs (getCol 0 b)) + (colPairs (getCol 1 b)) + (colPairs (getCol 2 b)) + (colPairs (getCol 3 b)) + (rowPairs (getRow 0 b)) + (rowPairs (getRow 1 b)) + (rowPairs (getRow 2 b)) + (rowPairs (getRow 3 b)) where
  colPairs : List Int -> Nat
  colPairs [] = 0
  colPairs [x] = 0
  colPairs (x :: y :: xs) = if (x == y)
                               then 1 + colPairs (y :: xs)
                               else colPairs (y :: xs)
  rowPairs : List Int -> Nat
  rowPairs [] = 0
  rowPairs [x] = 0
  rowPairs (x :: y :: xs) = if (x == y)
                               then 1 + rowPairs (y :: xs)
                               else rowPairs (y :: xs)



data GState = Running Bool Nat Int | NotRunning

data G2048 : GState -> Type where
      Init : G2048 NotRunning -- initialising, but not ready
      GameWon : Nat -> G2048 NotRunning
      GameLost : Nat -> G2048 NotRunning
      Mk2048 : (gboard : Board) ->
               (highest : Int) ->
               (score : Nat) ->
               (spaces : Nat) ->
               G2048 (Running False p highest)

instance Default (G2048 NotRunning) where 
  default = Init

instance Show (G2048 gs) where 
  show Init = "Setting up"
  show (GameWon s) = "You won with a score of " ++ show s
  show (GameLost s) = "You lost with a score of " ++ show s
  show (Mk2048 gboard highest s spaces) = showBoard gboard
     where showBoard : Board -> String
           showBoard [] = ""
           showBoard b = (makeBoard 0 0 b) ++ (makeBoard 0 1 b) ++ (makeBoard 0 2 b) ++ (makeBoard 0 3 b)
              where makeBoard : Int -> Int -> Board -> String
                    makeBoard 4 y [] = ""
                    makeBoard x y b = if (x < 4)
                                        then if (lookup b (x, y) == 0)
                                                then ". " ++ (makeBoard (x + 1) y b)
                                                else show (lookup b (x, y)) ++ " " ++ (makeBoard (x + 1) y b)
                                        else "\n"
                                        
                                        
initState : (b : Board) -> G2048 (Running False (getPairs b) (getHighest b))
initState b = Mk2048 b (getHighest b) 0 (getSpaces b)

                                      
edit : Board -> (Int, Int) -> Int -> (Int, Int, Int)
edit [] x y = (0, 0, 0)
edit ((x, y, val) :: xs) (a, b) c = if (x == a && y == b)
                                       then (x, y, c)
                                       else edit xs (a, b) c



putRow : Int -> Bool -> List Int -> Board -> Board
putRow x tf [] [] = []
putRow x tf [] b = []
putRow x tf (y :: xs) [] = []
putRow x tf (y :: xs) b = if (tf == True)
                             then fixRPos 0 1 x (y :: xs) b
                             else fixRPos 3 (-1) x (y :: xs) b where
    fixRPos : Int -> Int -> Int -> List Int -> Board -> Board
    fixRPos a mod x [] b = if (lookup b (a, x) > 0)
                            then fixRPos (a + mod) mod x [] b
                            else []
    fixRPos a mod x [y] b = if (lookup b (a, x) > 0)
                              then edit b (a, x) y :: fixRPos (a + mod) mod x [] b
                              else (a, x, y) :: fixRPos (a + mod) mod x [] b
    fixRPos a mod x (y :: xs) b = if (lookup b (a, x) > 0)
                                    then edit b (a, x) y :: fixRPos (a + mod) mod x xs b
                                    else (a, x, y) :: fixRPos (a + mod) mod x xs b

putCol : Int -> Bool -> List Int -> Board -> Board
putCol x tf [] [] = []
putCol x tf [] b = []
putCol x tf (y :: xs) [] = []
putCol x tf (y :: xs) b = if (tf == True)
                             then fixCPos 0 1 x (y :: xs) b
                             else fixCPos 3 (-1) x (y :: xs) b where
     fixCPos : Int -> Int -> Int -> List Int -> Board -> Board
     fixCPos a mod x [] b = if (lookup b (x, a) > 0)
                               then fixCPos (a + mod) mod x [] b
                               else []
     fixCPos a mod x [y] b = if (lookup b (x, a) > 0)
                                then edit b (x, a) y :: fixCPos (a + mod) mod x [] b
                                else (x, a, y) :: fixCPos (a + mod) mod x [] b
     fixCPos a mod x (y :: xs) b = if (lookup b (x, a) > 0)
                                    then edit b (x, a) y :: fixCPos (a + mod) mod x xs b
                                    else (x, a, y) :: fixCPos (a + mod) mod x xs b
                                    
shift : List Int -> List Int
shift [] = []
shift [x] = [x]
shift (x :: y :: xs) = if (x == y)
                             then ((x * 2) :: shift xs)
                             else x :: shift (y :: xs)


shiftLeft : Board -> Board
shiftLeft [] = []
shiftLeft b = (leftShift 0 b) ++ (leftShift 1 b) ++ (leftShift 2 b) ++ (leftShift 3 b) where
  leftShift : Int -> Board -> Board
  leftShift x b = putRow x True (shift (getRow x b)) b

shiftRight : Board -> Board
shiftRight [] = []
shiftRight b = (rightShift 0 b) ++ (rightShift 1 b) ++ (rightShift 2 b) ++ (rightShift 3 b) where
  rightShift : Int -> Board -> Board
  rightShift x b = reverse (putRow x False (shift (reverse (getRow x b))) b)

shiftUp : Board -> Board
shiftUp [] = []
shiftUp b = (upShift 0 b) ++ (upShift 1 b) ++ (upShift 2 b) ++ (upShift 3 b) where
  upShift : Int -> Board -> Board
  upShift x b = putCol x True (shift (getCol x b)) b

shiftDown : Board -> Board
shiftDown [] = []
shiftDown b = (downShift 0 b) ++ (downShift 1 b) ++ (downShift 2 b) ++ (downShift 3 b) where
  downShift : Int -> Board -> Board
  downShift x b = reverse (putCol x False (shift (reverse (getCol x b))) b)


data G2048Rules : Effect where
  Won : { G2048 (Running tf p 2048) ==> G2048 NotRunning } G2048Rules()
  Lost : { G2048 (Running True 0 h) ==> G2048 NotRunning } G2048Rules()
  NewBoard : (b : Board) -> { g ==> G2048 (Running False (getPairs b) (getHighest b)) } G2048Rules()
  Get : { g } G2048Rules g


GAME2048 : GState -> EFFECT
GAME2048 g = MkEff (G2048 g) G2048Rules

won : { [GAME2048 (Running tf p 2048)] ==> [GAME2048 NotRunning]} Eff ()
won = call Won

lost : { [GAME2048 (Running True 0 h)] ==> [GAME2048 NotRunning]} Eff ()
lost = call Lost

newBoard : (b : Board) -> { [GAME2048 g] ==> [GAME2048 (Running False (getPairs b) (getHighest b))] } Eff ()
newBoard b = call (NewBoard b)

get : { [GAME2048 g] } Eff (G2048 g)
get = call Get

instance Handler G2048Rules m where
  handle (Mk2048 b 2048 sc sp) Won k = k () (GameWon sc)
  {- handle (Mk2048 b h sc 0) Lost k = k () (GameLost sc) -}
  handle st Get k = k st st
  handle st (NewBoard b) k = k () (initState b)


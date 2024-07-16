type Position = (Int, Int)
data Color = W | B deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
data Move = M Position deriving (Eq, Show)
type Board = [Peg]
data State = S Move Board deriving (Eq, Show)

fixpos :: [Position]
fixpos = [(-3, -1), (-3, 0), (-3, 1),(-2, -1), (-2, 0), (-2, 1),(-1, -3), 
 (-1, -2), (-1, -1), (-1, 0), (-1, 1), (-1, 2), (-1, 3),(0, -3), (0, -2),
 (0, -1), (0, 0), (0, 1), (0, 2), (0, 3),(1, -3), (1, -2), (1, -1), (1, 0),
 (1, 1), (1, 2), (1, 3),(2, -1), (2, 0), (2, 1),(3, -1), (3, 0),(3,1)]

fixboard :: Board
fixboard = [Peg (-3, -1) B, Peg (-3, 0) B, Peg (-3, 1) B,Peg (-2, -1) B ,
            Peg (-2, 0) B, Peg (-2, 1) B,Peg (-1, -3) B , Peg (-1, -2) B,
			Peg (-1, -1) B,Peg (-1, 0) B, Peg (-1, 1) B, Peg (-1, 2) B,
            Peg (-1, 3) B, Peg (0, -3) B, Peg (0, -2) B,Peg (0, -1) B, 
			Peg (0, 0) B, Peg (0, 1) B,Peg (0, 2) B, Peg (0, 3) B, 
			Peg (1, -3) B,Peg (1, -2) B, Peg (1, -1) B, Peg (1, 0) B,
            Peg (1, 1) B, Peg (1, 2) B, Peg (1, 3) B,Peg (2, -1) B, 
	        Peg (2, 0) B,Peg (2, 1) B,Peg (3, -1) B, Peg (3, 0) B, Peg (3, 1) B ]

createBoard :: Position -> Board
createBoard pos = if elem pos fixpos then cbH pos fixboard 
                  else error "The position is not valid."

cbH :: Position -> Board -> Board
cbH _ [] = []
cbH (x,y) (Peg (xf,yf) c : t) =  if (x,y) == (xf,yf) then Peg (xf,yf) W : t 
                                 else Peg (xf,yf) c : cbH(x,y) t
	

isValidMove :: Move -> Board -> Bool

isValidMove (M (x,y)) b2 =  if ( mH (x,y) b2 && mH2 (x,y) b2 )then True
                            else False

mH :: Position -> Board -> Bool
mH _ []=False
mH (x,y) (Peg (xf,yf) c : t) = if ((x,y) == (xf,yf) && c==B ) then True
                               else  mH (x,y) t
mH2 :: Position -> Board -> Bool
mH2 _ [] = False
mH2 (x,y) (Peg (xf,yf) c : t) = if ((( xf==x+1 || xf==x-1) && y==yf && c==W) || (( yf==y+1 || yf==y-1) && x==xf && c==W)) then True
                                else mH2 (x,y) t

isGoal:: Board -> Bool
isGoal [] = True
isGoal (Peg (xf,yf) c : t) = if c == W then isGoal t
                             else False


showPossibleNextStates:: Board -> [State]
showPossibleNextStates 	b =  if isGoal b then error "No Possible States Exist."
                             else  sPNSH b b
							 
sPNSH:: Board -> Board ->[State]
sPNSH _ []=[]
sPNSH b (Peg (xf,yf) c : t) = if ( c == B && isValidMove (M (xf,yf)) b ) then ( S (M (xf,yf)) ( cbH (xf,yf) b )) : sPNSH b t
                              else sPNSH b t
 
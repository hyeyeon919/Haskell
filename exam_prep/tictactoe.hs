import Data.List (transpose)

data Play = O | X deriving (Eq, Show)
data TicTac = TicTac Int [[Maybe Play]] deriving (Eq, Show)
xss = TicTac 3 [[Just X, Just X, Just O],
                [Just O, Just O, Nothing],
                [Just X, Nothing, Nothing]]
yss = TicTac 2 [[Just X, Just O],
                [Nothing, Nothing]]
zss = TicTac 2 [[Just X, Nothing],
                [Just O, Nothing]]
x = [[Just X, Just X, Just O],
    [Just O, Just O, Nothing],
    [Just X, Nothing, Nothing]]
z = [[Just X, Nothing],
    [Nothing, Just X]]

winner :: TicTac -> Bool
winner (TicTac 0 _) = False
winner (TicTac 1 [[Nothing]]) = False
winner (TicTac 1 _) = True
winner (TicTac n x) = (checkC x) || (checkR x)|| (checkD x)

checkR [] = False
checkR (x:xs)
    | elem Nothing x = checkR xs
    | (all (== Just X) x) == True || (all (== Just O) x) == True  = True
    | otherwise = checkR xs

checkC x = checkR (rotate x)

rotate = reverse . transpose

checkD_1 [] = []
checkD_1 (x:xs) = (head(drop (length xs) x)) : checkD_1 xs

checkD_2 x = checkD_1 (rotate x)

checkD x = ((all (==Just O) (checkD_1 x) || all (==Just X) (checkD_1 x)) && (elem (Nothing) (checkD_1 x) == False))|| ((all (==Just O) (checkD_2 x) || all (==Just X) (checkD_2 x)) && (elem (Nothing) (checkD_2 x) == False))

module L4.Week4 where
import Data.Char ( isDigit, isAlpha, toUpper, digitToInt )
import Data.List

-- ################################################################
-- ## CW 
-- ################################################################

type Customer = String
type Product = String

-- An order of some positive quantity of a product by a customer
data Order = Order Customer Product Double
    deriving (Show)

placeOrder :: Order -> String
placeOrder (Order customerName productName quantity) = 
    customerName ++ " -- " ++ productName ++ " -- " ++ show quantity

--productList :: Order-> String
--productList (Product n) = "Sun"

-- Type Definition
-- data Customer = Alien1 | Alien2
--    deriving (Show)

-- data Product = Sun | Planet| Moon
--     deriving (Show)

-- List of products and total in stock stock
sun :: Double
sun = 1

planet :: Double
planet = 8

moon :: Double
moon = 219

totalProduct :: Double
totalProduct = sun + planet + moon



-- Possible solution to stock count
-- stockCount :: Product -> Int
--stockCount Sun = 1
--stockCount Planet = 8
--stockCount Moon = 219

productChoice :: String -> Int
productChoice n
    | n == "Sun"        = 1
    | n == "Planet"     = 2
    | n == "Star"       = 3
    | n == "Otherwise"  = 4

-- Function 3, Majority: 
-- Checking if customer has ordered more than 50% of product stock
-- Using an AUX function to calculate boolan outcome

moreThanHalf :: Double -> Double -> Double
moreThanHalf sold stock
    | checkPercentageX sold stock >= 0.5   = sold
    | checkPercentageX sold stock <0.5     = stock

-- **** Aux function to calculate 50%
checkPercentageX :: Double -> Double -> Double
checkPercentageX sold stock = sold/stock

overFifty :: String
overFifty = undefined

-- ################################################################
-- ## Lecture
-- ################################################################


digits :: String -> [Int]
digits s = [digitToInt c | c <- s, isDigit c]

odds :: [a] -> [a]
odds xs = [x | (i, x) <- zip [1..] xs, odd i]

evens :: [a] -> [a]
evens xs = [x | (i, x) <- zip [1..] xs, even i]

substitute :: Int -> Int
substitute n
    | n*2 > 9 = n*2 - 9
    | otherwise = n*2

valid :: String -> Bool
valid s = checkdigit s == 0

checkdigit :: String -> Int
checkdigit s = total `mod` 10
    where
        total = sum (odds rev_ns) +
            sum [substitute n | n <- evens rev_ns]
        rev_ns = reverse (digits s)


-- ################################################################
-- ## Tutorial
-- ################################################################

tripleAll :: [Int] -> [Int]
tripleAll ns = [3*n | n <- ns]

squareAll :: [Int] -> [Int]
squareAll ns = [n*n | n <- ns]

capitalize :: String -> String
capitalize cs = [toUpper c | c <- cs]

capitalizeLetters :: String -> String
capitalizeLetters cs = [toUpper c | c <- cs, isAlpha c]

backwards :: String -> String
backwards s = unwords (reverse (words s))

revwords :: String -> String
revwords s = unwords [reverse w | w <- words s]

divisors :: Int -> [Int]
divisors n = [d | d <- [1..n], n `mod` d == 0]

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

palindrome1 :: String -> Bool
palindrome1 w = reverse w == w

palindrome2 :: String -> Bool
palindrome2 w = if reverse w == w then True else False

palindrome3 :: String -> Bool
palindrome3 w = palindrome1 (capitalizeLetters w)

-- list of unique items in the input list, each paired with the
-- number of times it occurs
frequency :: [Char] -> [(Char, Int)]
frequency ws = [(head g, length g) | g <- group (sort ws)]

palindromic :: [Char] -> Bool
palindromic xs = length [x | (x, n) <- frequency xs, odd n] <= 1


-- all the ways of splitting a list, earliest first
splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

-- all non-empty sublists of a list
sublists :: [a] -> [[a]]
sublists xs = [ys | ts <- tails xs, ys <- inits ts, not (null ys)]

largestRectangle :: [Int] -> Int
largestRectangle hs = maximum [length l * minimum l | l <- sublists hs]


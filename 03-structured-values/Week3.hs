module L3.Week3 where

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
-- (1 * quantity)


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

data PriceTag = Item String Double

showPriceTag :: PriceTag -> String
showPriceTag (Item n price) =
    n ++ " -- " ++ show (1.2 * price)

-- ?? >> added to abovfe!
addVAT :: PriceTag -> PriceTag
addVAT (Item nm price) = Item nm (1.2*price)


data Shape 
    = Circle Double
    | Rectangle Double Double
    deriving (Eq, Show)

area :: Shape -> Double
area (Circle r) = pi*r*r
area (Rectangle w h) = w*h

rotate :: Shape -> Shape
rotate (Circle r) = Circle r
rotate (Rectangle w h) = Rectangle h w

scale :: Double -> Shape -> Shape
scale x (Circle r) = Circle (r*x)
scale x (Rectangle w h) = Rectangle (w*x) (h*x)

-- ################################################################
-- ## Tutorial
-- ################################################################

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

dup :: a -> (a,a)
dup x = (x,x)


-- Errors!
safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
    | y == 0 = Nothing
    | otherwise = Just (div x y)

--
--
-- Do question 6, 7, 8, 9 all errors


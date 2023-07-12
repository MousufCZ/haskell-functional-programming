module L5.Week5 where

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
module Orders where

type Customer = String
type Product = String

-- An order of some positive quantity of a product by a customer
data Order = Order Customer Product Double
    deriving (Show)

-- A delivery to the supplier of some quantity quantity of a product
data Delivery = Delivery Product Double
    deriving (Show)

-- All customers who submitted an order, with the number of different
-- products each of them ordered. 
numProducts :: [Order] -> [(Customer, Int)]
numProducts orders = undefined

-- All products that have been ordered, with the total quantity of each.
productQuantities :: [Order] -> [(Product, Double)]
productQuantities orders = undefined

-- The customers and products for which the customer has ordered
-- more than half the total quantity for that product.
majority :: [Order] -> [(Customer, Product)]
majority orders = undefined

-- Products for which the total quantity ordered exceeds the
-- total quantity delivered, with the difference in quantity.
shortfall :: [Order] -> [Delivery] -> [(Product, Double)]
shortfall orders deliveries = undefined

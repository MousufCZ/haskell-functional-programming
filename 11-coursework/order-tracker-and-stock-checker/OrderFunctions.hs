module BillingOTest where

import Data.Map (Map)
import qualified Data.Map as Map

type Customer = String
type Product = String

data Order = Order Customer Product Int
    deriving (Show)

type PriceList = Map Product Double

customers :: [Order] -> [Customer]
customers orders = removeDuplicates [c | Order c _ _ <- orders]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates [y | y <- xs, y /= x]

bill :: [Order] -> PriceList -> [(Customer, Double)]
bill orders priceList =
  Map.assocs $ combineBills orders priceList

combineBills :: [Order] -> PriceList -> Map Customer Double
combineBills [] _ = Map.empty
combineBills (Order c p qty : rest) priceList =
  case Map.lookup p priceList of
    Just price -> Map.insertWith (+) c (fromIntegral qty * price) (combineBills rest priceList)
    Nothing    -> combineBills rest priceList

unavailable :: [Order] -> PriceList -> [(Customer, [Product])]
unavailable orders priceList =
    [(c, removeDuplicates [p | Order c' p _ <- orders, c' == c, Nothing <- [Map.lookup p priceList]]) | c <- customers orders]

bill_discount :: [Order] -> PriceList -> [(Customer, Double)]
bill_discount orders priceList =
    [(c, sum [calculateDiscountedPrice priceList p qty | Order c' p qty <- orders, c' == c]) | c <- customers orders]

calculateDiscountedPrice :: PriceList -> Product -> Int -> Double
calculateDiscountedPrice priceList prod qty = fromIntegral discountedQty * price
    where
        discountedQty = (qty + 1) `div` 2
        price = case Map.lookup prod priceList of
            Just p -> p
            Nothing -> error "Product not found in the price list!"



bill_discount :: [Order] -> PriceList -> [(Customer, Double)]
bill_discount orders priceList =
    let customerBills = foldr combineBills Map.empty orders
    in Map.assocs customerBills
  where
    combineBills (Order c p qty) billMap =
      case Map.lookup p priceList of
        Just price -> Map.insertWith (+) c (calculateDiscountedPrice priceList p qty) billMap
        Nothing    -> billMap


bill_discount :: [Order] -> PriceList -> [(Customer, Double)]
bill_discount orders priceList = 
        let customerBills = foldr combineBills Map.empty orders
        in Map.assocs customerBills
    where
        combineBills (Order c p qty) billMap =
            case Map.lookup p priceList of
                Just price -> Map.insertWith (+) c (calculateDiscountedPrice priceList p qty) billMap
                Nothing -> billMap

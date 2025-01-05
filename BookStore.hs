-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
              deriving (Show)
-- We will introduce the CustomerID type shortly.

data BookReview = BookReview BookInfo CustomerID String
type CustomerID = Int
type ReviewBody = String
type BookRecord = (BookInfo, BookReview)
data BetterReview = BetterReview BookInfo CustomerID ReviewBody
type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)                
myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]
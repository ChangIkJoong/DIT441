module Blackjack where

import Cards
import RunGame

{-----------------------------------[Pre-Definitions]-----------------------------------}
{---------------------------------------------------------------------------------------}
hand2 :: Hand
hand2 = Card (Numeric 2) Hearts : (Card Jack Spades : [])

--hand2 :: Hand
--hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]



{--------------------------------------[FUNCTION:s]--------------------------------------}
{---------------------------[my test cards for the functions]----------------------------}
aCard1 :: Card
aCard1 = Card King Clubs -- define your favorite card here

aCard2 :: Card
aCard2 = Card Queen Hearts -- define another card here
-- aCard2 = Card King Clubs

aCard3 :: Card
aCard3 = Card Jack Spades

aCard4 :: Card
aCard4 = Card Ace Spades

aCard5 :: Card
aCard5 = Card Ace Hearts

aHand :: Hand
aHand = [aCard1 , aCard2, aCard3, aCard4, aCard5] -- a Hand with two Cards, aCard1 and aCard2



{---------------------------------------[TASK A1]---------------------------------------}
{---------------------------------------------------------------------------------------}
{-
size hand2
    = size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
    = 1 + size (Card Jack Spades : [])
    = 1 + 1 + size []
    = 1 + 1 + 0
    = 2
-}


-- As size (card:hand) = 1 + size hand (recursively calling itself), 
-- which in turn is the second card, Jack spades which when called again: 1 + 1 + []
-- As it once again = 1 + size hand, which in this iteration becomes: [] = 0
-- Adding the iterations together, it becomes 1 + 1 + 0 = 2.

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + size (Card Jack Spades : [])
            , 1 + 1 + size []
            , 1 + 1 + 0
            , 2]


{---------------------------------------[TASK A2]---------------------------------------}
{---------------------------------------------------------------------------------------}

-- | A card has a rank and belongs to a suit
-- | call upon card, numeric n and the suit
-- | prints a series of strings with number, " of " and the suit.
-- | int the second case, it calls upon the rank when a string is inserted and not a int.
displayCard :: Card -> String
displayCard (Card (Numeric n) suit) = show n ++ " of " ++ show suit
displayCard (Card rank suit) = show rank ++ " of " ++ show suit

-- | Displays the type hand = [Card], output as a string from the list.
-- |displays with unlines, inserting /n between the strings.
display :: Hand -> String
display hand = unlines (map displayCard hand)

--putStr (display aHand)

{---------------------------------------[TASK A3]---------------------------------------}
{---------------------------------------------------------------------------------------}
-- | Defines all the values assumed by the card Rank, in this case also for
-- | Jack, Queen, King = 10 , Ace =11
valueRank :: Rank -> Int
valueRank (Numeric n)           = n
valueRank Jack                  = 10
valueRank Queen                 = 10
valueRank King                  = 10
valueRank Ace                   = 11

-- | Inputs the card, extracts the rank and returns a numeric value from the valueRank rank.
valueCard :: Card -> Int
valueCard (Card rank _) = valueRank rank

-- | Number of aces when called upon uses a recursive function to find all of type Card Ace,
-- | and uses this to return the amount of aces using list comprehension.
numberOfAces :: Hand -> Int
numberOfAces []                 = 0
numberOfAces (Card Ace _:last)  = 1 + numberOfAces last
numberOfAces (Card _ _:last)    = numberOfAces last

-- | Calculates the total value of the Hand input, if list is empty 0 is returned, and then
-- | recursively adds the valueCard (value of the card) from card to last in the hand list,
-- | and iterates until the list is complete.
cardCalculator :: Hand -> Int
cardCalculator []           = 0
cardCalculator (card:xy)    = valueCard card + cardCalculator xy

-- | adds all the Card Ranks (values) together, if the function amounts to more than 21,
-- | and has included aces, the function reduces the aces to value 1 (11 -10 =1).
-- | Otherwise, it simply calculates the hand from cardCalculator together.
value :: Hand -> Int
value hand  | cardCalculator hand > 21 = cardCalculator hand - (10 * numberOfAces hand)
            | otherwise = cardCalculator hand

{---------------------------------------[TASK A4]---------------------------------------}
{---------------------------------------------------------------------------------------}

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

winner :: Hand -> Hand -> String
winner guest bank   |value(guest) > value(bank)     = "Win"
                    |value(guest) < value(bank)     = "Lost"
                    |value(guest) == value(bank)    = "Lost"
--                    |
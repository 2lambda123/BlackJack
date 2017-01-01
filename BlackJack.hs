module BlackJack where

import Cards
import Wrapper
mport Test.QuickCheck hiding (shuffle)
import System.Random

{-|
	size hand2		
	= size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
	= 1 + size( Add (Card Jack Spades) Empty)
	= 2 + size (Empty)		
	= 2 + 0
	= 2
-}

empty :: Hand
empty = Empty

value :: Hand -> Integer
value hand 
 | valueOfAHand hand <= 21 = valueOfAHand hand
 | otherwise = valueOfAHand hand - (10 * numberOfAces hand) 

{-| Total value of a hand -}
valueOfAHand :: Hand -> Integer
valueOfAHand Empty            = 0
valueOfAHand (Add card hand) = valueOfACard card + valueOfAHand hand

{-| Figure out the value of a given Card -}
valueOfACard :: Card -> Integer
valueOfACard (Card r s) = valueOfARank r

{-| Figure out the value of a given Rank -}
valueOfARank :: Rank -> Integer
valueOfARank (Numeric i) = i
valueOfARank Ace         = 11
valueOfARank _           = 10 

{-| Count the number of Aces in a Hand -}
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace s) hand) = 1 + numberOfAces hand
numberOfAces (Add _ hand) = numberOfAces hand

{-| Checks if a player is burst --}
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

{-| Checks for a winner -}
winner :: Hand -> Hand -> Player
winner handGuest handBank 
 | gameOver handGuest                 = Bank					
 | gameOver handBank                  = Guest					
 | value handGuest > value handBank   = Guest		
 | value handBank > value handGuest   = Bank		
 | value handGuest == value handBank  = Bank		

(<+) :: Hand -> Hand -> Hand
Empty <+ hand = hand
hand <+ Empty = hand
(Add card hand1) <+ hand2 = Add card (hand1 <+ hand2)

-- Test for associativity
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc hand1 hand2 hand3 = 
 hand1 <+ (hand2 <+ hand3) == (hand1 <+ hand2) <+ hand3

-- size of two separate hands 
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = 
 size hand1 + size hand2 == size (hand1 <+ hand2)

allCards :: Suit -> Hand
allCards s = 
  Add (Card King s) 
   (Add (Card Queen s) 
    (Add (Card Jack s) 
     (Add (Card Ace s) 
      (Add (Card (Numeric 10) s) 
       (Add (Card (Numeric 9) s) 
        (Add (Card (Numeric 8) s) 
         (Add (Card (Numeric 7) s) 
          (Add (Card (Numeric 6) s) 
           (Add (Card (Numeric 5) s) 
            (Add (Card (Numeric 4) s) 
             (Add (Card (Numeric 3) s) 
              (Add (Card (Numeric 2) s)  Empty))))))))))))
              
fullDeck :: Hand
fullDeck = 
 allCards Spades <+ allCards Hearts <+ allCards Clubs <+ allCards Diamonds

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty." 
draw (Add card hand1) hand2 = (hand1 , Add card hand2)

playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck bankHand 
 | value bankHand < 16 = playBankHelper deck' bankHand'
 | otherwise = bankHand
      where (deck' , bankHand') = draw deck bankHand  

playBank :: Hand -> Hand
playBank deckOfCards = playBankHelper deckOfCards Empty

shuffle :: StdGen -> Hand -> Hand
shuffle gen deck = shuffleHelperFunc gen deck Empty

shuffleHelperFunc :: StdGen -> Hand -> Hand -> Hand
shuffleHelperFunc gen Empty returned = returned
shuffleHelperFunc gen deck returned = 
 shuffleHelperFunc gen' the_deck (Add the_card returned) 
 where ((the_deck, the_card), gen') = getRandomCard gen deck

{-| Pick any random card a deck.-}
getRandomCard :: StdGen -> Hand -> ((Hand, Card), StdGen)
getRandomCard gen deck = (getNthCard deck Empty n, gen') 
 where (gen', n) = getRandomNumber gen (size deck - 1)

{-| Generate any random numbers based on any given range i.e max -}
getRandomNumber :: StdGen -> Integer -> (StdGen, Integer)
getRandomNumber gen max = (gen', n) 
 where (n, gen') = randomR (0, max) gen

getNthCard :: Hand -> Hand -> Integer -> (Hand, Card)
getNthCard (Add top_card source) dest n 
 | n >= 0 = (source <+ dest, top_card)
 | otherwise = getNthCard source (Add top_card dest) (n - 1)

card `belongsTo` Empty    = False
card `belongsTo` Add card' hand = card == card' || card `belongsTo` hand

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g card hand = 
 card `belongsTo` hand == card `belongsTo` shuffle' g hand

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle k q =  size (shuffle k q) == size q

implementation = Interface
  {  iEmpty     = empty
  ,  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle'
  }

main :: IO ()
main = runGame implementation

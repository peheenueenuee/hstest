
import System.Random.Shuffle
import Cards
import Hands
import Data.List

spA = allCards !! 12
sp2 = allCards !! 0
sp3 = allCards !! 1
sp4 = allCards !! 2
sp5 = allCards !! 3
sp6 = allCards !! 4
sp7 = allCards !! 5

ht2 = allCards !! 13
ht9 = allCards !! 20

cr2 = allCards !! 26
cr3 = allCards !! 27
cr4 = allCards !! 28

diA = last allCards
di2 = allCards !! 39
di3 = allCards !! 40
di4 = allCards !! 41
di5 = allCards !! 42

di10 = allCards !! 47
diJ = allCards !! 48
diQ = allCards !! 49
diK = allCards !! 50

smpFlush = toHand [sp2, sp3, sp4, sp6, sp7]
smpHighCard = toHand [sp2, sp3, sp4, ht9, sp7]
smpOnePair = toHand [sp2, ht2, sp4, ht9, sp7]
smpTwoPair = toHand [sp2, ht2, sp4, cr4, sp7]
smpTreeOfKind = toHand [sp2, ht2, cr2, cr4, sp7]
smpFourCard = toHand [sp2, ht2, cr2, di2, sp7]
smpStraight12 = toHand [diA, sp2, di3, di4, di5]
smpStraightKA = toHand [di10, diJ, diQ, diK, spA]

#!/bin/sh

cd BrzozowskiDerivatives
ghc Brzozowski.hs

echo "Testing Brzozowski.hs..."

./Brzozowski > result.txt

#extract and count failed and passed tests

echo "Failed tests in prop_AltIden:"
awk '/prop_AltIden/,/prop_AltIden/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_AltIden:"
awk '/prop_AltIden/,/prop_CatAssoc/' result.txt | grep -c "OK"


echo "Failed tests in prop_CatAssoc:"
awk '/prop_CatAssoc/,/prop_CatIdem/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_CatAssoc:"
awk '/prop_CatAssoc/,/prop_CatIdem/' result.txt | grep -c "OK"





#!/bin/sh

cd Brzozowski
ghc Brzozowski.hs

echo "Testing Brzozowski.hs..."

./Brzozowski > result.txt

#extract and count failed and passed tests

#echo "Failed tests in prop_AltAssoc:"
#cat result.txt | sed -n '/prop_AltAssoc/,/prop_AltCom/p' | grep -c "Falsified"
#echo "Succesful tests in prop_AltAssoc:"
#cat result.txt | sed -n '/prop_AltAssoc/,/prop_AltCom/p' | grep -c "OK"

#echo "Failed tests in prop_AltCom:"
#cat result.txt | sed -n '/prop_AltCom/,/prop_AltIdem/p' | grep -c "Falsified"
#echo "Succesful tests in prop_AltIdem:"
#cat result.txt | sed -n '/prop_AltCom/,/prop_AltIdem/p' | grep -c "OK"


#echo "Failed tests in prop_AltIdem:"
#cat result.txt | sed -n '/prop_AltIdem/,/AltIden/p' | grep -c "Falsified"
#echo "Succesful tests in prop_AltIdem:"
#cat result.txt | sed -n '/prop_AltIdem/,/AltIden/p' | grep -c "OK"

echo "Failed tests in prop_CatAssoc:"
awk '/prop_CatAssoc/,/prop_CatIdem/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_CatAssoc:"
awk '/prop_CatAssoc/,/prop_CatIdem/' result.txt | grep -c "OK"

echo "Failed tests in prop_CatIdem:"
awk '/prop_CatIdem/,/end/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_CatIdem:"
awk '/prop_CatIdem/,/end/' result.txt | grep -c "OK"




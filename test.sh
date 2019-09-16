#!/bin/sh

cd BrzozowskiDerivatives
ghc Brzozowski.hs

echo "Testing Brzozowski.hs..."

./Brzozowski > result.txt

#extract and count failed and passed tests
echo "Failed tests in prop_Grep:"
awk '/prop_Grep/,/prop_AltAssoc/' result.txt | grep -c "Failed"

echo "Successful tests in prop_Grep:"
awk '/prop_Grep/,/prop_AltAssoc/' result.txt | grep -c "OK"

echo "Failed tests in prop_AltAssoc:"
awk '/prop_AltAssoc/,/prop_AltCom/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_AltAssoc:"
awk '/prop_AltAssoc/,/prop_AltCom/' result.txt | grep -c "OK"

echo "Failed tests in prop_AltCom:"
awk '/prop_AltCom/,/prop_AltIdem/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_AltCom:"
awk '/prop_AltCom/,/prop_AltIdem/' result.txt | grep -c "OK"

echo "Failed tests in prop_AltIdem:"
awk '/prop_AltIdem/,/prop_AltIden/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_AltIdem:"
awk '/prop_AltIdem/,/prop_AltIden/' result.txt | grep -c "OK"

echo "Failed tests in prop_AltIden:"
awk '/prop_AltIden/,/prop_AltIden/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_AltIden:"
awk '/prop_AltIden/,/prop_CatAssoc/' result.txt | grep -c "OK"

echo "Failed tests in prop_CatAssoc:"
awk '/prop_CatAssoc/,/prop_CatIden/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_CatAssoc:"
awk '/prop_CatAssoc/,/prop_CatIden/' result.txt | grep -c "OK"

echo "Failed tests in prop_CatIden:"
awk '/prop_CatIden/,/prop_DistLeft/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_CatIden:"
awk '/prop_CatIden/,/prop_DistLeft/' result.txt | grep -c "OK"

echo "Failed tests in prop_DistLeft:"
awk '/prop_DistLeft/,/prop_DistRight/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_DistLeft:"
awk '/prop_DistLeft/,/prop_DistRight/' result.txt | grep -c "OK"

echo "Failed tests in prop_DistRight:"
awk '/prop_DistRight/,/prop_Clo/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_DistRight:"
awk '/prop_DistRight/,/prop_Clo/' result.txt | grep -c "OK"

echo "Failed tests in prop_Clo:"
awk '/prop_prop_Clo/,/prop_Clo2/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_Clo:"
awk '/prop_DistRight/,/prop_Clo/' result.txt | grep -c "OK"

echo "Failed tests in prop_Clo2:"
awk '/prop_Clo2/,/end/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_Clo2:"
awk '/prop_Clo2/,/end/' result.txt | grep -c "OK"



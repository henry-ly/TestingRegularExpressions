#!/bin/sh

cd BrzozowskiDerivatives
ghc Brzozowski.hs

echo "Testing Brzozowski.hs..."

./Brzozowski > result.txt

#extract and count failed and passed tests
echo "Failed tests in prop_Nil:" 
awk '/prop_Nil/,/prop_Eps/' result.txt | grep -c "Failed"

echo "Successful tests in prop_Nil:"
awk '/prop_Nil/,/prop_Eps/' result.txt | grep -c "OK"

echo "Failed tests in prop_Eps:"
awk '/prop_Eps/,/prop_Atom/' result.txt | grep -c "Failed"

echo "Successful tests in prop_Eps:"
awk '/prop_Eps/,/prop_Atom/' result.txt | grep -c "OK"

echo "Failed tests in prop_Atom:"
awk '/prop_Atom/,/prop_Plus/' result.txt | grep -c "Failed"

echo "Successful tests in prop_Atom:"
awk '/prop_Atom/,/prop_Plus/' result.txt | grep -c "OK"

echo "Failed tests in prop_Plus:"
awk '/prop_Plus/,/prop_Seq/' result.txt | grep -c "Failed"

echo "Successful tests in prop_Plus:"
awk '/prop_Plus/,/prop_Seq/' result.txt | grep -c "OK"

echo "Failed tests in prop_Seq:"
awk '/prop_Seq/,/prop_Grep/' result.txt | grep -c "Failed"

echo "Successful tests in prop_Seq:"
awk '/prop_Seq/,/prop_Grep/' result.txt | grep -c "OK"

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
awk '/prop_AltIdem/,/prop_CatAssoc/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_AltIdem:"
awk '/prop_AltIdem/,/prop_CatAssoc/' result.txt | grep -c "OK"

echo "Failed tests in prop_CatAssoc:"
awk '/prop_CatAssoc/,/prop_DistLeft/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_CatAssoc:"
awk '/prop_CatAssoc/,/prop_DistLeft/' result.txt | grep -c "OK"

echo "Failed tests in prop_DistLeft:"
awk '/prop_DistLeft/,/prop_DistRight/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_DistLeft:"
awk '/prop_DistLeft/,/prop_DistRight/' result.txt | grep -c "OK"

echo "Failed tests in prop_DistRight:"
awk '/prop_DistRight/,/prop_Clo/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_DistRight:"
awk '/prop_DistRight/,/prop_Clo/' result.txt | grep -c "OK"

echo "Failed tests in prop_Clo:"
awk '/prop_Clo/,/prop_Clo2/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_Clo:"
awk '/prop_Clo/,/prop_Clo2/' result.txt | grep -c "OK"

echo "Failed tests in prop_Clo2:"
awk '/prop_Clo2/,/prop_Derived/' result.txt | grep -c "Failed"

echo "Succesful tests in prop_Clo2:"
awk '/prop_Clo2/,/prop_Derived/' result.txt | grep -c "OK"

echo "Failed tests in prop_Derived:"
awk '/prop_Derived/,/End/' result.txt | grep -c "Failed"

echo "Successful tests in prop_Derived:"
awk '/prop_Derived/,/End/' result.txt | grep -c "OK"



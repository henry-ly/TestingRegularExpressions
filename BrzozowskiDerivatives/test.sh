#!/bin/sh

#cd BrzozowskiDerivatives
ghc Brzozowski.hs

echo "Testing Brzozowski.hs..."

./Brzozowski > result.txt

#extract and count failed and passed tests
echo "prop_Nil"
echo "Successful: $(awk '/prop_Nil/,/prop_Eps/' result.txt | grep -c "OK") Failed: $(awk '/prop_Nil/,/prop_Eps/' result.txt | grep -c "Failed")"
echo
echo "prop_Eps"
echo "Successful: $(awk '/prop_Eps/,/prop_Atom/' result.txt | grep -c "OK") Failed: $(awk '/prop_Eps/,/prop_Atom/' result.txt | grep -c "Failed")"
echo
echo "prop_Atom"
echo "Successful: $(awk '/prop_Atom/,/prop_Plus/' result.txt | grep -c "OK") Failed: $(awk '/prop_Atom/,/prop_Plus/' result.txt | grep -c "Failed")"
echo
echo "prop_Plus"
echo "Successful: $(awk '/prop_Plus/,/prop_Seq/' result.txt | grep -c "OK") Failed: $(awk '/prop_Plus/,/prop_Seq/' result.txt | grep -c "Failed")"
echo
echo "prop_Seq"
echo "Successful: $(awk '/prop_Seq/,/prop_Grep/' result.txt | grep -c "OK") Failed: $(awk '/prop_Seq/,/prop_Grep/' result.txt | grep -c "Failed")"
echo
echo "prop_Grep"
echo "Successful: $(awk '/prop_Grep/,/prop_AltAssoc/' result.txt | grep -c "OK") Failed: $(awk '/prop_Grep/,/prop_AltAssoc/' result.txt | grep -c "Failed")"
echo
echo "prop_AltAssoc"
echo "Succesful: $(awk '/prop_AltAssoc/,/prop_AltCom/' result.txt | grep -c "OK") Failed: $(awk '/prop_AltAssoc/,/prop_AltCom/' result.txt | grep -c "Failed")"
echo
echo "prop_AltCom"
echo "Succesful: $(awk '/prop_AltCom/,/prop_AltIdem/' result.txt | grep -c "OK") Failed: $(awk '/prop_AltCom/,/prop_AltIdem/' result.txt | grep -c "Failed")"
echo
echo "prop_AltIdem"
echo "Succesful: $(awk '/prop_AltIdem/,/prop_CatAssoc/' result.txt | grep -c "OK") Failed: $(awk '/prop_AltIdem/,/prop_CatAssoc/' result.txt | grep -c "Failed")"
echo
echo "prop_CatAssoc"
echo "Succesful: $(awk '/prop_CatAssoc/,/prop_DistLeft/' result.txt | grep -c "OK") Failed: $(awk '/prop_CatAssoc/,/prop_DistLeft/' result.txt | grep -c "Failed")"
echo
echo "prop_DistLeft"
echo "Succesful: $(awk '/prop_DistLeft/,/prop_DistRight/' result.txt | grep -c "OK") Failed: $(awk '/prop_DistLeft/,/prop_DistRight/' result.txt | grep -c "Failed")"
echo
echo "prop_DistRight"
echo "Succesful: $(awk '/prop_DistRight/,/prop_Clo/' result.txt | grep -c "OK") Failed: $(awk '/prop_DistRight/,/prop_Clo/' result.txt | grep -c "Failed")"
echo
echo "prop_Clo1"
echo "Succesful: $(awk '/prop_Clo1/,/prop_Clo2/' result.txt | grep -c "OK") Failed: $(awk '/prop_Clo1/,/prop_Clo2/' result.txt | grep -c "Failed")"
echo
echo "prop_Clo2"
echo "Succesful: $(awk '/prop_Clo2/,/prop_Clo3/' result.txt | grep -c "OK") Failed: $(awk '/prop_Clo2/,/prop_Clo3/' result.txt | grep -c "Failed")"
echo
echo "prop_Clo3"
echo "Succesful: $(awk '/prop_Clo3/,/prop_Brz/' result.txt | grep -c "OK") Failed: $(awk '/prop_Clo3/,/prop_Brz/' result.txt | grep -c "Failed")"
echo
echo "prop_Brz"
echo "Succesful: $(awk '/prop_Brz/,/prop_Brz2/' result.txt | grep -c "OK") Failed: $(awk '/prop_Brz/,/prop_Brz2/' result.txt | grep -c "Failed")"
echo
echo "prop_Brz2"
echo "Succesful: $(awk '/prop_Brz2/,/END/' result.txt | grep -c "OK") Failed: $(awk '/prop_Brz2/,/END/' result.txt | grep -c "Failed")"

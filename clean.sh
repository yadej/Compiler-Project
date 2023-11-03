rm tests/*.imp
rm tests/*.asm
rm testsImp/*.asm


cd Asimp
dune clean
cd ..
cd Objlng/AnnotParser
dune clean
cd ../..
cd ImpOpt
dune clean
cd ..
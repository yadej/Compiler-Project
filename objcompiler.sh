cd Objlng/AnnotParser
dune build
cd ..
cd ..

./Objlng/AnnotParser/objlngc.exe tests/test.obj
./Objlng/AnnotParser/objlngc.exe tests/inheritanceAttrTest.obj
./Objlng/AnnotParser/objlngc.exe tests/inheritanceMethTest.obj
./Objlng/AnnotParser/objlngc.exe tests/inheritanceOveridingTest.obj

cd Objlng/AnnotParser
cd ..
cd ..

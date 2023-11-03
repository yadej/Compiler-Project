cd Objlng/AnnotParser
dune build
cd ../..

./Objlng/AnnotParser/objlngc.exe tests/test.obj
./Objlng/AnnotParser/objlngc.exe tests/inheritanceAttrTest.obj
./Objlng/AnnotParser/objlngc.exe tests/inheritanceMethTest.obj
./Objlng/AnnotParser/objlngc.exe tests/inheritanceOveridingTest.obj
./Objlng/AnnotParser/objlngc.exe tests/inheritanceSuperTest.obj
cd Objlng/AnnotParser
cd ../..

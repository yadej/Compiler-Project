cd Objlng/AnnotParser
dune build
cd ..
cd ..

./Objlng/AnnotParser/objlngc.exe tests/test.obj

cd Objlng/AnnotParser
dune clean
cd ..
cd ..
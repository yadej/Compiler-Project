cd ImpOpt

dune build

cd ..

DOSSIER_TESTS="./testsImp/*.imp"
if ls $DOSSIER_TESTS 1> /dev/null 2>&1; then
    # Itère à travers les fichiers .imp dans le dossier et exécute impc.exe
    for File in $DOSSIER_TESTS; do
        echo "$File"
        ./ImpOpt/impc.exe "$File"
    done
else
    echo "Aucun fichier .simp trouvé dans le dossier 'tests'."
fi

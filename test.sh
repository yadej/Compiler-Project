cd Asimp
dune build
cd ..
DOSSIER_TESTS="./tests/*.simp"
if ls $DOSSIER_TESTS 1> /dev/null 2>&1; then
    # Itère à travers les fichiers .simp dans le dossier et exécute asimpc.exe
    for File in $DOSSIER_TESTS; do
        ./Asimp/asimpc.exe "$File"
    done
else
    echo "Aucun fichier .simp trouvé dans le dossier 'tests'."
fi

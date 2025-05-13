Pour compiler un test rust : 
cd test/rust
cargo build --bin [name]

Pour génerer un fichier test pour la points_to_analysis
cd test/rust && ../../charon/bin/charon && mv -t .. *.llbc

Todo : changer dune test pour automatiser la deuxième partie
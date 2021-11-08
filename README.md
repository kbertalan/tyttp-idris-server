# Idris Server over TyTTP

This small project is an attempt to integrate Idris Server with TyTTP.

Used versions:

- idris-server https://gitlab.com/avidela/idris-server/-/tree/661a4ecf0fadaa2bd79c8e922c2d4f79b0b7a445
- tyttp https://github.com/kbertalan/tyttp/tree/f80a3eb8eaa8ea67cc167062271cb2e7ae7c6e83

# Install prerequisities

If you have cloned the projects, then:

    cd idris-server
    idris2 --install server.ipkg

    cd tyttp
    idris2 --install tyttp.ipkg --codegen node

    cd tyttp-idris-server
    idris2 --build tyttp-idris-server.ipkg --codegen node

# Run

You need Node 14.x LTS to be installed, then:

    cd tyttp-idris-server
    node build/exec/tyttp-idris-server


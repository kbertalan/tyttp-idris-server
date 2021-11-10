# Idris Server over TyTTP

This small project is an attempt to integrate Idris Server with TyTTP.

Used versions:

- idris-server https://gitlab.com/avidela/idris-server/-/tree/661a4ecf0fadaa2bd79c8e922c2d4f79b0b7a445
- tyttp https://github.com/kbertalan/tyttp/tree/1eddf5d575778e3fc66ca2207a44a4e0787ffe5c

# Install prerequisities

If you have cloned the projects, then:

    cd idris-server
    // change HTTPVersion visibility to 'public export' in idris2/Requests.idr
    // remove '*> fflush stdout' from idris2/Data/IO/Logging.idr
    idris2 --install server.ipkg

    cd tyttp
    idris2 --install tyttp.ipkg --codegen node

    cd tyttp-idris-server
    idris2 --build tyttp-idris-server.ipkg --codegen node

# Run

You need Node 14.x LTS to be installed and Idris2 version 0.5.1-0a4fd3dc0 then:

    cd tyttp-idris-server
    node build/exec/tyttp-idris-server

# Other notes

A part of Idris Server has been copied over to `src/Server/Engine/TyTTP.idr` and modified to get request as parameter and return result.


cd frontend && ../deps/reflex-platform/work-on ./tr.nix  ./. --run "cabal configure --ghcjs && cabal build" && cd ..
cd backend  && ../deps/reflex-platform/work-on ./ghc.nix ./. --run "cabal configure         && cabal build" && cd ..
cp frontend/dist/build/frontend/frontend.jsexe/*.js app/static/js
cp backend/dist/build/backend/backend app/

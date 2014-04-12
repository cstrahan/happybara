#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "---------------------------"
echo "Configuring sandboxes . . ."
echo "---------------------------"

cd $DIR/happybara
cabal sandbox init

cd $DIR/happybara-webkit
cabal sandbox init
cabal sandbox add-source $DIR/happybara

cd $DIR/happybara-webkit-server
cabal sandbox init
cabal sandbox add-source $DIR/happybara

cd $DIR/happybara-demo
cabal sandbox init
cabal sandbox add-source $DIR/happybara
cabal sandbox add-source $DIR/happybara-webkit
cabal sandbox add-source $DIR/happybara-webkit-server

echo "-----------------------------"
echo "Installing dependencies . . ."
echo "-----------------------------"

cd $DIR/happybara
cabal install --only-dependencies

cd $DIR/happybara-webkit
cabal install --only-dependencies

cd $DIR/happybara-webkit-server
cabal install --only-dependencies

cd $DIR/happybara-demo
cabal install --only-dependencies

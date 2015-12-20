#!/bin/bash

set -eu

mkdir -p build/

cp -r css/ node_modules/ index.html elm.js runElm.js build

rsync -duav -e"ssh -p 49620" build bklx@igreque.info:/var/www/lovely-commune

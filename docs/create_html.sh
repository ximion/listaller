#!/bin/bash
set -e

mkdir -p html
cd html
yelp-build html ../pages/*page
#mv *.html ../html/
#mv *.css ../html/
#mv *.js ../html/
#mv *.png ../html/
cd ..
cp img/*.png html/

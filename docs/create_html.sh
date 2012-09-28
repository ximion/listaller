#!/bin/bash
set -e

mkdir -p html
cd pages
yelp-build html *page
mv *.html ../html/
mv *.css ../html/
mv *.js ../html/
mv *.png ../html/
cd ..
cp img/*.png html/

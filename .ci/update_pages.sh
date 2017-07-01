#!/usr/bin/env bash
BRANCH=master
TARGET_REPO=vaibhavsagar/vaibhavsagar.github.io.git
OUTPUT_FOLDER=_site

echo -e "Starting to deploy to Github Pages\n"
if [ "$TRAVIS" == "true" ]; then
    git config --global user.email "travis@travis-ci.org"
    git config --global user.name "Travis"
fi
# Using token, clone gh-pages branch
git clone --depth 1 --quiet --branch=$BRANCH "https://$GH_TOKEN@github.com/$TARGET_REPO" build > /dev/null
# Go into directory and copy data we're interested in to that directory
cd build || exit
rsync -av --delete --exclude=.git  ../$OUTPUT_FOLDER/ ./
# Add, commit and push files
git add --all .
git commit --allow-empty -m "Travis build $TRAVIS_BUILD_NUMBER pushed to Github Pages"
git push -fq origin $BRANCH > /dev/null
echo -e "Deploy completed\n"
#!/bin/sh

PKL=~/sites/grandchallenge/darpa.pkl
RSS=~/sites/grandchallenge/html/darpa-forum.xml
URL=http://dtsn.darpa.mil/ibb/categoryindex.aspx?boardID=1

cd ~/sites/grandchallenge/src/darpa-gc-forum-scraper
time python darpascraper.py -n 100 $URL $PKL $RSS

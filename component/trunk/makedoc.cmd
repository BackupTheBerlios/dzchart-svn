@echo off
rem pasdoc.exe is from pasdoc.sourceforge.net

mkdir docs
utils\pasdoc --marker=: --output=docs *.pas

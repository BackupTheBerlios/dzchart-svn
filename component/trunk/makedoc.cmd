@echo off
rem pasdoc.exe is from pasdoc.sourceforge.net

mkdir docs
utils\pasdoc --graphviz-uses --graphviz-classes --marker=: --output=docs *.pas


@echo off
rem pasdoc.exe is from pasdoc.sourceforge.net

mkdir docs
cd source
..\utils\pasdoc --marker=: --output=..\docs --graphviz-uses --link-gv-uses=jpg --graphviz-classes --link-gv-classes=jpg *.pas
dot -Grankdir=LR -T jpg ..\docs\GVUses.dot > ..\docs\GVUses.jpg
dot -Grankdir=LR -T jpg ..\docs\GVClasses.dot > ..\docs\GVClasses.jpg
cd ..

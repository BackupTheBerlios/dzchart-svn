Pre-Build events:
..\buildtools\prepbuild.exe --incbuild --readini=$(PROJECTPATH) --exec=..\buildtools\prep.cmd $(SAVE)

(optional) Post-Build events:
..\buildtools\makejcldbg -e $(OUTPUTDIR)\$(OUTPUTNAME).map

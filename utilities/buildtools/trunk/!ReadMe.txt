Pre-Build events:
..\buildtools\prepbuild.exe --incbuild --readini=$(PROJECTPATH) --exec=..\buildtools\prep.cmd

Post-Build events:
..\buildtools\makejcldbg -e $(OUTPUTDIR)\$(OUTPUTNAME).map

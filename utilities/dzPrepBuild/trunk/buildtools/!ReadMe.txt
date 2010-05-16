Pre-Build events:
..\buildtools\prebuild.cmd $(PROJECTPATH)

(optional) Post-Build events:
..\buildtools\makejcldbg -e $(OUTPUTDIR)\$(OUTPUTNAME).map

@rem This batch will
@rem * call dxgettext to extract all strings to translate
@rem * call msgremove remove any strings stored in the ignore.po file

set BASE=.

@rem extract from subdirectories src and forms
dxgettext --delphi -r -b %BASE%\src -b %BASE%\forms -o %BASE%

@rem remove strings given in ignore.po
msgremove %BASE%\default.po -i %BASE%\ignore.po -o %BASE%\filtered.po

@rem merge German translations
msgmerge --no-wrap --update %BASE%\translations\de\dzCmdLineParser.po %BASE%\filtered.po

@rem merge Eerman translations
msgmerge --no-wrap --update %BASE%\translations\en\dzCmdLineParser.po %BASE%\filtered.po

pause

@rem * compile translations
@rem * bind them to the executable
@rem copy this file to the projects root directory if
@rem you want it automatically called by postbuild.cmd

echo * %0 running in
cd

if exist locale goto locexists
echo subdirectory locale does not exist, no translation added
goto :eof

:locexists

if not exist libs\dzlib\translations\de\dzlib.po goto nodzlib
@echo compile German dzlib.po
%~dp0\msgfmt libs\dzlib\translations\de\dzlib.po -o locale\de\lc_messages\dzlib.mo
:nodzlib

if not exist libs\dxgettext\translations\de\delphi2007.po goto nodelphi
@echo compile German delphi2007.po
%~dp0\msgfmt libs\dxgettext\translations\de\delphi2007.po -o locale\de\lc_messages\delphi2007.mo
:nodelphi
@echo compile German default.po
%~dp0\msgfmt locale\de\lc_messages\default.po -o locale\de\lc_messages\default.mo

@echo bind all translations to the executable
%~dp0\assemble --dxgettext %1.exe
echo off

echo * %0 exiting

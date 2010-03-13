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


@echo compile German default.po
%~dp0\msgfmt locale\de\lc_messages\default.po -o locale\de\lc_messages\default.mo

@echo bind all translations to the executable
%~dp0\assemble --dxgettext %1.exe
echo off

echo * %0 exiting

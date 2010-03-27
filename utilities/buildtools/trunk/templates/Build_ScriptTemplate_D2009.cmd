@rem builds a project using msbuild
@rem Copy this to the project's root directory and
@rem change the projectname below
call "%ProgramFiles%\CodeGear\RAD Studio\6.0\bin\rsvars.bat"
pushd src
msbuild <projectname>.dproj
pause
popd

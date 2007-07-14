@echo off
echo %dzDate%
echo %dzTime%
echo %dzDateTime%
echo %dzMyDocuments%

echo NajorVer=%dzVersion.MajorVer%
echo MinorVer=%dzVersion.MinorVer%
echo Revision=%dzVersion.Release%
echo Build=%dzVersion.Build%
echo FileDesc=%dzVersion.FileDesc%
echo InternalName=%dzVersion.InternalName%
echo OriginalName=%dzVersion.OriginalName%
echo Product=%dzVersion.Product%
echo ProductVersion=%dzVersion.ProductVersion%
echo Company=%dzVersion.Company%
echo Copyright=%dzVersion.Copyright%
echo Trademark=%dzVersion.Trademark%
echo Comments=%dzVersion.Comments%

@echo on
PrepBuild --NajorVer=%dzVersion.MajorVer% --MinorVer=%dzVersion.MinorVer% --Revision=%dzVersion.Release% --Build=%dzVersion.Build% --FileDesc="%dzVersion.FileDesc%" --InternalName="%dzVersion.InternalName%" --OriginalName="%dzVersion.OriginalName%" --Product="%dzVersion.Product%" --ProductVersion="%dzVersion.ProductVersion%" --Company="%dzVersion.Company%" --Copyright="%dzVersion.Copyright%" --Trademark="%dzVersion.Trademark%" --Comments="%dzVersion.Comments%"


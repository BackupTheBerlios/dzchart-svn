@echo off
echo dzDate=%dzDate%
echo dzTime=%dzTime%
echo dzDateTime=%dzDateTime%
echo dzMyDocuments=%dzMyDocuments%

echo dzProject=%dzProject%

echo NajorVer=%dzVersion.MajorVer%
echo MinorVer=%dzVersion.MinorVer%
echo Release=%dzVersion.Release%
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
PrepBuild --writerc=%dzProject%_Version.rc --updateini=%dzProject% --MajorVer=%dzVersion.MajorVer% --MinorVer=%dzVersion.MinorVer% --Release=%dzVersion.Release% --Build=%dzVersion.Build% --FileDesc="%dzVersion.FileDesc%" --InternalName="%dzVersion.InternalName%" --OriginalName="%dzVersion.OriginalName%" --Product="%dzVersion.Product%" --ProductVersion="%dzDate%" --Company="%dzVersion.Company%" --Copyright="%dzVersion.Copyright%" --Trademark="%dzVersion.Trademark%" --Comments="%dzVersion.Comments%"
brcc32 %dzProject%_Version.rc

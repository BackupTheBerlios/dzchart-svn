Library JclThreadNameExpertDLL;
{
-----------------------------------------------------------------------------
     DO NOT EDIT THIS FILE, IT IS GENERATED BY THE PACKAGE GENERATOR
            ALWAYS EDIT THE RELATED XML FILE (JclThreadNameExpertDLL-L.xml)

     Last generated: 27-02-2006  20:07:09 UTC
-----------------------------------------------------------------------------
}

{$R *.res}
{$ALIGN 8}
{$ASSERTIONS ON}
{$BOOLEVAL OFF}
{$DEBUGINFO OFF}
{$EXTENDEDSYNTAX ON}
{$IMPORTEDDATA ON}
{$IOCHECKS ON}
{$LOCALSYMBOLS ON}
{$LONGSTRINGS ON}
{$OPENSTRINGS ON}
{$OPTIMIZATION ON}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}
{$REFERENCEINFO ON}
{$SAFEDIVIDE OFF}
{$STACKFRAMES OFF}
{$TYPEDADDRESS OFF}
{$VARSTRINGCHECKS ON}
{$WRITEABLECONST OFF}
{$MINENUMSIZE 1}
{$IMAGEBASE $580A0000}
{$DESCRIPTION 'JCL Thread Name IDE expert'}
{$LIBSUFFIX 'D60'}
{$IMPLICITBUILD OFF}

uses
  ToolsAPI,
  ThreadExpertSharedNames in '..\..\experts\debug\threadnames\ThreadExpertSharedNames.pas' ,
  ThreadExpertUnit in '..\..\experts\debug\threadnames\ThreadExpertUnit.pas' 
  ;

exports
  JCLWizardInit name WizardEntryPoint;

end.

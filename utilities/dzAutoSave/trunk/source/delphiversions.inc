// global defines und options
// to be included in all source files

// to make it easier to find out which version of Delphi we are using
{$IFDEF VER130}
{$DEFINE Delphi5up}
{$ENDIF}

{$IFDEF VER140}
{$DEFINE Delphi5up}
{$DEFINE Delphi6up}
{$ENDIF}

{$IFDEF VER150}
{$DEFINE Delphi5up}
{$DEFINE Delphi6up}
{$DEFINE Delphi7up}
{$ENDIF}

{$IFDEF VER170}
{$DEFINE Delphi5up}
{$DEFINE Delphi6up}
{$DEFINE Delphi7up}
{$DEFINE Delphi2005up}
{$ENDIF}


{$IFDEF VER180}
{$DEFINE Delphi5up}
{$DEFINE Delphi6up}
{$DEFINE Delphi7up}
{$DEFINE Delphi2005up}
{$DEFINE Delphi2006up}
{$ENDIF}

{$IFDEF Delphi7up}
{$WARN unsafe_type off}
{$WARN unsafe_code off}
{$WARN unsafe_cast off}
{$ENDIF}


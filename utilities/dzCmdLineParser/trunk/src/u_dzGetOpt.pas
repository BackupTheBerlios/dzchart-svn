unit u_dzGetOpt;

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzParamDescList,
  u_dzParamFoundList,
  u_dzOptionDescList,
  u_dzOptionNameList,
  u_dzOptionFoundList;

type
  EGetOpt = class(Exception);
  EUnknownOption = class(EGetOpt);
  ETooManyParams = class(EGetOpt);
  ETooFewParams = class(EGetOpt);
  EOptionValue = class(EGetOpt);

type
  TGetOpt = class
  private
    FProgName: string;
    FOptionDescList: TOptionDescList;
    FOptionNameList: TOptionNameList;
    FParamDescList: TParamDescList;
    FParamsFoundList: TParamFoundList;
    FOptionsFoundList: TOptionFoundList;
    procedure EvaluateCmdLine(_Options, _Params: TStrings);
  public
    ///<summary> Creates a TdzGetOpt instance. </summary>
    ///          @param ProgName is an optional parameter, if not given, the value of
    ///                          ParamStr(0) is used. }
    constructor Create(const _ProgName: string = '');
    ///<summary> standard destructor </summary>
    destructor Destroy; override;
    ///<summary> registers a commandline option
    ///          @param Names is an array of string containing the names of the option
    ///                       (usually one long, descriptive name and one short, one
    ///                       character name)
    ///                       Note: One character option names are case sensitive,
    ///                       that is -o and -O are not the same, but longer option
    ///                       names are not, that is --help and --HELP are the same.
    ///          @param Description is a string describing the option for the usage string
    ///          @param HasValue is a boolean which tells the parser whether to expect
    ///                          a parameter for this option. </summary>
    procedure RegisterOption(const _Names: array of string; const _Description: string; _HasValue: boolean = false); overload;
    procedure RegisterOption(const _Name, _Description: string; _HasValue: boolean = false); overload;
    ///<summary> registers the standard options --help, -?, -h and -H for displaying help </summary>
    procedure RegisterHelpOptions;
    ///<summary> registers a commandline parameter
    ///          @param Name is the name of the parameter as displayed in the usage string.
    ///          @param Description is the description of the parameter for the usage.
    ///          @param MinCount is the minimum number of parameters to expect
    ///          @param MaxCount is the maximum number of parameters to expect
    ///          Note: only the last parameter should really have MinCount and MaxCount <> 1 </summary>
    procedure RegisterParam(const _Name, _Description: string; _MinCount: integer = 1; _MaxCount: integer = 1);
    ///<summary> parses the commandline
    ///          This method uses the information given to it via the RegisterOption and
    ///          RegisterParameter methods to parse the commandline. After it has been
    ///          called the methods OptionPassed and ParamPassed can be used to retrieve
    ///          the actual parameters.
    ///          @param CmdLine is a string giving the commandline.
    ///                         NOTE: Do not pass System.CmdLine since it contains the
    ///                         program's name as the first "parameter".
    ///                         If you want to parse the commandline as passed by
    ///                         windows, call the overloaded Parse method without
    ///                         parameters. It handles this. </summary>
    procedure Parse(const _CmdLine: string); overload;
    ///<summary> parses the commandline
    ///          This method uses the information given to it via the RegisterOption and
    ///          RegisterParameter methods to parse the commandline. After it has been
    ///          called the methods OptionPassed and ParamPassed can be used to retrieve
    ///          the actual parameters. </summary>
    procedure Parse; overload;
    ///<summary> checks whether the option given by the short or long name was passed on the commandline,
    ///          The order of the (same) options on the commandline is preserved,
    ///          eg: '-a abc -b -a def' will return ('abc', 'def') for OptionFound('a').
    ///          @param Name is one of the names of the option
    ///          @param Values is a TStrings class which will return the option's values, if any.
    ///                        It can be nil if the values are of no interest.
    ///          @returns the number of matching options found on the commandline, the
    ///                   value -1 has a special meaning: The option name is unknown
    ///                   to the parser. </summary>
    function OptionPassed(const _Name: string; _Values: TStrings): integer; overload;
    function OptionPassed(const _Name: string; var _Value: string): boolean; overload;
    function OptionPassed(const _Name: string): boolean; overload;
    ///<summary> returns true if any of the help options (--help, -?, -h, -H) was passed on the commandline. </summary>
    function HelpOptionFound: boolean;
    ///<summary> checks whether the parameter given by the name was passed on the commandline,
    ///          note that parameters will be filled left to right, eg. if you register
    ///          a parameter that can be given 5 times and one that can be given 2
    ///          times and then pass 4 parameters, you will get 4 values for the
    ///          first parameter and none fo the second. </summary>
    function ParamPassed(const _ParamName: string; _Values: TStrings): integer; overload;
    function ParamPassed(const _ParamName: string; var _Value: string): boolean; overload;
    ///<summary> returns a commandline with all the parameters and option, e.g. [--option1=value1] <param1> [<param2>] </summary>
    function GetCmdLineDesc: string;
    ///<summary> returns a description for all the options and parameters </summary>
    function GetOptionHelp(_Indent: integer = 20): string;
    function GetParamHelp(_Indent: integer = 20): string;
    ///<summary> the program's name as either passed to the constructor or read from ParamStr(0). </summary>
    property ProgName: string read FProgName;
    ///<summary> List of all parameters found </summary>
    property ParamsFoundList: TParamFoundList read FParamsFoundList;
    ///<summary> List of all Options found </summary>
    property OptionsFoundList: TOptionFoundList read FOptionsFoundList;
  end;

implementation

uses
  u_dzCmdLineParser;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzCmdLineParser');
end;

{ TdzGetOpt }

constructor TGetOpt.Create(const _ProgName: string = '');
begin
  inherited Create;
  if _ProgName = '' then
    FProgName := ChangeFileExt(ExtractFilename(ParamStr(0)), '')
  else
    FProgName := _ProgName;
  FOptionDescList := TOptionDescList.Create;
  FOptionNameList := TOptionNameList.Create;
  FOptionsFoundList := TOptionFoundList.Create;
  FOptionsFoundList.Duplicates := dupAccept;
  FParamDescList := TParamDescList.Create;
  FParamsFoundList := TParamFoundList.Create;
end;

destructor TGetOpt.Destroy;
begin
  FOptionDescList.Free;
  FOptionNameList.Free;
  FOptionsFoundList.Free;
  FParamDescList.Free;
  FParamsFoundList.Free;
  inherited;
end;

function TGetOpt.GetCmdLineDesc: string;
var
  i: integer;
  ParamDesc: TParamDesc;
  s: string;
begin
  if FOptionDescList.Count <> 0 then
    Result := _('[options]')
  else
    Result := '';
  for i := 0 to FParamDescList.Count - 1 do begin
    ParamDesc := FParamDescList[i];
    s := ParamDesc.GetCmdMask;
    if Result <> '' then
      Result := Result + ' ' + s
    else
      Result := s;
  end;
end;

function TGetOpt.GetOptionHelp(_Indent: integer): string;
var
  i: integer;
  OptionDesc: TOptionDesc;
begin
  Result := '';
  for i := 0 to FOptionDescList.Count - 1 do begin
    OptionDesc := FOptionDescList[i];
    if Result <> '' then
      Result := Result + #13#10;
    Result := Result + OptionDesc.GetDescription(_Indent);
  end;
end;

function TGetOpt.GetParamHelp(_Indent: integer): string;
var
  i: integer;
  ParamDesc: TParamDesc;
begin
  for i := 0 to FParamDescList.Count - 1 do begin
    ParamDesc := FParamDescList[i];
    if Result <> '' then
      Result := Result + #13#10;
    Result := Result + ParamDesc.GetDescription(_Indent);
  end;
end;

function TGetOpt.HelpOptionFound: boolean;
begin
  Result := OptionPassed('help', nil) <> 0;
end;

function TGetOpt.OptionPassed(const _Name: string): boolean;
var
  s: string;
begin
  Result := OptionPassed(_Name, s);
end;

function TGetOpt.OptionPassed(const _Name: string; _Values: TStrings): integer;
var
  Idx: integer;
  Option: TOptionFound;
  PrimaryName: string;
begin
  if not FOptionNameList.Find(PChar(_Name), Idx) then
    raise EUnknownOption.CreateFmt(_('%s is not a registered option.'), [_Name]);

  Result := 0;
  PrimaryName := FOptionNameList[Idx].GetPrimaryName;
  if FOptionsFoundList.Find(PrimaryName, Idx) then begin
    Option := FOptionsFoundList[Idx];
    while Assigned(Option) and (Option.GetPrimaryName = PrimaryName) do begin
      Inc(Result);
      if Assigned(_Values) then
        _Values.Add(Option.Value);
      Inc(Idx);
      if Idx >= FOptionsFoundList.Count then
        break;
      Option := FOptionsFoundList[Idx];
    end;
  end;
end;

function TGetOpt.OptionPassed(const _Name: string; var _Value: string): boolean;
var
  st: TStringList;
begin
  st := TStringList.Create;
  try
    Result := OptionPassed(_Name, st) = 1;
    if Result then
      _Value := st[0];
  finally
    st.Free;
  end;
end;

function TGetOpt.ParamPassed(const _ParamName: string; _Values: TStrings): integer;
var
  i: integer;
  ParamFound: TParamFound;
begin
  { TODO -otwm : This should raise an exception if ParamName is not one of the registered parameters }
  Result := 0;
  for i := 0 to FParamsFoundList.Count - 1 do begin
    ParamFound := FParamsFoundList[i];
    if SameText(ParamFound.ParamDesc.Name, _ParamName) then begin
      Inc(Result);
      if Assigned(_Values) then
        _Values.Add(ParamFound.Value);
    end;
  end;
end;

function TGetOpt.ParamPassed(const _ParamName: string; var _Value: string): boolean;
var
  st: TStringList;
begin
  st := TStringList.Create;
  try
    Result := ParamPassed(_ParamName, st) = 1;
    if Result then
      _Value := st[0];
  finally
    st.Free;
  end;
end;

procedure TGetOpt.EvaluateCmdLine(_Options, _Params: TStrings);
var
  DescIdx: integer;
  FoundIdx: integer;
  DesCount: Integer;
  Cnt: integer;
  pd: TParamDesc;
  OptDesc: TOptionDesc;
  OptName: TOptionName;
  s: string;
begin
  FoundIdx := 0;
  DescIdx := 0;
  while FoundIdx < _Params.Count do begin
    DesCount := FParamDescList.Count;
    if (DescIdx >= DesCount) then
      raise ETooManyParams.Create(_('There are too many parameters.'));
    pd := FParamDescList[DescIdx];
    Cnt := 0;
    while ((Cnt < pd.MaxCount) or (pd.MaxCount = -1)) and (FoundIdx < _Params.Count) do begin
      s := _Params[FoundIdx];
      FParamsFoundList.Add(TParamFound.Create(pd, s));
      Inc(Cnt);
      Inc(FoundIdx);
    end;
    if Cnt < pd.MinCount then
      raise ETooFewParams.CreateFmt(_('There are not enough %s parameters (min: %d)'), [pd.Name, pd.MinCount]);
    Inc(DescIdx);
  end;
  while DescIdx < FParamDescList.Count do begin
    pd := FParamDescList[DescIdx];
    if pd.MinCount > 0 then
      raise ETooFewParams.CreateFmt(_('There are not enough %s parameters (min: %d)'), [pd.Name, pd.MinCount]);
    Inc(DescIdx);
  end;

  FoundIdx := 0;
  while FoundIdx < _Options.Count do begin
    if not FOptionNameList.Find(_Options.Names[FoundIdx], OptName) then
      raise EUnknownOption.CreateFmt(_('Option %s is unknown'), [_Options.Names[Foundidx]]);
    s := _Options.ValueFromIndex[FoundIdx];
    OptDesc := OptName.OptionDesc;
    if OptDesc.HasValue and (s = '') then
      raise EOptionValue.CreateFmt(_('Option %s requires a value.'), [OptName.Name]);
    FOptionsFoundList.Add(TOptionFound.Create(OptDesc, OptName.Name, s));
    Inc(FoundIdx);
  end;
end;

procedure TGetOpt.Parse;
var
  Options: TStringList;
  Params: TStringList;
begin
  Options := nil;
  Params := TStringList.Create;
  try
    Options := TStringList.Create;
    TCmdLineParser.Execute(System.CmdLine, Options, Params);
    // delete the first "parameter", it is the executable name
    Params.Delete(0);
    EvaluateCmdLine(Options, Params);
  finally
    Options.Free;
    Params.Free;
  end;
end;

procedure TGetOpt.Parse(const _CmdLine: string);
var
  Options: TStringList;
  Params: TStringList;
begin
  Params := nil;
  Options := TStringList.Create;
  try
    Params := TStringList.Create;
    TCmdLineParser.Execute(_CmdLine, Options, Params);
    EvaluateCmdLine(Options, Params);
  finally
    Params.Free;
    Options.Free;
  end;
end;

procedure TGetOpt.RegisterHelpOptions;
begin
  RegisterOption(['help', '?', 'h', 'H'], _('display parameter help'), false);
end;

procedure TGetOpt.RegisterOption(const _Name, _Description: string; _HasValue: boolean);
begin
  RegisterOption([_Name], _Description, _HasValue);
end;

procedure TGetOpt.RegisterOption(const _Names: array of string;
  const _Description: string; _HasValue: boolean);
var
  i: integer;
  Desc: TOptionDesc;
begin
  Desc := TOptionDesc.Create(_Names, _Description, _HasValue);
  FOptionDescList.Add(Desc);
  for i := 0 to High(_Names) do
    FOptionNameList.Add(TOptionName.Create(_Names[i], Desc));
end;

procedure TGetOpt.RegisterParam(const _Name, _Description: string;
  _MinCount, _MaxCount: integer);
begin
  FParamDescList.Add(TParamDesc.Create(_Name, _Description, _MinCount, _MaxCount));
end;

end.


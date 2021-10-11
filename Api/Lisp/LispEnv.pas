unit LispEnv;
interface //////////////////////////////////////////////////////////////////////

uses
  DynTypes;

procedure RegisterFunctions(Scope: IDynScope; Fn: array of TLispProcRec);
procedure RegisterSyntax(Scope: IDynScope; Fn: array of TLispSyntaxRec);

procedure ReadDatumFromFile(out Result: TDatumRef; Path: String);
procedure EvalDatumFromFile(out Result: TDatumRef; Path: String; Scope: IDynScope);

procedure ReadA(out Result: TDatumRef; Path: PChar);

procedure WriteDatumToFile(Datum: TDynDatum; Path: String);

function ImportProcedures(hDll: THandle; Symbols: TDynDatum; const Scope: IDelphiScope): IDynPair;
function LoadDynLib(const Name: String; Scope: IDelphiScope; Symbols: TDynDatum = nil): Boolean;
function LoadSrcLib(const Name: String; Scope: IDelphiScope): Boolean;
procedure UnloadLibraries;
function LoadedLibraries: IDynArray;

implementation /////////////////////////////////////////////////////////////////

uses
  {$IFNDEF LINUX} Windows, {$ENDIF}
  SysUtils, Classes, LispParserA, DTProc;

type
  TLibraryInfo = class
    hDll: THandle;
    Funcs: IDynPair;
    constructor Create(AhDll: THandle; AFuncs: IDynPair);
  end;

var
  FLoadedLibraries: TStringList;

procedure AddLoadedLibrary(const FileName: String; hDll: THandle; Imp: IDynPair);
var
  LibI: TLibraryInfo;
begin
  if not Assigned(FLoadedLibraries) then
    FLoadedLibraries := TStringList.Create;
  LibI := TLibraryInfo.Create(hDll, Imp);
  FLoadedLibraries.AddObject(FileName, LibI);
end;

function LoadedLibraries: IDynArray;
var
  List: TStringList;
  i, n: Integer;
  s, Name, Path: String;
  Entry: IDynPair;
  LibI: TLibraryInfo;
begin
  List := FLoadedLibraries;
  n := List.Count;
  Result := make_vector(n);
  for i := 0 to n - 1 do
  begin
    s := List.Strings[i];
    Name := ExtractFileName(s);
    Path := ExtractFilePath(s);
    LibI := TLibraryInfo(List.Objects[i]);
    if Assigned(LibI.Funcs) then
    begin
      Entry := make_list([Name, Path, LibI.Funcs]);
    end
    else
      Entry := make_list([Name, Path]);
    Result.Item[i] := Pointer(Entry);
  end;
end;

procedure UnloadLibraries;
var
  i: Integer;
  hDll: THandle;
  Lib: TStringList;
  LibI: TLibraryInfo;
begin
  Lib := FLoadedLibraries;
  FLoadedLibraries := nil;
  if not Assigned(Lib) then Exit;
  FLoadedLibraries := nil;
  for i := Lib.Count - 1 downto 0 do
  begin
    LibI := TLibraryInfo(Lib.Objects[i]);
    hDll := LibI.hDll;
    {$IFNDEF LINUX}
    FreeLibrary(hDll);
    {$ENDIF}
    LibI.Free;
  end;
  Lib.Free;
end;

function OpenForRead(var F: File; const FileName: String): Boolean;
var
  LastFileMode: Integer;
begin
  Result := FileExists(FileName);
  if not Result then exit;
  LastFileMode := FileMode;
  try
    FileMode := fmOpenRead;
    Assign(F, FileName);
    Reset(F, 1);
  except
    Result := false
  end;
  FileMode := LastFileMode;
end;

function  LoadFile(const FileName:String): RawByteString;
var
  FSrc: File;
begin
  if OpenForRead(FSrc, FileName) then
  begin
    SetLength(Result, FileSize(FSrc));
    BlockRead(FSrc, Pointer(Result)^, Length(Result));
    Close(FSrc);
  end
  else
  begin
    Result:= ''
  end
end;

procedure ReadDatumFromFile(out Result: TDatumRef; Path: String);
var
  data: Utf8String;
  Parser: TLispParser;
begin
  if ExtractFileDrive(Path) = '' then
    Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Path;

  data := LoadFile(Path);

  Parser := TLispParser.Create;
  Parser.Evaluate(Result, data);
  Parser.Free;
end;

procedure ReadA(out Result: TDatumRef; Path: PChar);
begin
  ReadDatumFromFile(Result, Path);
end;

procedure SaveFile(const FileName: String; pData: Pointer; cbData: Cardinal);
var
  FDes : File;
  AmtTransferred : integer;
begin
  Assign(FDes, FileName); ReWrite(FDes, 1);
  BlockWrite(FDes, pData^, cbData, AmtTransferred);
//If (cbData <> AmtTransferred) then
  Close(FDes);
end;

procedure WriteDatumToFile(Datum: TDynDatum; Path: String);
var
  St : string;
begin
  St := Datum.WriteStr(MaxInt);
  If ExtractFileDrive(Path) = '' then
    Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Path;
  SaveFile(Path, @St[1], Length(St));
end;

procedure EvalDatumFromFile(out Result: TDatumRef; Path: String; Scope: IDynScope);
var
  Ref: TDatumRef;
begin
  // (load filename) -> evalua el contenido de
  ReadDatumFromFile(Ref, Path);
  Result := Eval(Ref.Value, Scope);
end;

procedure RegisterFunctionG(Scope: IDynScope; const NameStr: Utf8String;
  const Method: TDynFuncG);
var
  Datum: IDynFunc;
  Name: TDynDatum;
begin
  Name := InitSymbol(PAnsiChar(NameStr), Length(NameStr));
  Datum := TNamedDynFuncG.Create(Name, Method).AsIDynFunc;
  Scope.Value[Pointer(Name)] := Pointer(Datum);
end;

function ImportProcedures(hDll: THandle; Symbols: TDynDatum; const Scope: IDelphiScope): IDynPair;
var
  Entry: TDynDatum;
  ArgName, ArgParams, ArgRes, ArgOpt, ArgExpName: TDynDatum;
  ExpName: String;
  FnDatum: IDynFunc;
  Method: TDynFuncG;
  ResEntry: IDynDatum;
begin
  Result := nil;
  while Assigned(Symbols) do
  begin
    Entry := car(Symbols);
    Symbols := cdr(Symbols);
    // (name ref list "exported_name")
    NeedParams(Entry, [@ArgName, @ArgRes, @ArgParams], @ArgOpt);
    NeedSymbol(ArgName);
    if Assigned(ArgOpt) then
    begin
      NeedParams(ArgOpt, [@ArgExpName]);
      NeedString(ArgExpName, ExpName);
    end
    else
      ExpName := Deb(ArgName);
    {$IFNDEF LINUX}
    @Method := GetProcAddress(hDll, PChar(ExpName));
    {$ENDIF}
    if Assigned(@Method) then
    begin
      FnDatum := TNamedDynFuncG.Create(ArgName, Method).AsIDynFunc;
      Scope.Value[Pointer(ArgName)] := Pointer(FnDatum);
      ResEntry := make_list([ArgName, FnDatum]);
      Result := cons(ResEntry, Result)
    end;
  end;
end;

function LoadDynLib(const Name: String; Scope: IDelphiScope; Symbols: TDynDatum = nil): Boolean;
var
  FileName, Ext: String;
  hDll: THandle;
  ExportSymbols: function(const Scope: IDelphiScope): Boolean stdcall;
  Imp: IDynPair;
begin
  Result := False;
  FileName := ExtractFilePath(ParamStr(0));
  FileName := IncludeTrailingPathDelimiter(FileName);
  Ext := ExtractFileExt(Name);
  if Ext = '' then
    FileName := FileName + Name + '.dls'
  else
    FileName := FileName + Name;
  try
    {$IFNDEF LINUX}
    hDll := LoadLibrary(PChar(FileName));
    {$ENDIF}
  except
    Exit;
  end;
  {$IFNDEF LINUX}
  if hDll = INVALID_HANDLE_VALUE then
    Exit;
  ExportSymbols := GetProcAddress(hDll, 'ExportSymbols');
  if Assigned(ExportSymbols) then
    Result := ExportSymbols(Scope);
  if Assigned(Symbols) then
  begin
    Imp := ImportProcedures(hDll, Symbols, Scope);
    Result := True;
  end;
  if Result then
    AddLoadedLibrary(FileName, hDll, Imp);
  {$ENDIF}
end;

function LoadSrcLib(const Name: String; Scope: IDelphiScope): Boolean;
var
  Ref: TDatumRef;
begin
  {if not Assigned(Scope) then
    Scope := GlobalScope;}
  EvalDatumFromFile(Ref, Name, Scope);
  Result := True;
end;

procedure RegisterFunctions(Scope: IDynScope; Fn: array of TLispProcRec);
var
  e: TLispProcRec;
  Symbol: TDynDatum;
  f: dyn;
begin
  for e in Fn do
  begin
    Symbol := InitSymbol(e.Name);
    f := TDynFuncNat.Create(e) as IDynFunc;
    Scope.Value[Symbol] := f;
  end;
end;

procedure RegisterSyntax(Scope: IDynScope; Fn: array of TLispSyntaxRec);
var
  e: TLispSyntaxRec;
  Symbol: TDynDatum;
  f: dyn;
begin
  for e in Fn do
  begin
    Symbol := InitSymbol(e.Name);
    f := TDynSyntaxNat.Create(e) as IDynSyntax;
    Scope.Value[Symbol] := f;
  end;
end;

{ TLibraryInfo }

constructor TLibraryInfo.Create(AhDll: THandle; AFuncs: IDynPair);
begin
  hDll := AhDll;
  Funcs := AFuncs;
end;

initialization /////////////////////////////////////////////////////////////////

finalization
  UnloadLibraries;
end. ///////////////////////////////////////////////////////////////////////////


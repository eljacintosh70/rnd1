unit LispFiles;
interface //////////////////////////////////////////////////////////////////////

uses
  Classes, SysUtils,
  DynTypes, LispEnv;

type
  {$TYPEINFO ON}

  TFileFunctions = class(TObject)
  published
    // (read filename)
    procedure read(out Result: TDatumRef; Datum: TDynDatum);
    // (begin a b c ... res)
    procedure _begin(out Result: TDatumRef; Datum: TDynDatum);
    // (save filename data)
    procedure save(out Result: TDatumRef; Datum: TDynDatum);
  end;

  TFileSintax = class(TObject)
  published
    // (load filename)
    procedure load(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
    // (load-dll filename)
    procedure _load_dll(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
  end;

function ExportSymbols(const Scope: IDelphiScope): Boolean; stdcall;

implementation /////////////////////////////////////////////////////////////////


function ExportSymbols(const Scope: IDelphiScope): Boolean; stdcall;
var
  FileFunctions: TFileFunctions;
  FileSintax: TFileSintax;
begin
  FileFunctions := TFileFunctions.Create;
  Scope.RegisterFunctions(FileFunctions);
  FileSintax := TFileSintax.Create;
  Scope.RegisterSintax(FileSintax);
  
  Result := True;
end;

{ TFileFunctions }

procedure TFileFunctions.read(out Result: TDatumRef; Datum: TDynDatum);
var
  PathDatum: TDynDatum;
  Path: string;
begin
  // (read filename) -> atom
  NeedParams(Datum, [@PathDatum]);
  NeedString(PathDatum, Path);
  ReadDatumFromFile(Result, Path);
end;

procedure TFileFunctions.save(out Result: TDatumRef; Datum: TDynDatum);
var
  PathDatum, DataDatum: TDynDatum;
  Path: string;
  Mem: IDynMemory;
  Block: TArrayBlock;
  ST: TStream;
  s: String;
  s8: Utf8string;
  p: Pointer;
  cb: Integer;
begin
  // (save filename data) -> evalua el contenido de
  NeedParams(Datum, [@PathDatum, @DataDatum]);
  NeedString(PathDatum, Path);
  if IsByteVector(DataDatum) then
  begin
    NeedByteVector(DataDatum, Mem);
    Mem.Lock(Block);
    p := Block.Ptr;
    cb := Block.Size;
  end
  else
  begin
    s := DataDatum.ToString;
    s8 := Utf8string(s);
    p := Pointer(s8);
    cb := Length(s8);
  end;
  ST := TFileStream.Create(Path, fmCreate); //fmOpenWrite);
  ST.Write(p, cb);
  ST.Free;
end;


procedure TFileFunctions._begin(out Result: TDatumRef; Datum: TDynDatum);
var
  D: TDynDatum;
begin
  if IsPair(Datum) then
  repeat
    D := cdr(Datum);
    if not IsPair(D) then
    begin
      Result := (car(Datum));
      Exit;
    end;
    Datum := D;
  until False;
  //Error;
end;

{ TFileSintax }

procedure TFileSintax.load(out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
var
  Ref: TDatumRef;
  PathDatum: TDynDatum;
  Path: string;
begin
  // (load filename) -> evalua el contenido de
  NeedParams(Datum, [@PathDatum]);
  Ref := Eval(PathDatum, Scope);
  NeedString(Ref.Value, Path);

  EvalDatumFromFile(Result, Path, Scope);
  //Deb(Result);
end;

procedure TFileSintax._load_dll(out Result: TDatumRef; Datum: TDynDatum; Scope:
    IDynScope);
var
  Ref: TDatumRef;
  DelphiScope: IDelphiScope;
  PathDatum: TDynDatum;
  Declar: TDynDatum;
  Path: string;
  Res: Boolean;
begin
  // (load-dll filename)
  NeedParams(Datum, [@PathDatum], @Declar);
  Ref := Eval(PathDatum, Scope);
  NeedString(Ref.Value, Path);

  while not Supports(Scope, IDelphiScope, DelphiScope) do
  begin
    Scope := Scope.Parent;
    if not Assigned(Scope) then
      raise Exception.Create('Invalid Scope');
  end;
  Res := LoadDynLib(Path, DelphiScope, Declar);
  Result := (MakeBool(Res));
end;

end. ///////////////////////////////////////////////////////////////////////////


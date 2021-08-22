
function CreateDynMem(cbData: Integer): IDynMemory; stdcall; overload;
var
  Helper: TDynMemory;
begin
  Helper := TDynMemory.Create(cbData);
  Result := Helper;
end;

function CreateDynMem(pData: Pointer; cbData: Integer): IDynMemory; stdcall; overload;
var
  Helper: TDynMemory;
begin
  Helper := TDynMemory.Create(pData, cbData);
  Result := Helper;
end;

function CreateDynMem(const Ref: IInterface; pData: Pointer; cbData: Integer): IDynMemory; stdcall; overload;
var
  Helper: TRefDynMemory;
begin
  Helper := TRefDynMemory.Create(IDynDatum(Ref), pData, cbData);
  Result := Helper;
end;

function CreateMapFile(const FileName: String; AlignMask: Integer; Write: Boolean =
    false; Size: Int64 = 0; const MapName: String = ''; UssingUtf8: Boolean =
    False): IDynMemory; stdcall;
var
  Map: TFileMapEx;
begin
  Map := TFileMapEsp.Create(FileName, AlignMask, Write, Size, MapName, UssingUtf8); //not FileMapReadOnly);
  Result := Map.AsIDynMemory;
end;

function ListToDynMemory(List: dyn): IDynMemory;
var
  i, n: Integer;
  b: dyn;
begin
  n := list_length(List);
  Result := CreateDynMem(n);
  for i := 0 to n - 1 do
  begin
    b := car(List);
    Result.Bytes[i] := b;
    List := cdr(List);
  end;
end;

function ListRevToDynMemory(List: dyn): IDynMemory;
var
  i, n: Integer;
  b: dyn;
begin
  n := list_length(List);
  Result := CreateDynMem(n);
  for i := n - 1 downto 0 do
  begin
    b := car(List);
    Result.Bytes[i] := b;
    List := cdr(List);
  end;
end;

function ExtendDynMem(AFirst: IDynMemory; ARef: IDynDatum; pNew: Pointer;
    cbNew: Integer): IDynMemory;
begin
  raise Exception.Create('Not implemented');
end;

exports
  CreateDynMem(cbData: Integer) name 'DynMem.Create@1', // (make-bytevector k)
                                                        // (make-bytevector k fill)
  CreateDynMem(pData: Pointer; cbData: Integer) name 'DynMem.Create@2',
  CreateDynMem(const Ref: IInterface; pData: Pointer; cbData: Integer) name 'DynMem.Create@3',
  ListToDynMemory name 'list->DynMem',
  ListRevToDynMemory name 'listR->DynMem',

  ExtendDynMem name 'DynMem.Extend',

  CreateMapFile name 'MapFile.Create';


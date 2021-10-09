//: Manejo de MapFiles
{:
 _______________________________________________________
| DEVELOPERS: Jacinto J. França                         |
|_______________________________________________________|

HISTORIA:
  28/05/2010 jf
}
unit MapFiles;
interface //////////////////////////////////////////////////////////////////////

{$define MODO2}

uses
  Types, Windows, SysUtils, Classes,
{$ifdef SUPPORT_UTF8}
  Utils, Utf8,
{$endif}
  DynTypes, DTArray;
  // DynFilters,

const
  FullBlock = MaxInt div 2;
  NotFound = -1;

{$ifdef MODO2} ////////////////////////////////////////////////////////////////

type
  TMemBlock = record
    Ptr: Pointer;
    Size: Integer;
  end;

  //: Bloque de memoria contenido en RAM
  IMemBlock = interface (IInterface)
    ['{26E5F049-23CF-4D9C-8548-226CDFFB0E22}']
    function GetSize: Integer;
    function GetPosition: Int64;
    //: obtiene acceso a un subrango como un bloque contiguo
    function SubBlock(Pos, Size: Integer): TMemBlock;
    //: buscar una secuencia de bytes a partir de Pos, y retorna si la encontró
    function FindNext(const Data: RawData; var Pos: Integer; MaxPos: Integer =
            FullBlock): Boolean;
    //: indica que se modificó el último subbloque, retorna si se pudo actualizar
    function UpdateSubBlock: Boolean;
    //: bloque de datos
    property Size: Integer read GetSize;
    //: posición del bloque en su capa
    property Position: Int64 read GetPosition;
  end;

  //: Bloque de memoria que puede tener mas de 4GB
  {:
  Se accesa a travez de SubBloques, los cuales sí se pueden manejar como memoria
  }
  IBigMemBlock = interface (IInterface)
    ['{837C19E2-90B0-4C79-A0B9-8B27C60C6A20}']
    function GetSize: Int64;
    //: sub-bloque de memoria
    function SubBlock(Pos: Int64; Size: Integer): IMemBlock;
    //: cantidad total de memoria
    property Size: Int64 read GetSize;
  end;

  IMemBlockEx = interface(IMemBlock)
    ['{BD6114F3-CCD2-45C1-9CD4-DBF821CB1A4E}']
    function SubBlockDyn(Ofs: TArrayPos; Size: TArraySize): IDynMemory;
    function SubBlockMem(Pos: Int64; Size: Integer): IMemBlock;
  end;

  TRefByteVectorHelperEx = class(TRefDynMemory, IMemBlock)
  protected
    FPosition: Int64;
    function GetSize: Integer;
    function GetPosition: Int64;
    function IMemBlock_SubBlock(Pos, Size: Integer): TMemBlock;
    function IMemBlock_FindNext(const Data: RawData; var Pos: Integer; MaxPos: Integer =
            FullBlock): Boolean;
    function IMemBlock.SubBlock = IMemBlock_SubBlock;
    function IMemBlock.FindNext = IMemBlock_FindNext;
    function UpdateSubBlock: Boolean;
  public
    constructor Create(ARef: IDynDatum; Ptr: Pointer; n: Integer; APosition: Int64);
  end;

{$else} ///////////////////////////////////////////////////////////////// MODO2

type
  IMemBlock = IDynMemory;
  IBigMemBlock = IDynMemory;
  IMemBlockEx = IDynMemory;

{$endif} //////////////////////////////////////////////////////////////// MODO2

type
  //: TFileMap: Objeto que accesa un archivo como un bloque de memoria.
  TFileMap = class (TAbstractDynMemory)//, IBigMemBlock)
  private
    FSize: TArraySize;
    FFileName: String;
    //function GetSize: Int64;
  protected
    hFile: THANDLE;
    hMapFile: THANDLE;
    Write: Boolean;
    procedure ReleaseMap;
    function MapTheFile(const AName: String; AWrite: Boolean = false; ASize:
            Int64 = 0; const MapName: String = ''; UssingUtf8: Boolean = False): Boolean; virtual;
  public
    function Length: TArraySize; override;
    constructor Create(const AName: String; AWrite: Boolean = false; ASize:
            Int64 = 0; const MapName: String = ''; UssingUtf8: Boolean = False);
    destructor Destroy; override;
    //: sub-bloque de memoria
    procedure Lock(var Block: TArrayBlock; Pos: TArrayPos = 0; Size: TLockSize =
        UpToEnd; Writeable: Boolean = False); override;
    function SubBlock(Pos: TArrayPos; Size: TArraySize): IDynMemory; override;
    property FileName: String read FFileName;
    property Size: TArraySize read Length;
  end;

  TFileMapEx = class(TFileMap, IMapHandles, IHandle
    {$ifdef MODO2}, IBigMemBlock {$endif})
  private
    FAlignMask: Integer;
    FCurBlock: IMemBlockEx;
    procedure SetCurBlock(const Value: IMemBlockEx);
  public
    constructor Create(const AName: String; AAlignMask: Integer; AWrite: Boolean = false; ASize:
            Int64 = 0; const MapName: String = ''; UssingUtf8: Boolean = False);
    function _Release: Integer; override; {$IFDEF LINUX} Cdecl {$ELSE} stdcall {$ENDIF};
    function SubBlock(Pos: TArrayPos; Size: TArraySize): IDynMemory; overload; override;
    function FindNext(const Data: RawData; var Pos: TArrayPos; MaxPos: TArrayPos =
            FullBlock): Boolean; override;
    property AlignMask: Integer read FAlignMask;
    property CurBlock: IMemBlockEx read FCurBlock write SetCurBlock;
  {$ifdef MODO2}
  protected
    function GetSize: Int64;
    function SubBlock(Pos: Int64; Size: Integer): IMemBlock; reintroduce; overload;
  {$endif}
  protected
    // IMapHandles
    function Handle: THandle;
    function MapHandle: THandle;
    function GetAsIHandle: IHandle;
    property AsIHandle: IHandle read GetAsIHandle implements IHandle;
  end;

  TFileMapEsp = class(TFileMapEx)
  protected
    function MapTheFile(const AName: String; AWrite: Boolean; ASize: Int64;
      const MapName: String; UssingUtf8: Boolean): Boolean; override;
  end;

  TMapView = class(TRefDynMemory{$ifdef MODO2}, IMemBlockEx {$endif})
  private
    FPosition: Int64;
    Map: TFileMap;
  public
    constructor Create(AMap: TFileMap; Offset: Int64; ASize: Integer);
    destructor Destroy; override;
    function FindNext(const Data: RawData; var Pos: TArrayPos; MaxPos: TArrayPos =
            FullBlock): Boolean; override;
    function UpdateSubBlock: Boolean;
  {$ifdef MODO2}
  protected
    function GetSize: Integer;
    function GetPosition: Int64;
    function SubBlockMem(Pos: Int64; Size: Integer): IMemBlock;
    function IMemBlock_SubBlock(Pos, Size: Integer): TMemBlock;
    function IMemBlock_FindNext(const Data: RawData; var Pos: Integer; MaxPos: Integer =
            FullBlock): Boolean;
    function IMemBlockEx.SubBlock = IMemBlock_SubBlock;
    function IMemBlockEx.FindNext = IMemBlock_FindNext;
    function IMemBlockEx.SubBlockDyn = SubBlock;
  public
    function SubBlock(Ofs: TArrayPos; Size: TArraySize): IDynMemory; override;
  {$endif}
  end;

function SysErrorMessage(ErrNo: Integer): string;
function LastSysErrorMessage: string;
var
  LastKnownErrorMessage: String;

implementation /////////////////////////////////////////////////////////////////

var
  FileMappings: TList;

procedure HandleError(const Message: String);
begin
  LastKnownErrorMessage := Message;
end;

procedure MappingCreated(Map: TFileMap);
begin
  if not assigned(FileMappings) then
    FileMappings := TList.Create
  else
    if FileMappings.IndexOf(Map) <> NotFound then
      exit;
  FileMappings.Add(Map)
end;

procedure MappingReleased(Map: TFileMap);
begin
  if assigned(FileMappings) then
    FileMappings.Remove(Map)
end;

function SysErrorMessage(ErrNo: Integer): string;
var
  Size: Integer;
  Buffer: PChar;
begin
  GetMem(Buffer, 4000);
  Size := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, ErrNo, 0, Buffer, 4000, nil);
  SetString(Result, Buffer, Size);
end;

function LastSysErrorMessage: string;
begin
  Result := SysErrorMessage(GetLastError)
end;

{$ifdef MODO2} ////////////////////////////////////////////////////////////////

{ TRefByteVectorHelperEx }

constructor TRefByteVectorHelperEx.Create(ARef: IDynDatum; Ptr: Pointer;
  n: Integer; APosition: Int64);
begin
  FPosition := APosition;
  inherited Create(ARef, Ptr, n);
end;

function TRefByteVectorHelperEx.GetPosition: Int64;
begin
  Result := FPosition
end;

function TRefByteVectorHelperEx.GetSize: Integer;
begin
  Result := FLength
end;

function TRefByteVectorHelperEx.IMemBlock_FindNext(const Data: RawData;
  var Pos: Integer; MaxPos: Integer): Boolean;
var
  Pos64: TArrayPos;
begin
  Pos64 := Pos;
  Result := FindNext(Data, Pos64, MaxPos);
  Pos := Pos64;
end;

function TRefByteVectorHelperEx.IMemBlock_SubBlock(Pos,
  Size: Integer): TMemBlock;
var
  MaxSize: TArraySize;
  p: PRaw;
begin
  if Pos < 0 then
    Pos := 0;
  MaxSize := FLength - Pos;
  if Size > MaxSize then
    Size := MaxSize;
  if Size > 0 then
  begin
    p := FDataPtr;
    Inc(p, Pos);
    Result.Size := Size;
    Result.Ptr := p;
  end
  else
  begin
    Result.Size := 0;
    Result.Ptr := nil;
  end
end;

function TRefByteVectorHelperEx.UpdateSubBlock: Boolean;
begin
  Result := False;
end;

{$endif} //////////////////////////////////////////////////////////////// MODO2

{ TFileMap }

constructor TFileMap.Create(const AName: String; AWrite: Boolean = false;
        ASize: Int64 = 0; const MapName: String = ''; UssingUtf8: Boolean =
        False);
begin
  Write := AWrite;
  FSize := ASize;
  FFileName := AName;
  MapTheFile(AName, AWrite, ASize, MapName, UssingUtf8);
  MappingCreated(Self);
end;

destructor TFileMap.Destroy;
begin
  ReleaseMap
end;

function TFileMap.Length: TArraySize;
begin
  Result := FSize
end;

procedure TFileMap.Lock(var Block: TArrayBlock; Pos: TArrayPos;
  Size: TLockSize; Writeable: Boolean);
var
  Mem: IDynMemory;
begin
  Mem := SubBlock(Pos, Size);
  Mem.Lock(Block, 0, Size, Writeable);
end;

function TFileMap.MapTheFile(const AName: String; AWrite: Boolean = false;
    ASize: Int64 = 0; const MapName: String = ''; UssingUtf8: Boolean = False):
    Boolean;
var
  Permision, Sharing: DWORD;
  {$ifdef SUPPORT_UTF8}
  NameW: WideString;
  {$endif}
  SizeHigh, SizeLow: Integer;
begin
  Result := False;
  if ((ASize = 0) or not Write) then
    {$ifdef SUPPORT_UTF8}
      if UssingUtf8 then
      begin
        if not Utf8.FileExists(FileName) then exit;
      end
      else
    {$endif}
      begin
        if not SysUtils.FileExists(FileName) then exit;
      end;
  Permision := GENERIC_READ;
  Sharing := FILE_SHARE_READ;
  if Write then
  begin
    Permision := GENERIC_READ or GENERIC_WRITE;
    Sharing := FILE_SHARE_READ or FILE_SHARE_WRITE;
  end;
  if AName = '' then
    hFile := INVALID_HANDLE_VALUE
  else
  begin
    {$ifdef SUPPORT_UTF8}
      if UssingUtf8 then
      begin
        NameW := Utf8ToWide(FileName);
        hFile := CreateFileW(
          Pointer(NameW),
          Permision,
          Sharing,
          nil,
          OPEN_ALWAYS,
          FILE_ATTRIBUTE_ARCHIVE,
          0);
      end
      else
    {$endif}
      begin
        hFile := CreateFile(
          PChar(FileName),
          Permision,
          Sharing,
          nil,
          OPEN_ALWAYS,
          FILE_ATTRIBUTE_ARCHIVE,
          0);
      end;
  end;
  Permision := PAGE_READONLY;
  if Write then
    Permision := PAGE_READWRITE;
  {$R-,Q-}
  SizeHigh := ASize shr 32;
  SizeLow  := ASize and Cardinal(-1);
  {$ifdef SUPPORT_UTF8}
    if UssingUtf8 then
    begin
      NameW := Utf8ToWide(MapName);
      hMapFile := CreateFileMappingW(
        hFile,     // Current file handle.
        nil,       // Default security.
        Permision, // Read/write permission.
        SizeHigh,  // Size of hFile (High)
        SizeLow,   // Size of hFile (Low)
        Pointer(NameW)); // Name of mapping object.
    end
    else
  {$endif}
    begin
      hMapFile := CreateFileMapping(
        hFile,     // Current file handle.
        nil,       // Default security.
        Permision, // Read/write permission.
        SizeHigh,  // Size of hFile (High)
        SizeLow,   // Size of hFile (Low)
        Pointer(MapName)); // Name of mapping object.
    end;
  if ASize = 0 then
  begin
    SizeLow := GetFileSize(hFile, @SizeHigh);
    FSize := Int64(SizeHigh) shl 32 + Cardinal(SizeLow);
  end;
  if hMapFile = 0 then
    HandleError('Could not create file-mapping object.'#13#10 +
      LastSysErrorMessage());

  Result := True;
end;

procedure TFileMap.ReleaseMap;
begin
  MappingReleased(Self);
  if hMapFile <> 0 then
  begin
    CloseHandle(hMapFile);
    hMapFile := 0;
  end;
  if hFile <> 0 then
  begin
    CloseHandle(hFile);
    hFile := 0;
  end;
end;

function TFileMap.SubBlock(Pos: TArrayPos; Size: TArraySize): IDynMemory;
begin
  Result := TMapView.Create(Self, Pos, Size).AsIDynMemory
end;

{ TFileMapEx }

procedure TFileMapEx.SetCurBlock(const Value: IMemBlockEx);
begin
   if Assigned(FCurBlock) then
   begin
     if not Assigned(Value) then
       // contar la referencia del miembro cuando NO ESTÉ CONECTADO
       InterlockedIncrement(FRefCount);
     FCurBlock := Value;
   end
   else
   begin
     FCurBlock := Value;
     if Assigned(Value) then
       // NO CONTAR la referencia del miembro cuando esté conectado
       InterlockedDecrement(FRefCount);
   end
end;

function TFileMapEx._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    if Assigned(FCurBlock) then
    begin
      // si el único usuario es el objeto contenido,
      // incrementar el conteo de referencias para esperarlo
      InterlockedIncrement(FRefCount);
      FCurBlock := nil;
    end
    else
      Destroy;
end;

constructor TFileMapEx.Create(const AName: String; AAlignMask: Integer;
  AWrite: Boolean; ASize: Int64; const MapName: String;
  UssingUtf8: Boolean);
begin
  FAlignMask := AAlignMask;
  inherited Create(AName, AWrite, ASize, MapName, UssingUtf8);
end;

function TFileMapEx.SubBlock(Pos: TArrayPos; Size: TArraySize): IDynMemory;
var
  HalfBlock: Integer;
  MapView: TMapView;
  LocBlock: IMemBlockEx;
  CurBlockPos: Int64;
  CurBlockSize: Integer;
begin
  LocBlock := CurBlock;
  CurBlockPos := 0;
  if Assigned(LocBlock) then
  begin
    CurBlockPos := LocBlock.Position;
    CurBlockSize := LocBlock.Size;
    if (CurBlockPos > Pos)
    or (CurBlockPos + CurBlockSize < Pos + Size) then
      LocBlock := nil
  end;
  if not Assigned(LocBlock) then
  begin
    HalfBlock := FAlignMask + 1;
    CurBlockPos := Pos and not Int64(FAlignMask);
    CurBlockSize := Size + (Pos - CurBlockPos);
    if CurBlockSize < 2 * HalfBlock then
      CurBlockSize := 2 * HalfBlock
    else
      CurBlockSize := ((CurBlockSize - 1) or FAlignMask) + 1;
    if FSize <> 0 then
      if Cardinal(CurBlockSize) > FSize - CurBlockPos then
        CurBlockSize := FSize - CurBlockPos;
    MapView := TMapView.Create(Self, CurBlockPos, CurBlockSize);
    {$ifdef MODO2}
      LocBlock := MapView;
    {$else}
      LocBlock := MapView.AsISchByteVector;
    {$endif}
    CurBlock := LocBlock;
  end;
  Result := LocBlock.SubBlockDyn(Pos - CurBlockPos, Size);
end;

function TFileMapEx.FindNext(const Data: RawData; var Pos: TArrayPos;
  MaxPos: TArrayPos): Boolean;
var
  HalfBlock: Integer;
  MapView: TMapView;
  DataSize: TArraySize;
  P: TArrayPos;
  MaxBlockPos: TArrayPos;
{$ifdef MODO2}
  BlkPos: Integer;
{$endif}
  LocBlock: IMemBlockEx;
  CurBlockPos: Int64;
  CurBlockSize: Integer;
begin
  { TODO : REVISAR!!! }
  DataSize := System.Length(Data);
  if MaxPos > FSize then
    MaxPos := FSize;
  Dec(MaxPos, DataSize);
  HalfBlock := FAlignMask + 1;
  while Pos < MaxPos do
  begin
    LocBlock := CurBlock;
    CurBlockPos := 0;
    CurBlockSize := 0;
    if Assigned(LocBlock) then
    begin
      CurBlockPos := LocBlock.Position;
      CurBlockSize := LocBlock.Size;
      if (CurBlockPos > Pos)
      or (CurBlockPos + CurBlockSize < Pos + DataSize) then
        LocBlock := nil
    end;
    if not Assigned(LocBlock) then
    begin
      CurBlockPos := Pos and not Int64(FAlignMask);
      CurBlockSize := DataSize + (Pos - CurBlockPos);
      if CurBlockSize < 2 * HalfBlock then
        CurBlockSize := 2 * HalfBlock
      else
        CurBlockSize := ((CurBlockSize - 1) or FAlignMask) + 1;
      if FSize <> 0 then
        if CurBlockSize > FSize - CurBlockPos then
          CurBlockSize := FSize - CurBlockPos;
      MapView := TMapView.Create(Self, CurBlockPos, CurBlockSize);
    {$ifdef MODO2}
      LocBlock := MapView;
    {$else}
      LocBlock := MapView.AsISchByteVector;
    {$endif}
      CurBlock := LocBlock;
    end;
    P := Pos - CurBlockPos;
    MaxBlockPos := MaxPos - CurBlockPos;
    if MaxBlockPos > CurBlockSize - DataSize then
      MaxBlockPos := CurBlockSize - DataSize;

    {$ifdef MODO2}
      BlkPos := P;
      Result := LocBlock.FindNext(Data, BlkPos, MaxBlockPos);
      P := BlkPos;
    {$else}
      Result := LocBlock.FindNext(Data, P, MaxBlockPos);
    {$endif}
    if Result then
    begin
      Pos := P + CurBlockPos;
      Exit;
    end;
    Pos := CurBlockPos + CurBlockSize - DataSize + 1;
  end;
  Result := False;
end;

{$ifdef MODO2} ////////////////////////////////////////////////////////////////

function TFileMapEx.GetSize: Int64;
begin
  Result := FSize
end;

function TFileMapEx.SubBlock(Pos: Int64; Size: Integer): IMemBlock;
var
  HalfBlock: Integer;
  MapView: TMapView;
  LocBlock: IMemBlockEx;
  CurBlockPos: Int64;
  CurBlockSize: Integer;
begin
  LocBlock := CurBlock;
  CurBlockPos := 0;
  if Assigned(LocBlock) then
  begin
    CurBlockPos := LocBlock.Position;
    CurBlockSize := LocBlock.Size;
    if (CurBlockPos > Pos)
    or (CurBlockPos + CurBlockSize < Pos + Size) then
      LocBlock := nil
  end;
  if not Assigned(LocBlock) then
  begin
    HalfBlock := FAlignMask + 1;
    CurBlockPos := Pos and not Int64(FAlignMask);
    CurBlockSize := Size + (Pos - CurBlockPos);
    if CurBlockSize < 2 * HalfBlock then
      CurBlockSize := 2 * HalfBlock
    else
      CurBlockSize := ((CurBlockSize - 1) or FAlignMask) + 1;
    if FSize <> 0 then
      if CurBlockSize > FSize - CurBlockPos then
        CurBlockSize := FSize - CurBlockPos;
    MapView := TMapView.Create(Self, CurBlockPos, CurBlockSize);
    LocBlock := MapView;
    CurBlock := LocBlock;
  end;
  Result := LocBlock.SubBlockMem(Pos - CurBlockPos, Size);
end;

{$endif} //////////////////////////////////////////////////////////////// MODO2

function TFileMapEx.GetAsIHandle: IHandle;
begin
  Result := IMapHandles(Self)
end;

function TFileMapEx.Handle: THandle;
begin
  Result := hFile
end;

function TFileMapEx.MapHandle: THandle;
begin
  Result := hMapFile
end;

{ TMapView }

constructor TMapView.Create(AMap: TFileMap; Offset: Int64; ASize: Integer);
var
  Access: Integer;
  OffsetHigh: Integer;
  OffsetLow: Integer;
  Ptr: Pointer;
  Ref: IDynMemory;
begin
  FPosition := Offset;
  Map := AMap;
  Ref := AMap.AsIDynMemory;

  if AMap.Write then
    Access := FILE_MAP_ALL_ACCESS
  else
    Access := FILE_MAP_READ;
  {$R-,Q-}
  OffsetHigh := Offset shr 32;
  OffsetLow := Offset and Cardinal(-1);
  Ptr := MapViewOfFile(
    AMap.hMapFile, // Handle to mapping object.
    Access,       // Read/write permission
    OffsetHigh,   // Offset (High)
    OffsetLow,    // Offset (Low)
    ASize);       // Block Size
  if not Assigned(Ptr) then
    HandleError(LastSysErrorMessage());
  inherited Create(Ref, Ptr, ASize);
end;

destructor TMapView.Destroy;
begin
  if assigned(FDataPtr) then
  begin
    UnmapViewOfFile(FDataPtr);
    FDataPtr := nil;
  end;
  Ref := nil;
  inherited Destroy;
end;

function TMapView.FindNext(const Data: RawData; var Pos: TArrayPos;
  MaxPos: TArrayPos): Boolean;
var
  p, pe: PRaw;
  pData: PRaw;
  cbData: Integer;
  i: Integer;
  ch0: Raw;
begin
  cbData := System.Length(Data);
  Result := False;
  if (cbData = 0) then Exit;
  if (MaxPos > FLength - cbData) then
    MaxPos := FLength - cbData;
  pData := Pointer(Data);
  p := PRaw(FDataPtr) + Pos;
  pe := PRaw(FDataPtr) + MaxPos;
  ch0 := pData^;
  while (p < pe) do
  begin
    if (p^ <> ch0) then
      Inc(p)
    else
    begin
      Inc(p);
      Result := True;
      for i := 0 to cbData - 2 do
      begin
        if pData[i + 1] <> p[i] then
        begin
          Result := False;
          Break {for i};
        end;
      end;
      if Result then
      begin
        Pos := p - 1 - PRaw(FDataPtr);
        exit;
      end;
    end
  end;
end;

function TMapView.UpdateSubBlock: Boolean;
begin
  // si es Writeable, el bloque se actualiza directamente
  Result := Map.Write
end;

{$ifdef MODO2} ////////////////////////////////////////////////////////////////

function TMapView.GetPosition: Int64;
begin
  Result := FPosition;
end;

function TMapView.GetSize: Integer;
begin
  Result := FLength
end;

function TMapView.IMemBlock_FindNext(const Data: RawData; var Pos: Integer;
  MaxPos: Integer): Boolean;
var
  Pos64: TArrayPos;
begin
  Pos64 := Pos;
  Result := FindNext(Data, Pos64, MaxPos);
  Pos := Pos64;
end;

function TMapView.IMemBlock_SubBlock(Pos, Size: Integer): TMemBlock;
var
  MaxSize: TArraySize;
  p: PRaw;
begin
  if Pos < 0 then
    Pos := 0;
  MaxSize := FLength - Pos;
  if Size > MaxSize then
    Size := MaxSize;
  if Size > 0 then
  begin
    p := FDataPtr;
    Inc(p, Pos);
    Result.Size := Size;
    Result.Ptr := p;
  end
  else
  begin
    Result.Size := 0;
    Result.Ptr := nil;
  end
end;

function TMapView.SubBlockMem(Pos: Int64; Size: Integer): IMemBlock;
var
  Block: TArrayBlock;
  Obj: TRefByteVectorHelperEx;
begin
  Lock(Block, Pos, Size, False);
  Obj := TRefByteVectorHelperEx.Create(Block.Lock, Block.Ptr, Block.Size,
    FPosition + Pos);
  Result := Obj;
end;

function TMapView.SubBlock(Ofs: TArrayPos; Size: TArraySize): IDynMemory;
var
  Block: TArrayBlock;
  Obj: TRefByteVectorHelperEx;
begin
  Lock(Block, Ofs, Size, False);
  Obj := TRefByteVectorHelperEx.Create(Block.Lock, Block.Ptr, Block.Size,
    FPosition + Ofs);
  Result := Obj.AsIDynMemory;
end;

{$endif} //////////////////////////////////////////////////////////////// MODO2

{ TFileMapEsp }

function TFileMapEsp.MapTheFile(const AName: String; AWrite: Boolean;
  ASize: Int64; const MapName: String; UssingUtf8: Boolean): Boolean;
var
  Permision, Sharing: DWORD;
  SizeHigh, SizeLow: Integer;
  //NameW: WideString;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD;
begin
  (*Result := False;
  if ((ASize = 0) or not Write) then
    {$ifdef SUPPORT_UTF8}
      if UssingUtf8 then
      begin
        if not Utf8.FileExists(FileName) then exit;
      end
      else
    {$endif}
      begin
        if not SysUtils.FileExists(FileName) then exit;
      end;            *)
  Permision := GENERIC_READ;
  Sharing := FILE_SHARE_READ or FILE_SHARE_WRITE;
  dwCreationDisposition := OPEN_EXISTING; //OPEN_ALWAYS,
  dwFlagsAndAttributes := 0;              //FILE_ATTRIBUTE_ARCHIVE,

  if Write then
  begin
    Permision := GENERIC_READ or GENERIC_WRITE;
    // Sharing := FILE_SHARE_READ or FILE_SHARE_WRITE;
    if ASize <> 0 then
      dwCreationDisposition := OPEN_ALWAYS;
  end;
  {$ifdef SUPPORT_UTF8}
    if UssingUtf8 then
    begin
      NameW := Utf8ToWide(FileName);
      hFile := CreateFileW(
        Pointer(NameW),
        Permision,
        Sharing,
        nil,
        dwCreationDisposition,
        dwFlagsAndAttributes
        0);
    end
    else
  {$endif}
    begin
      hFile := CreateFile(
        PChar(FileName),
        Permision,
        Sharing,
        nil,
        dwCreationDisposition,
        dwFlagsAndAttributes,
        0);
    end;
  if hFile = INVALID_HANDLE_VALUE then
  begin
    FSize := 0;
    Result := False;
    Exit;
  end;

  Permision := PAGE_READONLY;
  if Write then
    Permision := PAGE_READWRITE;
  {$R-,Q-}
  SizeHigh := ASize shr 32;
  SizeLow  := ASize and Cardinal(-1);
  {$ifdef SUPPORT_UTF8}
    if UssingUtf8 then
    begin
      NameW := Utf8ToWide(MapName);
      hMapFile := CreateFileMappingW(
        hFile,     // Current file handle.
        nil,       // Default security.
        Permision, // Read/write permission.
        SizeHigh,  // Size of hFile (High)
        SizeLow,   // Size of hFile (Low)
        Pointer(NameW)); // Name of mapping object.
    end
    else
  {$endif}
    begin
      hMapFile := CreateFileMapping(
        hFile,     // Current file handle.
        nil,       // Default security.
        Permision, // Read/write permission.
        SizeHigh,  // Size of hFile (High)
        SizeLow,   // Size of hFile (Low)
        Pointer(MapName)); // Name of mapping object.
    end;
  if ASize = 0 then
  begin
    SizeLow := GetFileSize(hFile, @SizeHigh);
    FSize := Int64(SizeHigh) shl 32 + Cardinal(SizeLow);
  end;
  {if hMapFile = 0 then
     ErrorHandler('Could not create file-mapping object.');
  }
  Result := True;
end;

end. ///////////////////////////////////////////////////////////////////////////


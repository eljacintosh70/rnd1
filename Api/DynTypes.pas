unit DynTypes;
interface //////////////////////////////////////////////////////////////////////

{$define IMP_DYNDATUM}

{$ifndef FPC}
  {$define LINK_DEB}
  {$if CompilerVersion >= 20}
     {$define HAS_UNICODE_STRING}
  {$ifend}
{$endif}

uses
  SysUtils;

const
  UpToEnd = MaxInt;
  FullBlock = High(Int64) div 2;
  PointerMask = not 3;
  FixNumMask  = not 3;
  StorageMask = 3;

  smInterface = 0;
  smInteger   = 1;
  //smRef       = 2;
  smInline    = 3;

type
  Real = Double;

  IDyn = interface;
  IDynEnumerator = interface;
  weak_IInterface = class;
  weak_IDyn = class;
  TDynDatum = class;

  {$include 'DynTypes_BaseTypes.inc'}

  TInterface = weak_IInterface;
  TDyn = weak_IDyn;

  FixNum = Low(NativeInt) div 4 .. High(NativeInt) div 4;

  //TInterface = class;
  //  TDynDatum = class;
      //TDynSymbol = class;
  PISymbol = ^IDynDatum;

{$ifdef HAS_UNICODE_STRING}
  RawData = RawByteString;
{$else}
  RawData = AnsiString;
  NativeInt = Integer;
{$ifend}

  Raw = AnsiChar;
  PRaw = PAnsiChar;
  PRawArray = ^TRawArray;
  TRawArray = array[0..MaxInt - 1] of AnsiChar;

  TLockSize = Integer;
  TArrayPos = Int64;
  TArraySize = Int64;

  PDatumRef = ^TDatumRef;
  IDynDatum = interface;

    IDynSymbol = interface;
    IDynInt = interface;
    IDynFloat = interface;
    IDynSeq = interface;
      IDynPair = interface;
    IDynArray = interface;
      IDynMemory = interface;
      IDynString = interface;
    IDynFunc = interface;
    IDynMethod = interface;
    IDynSyntax = interface;

  IInterpreter = interface;

  TArrayBlock = record
    Size: TLockSize;
    Ptr: Pointer;
    Lock: IDynDatum;
  end;

  {$MINENUMSIZE 1}
  TDatumType = (atInvalid, atPair, atStream, atSymbol, atString,
    atReal, atInteger, atChar, atBool, atNil, atUnbound, atUndefined,
    atVector, atByteVector,
    atSeq, atOpaque,
    atScope,                                          // IDynScope
    atSyntax,                                         // IDynSyntax
    atExtFunc, atAutoFunc, atLambda,                  // IDynFunc
    atMethod // IDynMethod: como atExtFunc, pero con un parámetro this: IDynScope
    );


  TDatumRef = dyn;

  IDynDatum = interface(IDyn)
    ['{39D784FE-CB51-4503-8751-CD6CE98D5C81}']
    function DatumType: TDatumType;
  end;

  TDynDatum = class(TDyn)
  public
    function _DatumType: TDatumType; virtual; abstract;
  {$ifdef IMP_DYNDATUM}
    function Data: Pointer; {$ifdef INLINE} inline; {$endif}
  public
    procedure Free;
    function NewRef: TDynDatum;
    function Kind: TDatumType;
    function AsInteger: Integer; {$ifdef INLINE} inline; {$endif}
    function AsString: String;
  {$endif}
  end;

  IDynInt = interface(IDynDatum)
    function ByteCount: Integer;
    function BytePtr: PByte;
  end;

  IDynFloat = interface(IDynDatum)
    function Value: Real;
  end;

  {$ifndef LISP_LIB}
  TSymbolAtom = TDynDatum;
  {$endif}

  IDynSymbol = interface(IDynDatum)
    function FoldedCase: IDynSymbol;
    function Name: Utf8String;
  end;

  IDynSeq = interface(IDynDatum)
    function Rest: IDynSeq;
    function First: TDynDatum;
    function HasData: Boolean;
  end;

  IDynPair = interface(IDynSeq)
    function Getcar: TDynDatum;
    procedure Setcar(const Value: TDynDatum);
    property car: TDynDatum read Getcar write Setcar;
    function Getcdr: TDynDatum;
    procedure Setcdr(const Value: TDynDatum);
    property cdr: TDynDatum read Getcdr write Setcdr;
  end;

  IDynArray = interface(IDynDatum)
    function Length: TArraySize;
    function GetItemA(i: Integer): dyn;
    procedure SetItemA(i: Integer; const Value: dyn);
    property Item[i: Integer]: dyn read GetItemA write SetItemA; default;
    procedure Lock(var Block: TArrayBlock; Ofs: TArrayPos = 0; Count: TLockSize =
         UpToEnd; Writeable: Boolean = False);
    function DataPtr: Pointer;
  end;

  IDynArrayL = interface(IDynArray)
    ['{EEC2DD77-381D-42BF-A9D2-115A547861AA}']
    function GetItemL(i: Integer): IDynDatum;
  end;

  IDynString = interface(IDynArray)
    function GetChars(i: Integer): Char;
    procedure SetChars(i: Integer; const Value: Char);
    property Item[i: Integer]: Char read GetChars write SetChars;
    function AsString: String;
  end;

  IDynMemory = interface(IDynArray)
    ['{A4DE9378-5DB3-4A76-96DC-F85938A9DC57}']
    function SubBlock(Ofs: TArrayPos; Size: TArraySize): IDynMemory;
    function GetBytes(i: Integer): Byte;
    procedure SetBytes(i: Integer; const Value: Byte);
    property Bytes[i: Integer]: Byte read GetBytes write SetBytes;
    function FindNext(const Data: RawData; var Pos: TArrayPos; MaxPos: TArrayPos =
            FullBlock): Boolean;
  end;

  IDynScope = interface(IDynDatum)
    ['{F253350A-BE45-4D1F-A02B-64D8C2EE7510}']
    function GetParent: IDynScope;
    function GetLocalValue(Symbol: TDynDatum): TDynDatum;        
    property Value[const Key: dyn]: dyn read GetItem write SetItem;
    property Parent: IDynScope read GetParent;
  end;

  IScriptObject = interface(IDynScope)
    ['{629FF94F-2311-43F9-A2C4-E98AF566491F}']
    function Symbols: IDynPair;
  end;

  TLispProc = procedure (out Result: TDatumRef; Datum: TDynDatum);
  TLispSyntax = procedure (out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
  TLispProcRec = record Name: Utf8String; Fn: TLispProc end;
  TLispSyntaxRec = record Name: Utf8String; Fn: TLispSyntax end;
  TFunc1R = function (x: Real): Real;
  TFunc1RRec = record Name: Utf8String; Fn: TFunc1R end;

  IDynFunc = interface(IDynDatum)
    ['{F374F0B3-A60D-40C6-A228-8A26A6B841F8}']
    procedure Call(out Result: TDatumRef; Params: TDynDatum);
  end;

  IDynMethod = interface(IDynDatum)
    ['{D4B787CD-612D-4691-BD84-3AB6C4A28327}']
    procedure Call(out Result: TDatumRef; const this: IDynScope; Params: TDynDatum);
  end;
  TDynMethodG = procedure (out Result: TDatumRef; const this: IDynScope; Params: TDynDatum) stdcall;
  TDynMethodO = procedure (out Result: TDatumRef; const this: IDynScope; Params: TDynDatum) of object;
  TDynFuncG = procedure (out Result: TDatumRef; Datum: TDynDatum) stdcall;
  TDynFuncO = procedure (out Result: TDatumRef; Datum: TDynDatum) of object;
  TDynSyntaxG = procedure (out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope);
  TDynSyntaxO = procedure (out Result: TDatumRef; Datum: TDynDatum; Scope: IDynScope) of object;

  IDynSyntax = interface(IDynDatum)
    ['{6B10D520-8F3E-4FFF-A501-01A47F64F594}']
    procedure Eval(out Result: TDatumRef; Params: TDynDatum; Scope: IDynScope);
  end;

  ILibScope = interface(IDynScope)
    ['{B5BC8C1B-4899-4D7A-A953-9950BDD1781F}']
    function Imported: IDynDatum;
    procedure Import(Lib: IDynDatum);
  end;

  IDelphiScope = interface(ILibScope)
    ['{7725310B-6A7E-4FD1-B285-DE92C3E24A9D}']
    procedure RegisterSintax(List: TObject);
    procedure RegisterFunctions(List: TObject);
    procedure Rename(const OldNames, NewNames: array of UTF8String);
  end;

  IInterpreter = interface(IDelphiScope)
    ['{32E9C489-714E-438F-8544-CFDB2905FAC1}']
    procedure Parse(out Result: TDatumRef; const Source: String);
    procedure Eval(out Result: TDatumRef; Datum: TDynDatum);
    procedure EnableRead;
    function LoadSrcLib(const Name: String): Boolean;
    function LoadDynLib(const Name: String): Boolean;
  end;

  IHandle = interface(IInterface)
    ['{246DAD13-1418-4624-BB01-551249B636C4}']
    function Handle: THandle;
  end;

  IMapHandles = interface(IHandle)
    ['{413EB97B-B929-4677-AB6E-B2451EF3730C}']
    function MapHandle: THandle;
  end;

const
  MaxLocalItems = 3;

type
  TAutoDestroyRecord = object
    Ref: IInterface;
    VMT: TClass;
    // los campos de TAutoDestroyNoRefHelper en orden...
    ItemCount: Integer;
    LocalItems: array[0..MaxLocalItems - 1] of Pointer;
   end;
type
  PDynDatum = ^TDynDatum;

type
  EWrongType = class(Exception)
    constructor Create(Datum: TDynDatum; const RequiredType: String);
  end;

type
  IGroupWriter = interface;

  IDynDatumWriter = interface(IDynDatum)
    ['{4D76CB35-259D-40C1-8180-736F380A57F0}']
    function WriteDatum(Val: TDynDatum): Boolean;
    function WriteInteger(const Num; Size: Integer; Signed: Boolean): Boolean;
    function WriteStr(const Val: IDynString; Ofs: TArrayPos = 0; MaxCount: TArraySize = High(TArraySize)): Boolean;
    function WriteMem(const Val: IDynMemory; Ofs: TArrayPos = 0; MaxCount: TArraySize = High(TArraySize)): Boolean;
    function OpenList: IGroupWriter;
    function OpenVector(Count: TArraySize): IGroupWriter;
    function OpenObject(TypeId: TDynDatum): IGroupWriter;
  end;

  IGroupWriter = interface(IDynDatumWriter)
    function Close: Boolean;
  end;

const
  akNil       = smInline or $00;
  akChar      = smInline or $10;
  akBool      = smInline or $20;
  akUndefined = smInline or $40;
  akUnbound   = smInline or $80;

  Unbound = TDynDatum(smInline + $80);

{$REGION 'IDynInt'}
function FixNumValue(Value: TDynDatum): FixNum; {$ifdef INLINE} inline; {$endif}
function Int64Value(Value: TDynDatum): Int64;

function  IsInteger(Datum: TDynDatum): Boolean; overload;
function  IsInteger(Datum: TDynDatum; var Value: Int64): Boolean; overload;
function  IsInteger(Datum: TDynDatum; var Value: Integer): Boolean; overload;
procedure NeedInteger(Datum: TDynDatum; var Value: Int64); overload;
procedure NeedInteger(Datum: TDynDatum; var Value: Integer); overload;
procedure NeedIntegerSt(Datum: TDynDatum; var Value: Integer);

procedure NeedInt64(Datum: TDynDatum; var Value: Int64);
{$ENDREGION}

{$REGION 'IDynFloat'}
function FloNumValue(Self: TDynDatum): Double;

function  IsReal(Datum: TDynDatum): Boolean; overload;
function  IsReal(Datum: TDynDatum; var Value: Real): Boolean; overload;
procedure NeedReal(Datum: TDynDatum; var Value: Real);
{$ENDREGION}

{$REGION 'Bool'}
var
  _f: dyn;
  _t: dyn;
function MakeBool(Value: Boolean): IDyn;
{$ENDREGION}

{$REGION 'null'}
const
{$ifdef FPC}
  _null: TDynDatum = nil;
{$else}
  _null = TDynDatum(nil); //3);
{$endif}
// (null? Datum) Marca el fin de una lista.
function IsNull(Datum: TDynDatum): Boolean;
{$ENDREGION}

{$REGION 'IDynSeq'}
function GetNext(var Seq: IDynSeq; var Item: TDynDatum): Boolean; overload;
{$ENDREGION}

{$REGION 'IDynPair'}
// (pair? Datum) Elemento de una lista.
function IsPair(Datum: dyn): Boolean; overload;
function IsPair(Datum: dyn; out Ref: IDynPair): Boolean; overload;

procedure NeedPair(Datum: dyn); overload;
procedure NeedPair(Datum: dyn; var Value: IDynPair); overload;
procedure NeedPairNonNil(Datum: dyn); overload;
procedure NeedPairNonNil(Datum: dyn; var Value: IDynPair); overload;

//  external dll name 'const->datum';
function cons(const Car, Cdr: dyn): IDynPair; stdcall;
function cons_(const elem: array of dyn): IDynPair; stdcall;
function list(const elem: array of dyn): IDynPair; stdcall;
function make_list(const Arr: array of const): IDynPair; stdcall;
function list_length(list: dyn): Integer; stdcall;

function car(const Value: dyn): TDynDatum;
function cdr(const Value: dyn): TDynDatum;

function Reverse(const List: IDynPair): IDynPair;

function GetNext(var Seq: IDynPair; var Item: TDynDatum): Boolean; overload;
{$ENDREGION}

{$REGION 'IDynArray'}
// vector? Arreglo de valores accesibles por índice.
function IsVector(Datum: TDynDatum): Boolean;
procedure NeedVector(Datum: TDynDatum; var Value: IDynArray); overload;

function make_vector(n: Integer; Fill: TDynDatum = nil): IDynArray; stdcall; overload;
function make_vector(const Arr: array of const): IDynArray; stdcall; overload;
function ListToDynArray(List: dyn): IDynArray; stdcall;

{$ENDREGION}

{$REGION 'IDynMemory'}
function IsByteVector(Datum: TDynDatum): Boolean;
procedure NeedByteVector(Datum: TDynDatum; var Value: IDynMemory); overload;

function CreateDynMem(cbData: Integer): IDynMemory; stdcall; overload;
function CreateDynMem(pData: Pointer; cbData: Integer): IDynMemory; stdcall; overload;
function CreateDynMem(const Ref: IInterface; pData: Pointer; cbData: Integer): IDynMemory; stdcall; overload;
function ExtendDynMem(AFirst: IDynMemory; ARef: IDynDatum; pNew: Pointer;
    cbNew: Integer): IDynMemory;
function CreateMapFile(const FileName: String; AlignMask: Integer; Write: Boolean =
    false; Size: Int64 = 0; const MapName: String = ''; UssingUtf8: Boolean =
    False): IDynMemory; stdcall;
function ListToDynMemory(List: dyn): IDynMemory; stdcall;
function ListRevToDynMemory(List: dyn): IDynMemory; stdcall;

function EqualSubBlock(Mem : IDynMemory; const Valor : RawData; Offset :
    TArrayPos = 0): Boolean;
{$ENDREGION}

{$REGION 'IDynString'}
//(string? datum)
function IsString(Datum: TDynDatum): Boolean;

function make_string(pData: PAnsiChar; cbData: Integer): IDynString; stdcall; overload;
function make_string(pData: PWideChar; cbData: Integer): IDynString; stdcall; overload;
function make_string(const s: UnicodeString): IDynString; overload;
function make_string(const s: AnsiString): IDynString; overload;

{--$ifdef HAS_UNICODE_STRING}
procedure NeedString(Datum: TDynDatum; var Value: UnicodeString); overload;
{--$endif}
procedure NeedString(Datum: TDynDatum; var Value: AnsiString); overload;
{$ENDREGION}

{$REGION 'IDynSymbol'}
// symbol? Nombre de una variable.
function IsSymbol(Datum: dyn): Boolean; overload;
function IsSymbol(Datum: dyn; out Ref: IDynSymbol): Boolean; overload;
procedure NeedSymbol(Datum: dyn); overload;
procedure NeedSymbol(Datum: dyn; out Ref: IDynSymbol); overload;

function InitSymbol(pName: PAnsiChar; {Utf8} cbName: Integer): IDynSymbol;
  stdcall; overload;
function InitSymbol(const Name: Utf8String): IDynSymbol; overload;
procedure InitSymbols(const Names: array of Utf8String; const Ref: array of
  PISymbol); stdcall;
{$ENDREGION}

{$REGION 'IDynScope'}
function CreateObject(AParent: IDynScope; const PropNames: array of TDynDatum;
      const PropValues: array of const): IDynScope;
function CreateMethod(AMethod: TDynMethodG): IDynMethod; overload;
function CreateMethod(AMethod: TDynMethodO): IDynMethod; overload;
procedure CallMember(out Result: TDatumRef; Scope: IDynScope; MemberId: TDynDatum;
  Params: TDynDatum = nil);

procedure RegisterFunctions(Scope: IDynScope; const Names: array of Utf8String;
  const Func: array of TDynFuncO);
procedure RegisterFunctionsG(Scope: IDynScope; const Names: array of Utf8String;
  const Func: array of TDynFuncG);

function TestParams(Params: TDynDatum; const Required: array of PDynDatum; Rest:
    PDynDatum = nil): Boolean;
procedure NeedParams(Params: TDynDatum; const Required: array of PDynDatum;
    Rest: PDynDatum = nil);

function CreateLambda(Params, FnDef: dyn; Scope: IDynScope): dyn;
{$ENDREGION}

{$REGION 'General'}
function ConstToDatum(const Val: TVarRec): dyn; stdcall;
function DatumType(Datum: TDynDatum): TDatumType;

function Deb(Obj: TDynDatum; Max: Integer = 256): String; overload;
function Deb(const Obj: IDynDatum; Max: Integer = 256): String; overload;

function Supports(Datum: TDynDatum; IID: TGuid; out Res): Boolean; overload;
function Supports(Datum: TDynDatum; IID: TGuid): Boolean; overload;
procedure NeedInterface(Datum: TDynDatum; IID: TGuid; out Res);
{$ENDREGION}

{$ifndef DTYPES}
const
  dll = 'DType.dll';
{$endif}

function EnsureInterface(A: weak_IDyn): IDyn;
{$include 'DynTypes_BaseFunc.inc'}

{$include 'DynTypes_Match.inc'}

procedure DynError(const MsgFormat: String; const Params: array of dyn);
function Eval(Exp: dyn; Scope: IDynScope): dyn;
function EvalItems(List: dyn; Scope: IDynScope): dyn;

implementation /////////////////////////////////////////////////////////////////

uses
{$ifdef DTYPES}
  {$IFNDEF LINUX} Windows, MapFiles, {$ENDIF}
  DTBool, DTInt, DTFloat, DTPair, DTArray, DTString, DTProc,
  DTScript, DTSymbol, DTPortW, LispWrite,
{$endif}
  DUtils;    // LispTypes, SchInterpreter, LispEnv,

var
  DebStr: string;

procedure DynError(const MsgFormat: String; const Params: array of dyn);

  {function ReturnAddress: Pointer;
  asm
          MOV     EAX,[EBP + 4]
  end;}

begin
  raise EDynError.Create(MsgFormat, Params) {$IFNDEF FPC} at ReturnAddress {$ENDIF};
end;

function InvalidType: Exception;
begin
  Result := Exception.Create('Invalid Type');
end;

{$REGION 'IDynInt'}

function FixNumValue(Value: TDynDatum): FixNum;
begin
  Result := NativeInt(Value) div 4;
end;

function IBigIntToInt64(Value: Pointer): Int64;
begin
  case IDynInt(Value).ByteCount of
    4: Result := PInteger(IDynInt(Value).BytePtr)^;
    8: Result := PInt64(IDynInt(Value).BytePtr)^;
  else raise EWrongType.Create(Value, 'int');
  end
end;

function Int64Value(Value: TDynDatum): Int64;
begin
  case Integer(Value) and StorageMask of
    smInteger:
      // hay que conservar el signo...
      begin
        Result := (Integer(Value) and PointerMask) div 4;
        Exit;
      end;
    smInterface:
      if Value._DatumType = atInteger then
        Result := IBigIntToInt64(Value)
      else
        raise EWrongType.Create(Value, 'int');
    else raise EWrongType.Create(Value, 'int');
  end;
end;

function  IsInteger(Datum: TDynDatum): Boolean;
var
  Tmp: Int64;
begin
  Result := HandleMessageWithPointer(Datum, MsgCastToInt64, @Tmp);
end;

function  IsInteger(Datum: TDynDatum; var Value: Int64): Boolean;
begin
  Result := HandleMessageWithPointer(Datum, MsgCastToInt64, @Value)
end;

function  IsInteger(Datum: TDynDatum; var Value: Integer): Boolean;
var
  Tmp: Int64;
begin
  Result := HandleMessageWithPointer(Datum, MsgCastToInt64, @Tmp);
  Value := Tmp;
end;

procedure NeedInteger(Datum: TDynDatum; var Value: Int64);
begin
  HandleMessageWithPointer_Err(Datum, MsgCastToInt64, @Value, @ErrCastToInt)
end;

procedure NeedInteger(Datum: TDynDatum; var Value: Integer);
var
  Tmp: Int64;
begin
  HandleMessageWithPointer_Err(Datum, MsgCastToInt64, @Tmp, @ErrCastToInt);
  Value := Tmp;
end;

function IntegerFromStrDatum(Datum: TDynDatum): Integer;
var
  s: String;
begin
  NeedString(Datum, s);
  Result := StrToInt(s);
end;

procedure NeedIntegerSt(Datum: TDynDatum; var Value: Integer);
var
  cb: Integer;
  Val: Integer;
  Val64: Int64;
  DatumType: TDatumType;
begin
  Val := Integer(Pointer(Datum));
  case Val and StorageMask of
    smInterface:
      begin
        DatumType := IDynDatum(Pointer(Val)).DatumType;
        case DatumType of
          atInteger:
            begin
              cb := IDynInt(Pointer(Val)).ByteCount;
              case cb of
                4: begin
                     Value := PInteger(IDynInt(Pointer(Val)).BytePtr)^;
                     Exit;
                   end;
                8: begin
                     Val64 := PInt64(IDynInt(Pointer(Val)).BytePtr)^;
                     if (Val64 >= Low(Integer)) and (Val64 <= High(Integer)) then
                     begin
                       Value := Val64;
                       Exit;
                     end;
                   end;
              end
            end;
          atString:
            begin
              Value := IntegerFromStrDatum(Datum);
              Exit;
            end;
        end;
      end;
    smInteger:
      begin
        Value := FixNumValue(Datum);
        Exit;
      end;
  end;
  raise EWrongType.Create(Datum, 'Integer');
end;

procedure NeedInt64(Datum: TDynDatum; var Value: Int64);
var
  cb: Integer;
  Val: Integer;
begin
  Val := Integer(Pointer(Datum));
  case Val and StorageMask of
    smInterface:
      if IDynDatum(Pointer(Val)).DatumType = atInteger then
      begin
        cb := IDynInt(Pointer(Val)).ByteCount;
        case cb of
          4: begin
               Value := PInteger(IDynInt(Pointer(Val)).BytePtr)^;
               Exit;
             end;
          8: begin
               Value := PInt64(IDynInt(Pointer(Val)).BytePtr)^;
               Exit;
             end;
        end
      end;
    smInteger:
      begin
        Value := (Integer(Pointer(Datum)) and PointerMask) div 4;
        Exit;
      end;
  end;
  raise EWrongType.Create(Datum, 'Int64');
end;

{$ENDREGION}

{$REGION 'IDynFloat'}

function FloNumValue(Self: TDynDatum): Double;
begin
  Result := 0;
  case Integer(Self) and StorageMask of
    smInteger:
      begin
        Result := (Integer(Self) and PointerMask) div 4;
        Exit;
      end;
    {smRef:
      case PRefDatum(Self.Data).DatumType of
        atInteger:
          Result := IBigIntToInt64(Self);
        atReal:
          Result := IDynFloat(Pointer(Self)).Value;
      end; }
    smInterface:
      case IDynDatum(Self.Data).DatumType of
        atInteger:
          Result := IBigIntToInt64(Self);
        atReal:
          Result := IDynFloat(Pointer(Self)).Value;
      end;
    else
      DynError('Format: %p', [Pointer(Self)]);
  end
end;

function  IsReal(Datum: TDynDatum): Boolean;
var
  Value: Real;
begin
  Result := HandleMessageWithPointer(Datum, MsgCastToDouble, @Value);
end;

function  IsReal(Datum: TDynDatum; var Value: Real): Boolean;
begin
  Result := HandleMessageWithPointer(Datum, MsgCastToDouble, @Value);
end;

procedure NeedReal(Datum: TDynDatum; var Value: Real);
begin
  HandleMessageWithPointer_Err(Datum, MsgCastToDouble, @Value, @ErrCastToDouble)
end;

{$ENDREGION}

{$REGION 'Char'}
function CreateCharAtom(Value: WideChar): TDynDatum;
begin
  Result := Pointer(Word(Value) shl 8 + akChar);
end;
{$ENDREGION}

{$REGION 'null'}
function IsNull(Datum: TDynDatum): Boolean;
begin
  Result := (Datum = _null)
end;
{$ENDREGION}

{$REGION 'IDynSeq'}
function GetNext(var Seq: IDynSeq; var Item: TDynDatum): Boolean;
begin
  if Assigned(Seq) then
    if Seq.HasData then
    begin
      Item := Seq.First;
      Seq := Seq.Rest;
      Result := True;
      Exit;
    end;
  Result := False;
end;
{$ENDREGION}

{$REGION 'IDynPair'}

{$ifdef DTYPES}
  {$include 'Inc\DTypes_IDynPair.inc'}
{$else}
function cons(const Car, Cdr: dyn): IDynPair; stdcall; overload;
  external dll name 'cons';
{
function cons(const Car: TDynDatum; const Cdr: TDynDatum): IDynPair; stdcall; overload;
  external dll name 'cons';
function cons(const Car: IDynDatum; const Cdr: IDynDatum): IDynPair; stdcall; overload;
  external dll name 'cons';
function cons(const Car: IDynDatum; const Cdr: TDynDatum): IDynPair; stdcall; overload;
  external dll name 'cons';
function cons(const Car: TDynDatum; const Cdr: IDynDatum): IDynPair; stdcall; overload;
  external dll name 'cons';
}
function cons_(const elem: array of dyn): IDynPair; stdcall;
  external dll name 'cons*';
function list(const elem: array of dyn): IDynPair; stdcall;
  external dll name 'list';
function make_list(const Arr: array of const): IDynPair; stdcall;
  external dll name 'make_list';

function list_length(list: dyn): Integer; stdcall;
  external dll name 'list_length';

function Reverse(const List: IDynPair): IDynPair;
  external dll name 'Reverse';
{$endif}

function IsPair(Datum: dyn): Boolean;
var
  Msg: TVarMessage;
begin
  if Datum = nil then
  begin
    Result := False;
    Exit;
  end;
  Msg.Msg := MsgIsPair;
  Msg.Res := 0;  
  Msg.VarPtr := nil;
  Datum.DispatchMsg(Msg);
  Result := (Msg.Res <> 0);
end;

function IsPair(Datum: dyn; out Ref: IDynPair): Boolean;
var
  Msg: TVarMessage;
begin
  if Datum = nil then
  begin
    Result := False;
    Exit;
  end;
  Msg.Msg := MsgIsPairR;
  Msg.Res := 0;
  Msg.VarPtr := @Ref;
  Datum.DispatchMsg(Msg);
  Result := (Msg.Res <> 0);
end;

procedure NeedPair(Datum: dyn);
begin
  if Datum = nil then
    Exit;
  if not IsPair(Datum) then
    raise EWrongType.Create(Datum, 'Pair');
end;

procedure NeedPair(Datum: dyn; var Value: IDynPair);
begin
  if Datum = nil then
  begin
    Value := nil;
    Exit;
  end;
  if not IsPair(Datum, Value) then
    raise EWrongType.Create(Datum, 'Pair');
end;

procedure NeedPairNonNil(Datum: dyn);
begin
  if Datum = nil then
    raise EWrongType.Create(Datum, 'Pair is nil');
  if not IsPair(Datum) then
    raise EWrongType.Create(Datum, 'Pair');
end;

procedure NeedPairNonNil(Datum: dyn; var Value: IDynPair);
begin
  if Datum = nil then
    raise EWrongType.Create(Datum, 'Pair is nil');
  if not IsPair(Datum, Value) then
    raise EWrongType.Create(Datum, 'Pair');
end;

function car(const Value: dyn): TDynDatum;
var
  Pair: IDynPair;
begin
  NeedPairNonNil(Value, Pair);
  Result := Pair.car;
end;

function cdr(const Value: dyn): TDynDatum;
var
  Pair: IDynPair;
begin
  NeedPairNonNil(Value, Pair);
  Result := Pair.cdr;
end;

function GetNext(var Seq: IDynPair; var Item: TDynDatum): Boolean;
var
  pNext: TDynDatum;
begin
  case Integer(Pointer(Seq)) of
    0, 3:    // nil, _null
      begin
        Seq := nil;
        Result := False;
      end;
  //if Assigned(Seq) then
    else begin
      Item := Seq.car;
      pNext := Seq.cdr;
      // lista impropia genera una excepción acá
      NeedPair(pNext);
      case Integer(Pointer(pNext)) of
        0, 3: Seq := nil;
      else
        Seq := IDynPair(Pointer(pNext));
      end;
      Result := True;
    end;
//      Exit;
  end;
//  Result := False;
end;

{$ENDREGION}

{$REGION 'IDynArray'}

{$ifdef DTYPES}
  {$include 'Inc\DTypes_IDynArray.inc'}
{$else}

function make_vector(n: Integer; Fill: TDynDatum = nil): IDynArray;
  external dll name 'make_vectorN';
function make_vector(const Arr: array of const): IDynArray; stdcall;
  external dll name 'make_vector';
function ListToDynArray(List: dyn): IDynArray; stdcall;
  external dll name 'list->array';
{$endif}

function IsVector(Datum: TDynDatum): Boolean;
begin
  Result := Datum.Kind = atVector;
end;

procedure NeedVector(Datum: TDynDatum; var Value: IDynArray); overload;
begin
  if Integer(Pointer(Datum)) and 3 = 0 then
    if IDynDatum(Pointer(Datum)).DatumType = atVector then
    begin
      Value := IDynArray(Pointer(Datum));
      Exit;
    end;
  raise EWrongType.Create(Datum, 'vector');
end;

{$ENDREGION}

{$REGION 'IDynMemory'}

{$ifdef DTYPES}
  {$include 'Inc\DTypes_IDynMemory.inc'}
{$else}
function CreateDynMem(cbData: Integer): IDynMemory; stdcall; overload;
  external dll name 'DynMem.Create@1';
function CreateDynMem(pData: Pointer; cbData: Integer): IDynMemory; stdcall; overload;
  external dll name 'DynMem.Create@2';
function CreateDynMem(const Ref: IInterface; pData: Pointer; cbData: Integer): IDynMemory; stdcall; overload;
  external dll name 'DynMem.Create@3';
function ListToDynMemory(List: dyn): IDynMemory; stdcall;
  external dll name 'list->DynMem';
function ListRevToDynMemory(List: dyn): IDynMemory; stdcall;
  external dll name 'listR->DynMem';

function ExtendDynMem(AFirst: IDynMemory; ARef: IDynDatum; pNew: Pointer;
    cbNew: Integer): IDynMemory;
  external dll name 'DynMem.Extend';
function CreateMapFile(const FileName: String; AlignMask: Integer; Write: Boolean =
    false; Size: Int64 = 0; const MapName: String = ''; UssingUtf8: Boolean =
    False): IDynMemory; stdcall;
  external dll name 'MapFile.Create';
{$endif}

function IsByteVector(Datum: TDynDatum): Boolean;
begin
  Result := Datum.Kind = atByteVector;
end;

procedure NeedByteVector(Datum: TDynDatum; var Value: IDynMemory); overload;
begin
  if Integer(Pointer(Datum)) and 3 = 0 then
    if IDynDatum(Pointer(Datum)).DatumType = atByteVector then
    begin
      Value := IDynMemory(Pointer(Datum));
      Exit;
    end;
  raise EWrongType.Create(Datum, 'vector');
end;

function EqualSubBlock(Mem : IDynMemory; const Valor : RawData; Offset : TArrayPos = 0) : boolean;
var
  n : TLockSize;
  p : PRaw;
  Block : TArrayBlock;
begin
  Result := False;
  n := Length(Valor);
  Mem.Lock(Block, Offset, n);
  If (Block.Size = n) then begin
    p := Block.Ptr;
    Result := CompareMem(p, @Valor[1], n);
  end;
end;
{$ENDREGION}

{$REGION 'IDynString'}

{$ifdef DTYPES}
{$include 'Inc\DTypes_IDynString.inc'}
{$else}

function CreateStringANR(p: PAnsiChar; cb: Integer): TDynDatum; stdcall;
  external dll name 'StrA->';
function CreateStringWNR(p: PWideChar; cb: Integer): TDynDatum; stdcall;
  external dll name 'StrW->';

function make_string(pData: PAnsiChar; cbData: Integer): IDynString; stdcall; overload;
  external dll name 'make_stringA';
function make_string(pData: PWideChar; cbData: Integer): IDynString; stdcall; overload;
  external dll name 'make_stringW';

{$endif}
function IsString(Datum: TDynDatum): Boolean;
var
  Val: Integer;
  //p: PStringAtomRec;
begin
  Result := False;
  Val := Integer(Pointer(Datum));
  case Val and StorageMask of
    smInterface:
      begin
        if not Assigned(Datum) then
          Exit;
        if IDynDatum(Val and PointerMask).DatumType <> atString then
          Exit;
        Result := True;
      end;
    {smRef:
      begin
        p := PStringAtomRec(Val and PointerMask);
        if p.DatumType <> atString then Exit;
      end; }
  end;
end;

function make_string(const s: UnicodeString): IDynString;
begin
  Result := make_string(PWideChar(s), Length(s));
end;

function make_string(const s: AnsiString): IDynString;
begin
  Result := make_string(PAnsiChar(s), Length(s));
end;

procedure NeedString(Datum: TDynDatum; var Value: UnicodeString);
begin
  HandleMessageWithPointer_Err(Datum, MsgCastToString, @Value, @ErrCastToString)
end;

procedure NeedString(Datum: TDynDatum; var Value: AnsiString);
var
  s: UnicodeString;
begin
  NeedString(Datum, s);
  Value := AnsiString(s);
end;
{$ENDREGION}

function Eval(Exp: dyn; Scope: IDynScope): dyn;
var
  Msg: TEvalMessage;
begin
  Msg.Msg := MsgEval;
  Msg.Scope := Scope;
  Exp.DispatchMsg(Msg);
  Result := Msg.Res;
end;

function EvalItems(List: dyn; Scope: IDynScope): dyn;
var
  Msg: TEvalMessage;
begin
  Msg.Msg := MsgEvalItems;
  Msg.Scope := Scope;
  List.DispatchMsg(Msg);
  Result := Msg.Res;
end;

{$REGION 'IDynSymbol'}

{$ifdef DTYPES}
{$include 'Inc\DTypes_IDynSymbol.inc'}
{$else}
function InitSymbol(pName: PAnsiChar; {Utf8} cbName: Integer): TDynDatum;
  stdcall; overload; external dll name 'Symbol.Create';
procedure InitSymbols(const Names: array of Utf8String; const Ref: array of
  PISymbol); stdcall; external dll name 'Symbol.Create*';
{$endif}

function IsSymbol(Datum: dyn): Boolean;
var
  Msg: TVarMessage;
begin
  if Datum = nil then
  begin
    Result := False;
    Exit;
  end;
  Msg.Msg := MsgIsSymbol;
  Msg.Res := 0;
  Datum.DispatchMsg(Msg);
  Result := (Msg.Res <> 0);
end;

function IsSymbol(Datum: dyn; out Ref: IDynSymbol): Boolean;
var
  Msg: TVarMessage;
begin
  if Datum = nil then
  begin
    Result := False;
    Exit;
  end;
  Msg.Msg := MsgIsSymbolR;
  Msg.Res := 0;
  Msg.VarPtr := @Ref;
  Datum.DispatchMsg(Msg);
  Result := (Msg.Res <> 0);
end;

procedure NeedSymbol(Datum: dyn);
begin
  if not IsSymbol(Datum) then
    raise EWrongType.Create(Datum, 'Symbol');
end;

procedure NeedSymbol(Datum: dyn; out Ref: IDynSymbol);
begin
  if not IsSymbol(Datum, Ref) then
    raise EWrongType.Create(Datum, 'Symbol');
end;

function InitSymbol(const Name: Utf8String): IDynSymbol;
begin
  Result := InitSymbol(Pointer(Name), Length(Name));
end;

{$ENDREGION}

{$REGION 'IDynScope'}
{$ifdef DTYPES}
{$include 'Inc\DTypes_IDynScope.inc'}
{$else}
function CreateObject(AParent: IDynScope; const PropNames: array of TDynDatum;
  const PropValues: array of const): IDynScope;
  external dll name 'CreateObject';
function CreateMethod(AMethod: TDynMethodG): IDynMethod; overload;
  external dll name 'CreateMethodG';
function CreateMethod(AMethod: TDynMethodO): IDynMethod; overload;
  external dll name 'CreateMethodO';
procedure CallMember(out Result: TDatumRef; Scope: IDynScope; MemberId: TDynDatum;
  Params: TDynDatum = nil);
  external dll name 'CallMember';

procedure RegisterFunctions(Scope: IDynScope; const Names: array of Utf8String;
  const Func: array of TDynFuncO);
  external dll name 'RegisterFunctionsO';
procedure RegisterFunctionsG(Scope: IDynScope; const Names: array of Utf8String;
  const Func: array of TDynFuncG);
  external dll name 'RegisterFunctionsG';

function CreateInterpreter: IInterpreter; stdcall;
  external dll name 'CreateInterpreter';
{$endif}


function TestParams(Params: TDynDatum; const Required: array of PDynDatum;
    Rest: PDynDatum = nil): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Required) do
  begin
    case Integer(Pointer(Params)) of
      0, 3: Exit;
    end;

    //if not Assigned(Params) then  // si hay menos parámetros que los requeridos
    //  Exit;                       // falla
    Required[i]^ := IDynPair(Pointer(Params)).Car;
    Params := IDynPair(Pointer(Params)).Cdr;
  end;
  if Assigned(Rest) then
    Rest^ := Params
  else if Params <> nil then  // si hay mas parámetros que los requeridos
    if Params <> _null then
      Exit;                   // falla
  Result := True;
end;

procedure NeedParams(Params: TDynDatum; const Required: array of PDynDatum;
    Rest: PDynDatum = nil);
var
  Fm: String;
begin
  if not TestParams(Params, Required, Rest) then
  begin
    if Assigned(Rest) then
      Fm := 'Required Parameters:(%d . 1) in %s'
    else
      Fm := 'Required Parameters:(%d) in %s';
    DynError(Fm, [Length(Required), Deb(Params)]);
  end;
end;
{$ENDREGION}

{$REGION 'General'}
{$ifdef DTYPES}
{$include 'Inc\DTypes_General.inc'}
{$else}

{$ifdef FPC}

function DebX(X: TDynDatum; Max: Integer = 256): WideString; overload;
  external dll name 'DebDatum';

function Deb(X: TDynDatum; Max: Integer = 256): String; overload;
var
  s: WideString;
begin
  s := DebX(X, Max);
  Result := s;
  Pointer(s) := nil;
end;

function Deb(const X: IDynDatum; Max: Integer = 256): String; overload;
var
  s: WideString;
begin
  s := DebX(Pointer(X), Max);
  Result := s;
  Pointer(s) := nil;
end;

{$else}// FPC

function Deb(X: TDynDatum; Max: Integer = 256): String; overload;
  external dll name 'DebDatum';
function Deb(const X: IDynDatum; Max: Integer = 256): String; overload;
  external dll name 'DebDatum';

{$endif}// FPC

{$endif}

function ConstToDatum(const Val: TVarRec): dyn; stdcall;
begin
  case Val.VType of
    vtPointer, vtObject, vtInterface:  // se asume que son TDynDatum;
      begin
        if Assigned(Val.VPointer) then
          Result := Val.VPointer
        else
          Result := _null;              // nil -> _null
      end;
    vtInteger:
      Result := MakeInt64(Val.VInteger);    // CreateFixNum(Val.VInteger);
    vtInt64:
      Result := MakeInt64(Val.VInt64^);   // CreateFixNum(Val.VInt64^);
    vtBoolean:
      if (Val.VBoolean) then
        Result := _t
       else
        Result := _f;
    vtChar:
      Result := CreateCharAtom(WideChar(Ord(Val.VChar)));  // TCharAtom.Create(Ord(Val.VChar));
    vtWideChar:
      Result := CreateCharAtom(Val.VWideChar);             //TCharAtom.Create(Val.VWideChar);
    vtExtended:
      Result := MakeDouble(Val.VExtended^);
    vtString:
      Result := make_string(PAnsiChar(@Val.VString^[1]), Length(Val.VString^));
    vtAnsiString:
      Result := make_string(PAnsiChar(Val.VAnsiString), Length(AnsiString(Val.VAnsiString)));
    vtWideString:
      Result := make_string(PWideChar(Val.VWideString), Length(WideString(Val.VWideString)));
    vtUnicodeString:
      Result := make_string(PWideChar(Val.VUnicodeString), Length(UnicodeString(Val.VUnicodeString)));

    {
      vtPChar:      (VPChar: PChar);
      vtClass:      (VClass: TClass);
      vtCurrency:   (VCurrency: PCurrency);
      vtVariant:    (VVariant: PVariant);
      vtInterface:  (VInterface: Pointer); }
    else
      Result := nil;
  end
end;

function DatumType(Datum: TDynDatum): TDatumType;
begin
  case Integer(Pointer(Datum)) and 3 of
    smInterface:
      if Assigned(Datum) then
        Result := IDynDatum(Pointer(Datum)).DatumType
      else
        Result := atNil;
    smInteger:
      Result := atInteger;
    smInline:
      case (Integer(Pointer(Datum)) and $FF) of
        akChar: Result := atChar;
        akBool: Result := atBool;
        akNil : Result := atNil;
        akUnbound: Result := atUnbound;
      else // akUndefined:
        Result := atUndefined;
      end
    else
      Result := atUndefined;
  end
end;

function Supports(Datum: TDynDatum; IID: TGuid; out Res): Boolean;
begin
  Result := (Integer(Pointer(Datum)) and StorageMask = smInterface)
    and Supports(IInterface(Pointer(Datum)), IID, Res)
end;

function Supports(Datum: TDynDatum; IID: TGuid): Boolean;
begin
  Result := (Integer(Pointer(Datum)) and StorageMask = smInterface)
    and Supports(IInterface(Pointer(Datum)), IID)
end;

procedure NeedInterface(Datum: TDynDatum; IID: TGuid; out Res);
var
  Val: Integer;
begin
  Val := Integer(Pointer(Datum));
  case Val and StorageMask of
    smInterface:
      if Supports(IInterface(Pointer(Datum)), IID, Res) then
        Exit;
  end;
  raise EWrongType.Create(Datum, 'interface:' + GUIDToString(IID));
end;

{$ENDREGION}

{$ifdef IMP_DYNDATUM}

{procedure DisposePRefDatum(p: PRefDatum);
begin
  Dispose(p);
end;}

{ TDynDatum }

function TDynDatum.Data: Pointer;
begin
  Result := Pointer(NativeInt(Pointer(Self)) and PointerMask);
end;

procedure TDynDatum.Free;
{$ifdef FPC}
begin
  if Self <> nil then
    if NativeInt(Self) and 3 = 0 then
      _Release
{$else}
{var
  p: PRefDatum;
  Itfc: Pointer;
begin    }
asm
  test eax,1         // smInteger = 1; smInline = 3;
  jz   @@NeedFree
  ret
  nop
@@NeedFree:
  test eax,3      // smInterface = 0; smRef = 2;
  jnz  @@smRef
@@smInterface:
  or   eax,eax    // if not Assigned(Self) then Exit;
  jz   @@end
  mov  ecx,[eax]   // vmt IInterface(Self)._Release;
  xchg eax,[esp]
  push eax         // [ret] -> [Self, ret]
  jmp  [ecx+8]     //  IInterface._Release

@@smRef:
{
  and  eax,PointerMask         // p := Pointer(Integer(Self) and PointerMask);
  dec  TRefDatum(eax).RefCount // Dec(p.RefCount);
  jz   DisposePRefDatum        // if p.RefCount = 0 then
}                               //   Dispose(p);
@@end:
{$endif}
end;

function TDynDatum.NewRef: TDynDatum;
begin
  Result := Self;
  case NativeInt(Pointer(Result)) and StorageMask of
    smInterface:
      if Assigned(Result) then
        IInterface(Pointer(Result))._AddRef;
    //smRef:
    //  Inc(PRefDatum(Integer(Pointer(Result)) and PointerMask).RefCount);
  end;
end;

function TDynDatum.Kind: TDatumType;
var
  Val: Integer;
begin
  Result := atInvalid;
  Val := Integer(Pointer(Self));
  case (Val and 3) of
    smInterface:
      begin
        if Val <> 0 then
          Result := IDynDatum(Val and PointerMask).DatumType
        else
          Result := atNil;
      end;
    {
    smRef:
      begin
        Result := PRefDatum(Val and PointerMask).DatumType;
        if Result = atStream then
          Result := atPair;
      end;
    }
    smInteger:
      Result := atInteger;
    smInline:
      case (Val and $FF) of
        akChar: Result := atChar;
        akBool: Result := atBool;
        akNil : Result := atNil;
        akUnbound: Result := atUnbound;
        akUndefined: Result := atUndefined;
      end
  end;
end;

{$endif}

{ EWrongType }

constructor EWrongType.Create(Datum: TDynDatum; const RequiredType: String);
begin
  inherited Create(Format('Required type: %s in %s', [RequiredType, Deb(Datum)]));
end;

{$ifndef DTYPES}

function TDynDatum.AsInteger: Integer;
  external dll name 'TDynDatum_AsInteger';
function TDynDatum.AsString: String;
  external dll name 'TDynDatum_AsString';

{$else}

function TDynDatum.AsInteger: Integer;
begin
  case Integer(Pointer(Self)) and StorageMask of
    smInteger:
      Result := FixNumValue(Self); // debería usar "sar 2" en vez de "shr 2";
    smInterface:
      Result := Int64Value(Self);
    else raise InvalidType;
  end;
end;

function TDynDatum.AsString: String;
begin
  case Kind of
    atString:
      HandleMessageWithPointer_Err(Self, MsgCastToString, @Result, @ErrCastToString);
    atSymbol:
      Result := string(IDynSymbol(Pointer(Self)).Name);
    else raise InvalidType;
  end
end;

function TDynDatum_AsInteger(Self: TDynDatum): Integer;
begin
  Result := TDynDatum(Self).AsInteger
end;

function TDynDatum_AsString(Self: TDynDatum): String;
begin
  Result := TDynDatum(Self).AsString
end;

{$ifdef ENABLE_EXPORTS}
exports
  TDynDatum_AsInteger name 'TDynDatum_AsInteger',
  TDynDatum_AsString name 'TDynDatum_AsString';  
{$endif}

{$endif} // DTYPES

function EnsureInterface(A: weak_IDyn): IDyn;
begin
  if NativeInt(Pointer(A)) and StorageMask = smInterface then
    Result := IDyn(Pointer(A))
  else
    Result := ValToRefType(A)
end;

{$ifdef DTYPES}
{$include 'DynTypes_BaseFunc_Imp.inc'}
{$endif}
{$include 'DynTypes_BaseTypes_Imp.inc'}
{$include 'DynTypes_Match_Imp.inc'}


{$ifdef LINK_DEB}
const
  INil: IDynDatum = nil;
{$endif}
initialization /////////////////////////////////////////////////////////////////
  InitBools(_f, _t);
{$ifdef LINK_DEB}
  Deb(TDynDatum(nil));
  Deb(INil);
{$endif}
end. ///////////////////////////////////////////////////////////////////////////


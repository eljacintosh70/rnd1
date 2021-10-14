
  dyn = record
  public
    Ref: IDyn;
    function Ptr: Pointer; inline;
    function Value: TDynDatum; inline;
  public
    // debería soportar el operador Obj(arg), pero no funciona,
    // hay que llamar Obj.Invoke(arg)
    function Invoke(const Arg: array of dyn): dyn;
    // soportar for in
    function GetEnumerator: IDynEnumerator;
    // esto permite usar el operador Obj[i] tanto para listas como para diccionarios
    function GetItem(const Key: dyn): dyn;
    procedure SetItem(const Key: dyn; const Value: dyn);
    property Item[const Key: dyn]: dyn read GetItem write SetItem;
        default;
    // llamar a método por Id: Obj.Id(Arg)
    function Method(Id: dyn; Arg: array of dyn): dyn;
    // usado por el evaluador
    procedure DispatchMsg(var Message);
  public
    procedure cons(const Car, Cdr: dyn); inline;
  public
    class operator Implicit(const A: IDyn): dyn; inline;
    class operator Implicit(const A: dyn): IDyn; inline;
    class operator Implicit(const A: weak_IDyn): dyn; inline;
    class operator Implicit(const A: dyn): weak_IDyn; inline;
    class operator Implicit(const A: TDynDatum): dyn; inline;
    class operator Implicit(const A: dyn): TDynDatum; inline;

    class operator Implicit(const A: Boolean): dyn; inline;
    class operator Implicit(const A: Int64): dyn; inline;
    class operator Implicit(const A: Double): dyn; inline;
    class operator Implicit(const A: WideChar): dyn; inline;
    class operator Implicit(const A: AnsiChar): dyn; inline;
    class operator Implicit(const A: String): dyn; inline;

    class operator Implicit(const A: dyn): Boolean; inline;
    class operator Implicit(const A: dyn): Int64; inline;
    class operator Implicit(const A: dyn): Double; inline;
    class operator Implicit(const A: dyn): WideChar; inline;
    class operator Implicit(const A: dyn): AnsiChar; inline;
    class operator Implicit(const A: dyn): String; inline;

    class operator Equal(const A, B: dyn): Boolean; inline;

    class operator Equal(const A: dyn; const B: IDyn): Boolean; inline;
    class operator Equal(const A: dyn; B: weak_IDyn): Boolean; inline;
    class operator Equal(const A: dyn; B: Pointer): Boolean; inline;

    class operator Equal(const A: IDyn; const B: dyn): Boolean; inline;
    class operator Equal(A: weak_IDyn; const B: dyn): Boolean; inline;
    class operator Equal(A: Pointer; const B: dyn): Boolean; inline;

    class operator NotEqual(const A, B: dyn): Boolean; inline;

    class operator NotEqual(const A: dyn; const B: IDyn): Boolean; inline;
    class operator NotEqual(const A: dyn; B: weak_IDyn): Boolean; inline;
    class operator NotEqual(const A: dyn; B: Pointer): Boolean; inline;

    class operator NotEqual(const A: IDyn; const B: dyn): Boolean; inline;
    class operator NotEqual(A: weak_IDyn; const B: dyn): Boolean; inline;
    class operator NotEqual(A: Pointer; const B: dyn): Boolean; inline;
  end;

  IDynEnumerator = interface(IInterface)
    function MoveNext: Boolean; stdcall;
    function GetCurrent: dyn; stdcall;
    property Current: dyn read GetCurrent;
  end;

  IDyn = interface(IInterface)
    ['{99AD078F-FE40-4FB5-A8C2-F92092684531}']
    // debería soportar el operador Obj(arg), pero no funciona,
    // hay que llamar Obj.Invoke(arg)
    function Invoke(const Arg: array of dyn): dyn; stdcall;
    // soportar for in
    function GetEnumerator: IDynEnumerator; stdcall;
    // esto permite usar el operador Obj[i] tanto para listas como para diccionarios
    function GetItem(const Key: dyn): dyn; stdcall;
    procedure SetItem(const Key: dyn; const Value: dyn); stdcall;
    property Item[const Key: dyn]: dyn read GetItem write SetItem;
        default;
    // llamar a método por Id: Obj.Id(Arg)
    function Method(Id: dyn; Arg: array of dyn): dyn; stdcall;
    // usado por el evaluador
    procedure DispatchMsg(var Message); stdcall;
  end;

  weak_IInterface = class(TObject)
  public
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; abstract;
    function _AddRef : longint; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; abstract;
    function _Release : longint; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; abstract;
  end;

  weak_IDyn = class(weak_IInterface)
    // debería soportar el operador Obj(arg), pero no funciona,
    // hay que llamar Obj.Invoke(arg)
    function Invoke(const Arg: array of dyn): dyn; virtual; stdcall; abstract;
    // soportar for in
    function GetEnumerator: IDynEnumerator; virtual; stdcall; abstract;
    // esto permite usar el operador Obj[i] tanto para listas como para diccionarios
    function GetItem(const Key: dyn): dyn; virtual; stdcall; abstract;
    procedure SetItem(const Key: dyn; const Value: dyn); virtual; stdcall; abstract;
    property Item[const Key: dyn]: dyn read GetItem write SetItem;
        default;
    // llamar a método por Id: Obj.Id(Arg)
    function Method(Id: dyn; Arg: array of dyn): dyn; virtual; stdcall; abstract;
    // usado por el evaluador
    procedure DispatchMsg(var Message); virtual; stdcall; abstract;
  end;


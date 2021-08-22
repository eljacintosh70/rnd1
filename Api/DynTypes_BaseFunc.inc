
function MakeBool(Value: Boolean): IDyn;   {$ifndef DTYPES} external dll name 'MakeBool';  {$endif}
function MakeChar(Value: Char): IDyn;      {$ifndef DTYPES} external dll name 'MakeChar';  {$endif}
function MakeInt64(Value: Int64): IDyn;    {$ifndef DTYPES} external dll name 'MakeInt64';  {$endif}
function MakeDouble(Value: Double): IDyn;  {$ifndef DTYPES} external dll name 'MakeDouble'; {$endif}
function MakeString(Value: string): IDyn;  {$ifndef DTYPES} external dll name 'MakeString'; {$endif}

function ValToRefType(A: weak_IDyn): IDyn; {$ifndef DTYPES} external dll name 'ValToRefType'; {$endif}


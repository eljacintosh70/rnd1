# RndPas2

Este proyecto busca crear una forma de compartir variables de tipos dinámicos entre varios programas.
El proyecto debe poder representar tipos simples, listas, arreglos diccionarios y funciones.

## Contenido
 * [Tipos](#Tipos)
    * [Tipos estructurados](#Estruct)
 * [Evaluación](#Eval)
 * [Parsers](Parsers)
    * [Lisp](Lisp)
    * [Rnd](Rnd)
 * [Displayers](Displayers)
 * [Proyectos](Proyectos)
    * [Dll](Dll)
    * [Tester](Tester)
    * [Evaluador](Evaluador)

## Tipos
<A name="Tipos"/>
Los tipos se representan como interfaces en Object Pascal:

<pre>
<b>type</b>
  IDyn<b><i>T</i></b> = <b>interface</b>(IDyn)
</pre>

Existe un record **dyn** que permite guardar cualquiera de esas interfaces, 
y soporta asignación directa a variables de tipos <b><i>T</i></b> nativos de Pascal:

<pre>
dyn = record
    class operator Implicit(const A: <b><i>T</i></b>): dyn; inline;
    class operator Implicit(const A: dyn): <b><i>T</i></b>; inline;
    class operator Implicit(const A: dyn): IDyn<b><i>T</i></b>; inline;
end;

function Make<b><i>T</i></b>(Value: <b><i>T</i></b>): IDyn<b><i>T</i></b>;

function Is<b><i>T</i></b>(const A: dyn): Boolean;
function Is<b><i>T</i></b>(const A: dyn; out Res: <b><i>T</i></b>): Boolean;
function Is<b><i>T</i></b>(const A: dyn; out Res: IDyn<b><i>T</i></b>): Boolean;

procedure Need<b><i>T</i></b>(const A: dyn);
procedure Need<b><i>T</i></b>(const A: dyn; out Res: <b><i>T</i></b>);
procedure Need<b><i>T</i></b>(const A: dyn; out Res: IDyn<b><i>T</i></b>);
</pre>

### Tipos estructurados
<A name="Estruct"/>

El tipo <b><i>IDynPair</i></b> representa una lista y se construye con las siguientes funciones:
<pre>
function cons(const Car, Cdr: dyn): IDynPair;
function list(const elem: array of dyn): IDynPair;
function list(const elem: array of dyn; Rest: IDynPair): IDynPair;
</pre>

El tipo <b><i>IDynArray</i></b> representa un arreglo unidimensional o vector.
Se diferencia de una lista en que es eficiente acceder a un elemento por su índice.
Se construye con las siguientes funciones:
<pre>
function make_vector(n: Integer; Fill: TDynDatum = nil): IDynArray;
function make_vector(const Arr: array of const): IDynArray;
function ListToDynArray(List: dyn): IDynArray;
</pre>

## Evaluación
<A name="Eval"/>

La función <b><i>Eval</i></b> evalúa un objeto en el contexto indicado por Scope:
<pre>
function Eval(Exp: dyn; Scope: IDynScope): dyn;
</pre>

Para la mayoría de los tipos, Eval retorna el mismo objeto original.

En caso de ser un símbolo, Eval retorna el valor asociado a ese símbolo en Scope.

En caso de ser un arreglo, evalúa todos los elementos y retorna un arreglo con los valores obtenidos.

En caso de ser una lista, Eval evalúa el primer valor.
Si ese valor es una sintaxis, le pasa el resto de los valores para que retorne un resultado. 
En caso de ser una función, evalúa primero los demás elementos de la lista, y luego se los pasa a la función.

## Parsers
<A name="Parsers"/>

### Lisp
<A name="Lisp"/>

Tipo TLispParser en LispParser.pas

Ejemplo (ver Test_DTPort.pas):
<pre>
  Parser := TLispParser.Create;
  Parser.Evaluate(Res, SrcText);
  Parser.Free;
</pre>

### Rnd
<A name="Rnd"/>

Tipo TParser en RndParser.pas
<pre>
  Parser := TParser.Create(RndText);
  Parser.GetNextTerm(Res);
  Parser.Free;
</pre>

## Displayers
<A name="Displayers"/>
Un displayer se encarga de mostrar un objeto como texto.

Debe estar asociado a un procedimiento <b><i>TWriteProc</i></b> que reciba ese texto y lo envía a su destino.

<pre>
  TextOut := TStrTextOutW.Create(200);
  
  DynOutPort := TDynOutPort.Create(TextOut.WriteProc);
  DynOutPortLisp := TDynOutPortLisp.Create(TextOut.WriteProc);
  DynOutPortRnd := TDynOutPortRnd.Create(TextOut.WriteProc);
</pre>

## Proyectos
<A name="Proyectos"/>

El grupo de proyectos RndPas\delphi\RndPas.groupproj incluye: 

### Dll
<A name="Dll"/>
El dll DType.dll, que es un dll que implementa los tipos dinámicos, sin incluir las funciones del intérprete como tal.

### Tester
<A name="Tester"/>
El programa de pruebas DTypeTests.exe, que usa DUnit.

Ese programa prueba los Parsers y Displayers de Lisp y Rnd con archivos de prueba Lib\Test\*.txt.

### Evaluador
<A name="Evaluador"/>
El proyecto RndPas\delphi\Rnd.dpr es el evaluador como tal.

Puede evaluar expresiones con la sintaxis estilo Lisp y la estilo Rnd.
Ambas afectan el mismo Namespace global, por lo que se pueden usar elementos definidos con una sintaxis desde la otra.




unit StringUtils;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, Graphics;
{$else}
interface
uses
  Classes, SysUtils;

type AnsiString = String;
{$endif}

function FoundString(inString,searchString:string):integer;  // find first occurrance of "searchString"
function myStringReplace(Instring,OldString,NewString:String;ReplaceNum,MaxStringLength:integer):String;
function MyBoolToStr(inBool:boolean):string;
function MyStrToBool(inStr:string):Boolean;
function TrimWhiteSpace(Instring:string):String;
function stringsplit(str:string; separator:string):TStringList;
function IsInStringList(myList:TStringList;elem:string):boolean;
function ListStringToStringList(ListString:String):TStringList;
function StringToSubStringList(InString,delimiter:String):TStringList;
function DelChars(Instring,FilterChar:String):String;
function stripLeadingStringIfPresent(instring,LeadingString:String):String;

{$ifndef JScript}
procedure LoadListFromFile(var myList:TStringList;filename:string);
function ColorToHexRGB(Color: TColor): string;
function HexRGBToColor(RGBString: String): TColor;
{$else}
procedure ShowMessage(text:String);
{$endif}

var MainUnitName:String;

implementation

{$ifndef JScript}
procedure LoadListFromFile(var myList:TStringList;filename:string);
begin
  myList:=TStringList.Create;
  try
    myList.LoadFromFile(filename);
  except
    //showmessage('no load of '+filename);
    exit;
  end;
end;

{$else}

procedure Showmessage(text:String);
begin
  asm
    alert(text);
  end;
end;

{$endif}

function IsInStringList(myList:TStringList;elem:string):boolean;
var
  i:integer;
  found:boolean;
begin
  found:=false;
  i:=mylist.IndexOf(elem);
  if i>-1 then found:=true;

  result:=found;
end;

function CheckMatch(Instring,teststring:string;startpos:integer):boolean;
var i:integer;
  match:boolean;
  temp1,temp2:string;
begin
   match:=true;
   for i:= 1 to Length(testString) do
   begin
      if (i+startpos-1)<= Length(Instring) then
      begin
        temp1:=Instring[i+startpos-1];
        temp2:=teststring[i] ;
        if temp1<>temp2
        then match:=false;
      end
      else match:=false;
   end;
   result := match;
end;

function CharReplace(Instring,OldChar,NewChar:String):string;
var i:integer;
    finalstring:string;
begin
  finalstring:='';
  for i:=1 to Length(Instring) do
  begin
    if Instring[i]= OldChar
    then finalstring:=finalstring+NewChar
    else finalstring:=finalstring+Instring[i] ;
  end;
  result :=  finalstring;
End;

function  myStringReplace(Instring,OldString,NewString:String;ReplaceNum,MaxStringLength:integer):String;
// replaces "ReplaceNum" occurrences of oldstring in the first "MaxStringLength" of Instring --- NB it is case sensitive
var i,matchLength:integer;
  match:Boolean;
  replaceCount:integer;
  finalstring,tempstr:String;
begin
  finalstring:='';
  replaceCount:=0;
  matchLength:=0;
  for i:=1 to Length(Instring) do
  begin
    tempstr:=Instring[i];
    if (( tempstr<>OldString[1])and (i>matchLength)) or (i>MaxStringLength)
    then
    begin
      finalstring:=finalstring+tempstr;
    end
    else   //potential match
    if (i>matchLength) then
    begin
      match := CheckMatch(Instring,oldstring,i);
      if (match = false) or (replaceCount>=ReplaceNum)
      then finalstring:=finalstring+tempstr
      else
      begin
          replaceCount:=replaceCount+1;
          finalstring:=finalstring+NewString;
          matchlength:= i+ Length(OldString)-1;
      end;
    end;
  end;
  result :=  finalstring;
end;

function FoundString(inString,searchString:string):integer;  // find first occurrance of "searchString"
  var
    match,found:Boolean;
    i,tempresult:integer;
    tempstr:String;
  begin
    tempresult:=0;
    found:=false;
    for i:=1 to Length(Instring) do
    begin
      if found=false then
      begin
        tempstr:=Instring[i];
        if ( tempstr=searchString[1]) then
        begin  //potential match
          match := CheckMatch(Instring,searchString,i);
          if (match = true) then
          begin
            tempresult:=i;
            found:=true;
          end;
        end;
      end;
    end;
    result :=  tempresult;
end;

function DelChars(Instring,FilterChar:String):String;
var i:integer;
  newstring,tempstr:String;
begin
  newstring:='';
  for i:=1 to Length(Instring) do
  begin
    tempstr:=Instring[i];
    if  tempstr<>FilterChar
    then
    begin
      newstring:=newstring+tempstr;
    end;
  end;
  result :=  newstring;
end;

function StringToSubStringList(InString,delimiter:String):TStringList;
var items : TStringList;
begin
  items := TstringList.Create;
  items.StrictDelimiter:=true;
  //items.SkipLastLineBreak:=false;
  items.LineBreak:=delimiter;
  items.text:= InString;
  StringToSubStringList:=items;
end;

function TrimWhiteSpace(Instring:string):String;
begin
  result:=DelChars(Instring,' ');
end;

function stripLeadingStringIfPresent(instring,LeadingString:String):String;
var i:integer;
  OutString:string;
  done:boolean;
begin
  OutString:='';
  done:=false;
  for i :=1 to Length(instring) do
  begin
    if ( i <=  Length(LeadingString)) and (done=false) then
    begin
      if instring[i]<>LeadingString[i]
      then
      begin
        done:=true;
        OutString:=OutString+instring[i]   ;
      end;
    end
    else OutString:=OutString+instring[i]   ;
  end;
  result:=OutString;
end;


function MyBoolToStr(inBool:boolean):string;
begin
  if inBool=true then
    result:='True'
  else if inBool=false then
    result:='False'
  else showmessage('invalid boolean');
end;

function MyStrToBool(inStr:string):Boolean;
begin
  if uppercase(TrimWhiteSpace(instr))='TRUE' then
    result:=True
  else if uppercase(TrimWhiteSpace(instr))='FALSE' then
    result:=False
  else
  showmessage('invalid boolean string '+inStr);
end;

function myFloatToStr(invar:real):string;
begin
  result:=FloatToStr(invar);
end;

function mystrtoFloat(instring:string):real;
begin
  result:=strtoFloat(instring);
end;

function stringsplit(str:string; separator:string):TStringList;
var
   localStringList:TStringList;
begin
  localStringList:=StringToSubStringList(str,separator);
  result:=localStringList;
end;

//function StringToSubStringList(InString,delimiter:String):TStringList;
//var items : TStringList;
//begin
//  items := TstringList.Create;
//  items.StrictDelimiter:=true;
//  items.SkipLastLineBreak:=false;
//  items.LineBreak:=delimiter;
//  items.text:= InString;
//  StringToSubStringList:=items;
//end;


function ListStringToStringList(ListString:String):TStringList;
var items : TStringList;
    TempString:String;
begin
  // example optionlist '["Banana","Cherry","Lemon","Carrot","Eggplant","Potato"]'
  TempString:=ListString;
  TempString := StringReplace(TempString, '[', '',[rfReplaceAll]);
  TempString := StringReplace(TempString, ']', '',[rfReplaceAll]);
  TempString := StringReplace(TempString, '"', '',[rfReplaceAll]);
  items := TstringList.Create;
  items.StrictDelimiter:=true;
  items.LineBreak:=',';
  items.text:= TempString;
  ListStringToStringList:=items;
end;


{$ifndef JScript}
function ColorToHexRGB(Color: TColor): string;
var
  N: Longint;
begin
  if Color=clNone then
    begin Result:= ''; exit end;
  N:= ColorToRGB(Color);
  Result:= '#'+
    IntToHex(Red(N), 2)+
    IntToHex(Green(N), 2)+
    IntToHex(Blue(N), 2);
end;
function HexRGBToColor(RGBString: String): TColor;
type
  T4Byte = array [0 .. 2] of Byte;
var
  bits:TStringList;
  str:string;
  ba: T4Byte;
  shadow: LongWord absolute ba;
begin
  if RGBString='' then
    result:=clNone
  else
  begin
    bits := StringSplit(RGBString,'#');
    str:=bits[1];
    shadow := StrToInt('$' + str);
    // ba now contains the bytes of YourHEXString, watch out with endianness
    result:=RGBToColor(ba[2],ba[1],ba[0]);
  end;
end;
{$endif}

begin
end.


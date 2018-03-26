unit NodeUtils;
interface
{$ifndef JScript}
uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls, SysUtils, Dialogs, Clipbrd, ProjectIntf, LazIDEIntf, TypInfo,
  StringUtils, UtilsJSCompile;
{$else}
uses
  Classes,  SysUtils, TypInfo, StringUtils;
{$endif}


type TDataNode = class;  //forward


type TEventHandler = procedure(nodeID:AnsiString;myNode:TDataNode;myValue:AnsiString) of object;
type TEventHandlers = Array of TEventHandler;
type TGenericHandler = function(MyEventType,myValue:string;myNode:TDataNode):Boolean of object;
//{$ifdef Win32}
//   type  WinSizeDepenedantInt = integer;
//{$else}
//   type   WinSizeDepenedantInt = int64;
//{$endif}
   {$ifdef Win64}
type   WinSizeDependentInt = int64;
   {$else}
type  WinSizeDependentInt = integer;
   {$endif}

{$ifndef JScript}
   type TAddComponentFunc = function(ParentNode:TDataNode;ObjectName:String;position:integer): TDataNode;
{$else}
type TAddComponentFunc = function(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer): TDataNode;

// dummy 'TForm' object for Javascript widget creation
type TForm = class(TObject)
      private
        fName:String;
      published
        property Name:String read fName write fName;
      end;

type TXPropertyLink = class(TPersistent)
  FTIObjectName:String;
  FTIObject:TObject;
  FTIPropertyName:String;
  published
    property TIObjectName:String read FTIObjectName write FTIObjectName;
    property TIObject:TObject read FTIObject write FTIObject;
    property TIPropertyName:String read FTIPropertyName write FTIPropertyName;
end;

{$endif}

type TCreateInObFunc = function(MyForm:TForm;NodeName:String):TObject;


type   TNodeAttribute = Record
         AttribName:string;
         AttribType:string;
         AttribValue:string;
         AttribReadOnly:boolean;
     end;
type  TNodeAttributesArray = Array of TNodeAttribute;

type TChildNodesArray = Array of  TDataNode;

type TNodeFuncsLookup = record
                        NodeType:String;
                        ScreenObjFunctionPtr:TAddComponentFunc;
                        InObFunctionPtr:TCreateInObFunc;
                     end;
type TNodeFuncsTable = Array of TNodeFuncsLookup;


type TComponentTag = class
       public
         HH,WW:string;
       end;

type TNodeTimerTag = class
       public
         myNode,DestinationNode:TDataNode;
         pos:integer;
       end;

//----------------- TDataNode Definition ---------------------------------------
// The data nodes store the definition of the user interface, including the object inspectors,
// plus all available resources.
// The tree of data nodes is the data which is stored when a system is saved.

type   TDataNode = class(TObject)
       public
          NodeName:String;
          NodeType:String;
          NodeClass:String;
          IsDynamic:Boolean;                             // true if
          //AdminNode:Boolean;                             // true if this element is part of objectinspector or system popups
          ScreenObject:TObject;                          // the actual visible component
          MyForm:TForm;                                  // the owning 'form' - object which contains event handlers

          NodeAttributes:TNodeAttributesArray;
          ChildNodes:TChildNodesArray;
          myEventTypes:TStringList;
          myEventHandlers:TEventHandlers;

          constructor Create(MyClass,MyName,MyType:string;NodeIsDynamic:Boolean=false);
          procedure DeleteMe;
          function GetAttribute(AttrName:string;AllowSpace:boolean):TNodeAttribute;
          procedure AddAttribute(AttributeName,AttributeType,AttributeValue:string;AttributeReadOnly:boolean);
          procedure SetAttributeValue(AttrName:string;NewValue,AttrType:string);  overload;
          procedure SetAttributeValue(AttrName:string;NewValue:string); overload;
          function GetChildIndex(ChildNode:TDataNode):integer;
          procedure RemoveChildNode(ChildNode:TDataNode);
          function FindRegisteredEvent(EventType:string):TEventHandler;
          procedure RegisterEvent(EventType:String;TheHandler:TEventHandler);
       end;

{$ifdef JScript}
type TInterfaceObject = class(TDataNode)
private
  FLink:TXpropertyLink;
published
  myNode:TDataNode;          // self-reference for compatibility with Laz component
  procedure SetLink(const AValue: TXPropertyLink);
  procedure LinkLoadFromProperty(Sender: TObject); virtual;
  procedure LinkSaveToProperty(Sender: TObject); virtual;

  property Link: TXPropertyLink read FLink write SetLink;
end;

{$endif}

function AddFormToNodeTree(myForm:TForm):TdataNode;
procedure AddAttrib(var AttrParams:TNodeAttributesArray;attrName,attrType,attrValue:string;attrReadOnly:Boolean);
procedure AddChildToParentNode(var ParentNode, ChildNode:TDataNode; position:integer);
function NodeTreeToXML(CurrentItem,ParentNode:TDataNode;IncludeDynamic:Boolean):String;
Procedure SaveSystem(ToClip:Boolean);
function FindDataNodeById(InTree:TDataNode; ScreenObjectID:String;showerror:boolean):TDataNode;
function FindParentOfNode(InTree:TDataNode;ScreenObjectID:String;showError:Boolean):TDataNode; overload;
function FindParentOfNode(InTree:TDataNode;ScreenObjectID:String):TDataNode; overload;
procedure ReParentNode(MyNode,NewParent:TDataNode);
procedure DeleteNode(ParentNode,MyNode:TDataNode);
function CopyNode(SourceNode:TDataNode):TDataNode;
function AddChildToDataNode(ParentNode:TDataNode;MyClass,MyName,MyType:string;MyAttributes:TNodeAttributesArray;
                           position:integer):TDataNode;
function LookupComponentFunc(NodeType:string):TAddComponentFunc;
function NodeIsDescendantOf(ThisNode:TDataNode;AncestorName:string):integer;
procedure Initialiselinks(StartNode:TDataNode);
procedure DeleteNodeChildren(ParentNode:TDataNode);
function InsertSystemNode(ParentNode,SourceNode:TDataNode;Position:integer):TDataNode;
procedure ClearAttribs(var AttrParams:TNodeAttributesArray);
function NodeNameIsUnique(NodeName:string; showerror:Boolean):Boolean;
procedure InitSystemNodetree;
procedure ClearAllDynamicNodes(StartNode:TDataNode);

{$ifndef JScript}
function myCopyToClip(stringname,instring:string):string;
procedure AddNodeFuncLookup(NodeType:string;ScreenObjFunc:TAddComponentFunc);
procedure SetAlignmentParameters(MyObj,MyParent:TWinControl);   overload;
procedure SetAlignmentParameters(ParentNode, MyNode:TDataNode); overload;
function FindOuterParentOf(InnerControl:TWinControl):TWinControl;
function CreateComponentDataNode(myName,myType:String; eventTypes:TStringList; myComponent,myOwner:TObject; IsDynamic:Boolean):TdataNode;
function XMLToNodeTree(XMLString:String):String;
procedure ClearAllScreenObjects;

{$else}
procedure InitFormObject(myForm:TForm;NodeName:String);
procedure AddNodeFuncLookup(NodeType:string;InObFuncPtr:TCreateInObFunc;ScreenObjFunc:TAddComponentFunc);
procedure PushTolinks(AObject:TObject; PropName:string; PropValue:String; StartNode:TDataNode);
procedure SetInterfaceProperty(myName,PropName,NewValue:string);
{$endif}

var MainForm:TForm;
SuppressEvents:Boolean;
{$ifndef JScript}


{$else}


type TMethod = record
  Code : Pointer;       //CodePointer;
  Data : Pointer;
end;

function XMLToNodeTree(XMLString:String):String;
procedure NilScreenObject(MyNode:TdataNode);
function CreateInterfaceObject(MyForm:TForm;NodeType, NodeName:String):TObject;

// this variable contains the system description to be loaded at startup
var LoadedSystemString:String;

{$endif}



const SystemRootName= 'UIRootNode';             // 'MyRootDiv' ;
const PortraitMode=false;

var SystemNodeTree,UIRootNode:TDataNode;
var StartingUp:Boolean;
var   loadingSystem:Boolean;
//var   HTMLNoWrapperPanel:TStringList;

var   NodeFuncsLookup:TNodeFuncsTable;             // lookup table used in data node creation

const EventAttributeDelimiter='|@|';
const EventListdelimiter='|^|';
const EventHistorydelimiter='|~|';
const delimiterBetweenTheEventHistoriesAndSystemDescription = '|@@|';
const attributeListdelimiter='|;'  ;
const NameValuePairdelimiter= '|=' ;
const AttribBitsDelimiter= '|{'   ;
const AttribLinkDelimiter= '|}'   ;
const RegisteredEventsDelimiter='|^^|';

const StartXMLString =  '<';
const ToggleFlagMarker = '/';
const EndXMLString =  '>';


implementation
{$ifndef JScript}
uses LazsUtils;
{$else}
uses WrapperPanel, Popup;
{$endif}


constructor TDataNode.Create(MyClass,MyName,MyType:string;NodeIsDynamic:Boolean=false);
var
  NodeString:string;
begin
  SetLength(self.ChildNodes,0) ;
  SetLength(self.NodeAttributes,0);
  self.myEventTypes:=TStringList.Create;
  SetLength(self.myEventHandlers,0);

  self.NodeClass:=MyClass;
  self.NodeName:=MyName;
  self.NodeType:=MyType;
  //if MyClass='SVG' then showmessage('TDataNode.Create   node '+MyType+' '+MyName);
  self.IsDynamic:=NodeIsDynamic;

end;


procedure TDataNode.DeleteMe;
var
  NodeString:string;
begin
  self.ScreenObject:=nil;
  self.Destroy;

end;

function TDataNode.GetAttribute(AttrName:string;AllowSpace:boolean):TNodeAttribute;
var
  i:integer;
  foundAttrib:TNodeAttribute;
  myAttribs:TNodeAttributesArray;
begin
  i:=0;
  foundAttrib.AttribName:='';
  myAttribs:=self.NodeAttributes;
  while i < length(myAttribs) do
  begin
    if self.NodeAttributes[i].AttribName=AttrName then
    begin
      foundAttrib:=self.NodeAttributes[i];
      i:= length(myAttribs);
    end;
    i:=i+1;
  end;
  if foundAttrib.AttribName<>AttrName then
    if AllowSpace then
    begin
      foundAttrib.AttribName:=AttrName;
      foundAttrib.AttribType:='';
      foundAttrib.AttribValue:='';
      foundAttrib.AttribReadOnly:=false;
    end
    else
      showmessage('Attribute '+AttrName+' not found in node '+self.NodeName);
  result:=foundAttrib;
end;

procedure TDataNode.AddAttribute(AttributeName,AttributeType,AttributeValue:string;AttributeReadOnly:boolean);
var
  numAttributes:Integer;
  myAttributes:TNodeAttributesArray;
begin
    myAttributes:= self.NodeAttributes;
    numAttributes:=Length(myAttributes);
    SetLength( self.NodeAttributes,numAttributes+1);
    self.NodeAttributes[numAttributes].AttribName := AttributeName ;
    self.NodeAttributes[numAttributes].AttribValue := AttributeValue;
    self.NodeAttributes[numAttributes].AttribType := AttributeType ;
    self.NodeAttributes[numAttributes].AttribReadOnly := AttributeReadOnly;
end;

procedure TDataNode.SetAttributeValue(AttrName:string;NewValue,AttrType:string); // overload;
var
  foundAttrib:TNodeAttribute;
  myAttribs:TNodeAttributesArray;
  i:integer;
  found:Boolean;
begin
  foundAttrib:=self.GetAttribute(AttrName,true);
  //if no attribute here, add one
  if foundAttrib.AttribName='' then
  begin
    foundAttrib.AttribName:=AttrName;
    if AttrType='' then
       foundAttrib.AttribType:='String'
    else
       foundAttrib.AttribType:=AttrType;
    foundAttrib.AttribReadOnly:=false;
  end;
  if foundAttrib.AttribName=AttrName then
  begin
    foundAttrib.AttribValue:=NewValue;
    if (foundAttrib.AttribType='')
    and (AttrType<>'') then
      foundAttrib.AttribType:=AttrType;
  end;

  // now have to put it back in the array
  found:=false;
  myAttribs:=self.NodeAttributes;
  i:=0;
  while i < length(myAttribs) do
  begin
    if self.NodeAttributes[i].AttribName=AttrName then
    begin
      self.NodeAttributes[i]:=foundAttrib;
      i:= length(myAttribs);
      found:=true;
    end;
    i:=i+1;
  end;
  if not found then
    self.AddAttribute(foundAttrib.AttribName, foundAttrib.AttribType, FoundAttrib.AttribValue, foundAttrib.AttribReadOnly);
end;

procedure TDataNode.SetAttributeValue(AttrName:string;NewValue:string); //overload;
begin
  self.SetAttributeValue(AttrName,NewValue,'');
end;

function TDataNode.GetChildIndex(ChildNode:TDataNode):integer;
var
  i:integer;
  mychildren:TChildNodesArray;
begin
  result:=-1;
  mychildren:=self.ChildNodes;
  for i:=0 to length(mychildren)-1 do
  begin
     if mychildren[i].NodeName=ChildNode.NodeName then
       result:=i;
  end;
end;

procedure TDataNode.RemoveChildNode(ChildNode:TDataNode);
var
  i,l:integer;
  found:Boolean;
  mychildren:TChildNodesArray;
begin
  found:=false;
  mychildren:=self.ChildNodes;
  l:=length(mychildren);
  i:=0;
  while (i < length(mychildren)) do
  begin
     if found=false then
     begin
       if mychildren[i]=ChildNode then
       begin
         found:=true;
       end;
     end;
     if (found) and (i<length(mychildren)) then
       mychildren[i]:=mychildren[i+1];
     i:=i+1;
  end;
  if found then
    setlength(mychildren,l-1);
  self.ChildNodes:=mychildren;
end;


function TDataNode.FindRegisteredEvent(EventType:string):TEventHandler;
var
  i:integer;
begin
  result:=nil;
  for i:=0 to self.myEventTypes.Count-1 do
  begin
     if self.myEventTypes[i] = EventType then
       result:=self.myEventHandlers[i];
  end;
end;

procedure TDataNode.RegisterEvent(EventType:String;TheHandler:TEventHandler);
var
  i:integer;
begin
  for i:=0 to self.myEventTypes.Count-1 do
  begin
     if self.myEventTypes[i] = EventType then
     begin
       self.myEventHandlers[i] := TheHandler;
       //showmessage('registered event '+EventType+' for '+self.NodeName);
     end;
  end;
end;

procedure ClearAttribs(var AttrParams:TNodeAttributesArray);
begin
  setlength(AttrParams,0);
end;

function CreateComponentDataNode(myName,myType:String; eventTypes:TStringList; myComponent,myOwner:TObject;IsDynamic:Boolean):TdataNode;
var
  NewNode:TDataNode;
begin
  NewNode:=TDataNode.Create('UI',myName,myType,false);
  NewNode.ScreenObject:=myComponent;
  NewNode.myEventTypes:=eventTypes;
  SetLength(NewNode.myEventHandlers,EventTypes.Count);
  NewNode.MyForm:=TForm(myOwner);
  NewNode.IsDynamic:=IsDynamic;
  // temporarily set as child of root node, so that name uniqueness checks can be done during design
  AddChildToParentNode(SystemNodetree,NewNode,-1);
  result:=NewNode;
end;

function ScanChildrenForNode(CurrentItem:TDataNode;ScreenObjectID:String;var FoundParent:TDataNode):TDataNode;
var FoundItem,TempItem:TDataNode;
    TempArrayOfChildren:TChildNodesArray;
    NumChildren,i:integer;
begin
   FoundItem:=nil;
   FoundParent:=nil;
   if Trim(Uppercase(CurrentItem.NodeName)) = Trim(Uppercase(ScreenObjectID))
   then
   begin
     FoundItem:= CurrentItem
   end
   else
   begin
      TempArrayOfChildren:= CurrentItem.ChildNodes;
      NumChildren:=Length(TempArrayOfChildren);
      i:=0;
      while i < NumChildren do
      begin
         if FoundItem=nil then  // object has not been found so keep looking
         begin
           //showmessage('parent='+CurrentItem.NodeName+' i='+inttostr(i));
            TempItem := CurrentItem.ChildNodes[i];
            //showmessage('TempItem='+TempItem.NodeName);
            if Trim(Uppercase(TempItem.NodeName)) = Trim(Uppercase(ScreenObjectID))
            then
            begin
              FoundItem:= TempItem;
              FoundParent:=CurrentItem;
              i:=NumChildren;
            end
            else
              FoundItem:= ScanChildrenForNode(TempItem,ScreenObjectID,FoundParent);
         end;
         i:=i+1;
      end;
   end;
   result:=FoundItem;
end;

function FindDataNodeById(InTree:TDataNode; ScreenObjectID:String;showerror:boolean):TDataNode;
var FoundItem, TempItem, FoundParent :TDataNode;
begin
   FoundItem:=nil;
   FoundParent:=nil;
   TempItem:=ScanChildrenForNode(InTree,ScreenObjectID,FoundParent);

   if TempItem<>nil
   then
   begin
       FoundItem:= TempItem ;
   end
   else
     if showerror then
       showmessage('Error in NodeUtils.FindDataNodeById >'+ScreenObjectID+'< not found');
   result:=FoundItem;
end;

function FindParentOfNode(InTree:TDataNode;ScreenObjectID:String;showerror:Boolean):TDataNode;
var FoundItem, TempItem, FoundParent :TDataNode;
begin
  //showmessage('FindParentOfNode '+ScreenObjectID+' in tree '+InTree.NodeName);
   FoundItem:=nil;
   TempItem:=nil;
   FoundParent:=nil;
   TempItem:=ScanChildrenForNode(InTree,ScreenObjectID,FoundParent);

   if (TempItem<>nil) and (FoundParent<>nil) then
   begin
       FoundItem:= FoundParent ;
   end
   else
     if showError then
       showmessage('Error in Nodeutils.FindParentOfNode >'+ScreenObjectID+'< not found');
   result:=FoundItem;
end;
function FindParentOfNode(InTree:TDataNode;ScreenObjectID:String):TDataNode;
begin
   result:=FindParentOfNode(InTree,ScreenObjectID,true);
end;

function MakeAttrib(attrName,attrType,attrValue:string;attrReadOnly:Boolean):TNodeAttribute;
var
  newAttrib:TNodeAttribute;
begin
  newAttrib.AttribName:=attrName;
  newAttrib.AttribType:=attrType;
  newAttrib.AttribValue:=attrValue;
  newAttrib.AttribReadOnly:=AttrReadOnly;
  result:=newAttrib;
end;

procedure AddAttrib(var AttrParams:TNodeAttributesArray;attrName,attrType,attrValue:string;attrReadOnly:Boolean);
var
  i:integer;
begin
  i:=Length(AttrParams);
  setlength(AttrParams,i+1);
  AttrParams[i]:=MakeAttrib(attrName,attrType,attrValue,attrReadOnly);
end;


function SubstituteSpecials(instring:string):string;
var
  tempstr:string;
begin
  tempstr:=instring;
  tempstr:=myStringReplace(tempstr,'<','&lt;',9999,9999);
  tempstr:=myStringReplace(tempstr,'>','&gt;',9999,9999);
  tempstr:=myStringReplace(tempstr,'''','&apos;',9999,9999);
  result:=tempstr;
end;
function UnSubstituteSpecials(instring:string):string;
var
  tempstr:string;
begin
  tempstr:=instring;
  tempstr:=myStringReplace(tempstr,'&lt;','<',9999,9999);
  tempstr:=myStringReplace(tempstr,'&gt;','>',9999,9999);
  tempstr:=myStringReplace(tempstr,'&apos;','''',9999,9999);
  result:=tempstr;
end;

function NodeTreeToXML(CurrentItem,ParentNode:TDataNode;IncludeDynamic:Boolean):String;
// Recursive
var XMLString,dlm1,dlm2,ParentName:String;
   i,numchildren,numAttributes,numEvents:integer;
   CurrentChildNodes:TChildNodesArray;
   myAttribs:TNodeAttributesArray;
   EventsString:string;
begin
  XMLString:='';

  if ParentNode<>nil then
     ParentName:=ParentNode.Nodename;

  if (CurrentItem.NodeClass='Root')
  or (CurrentItem.NodeClass='UI')
  or (CurrentItem.NodeClass='SVG')
  or (CurrentItem.NodeClass='Code') then
  begin
    begin
      XMLString:=StartXMLString+CurrentItem.NodeType+attributeListdelimiter;
      XMLString:=XMLString+' Class '+NameValuePairdelimiter + CurrentItem.NodeClass + attributeListdelimiter;
      XMLString:=XMLString+' Name '+NameValuePairdelimiter + CurrentItem.NodeName + attributeListdelimiter;

      myAttribs:= CurrentItem.NodeAttributes;
      numAttributes:=length(myAttribs);
      for i:=0 to numAttributes-1 do
        if (CurrentItem.NodeAttributes[i].AttribName<>'ParentName')
        and ((CurrentItem.NodeAttributes[i].AttribName<>'XMLString') or (CurrentItem.IsDynamic=false)) then
          XMLString:=XMLString
                           + CurrentItem.NodeAttributes[i].AttribName
                           + AttribBitsDelimiter+' '+CurrentItem.NodeAttributes[i].AttribType
                           + AttribBitsDelimiter+ SubstituteSpecials(CurrentItem.NodeAttributes[i].AttribValue)
                           + AttribBitsDelimiter+ myBoolToStr(CurrentItem.NodeAttributes[i].AttribReadOnly)
                           + attributeListdelimiter;
      // add the ParentName attribute
      XMLString:=XMLString
                       + 'ParentName'
                       + AttribBitsDelimiter+' String'
                       + AttribBitsDelimiter+ SubstituteSpecials(ParentName)
                       + AttribBitsDelimiter+ 'True'+attributeListdelimiter;

      XMLString:=XMLString+EndXMLString;

      CurrentChildNodes:= CurrentItem.ChildNodes;
      numchildren:=length( CurrentChildNodes);
      for i:=0 to numchildren-1 do
         XMLString:=XMLString+NodeTreeToXML(CurrentItem.ChildNodes[i],CurrentItem,IncludeDynamic);

      XMLString:=XMLString+StartXMLString+ToggleFlagMarker+CurrentItem.NodeType+EndXMLString;      //add '</abcd>'
    end;
  end;
  result:=( XMLString);
end;

{$ifndef JScript}
function NodeTreeToInterfaceString(CurrentItem:TDataNode):String;
var
  n,i:integer;
  resultString:string;
begin
  if (CurrentItem.IsDynamic=false) then  // item is declared in the form (not created dynamically)
  begin
    if (CurrentItem.NodeName<>SystemRootName)
    and (CurrentItem.NodeClass='UI')
    and (CurrentItem.NodeType<>'Popup')
    and (CurrentItem.NodeType<>'')
    and (CurrentItem.MyForm<>nil) then
    begin
       // eg.   ButtonName := TXButton(CreateInterfaceObject(MyForm,'TXButton','ButtonName'));
      resultString:=CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName +
                    ':= '+CurrentItem.NodeType
                    +'(CreateInterfaceObject('+CurrentItem.MyForm.Name+','''+CurrentItem.NodeType+''','''+CurrentItem.NodeName+'''));'
                    +LineEnding;

      if CurrentItem.NodeType='TXTree' then
      begin
         // has a function been declared to create hints for nodes in this tree?

         // If so, add code to assign the function to the new node in the javascript object
         if MainForm.MethodAddress(CurrentItem.NodeName+'TreeNodeHintFunc')<>nil then
           resultString:=resultString +
                         CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName+'.TreeNodeHintFunc:=@'+
                                             CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName+'TreeNodeHintFunc;'
                                             +LineEnding;
         if MainForm.MethodAddress(CurrentItem.NodeName+'TreeDropAccepted')<>nil then
           resultString:=resultString +
                         CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName+'.TreeNodeDropAccepted:=@'+
                                             CurrentItem.MyForm.Name+'.'+CurrentItem.NodeName+'TreeNodeDropAccepted;'
                                             +LineEnding;
      end;
    end
    else if (CurrentItem.NodeType='Popup')
         and (CurrentItem.NodeClass='UI')  then
    begin
      resultString:=CurrentItem.NodeName + ':= T'+CurrentItem.NodeName+'.Create; ' +LineEnding +
                    'InitFormObject('+CurrentItem.NodeName+','''+CurrentItem.NodeName+''');' +LineEnding;
                    //CurrentItem.NodeName+'.Name := '''+CurrentItem.NodeName+''';'+LineEnding;
    end;

    n:=length( CurrentItem.ChildNodes);
    for i:=0 to n-1 do
       resultString:=resultString+NodeTreeToInterfaceString(CurrentItem.ChildNodes[i])+LineEnding;
  end;
  result:=( resultString);
end;
{$endif}

Procedure SaveSystem(ToClip:Boolean);
var
  systemstring,eventstring,fullstring:string;
  interfaceString:string;
begin
  {$ifndef JScript}
  interfaceString:=NodeTreeToInterfaceString(SystemNodeTree);
  WriteFile('systemintface.inc',interfaceString);
  {$endif}
  systemstring:= NodeTreeToXML(SystemNodeTree,nil,true);
  fullstring:= systemstring;

  if ToClip then
  begin
    {$ifndef JScript}
    myCopyToClip('System',fullstring );
    {$endif}
  end
  else
  begin
    {$ifndef JScript}
    WriteFile('systemnodetree.inc','LoadedSystemString := '''+fullstring+''';');
    {$endif}
  end;
end;

{$ifndef JScript}
procedure AddNodeFuncLookup(NodeType:string;ScreenObjFunc:TAddComponentFunc);
var
    myRec:TNodeFuncsLookup;
    l:integer;
begin
  if NodeType='' then EXIT;
  l:=Length(NodeFuncsLookup);
  SetLength(NodeFuncsLookup,l+1);
  myRec.NodeType:=NodeType;
  myRec.ScreenObjFunctionPtr:=ScreenObjFunc;     // To create a Lazarus 'X' screen object of this type
  NodeFuncsLookup[l]:=myRec;
end;
{$else}
procedure AddNodeFuncLookup(NodeType:string;InObFuncPtr:TCreateInObFunc;ScreenObjFunc:TAddComponentFunc);
var
    myRec:TNodeFuncsLookup;
    l:integer;
begin
  if NodeType='' then EXIT;
  l:=Length(NodeFuncsLookup);
  SetLength(NodeFuncsLookup,l+1);
  myRec.NodeType:=NodeType;
  myRec.InObFunctionPtr:=InObFuncPtr;            // To create an interface object of this type (not relevant for Lazarus runtime)
  myRec.ScreenObjFunctionPtr:=ScreenObjFunc;     // To create a screen object of this type (not relevant for Lazarus runtime)
  NodeFuncsLookup[l]:=myRec;
end;
{$endif}

function LookupComponentFunc(NodeType:string):TAddComponentFunc;
var
  i:integer;
begin
  i:=0;
  while i < length(NodeFuncsLookup) do
  begin
    if NodeFuncsLookup[i].NodeType=NodeType then
    begin
      result:= NodeFuncsLookup[i].ScreenObjFunctionPtr;
      i:=length(NodeFuncsLookup);
    end;
    i:=i+1;
  end;
end;

function NodeIsDescendantOf(ThisNode:TDataNode;AncestorName:string):integer;
var
  myresult:integer;
  parentNode,CurrentNode:TDataNode;
  done:boolean;
begin
  myresult:=-1;
  if ThisNode.NodeName = AncestorName then
    myresult:=0
  else
  begin
    done:=false;
    CurrentNode:=ThisNode;
    while done=false do
    begin
      if CurrentNode.NodeName=SystemRootName then
      begin
        done:=true;
        myresult:=-1;
      end
      else
      begin
        ParentNode:=FindParentOfNode(SystemNodeTree,CurrentNode.NodeName);
        if (ParentNode<>nil) then
        begin
           myresult:=myresult+1;
           if ParentNode.NodeName=AncestorName then
           begin
             done:=true;
           end
           else
             CurrentNode:=ParentNode;
        end
        else
        begin
          done:=true;
          myresult:=-1;
        end;
      end;

    end;
  end;
  result:=myresult;
end;


{$ifndef JScript}
procedure SetAlignmentParameters(MyObj,MyParent:TWinControl);
var
  ParentAlignChildrenVertical:Boolean;
  PropInfo: PPropInfo;
begin
  if (MyObj<>nil) and (MyParent<>nil) then
  begin
    // Sort out alignment within the parent control...
    ParentAlignChildrenVertical := GetBooleanProperty(MyParent, 'AlignChildrenVertical');

    if (ParentAlignChildrenVertical) then
      MyObj.Align:=alTop
    else
      MyObj.Align:=alLeft;

  end;
end;
procedure SetAlignmentParameters(ParentNode, MyNode:TDataNode);
var
  MyObj,MyParent:TWinControl;
  ParentAlignChildrenVertical:Boolean;
  PropInfo: PPropInfo;
begin
  // Sort out alignment within the parent control...
  MyObj:=TWinControl(MyNode.ScreenObject);
  MyParent:=TWinControl(ParentNode.ScreenObject);
  SetAlignmentParameters(MyObj,MyParent);
end;
{$endif}


procedure AddChildToParentNode(var ParentNode, ChildNode:TDataNode; position:integer);
var numchildren:integer;
    i:integer;
    pn:TDataNode;
begin
  // remove the node from its existing parent, if any
  pn:= FindParentOfNode(SystemNodeTree,ChildNode.NodeName,false);
  if pn<>nil then
  begin
    pn.RemoveChildNode(ChildNode);
  end;

  // add the node to its new parent
  numchildren:= Length(ParentNode.ChildNodes);
  SetLength(ParentNode.ChildNodes,numchildren+1) ;

  if position=-1 then
  begin
    ParentNode.ChildNodes[numchildren]:=ChildNode;
  end
  else
  begin
     for i:=numchildren downto position+1 do
     begin
        ParentNode.ChildNodes[i]:=ParentNode.ChildNodes[i-1];
     end;
     ParentNode.ChildNodes[position]:=ChildNode;
  end;
  //if ParentNode=SystemNodeTree then showmessage('added '+ChildNode.NodeName+' to sysnodetree  tree has '+inttostr(Length(ParentNode.ChildNodes))+' children');
  {$ifndef JScript}
  if ChildNode.NodeClass='UI' then
    SetAlignmentParameters(ParentNode,ChildNode);
  {$endif}

end;

function NodeNameIsUnique(NodeName:string; showerror:Boolean):Boolean;
var
  myresult:Boolean;
  founditem:TDataNode;
begin
  myresult:=true;
  founditem:=FindDataNodeById(SystemNodeTree,NodeName,false);
  if (founditem<>nil) and (founditem.NodeName=NodeName) then
  begin
    if showerror then
      ShowMessage('Error. Name >'+NodeName+'< is not unique when creating a new object' );
    myresult:=false;
  end;
  result:=myresult;
end;

function AddChildToDataNode(ParentNode:TDataNode;MyClass,MyName,MyType:string;MyAttributes:TNodeAttributesArray;
                           position:integer):TDataNode;
var numchildren:integer;
    tempDataNodeArray:TChildNodesArray;
    newNode:TDataNode;
    i:integer;
begin

  if NodeNameIsUnique(MyName,true) then
  begin
    tempDataNodeArray:= ParentNode.ChildNodes;
    numchildren:= Length(tempDataNodeArray);
    SetLength(ParentNode.ChildNodes,numchildren+1) ;

    newNode:=TDataNode.Create(MyClass,MyName,MyType);
    newNode.NodeAttributes:=MyAttributes;

    if position=-1 then
    begin
      ParentNode.ChildNodes[numchildren]:=newNode;
      result:=  ParentNode.ChildNodes[numchildren];
    end
    else
    begin
       for i:=numchildren downto position+1 do
       begin
          ParentNode.ChildNodes[i]:=ParentNode.ChildNodes[i-1];
       end;
       ParentNode.ChildNodes[position]:=newNode;
       result:=  ParentNode.ChildNodes[position];
    end;
  end
  else
    result:=nil;
end;

function CopyNode(SourceNode:TDataNode):TDataNode;
// recursive
var
  NewNode:TDataNode;
  myAttribs:TNodeAttributesArray;
  myEventHandlers:TEventHandlers;
  i:integer;
begin
  setlength(myAttribs,length(SourceNode.NodeAttributes));
  for i:=0 to length(SourceNode.NodeAttributes)-1 do
     myAttribs[i]:=SourceNode.NodeAttributes[i];

  NewNode:=TDataNode.Create(SourceNode.NodeClass, SourceNode.NodeName,SourceNode.NodeType);
  NewNode.IsDynamic:=true;
  NewNode.NodeAttributes:=myAttribs;

  NewNode.myEventTypes:=TStringList.Create;
  SetLength(NewNode.myEventHandlers,SourceNode.myEventTypes.Count);
  for i:=0 to SourceNode.myEventTypes.count-1 do
  begin
     NewNode.myEventTypes.Add(SourceNode.myEventTypes[i]);
     NewNode.myEventHandlers[i]:=nil;
  end;

  setlength(NewNode.ChildNodes,length(SourceNode.ChildNodes));
  for i:=0 to length(SourceNode.ChildNodes)-1 do
    NewNode.ChildNodes[i]:=CopyNode(SourceNode.ChildNodes[i]);

  result:=NewNode;
end;

function InsertSystemNode(ParentNode,SourceNode:TDataNode;Position:integer):TDataNode;
// Dynamic runtime creation of a new component
var
    myparent,myself:TDataNode;
    n,i:integer;
    TempChildNodes:TChildNodesArray;
    fn:TAddComponentFunc;
//laz...type TAddComponentFunc = function(myParent:TObject;ObjectName:String;position:integer): TDataNode;
//html..type TAddComponentFunc = function(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer): TDataNode;
begin
  //showmessage('InsertSystemNode parent='+ParentNode.Nodename);
  myparent:=ParentNode;
  if (myParent<>nil)
  and (myparent.NodeName<>'')
  then
  begin
    // create the screen object and data node...
    fn:=LookupComponentFunc(SourceNode.NodeType);
    if fn<>nil then
    begin
      {$ifndef JScript}
      //Create Widget (also creates datanode)
      myself:=fn(parentNode,SourceNode.NodeName,position);
      for i:=0 to length(SourceNode.NodeAttributes)-1 do
      begin
        myself.SetAttributeValue(SourceNode.NodeAttributes[i].AttribName,SourceNode.NodeAttributes[i].AttribValue);
      end;
      {$else}
      //Create Interface Object (datanode)
      //showmessage('create inob '+SourceNode.NodeType);
      myself:=TDataNode(CreateInterfaceObject(ParentNode.MyForm,SourceNode.NodeType, SourceNode.NodeName));
      for i:=0 to length(SourceNode.NodeAttributes)-1 do
      begin
        myself.SetAttributeValue(SourceNode.NodeAttributes[i].AttribName,SourceNode.NodeAttributes[i].AttribValue);
      end;
      //Create Widget
      //showmessage('create widget '+SourceNode.NodeType);
      myself:= fn(myself, ParentNode,SourceNode.NodeName,position);
      //showmessage('done');
      {$endif}

      myself.IsDynamic:=true;
      mySelf.SetAttributeValue( 'ParentName', ParentNode.NodeName);

      AddChildToParentNode(myparent,myself,position);

      // now insert any child nodes
      for i:=0 to length(SourceNode.ChildNodes)-1 do
        InsertSystemNode(myself,SourceNode.ChildNodes[i],-1);
    end
    else
    begin
      showmessage('No function defined to instantiate component of type '+SourceNode.NodeType);
      myself:=nil;
    end;
  end;

  result:=myself;
end;

function AttribsFromXML(attributeList:TStringList;offset:integer;var ParentName:String;var NewLink:TXPropertyLink):TNodeAttributesArray;
var
  myAttribs:TNodeAttributesArray;
  LinkSet,AttribBits:TStringList;
  i:integer;
begin
  //showmessage('AttribsFromXML. count='+inttostr(attributeList.count)+' offset='+inttostr(offset));
  setlength(myAttribs,attributeList.count-offset);
  for i:=offset to attributeList.count-1 do
  begin
    AttribBits :=  stringsplit(attributeList[i],AttribBitsDelimiter);
    myAttribs[i-offset].AttribName:=TrimWhiteSpace(AttribBits[0]);
    myAttribs[i-offset].AttribType:=TrimWhiteSpace(AttribBits[1]);
    myAttribs[i-offset].AttribValue:=  UnSubstituteSpecials(AttribBits[2]);
    myAttribs[i-offset].AttribReadOnly:=myStrToBool(TrimWhiteSpace(AttribBits[3]));
    if myAttribs[i-offset].AttribName='ParentName' then
      ParentName:=AttribBits[2];
    if myAttribs[i-offset].AttribName='Link' then
    begin
      LinkSet:= stringsplit(myAttribs[i-offset].AttribValue,AttribLinkDelimiter);
      if LinkSet.count>1 then
      begin
       // showmessage('creating link for '+LinkSet[0]+' '+LinkSet[1]);
        NewLink:=TXPropertyLink.Create;
        NewLink.TIObjectName := LinkSet[0];
        NewLink.TIPropertyName := LinkSet[1];
      end;
    end;
  end;

  result:=myAttribs;
end;


{$ifndef JScript}
function myCopyToClip(stringname,instring:string):string;
begin
  Clipboard.AsText :=instring;
  Showmessage('The '+stringname+' has been saved to the Clipboard');
end;


function addComponentFromXML(XMLString:String):string;
 var
     ParentName,ScreenObjectName,ClassName,ScreenObjectType:string;
     attributeList:TStringList;
     NameValuePair:TStringList;
     i,pos:integer;
     myAttribs:TNodeAttributesArray;
     ParentNode:TDataNode;
     mynode,SourceNode:TDataNode;
     NodeString:String;
     fn:TAddComponentFunc;
     NewLink:TXPropertyLink;
  begin
  //  ShowMessage('addComponentFromXML  : '+XMLString);

   NodeString:=XMLString;

   //showmessage(ScreenObjectType+' '+ClassName+' '+ScreenObjectName);
   attributeList:= stringsplit(NodeString,attributeListdelimiter);
   // first find the node class, type and name
   ScreenObjectType:=attributeList[0];

   NameValuePair :=  stringsplit(attributeList[1],NameValuePairdelimiter);
   ClassName := TrimWhiteSpace(NameValuePair[1]);

   NameValuePair :=  stringsplit(attributeList[2],NameValuePairdelimiter);
   ScreenObjectName := TrimWhiteSpace(NameValuePair[1]);

    if (ScreenObjectName <> SystemRootName)         // this already exists
    and (ClassName <> 'Root') then                  // these already exist
    begin
     myAttribs := AttribsFromXML(attributeList,3,ParentName,NewLink);

     ParentNode:=FindDataNodeByID(SystemNodeTree,ParentName,true);


     // If the object was defined in a Lazarus form at design time,
     // the necessary data node (interface object) should already have been created.
     // Find it, add it to the identified parent, and set the relevant attributes.
     myNode:=FindDataNodeById(SystemNodeTree,ScreenObjectName,false);

     // If the object was created dynamically at run time, however, then we now need to
     // create a node for it.
     if myNode=nil then
     begin
       //showmessage('creating dynamic component '+ScreenObjectType+'; '+ ClassName+'; '+ScreenObjectName);
       SourceNode:=TDataNode.Create(ClassName,ScreenObjectName,ScreenObjectType,true);
       SourceNode.NodeAttributes:=myAttribs;
       myNode:=SourceNode;
       // Create a screen object by running the registered instantiation function for the node type
       InsertSystemNode(ParentNode,myNode,-1);
     end
     else
     begin
       ReparentNode(myNode,ParentNode);
       if (ParentNode.ScreenObject<>nil) then
         InsertUnderParent(TWinControl(MyNode.ScreenObject),TWinControl(ParentNode.ScreenObject),-1);
       for i:=0 to length(myattribs)-1 do
       begin
         mynode.SetAttributeValue(myAttribs[i].AttribName,myAttribs[i].AttribValue);
       end;
     end;

   end;
 end;

{$else}




procedure TInterfaceObject.SetLink(const AValue: TXPropertyLink);
begin
  if FLink=AValue then exit;
  FLink:=AValue;
end;

function LinkToStr(ALink:TXPropertyLink):string;
begin
  result:=ALink.TIObjectName;
  result:=result + AttribLinkDelimiter + ALink.TIPropertyName;
end;


procedure TInterfaceObject.LinkLoadFromProperty(Sender: TObject);
begin
  if Sender=nil then ;

  //writeln('TTICustomEdit.LinkLoadFromProperty A ',Name,
  //  ' FLink.GetAsText=',FLink.GetAsText,' Text=',Text,
  //  ' PropName=',FLink.TIPropertyName);
  //showmessage('loadfromproperty');

  SetAttributeValue('Link',LinkToStr(Link));

end;

procedure TInterfaceObject.LinkSaveToProperty(Sender: TObject);
begin
end;

procedure SetInterfaceProperty(myName,PropName,NewValue:string);
// set the property value on the interface object
var
  myObj:TObject;
  MyPropType:TTypeKind;
begin
  //showmessage('setintfprop. name='+myname+' prop='+PropName+' value='+NewValue);
  myObj:=TObject(FindDataNodeById(SystemNodeTree,myName,false));
  if myObj<>nil then
  begin
//    SetStringProp(myObj,PropName,NewValue);
    MyPropType := PropType(myObj, PropName);
    if MyPropType = tkString then
    begin
      //showmessage('set string prop');
      SetStringProp(myObj,PropName,NewValue);
    end
    else if MyPropType = tkBool then
    begin
      //showmessage('set boolean prop '+PropName);
      SetBoolProp(myObj,PropName,myStrToBool(NewValue));
    end
    else
      showmessage('SetInterfaceProperty.  Need to handle property type for '+PropName);
  end;
end;

function LookupNodeInObFunc(NodeType:string):TCreateInObFunc;
var
  i:integer;
begin
  i:=0;
  while i < length(NodeFuncsLookup) do
  begin
    if NodeFuncsLookup[i].NodeType=NodeType then
    begin
      result:= NodeFuncsLookup[i].InObFunctionPtr;
      i:=length(NodeFuncsLookup);
    end;
    i:=i+1;
  end;
end;

procedure InitFormObject(myForm:TForm;NodeName:String);
var
  myNode:TDataNode;
begin
  myForm.Name:=NodeName;
  myNode:=AddFormToNodeTree(myForm);
end;

function CreateInterfaceObject(MyForm:TForm;NodeType, NodeName:String):TObject;
var
  myObj:TObject;
  inobFn:TCreateInObFunc;
  mynode:TDataNode;
begin
  //showmessage('CreateInterfaceObject '+NodeType+' '+NodeName);
  // Find the interface object and link to this node
  // Object creation procs are stored in NodeFuncLookup.
  // Find the creation func for this node type, and execute it.
  inobFn:=LookupNodeInObFunc(NodeType);
  if inobFn=nil then
    showmessage('no interface object creation function for '+myNode.NodeName)
  else
  begin
    myObj:=inobFn(MyForm,NodeName);
  end;
  //showmessage('CreateInterfaceObject '+NodeName+' done');
  result:=myObj;
end;

function addComponentFromXML(XMLString:String):string;
 var
     ParentName,ScreenObjectName,ClassName,ScreenObjectType,mf:string;
     attributeList:TStringList;
     NameValuePair,AttribBits:TStringList;
     i,l:integer;
     myAttribs:TNodeAttributesArray;
     ParentNode:TDataNode;
     mynode:TDataNode;
     NodeString:String;
     fn:TAddComponentFunc;
     NewLink:TXPropertyLink;
     myDynamicWrapper:TWrapperPanel;
     myPopup:TObject;
  begin
   //ShowMessage('addComponentFromXML  : '+XMLString);

   NodeString:=XMLString;

   //showmessage(ScreenObjectType+' '+ClassName+' '+ScreenObjectName);
   attributeList:= stringsplit(NodeString,attributeListdelimiter);
   // first find the node class, type and name
   ScreenObjectType:=attributeList[0];

   NameValuePair :=  stringsplit(attributeList[1],NameValuePairdelimiter);
   ClassName := TrimWhiteSpace(NameValuePair[1]);

   NameValuePair :=  stringsplit(attributeList[2],NameValuePairdelimiter);
   ScreenObjectName := TrimWhiteSpace(NameValuePair[1]);

   if (ScreenObjectName <> SystemRootName)         // this already exists
   and (ClassName <> 'Root') then                  // these already exist
   begin
     //NameValuePair:=  stringsplit(attributeList[3],NameValuePairdelimiter);
     //Admin:=Mystrtobool(NameValuePair[1]);
     myAttribs:=AttribsFromXML(attributeList,3,ParentName,NewLink);

     if ((ClassName='UI') and (ScreenObjectType<>''))  // and (ScreenObjectType<>'Popup'))
     or (ClassName = 'SVG') then
     begin
       if ParentName='' then
         //showmessage('parentname is blank for '+ScreenObjectName);
         ParentNode:=SystemNodeTree
       else
         ParentNode:=FindDataNodeByID(SystemNodeTree,ParentName,true);
       //if parentnode<>nil then showmessage('found parent '+ParentName +' as '+ParentNode.NodeName)

       // If the object was defined in a Lazarus form at design time,
       // the necessary data node (interface object) should already have been created.
       // Find it, add it to the identified parent, and set the relevant attributes.
       myNode:=FindDataNodeById(SystemNodeTree,ScreenObjectName,false);

       // If the object was created dynamically at run time, however, then we still need to
       // create an interface object / data node for it.
       if myNode=nil then
       begin
        //ShowMessage('addComponentFromXML  dynamic');
        //showmessage('creating dynamic component '+ScreenObjectType+'; '+ ClassName+'; '+ScreenObjectName);
         myDynamicWrapper:=TWrapperPanel(CreateInterfaceObject(ParentNode.MyForm,ScreenObjectType,ScreenObjectName));
         if myDynamicWrapper<>nil then
         begin
           myNode:=myDynamicWrapper.myNode;
           myNode.IsDynamic:=true;
         end;
         //if myDynamicWrapper<>nil then showmessage('AddComponentFromXML created dynamic node '+myNode.NodeName);
       end;
       if myNode=nil then showmessage('oops node still nil');
       //if myNode.IsDynamic then
       //  if ParentNode<>nil then showmessage(myNode.NodeName+' is child of '+ParentNode.NodeName)
       //  else showmessage('parent is nil');
       ReparentNode(myNode,ParentNode);
       //myNode.AdminNode:=Admin;

       for i:=0 to length(myattribs)-1 do
       begin
         mynode.SetAttributeValue(myAttribs[i].AttribName,myAttribs[i].AttribValue);
       end;

       if myNode is TInterfaceObject then
         TInterfaceObject(myNode).Link := NewLink;

       mf:=MainForm.Name;
       asm
         if (ScreenObjectName!=mf) {
           // object may already exist if this is a system re-load, so delete the old one.
           var ob = document.getElementById(ScreenObjectName);
           if (ob!=null) {
             //alert('found '+ScreenObjectName);
             var Parent = pas.HTMLUtils.ScreenObjectInnerComponent(ParentNode);
             if (Parent!=null) {
               //alert('deleting '+ScreenObjectName);
               Parent.removeChild(ob); }
           }
         }
       end;
       // Create a screen object by running the registered instantiation function for the node type
       fn:=LookupComponentFunc(ScreenObjectType);
       fn(myNode,ParentNode,ScreenObjectName,-1);
     end ;
   end;
   //showmessage('FromXML done');
 end;
{$endif}

function AddFormToNodeTree(myForm:TForm):TdataNode;
var
  FormRootNode,myNode:TDataNode;
begin
  myNode:=nil;
  {$ifndef JScript}
  myNode:=TDataNode.Create('UI',myForm.Name,'Popup',false);
  myNode.ScreenObject:=myForm;
  myNode.MyForm:=myForm;
  if myForm<>MainForm then
  begin
    myNode.SetAttributeValue('Top',inttostr(myForm.Top));
    myNode.SetAttributeValue('Left',inttostr(myForm.Left));
    myNode.SetAttributeValue('Height',inttostr(myForm.Height));
    myNode.SetAttributeValue('Width',inttostr(myForm.Width));
    myNode.SetAttributeValue('Caption',myForm.Caption);
  end;
  {$else}
  myNode:=TDataNode(TXPopup.Create('UI',myForm.Name,'Popup',false));
  myNode.ScreenObject:=myForm;
  myNode.MyForm:=myForm;
  {$endif}

  AddChildToParentNode(UIRootNode,myNode,-1);

  result:=myNode;
end;


procedure NilScreenObject(MyNode:TdataNode);
var
  i:integer;
begin
  if (MyNode.ScreenObject<>nil) then
  begin
    MyNode.ScreenObject:=nil;
  end;


  for i:=0 to length(MyNode.ChildNodes)-1 do
  begin
     NilScreenObject(MyNode.ChildNodes[i]);
  end;
end;

procedure ReParentNode(MyNode,NewParent:TDataNode);
var
  OldParent:TDataNode;
begin
  // called during load from XML
  OldParent:=FindParentOfNode(SystemNodeTree,MyNode.NodeName,false);
  if OldParent<>nil then
  begin
    OldParent.RemoveChildNode(MyNode);
  end;
  AddChildToParentNode(NewParent,MyNode,-1);
end;


procedure ClearAllDynamicNodes(StartNode:TDataNode);
var
  i:integer;
begin
  for i:=length(StartNode.ChildNodes)-1 downto 0 do
  begin
    ClearAllDynamicNodes(StartNode.ChildNodes[i]);
    if StartNode.ChildNodes[i].IsDynamic then
      DeleteNode(StartNode,StartNode.ChildNodes[i]);
  end;
end;




{$ifndef JScript}

function DeleteScreenObject(MyNode:TDataNode):string;  overload;
var
    ThisObject:TObject;
begin
   if (MyNode.ScreenObject<>nil) then
     if Assigned(MyNode.ScreenObject) then
       FreeAndNil(MyNode.ScreenObject);  // this will free all child objects at same time
   NilScreenObject(MyNode);              // this clears all the pointers to the (now freed) children
end;

function DeleteScreenObject(ThisObject:TObject):string; overload;
begin
   if ThisObject<>nil then
   begin
     FreeAndNil(ThisObject);
   end;
end;

function ScanChildrenForWinControl(CurrentItem:TWinControl;ScreenObjectID:String):TWinControl;
var FoundItem,TempItem:TWinControl;
    TempControl :TControl;
    i:integer;
begin
   FoundItem:= nil;
   for i:=0 to CurrentItem.ControlCount - 1 do
   begin
      if FoundItem = nil then  // object has not been found so keep looking
      begin
        TempControl:= CurrentItem.Controls[i];
        begin
          if TempControl is TWinControl then   // it might be the wincontrol we are looking for..... or it may have children so search them for the object
          begin
            TempItem := TWinControl(TempControl);
            if Trim(Uppercase(TempItem.name)) = Trim(Uppercase(ScreenObjectID))
            then  FoundItem:= TempItem
            else  TempItem:= ScanChildrenForWinControl(TempItem,ScreenObjectID);
            if  TempItem<>nil
            then begin if TempItem.name = ScreenObjectID then  FoundItem:= TempItem; end;
          end;
        end ;
      end;
   end;
   ScanChildrenForWinControl:= FoundItem;
end;

function FindObjectByID(WithinForm:TForm;ScreenObjectID:String):TWinControl;
var FoundItem, TempItem :TWinControl;
    i:integer;
begin
   FoundItem:= nil;
   TempItem:=ScanChildrenForWinControl(WithinForm,ScreenObjectID);
   if TempItem <> nil
   then
   begin
       FoundItem:= TempItem ;
       //showmessage('Success .........>'+ScreenObjectID+'< has been found');
   end
   else
     showmessage('Error in Utilities.FindObjectByID >'+ScreenObjectID+'< not found');

   FindObjectByID:=FoundItem;
end;

procedure ClearAllScreenObjects;
// Delete all the screen objects, in all the forms, and delete dynamic popup forms
var temp:TWinControl;
  i,j,n:integer;
  fm:TForm;
  tempstr:string;
begin
  for j := Screen.FormCount - 1 downto 0 do
  begin
    fm := Screen.Forms[j];
    if fm = MainForm then
    begin
      n:=fm.ControlCount;
      for i := n-1 downto 0 do
      begin
         temp:=TWinControl(fm.Controls[i]);
         tempstr:=fm.Controls[i].Name;
         DeleteScreenObject(fm.Controls[i]);
      end;
    end
    else
    begin
      n:=fm.ControlCount;
      for i := n-1 downto 0 do
      begin
         temp:=TWinControl(fm.Controls[i]);
         tempstr:=fm.Controls[i].Name;
         DeleteScreenObject(fm.Controls[i]);
      end;
      fm.Destroy;
    end;
  end;




end;

function FindOuterParentOf(InnerControl:TWinControl):TWinControl;
begin
  if (InnerControl.Name<>'') and (FoundString(InnerControl.Name,'Contents')=0) then
    result:=InnerControl
  else if InnerControl.Parent<>nil then
    result:=FindOuterParentOf(InnerControl.Parent)
  else
    result:=nil;
end;



{$Else}
function DeleteScreenObject(MyNode:TDataNode):string;
var
    ObjName:string;
begin
   ObjName:=MyNode.NodeName;

  asm
    try{
    var ThisObject = document.getElementById(ObjName);
    if (ThisObject!=null) {
       ThisObject.parentNode.removeChild(ThisObject);
      }
    }catch(err) { alert(err.message+' in NodeUtils.DeleteScreenObject');}
  end;

  NilScreenObject(MyNode);

end;
//procedure ClearAllScreenObjects;
//var
//    SystemRootNameVar:string;
//begin
//  SystemRootNameVar:=SystemRootName;
//
//  asm
//try{
//       var self=document.getElementById(SystemRootNameVar);
//       while (self.firstChild)
//         { self.removeChild(self.firstChild);}
//    }catch(err) { alert(err.message+'  in NodeUtils.ClearAllScreenObjects');}
//
//end;
//
//end;
{$endif}

procedure DeleteNodeChildren(ParentNode:TDataNode);
var
    i:integer;
    //handlers:TEventHandlers;
begin
//ShowMessage('dnc '+ParentNode.NodeName);
  if ParentNode=nil then
    ShowMessage('parentnode nil in DeleteNodeChildren')
  else
  begin
     // delete the child screenobjects
     for i:=0 to length(ParentNode.ChildNodes)-1 do
     begin
        //ShowMessage('i='+inttostr(i)+' deleting children of node '+ParentNode.ChildNodes[i].NodeName);
        //setlength(handlers,0);
        //ParentNode.ChildNodes[i].RegisteredEventHandlers:=handlers;
        DeleteNodeChildren(ParentNode.ChildNodes[i]);
        //ShowMessage('i='+inttostr(i)+' deleting screenobject '+ParentNode.ChildNodes[i].NodeName);
        DeleteScreenObject(ParentNode.ChildNodes[i]);
        //ShowMessage('i='+inttostr(i)+' deleting node '+ParentNode.ChildNodes[i].NodeName);
        ParentNode.ChildNodes[i].DeleteMe;
     end;
     // Clear out all the child nodes
     setlength(ParentNode.ChildNodes,0);
   end;
end;

procedure DeleteNode(ParentNode,MyNode:TDataNode);
var
    siblings:TChildNodesArray;
    i,j:integer;
begin
  SuppressEvents:=true;
  //setlength(handlers,0);
  //MyNode.RegisteredEventHandlers:=handlers;
  // delete my child nodes
  DeleteNodeChildren(MyNode);
  // delete my screen object
  DeleteScreenObject(MyNode);
  // remove me from my parent
  setlength(siblings,0);
  if ParentNode=nil then
    ParentNode:=FindParentOfnode(SystemNodeTree,MyNode.NodeName);
  if ParentNode<>nil then
  begin
    ParentNode.RemoveChildNode(MyNode);
//    j:=0;
//    for i:=0 to length(ParentNode.ChildNodes)-1 do
//    begin
//      if ParentNode.ChildNodes[i].NodeName<>MyNode.NodeName then
//      begin
//        setlength(siblings,j+1);
//        siblings[j]:=ParentNode.ChildNodes[i];
//        j:=j+1;
//      end;
//    end;
//    ParentNode.ChildNodes:=siblings;
  end;

  MyNode.DeleteMe;
  SuppressEvents:=false;
end;

function checkData(SystemDescription:string):boolean;
var teststring:string;    // this checs a longer string after un encryption than the function isvalidsystemdata
   i:integer;
   MatchFound:boolean;
begin
  MatchFound:=true;
  teststring :='<Root|; Class |=R';
  for i :=1 to Length(teststring) do
  begin
     if (SystemDescription[i]<> teststring[i])
     then  MatchFound:=false;
  end;
  result:=MatchFound;
end;

function XMLToNodeTree(XMLString:String):String;
var i,ArrayLength:integer;
     TempChar,NextChar,NewString:String;
     BracesToggleFlag:boolean;
     StringList:TStringList;
     wasRecording:boolean;
     wasUndoRedoRecording:boolean;
     RootNodeName:string;
begin

  if checkData(XMLString)= true then
  begin
    StringList:=stringsplit(XMLString,delimiterBetweenTheEventHistoriesAndSystemDescription);

    StartingUp:=true;
   {$ifdef JScript}
   RootNodeName:=SystemRootName;
   asm
      var ob=document.getElementById(RootNodeName);
      $mod.SystemNodeTree.ScreenObject=ob;
    end;

    {$endif}

    NewString:='';
    for i:=1 to Length(XMLString) do
    begin
      TempChar:=XMLString[i];
      if ( TempChar='<') then  // start recording this node string unless it is a closing "</ mytype>"
      begin
        Tempchar:='';// do not save the '<' char to the string being parsed
        BracesToggleFlag:=true;
        NextChar:= XMLString[i+1];
        if NextChar = '/' then BracesToggleFlag:=false;
      end;
      if  TempChar = '>' then  //stop recording this node string and process it before moving on to the next item
      begin
        if (BracesToggleFlag=true) then
        begin
          addComponentFromXML(newstring);// process the record string if this was a leading xml string such as "< type; name= ''; attr_1 = '???' etc>"
        end;
        BracesToggleFlag:=false;
        newstring:='';
      end;
      if BracesToggleFlag=true
      then
      begin
        newstring:=newstring+TempChar;
      end;
    end;

    InitialiseLinks(SystemNodeTree);

//ShowMessage('all components added');
    StartingUp:=false;

  end
  else
    ShowMessage('Error .....Unable to load data');

end;

{$ifndef JScript}
procedure Initialiselinks(StartNode:TDataNode);
var
  i:integer;
  targetNode:TDataNode;
  myLink:TXPropertyLink;
begin
  // system has been loaded.  Look for links specified in all nodes and
  // locate the named target nodes.
 //showmessage('init links '+StartNode.NodeName);

  //!!!!  TBA

  for i := 0 to length(StartNode.ChildNodes) -1 do
  begin
//     showmessage('child '+inttostr(i)+' of '+StartNode.NodeName+' - '+StartNode.ChildNodes[i].NodeName);
     Initialiselinks(StartNode.ChildNodes[i]);
  end;
end;
{$else}
procedure Initialiselinks(StartNode:TDataNode);
var
  i:integer;
  targetNode:TDataNode;
  myLink:TXPropertyLink;
begin
  // system has been loaded.  Look for links specified in all nodes and
  // locate the named target nodes.
 //showmessage('init links '+StartNode.NodeName);
  if (StartNode is TInterfaceObject) then
    if TInterfaceObject(StartNode).Link <> nil then
    begin
      myLink:=TInterfaceObject(StartNode).Link;
      if myLink.TIObjectName<>'' then
      begin
        targetNode:=FindDataNodeById(SystemNodeTree,myLink.TIObjectName,false);
        if targetNode<>nil then
           myLink.TIObject := targetNode
        else
           showmessage('Initialiselinks.  Node is nil. '+myLink.TIObjectName);
      end;
    end;
  for i := 0 to length(StartNode.ChildNodes) -1 do
  begin
//     showmessage('child '+inttostr(i)+' of '+StartNode.NodeName+' - '+StartNode.ChildNodes[i].NodeName);
     Initialiselinks(StartNode.ChildNodes[i]);
  end;
end;

procedure PushTolinks(AObject:TObject; PropName:string; PropValue:String; StartNode:TDataNode);
var
  i:integer;
  MyPropType:TTypeKind;
begin
  // a component property has changed.  Look for links in other components that
  // reference this property, and update those values.
  // Propname is the property in AObject that has changed.
 //showmessage('PushTolinks 1. '+StartNode.NodeName);
  if (StartNode is TInterfaceObject) then
    if TInterfaceObject(StartNode).Link <> nil then
      if (TInterfaceObject(StartNode).Link.FTIObject = AObject)
      and (TInterfaceObject(StartNode).Link.TIPropertyName = PropName) then
      begin
        MyPropType := PropType(AObject, PropName);
        if MyPropType = tkString then
          SetStringProp(AObject,PropName,PropValue)
        else if MyPropType = tkBool then
          SetBoolProp(AObject,PropName,myStrToBool(PropValue))
        else
          showmessage('PushTolinks.  Need to handle property type for '+PropName);
      end;
  //showmessage('PushTolinks 2');
  //for i := 0 to length(TDataNode(AObject).ChildNodes) -1 do
  for i := 0 to length(StartNode.ChildNodes) -1 do
     PushToLinks(AObject,PropName,PropValue,StartNode.ChildNodes[i]);
  //showmessage('PushTolinks done');
end;

{$endif}

procedure InitSystemNodetree;
begin
  SystemNodetree:=TDataNode.Create('Root','ApplicationRoot','Root',false);
  // create a parent node for all UI nodes
  UIRootNode:=TDataNode.Create('Root',SystemRootName,'Root',false);
  AddChildToParentNode(SystemNodeTree,UIRootNode,-1);
end;

//-------------------------------------------------------------------------------------------
begin
 // HTMLNoWrapperPanel:=TStringlist.Create;
  InitSystemNodeTree;
  SuppressEvents:=false;

end.


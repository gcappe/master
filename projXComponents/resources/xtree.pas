unit XTree;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls, Graphics, Dialogs, ExtCtrls,ComCtrls,
  TypInfo, Propedits,RTTICtrls,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel;

{$endif}

type TTreeNodeHint = function(TreeLabelStr:String):String of object;


{$ifndef JScript}
type TXTree=class;
type TTreeNodeDropAccepted = function(myTree:TXTree; SourceName:String):Boolean of object;
function SetTreeNodeSelected(TreeComponent:TDataNode;NodeText:string):string;

{$else}

function addTreeStyles(dummy:string):string;
function addTreeNode(WrapperNodeId,TreeName,parentName,NameOfDetailsList,SummaryText,HasChildren,color,isopen:string;
                 NodeHintFunc:TTreeNodeHint):string;
function addnode(WrapperNodeId,TreeName,ParentName,currentNodeTree,SelectedNodeString,IdOfNodeBeingAdded:string;
                 level:integer;normalColor:String;NodeHintFunc:TTreeNodeHint):string;
function SetOpenStatusOfNodeChildren(NodeName,NameOfSelectedNode:string;level:integer):boolean;
function GetTreeRootID(NodeID:String):String;
function deselectNodeChildren(NodeName,normalColor:String):string;
function clearNodeSelectedMarker(NameOfDetailsList,normalColor:String):string;
function OpenAndScrollToSelectedNode(NameOfDetailsList:string):string;
procedure SetTreeNodeSelected(TreeComponent:TDataNode;NodeId:string);
procedure clearTreeNode(parentName:string);
procedure HandleTreeNodeClick(WrapperNodeId,SelectedNodeText:String);
function HandleTreeNodeDragOver(ob:TObject;str1,str2:string): Boolean;
function NodeIdFromText(TreeNode:TDataNode;NodeText:String):String;

{$endif}




{$ifndef JScript}
type
PNodeDataRec = ^TNodeDataRec;
TNodeDataRec = record
  NodeHint: string;
end;
//type TTreeNodeHint = function(TreeLabelStr:String):String of object;


type TMyTreeView = class(TTreeView)
  public
     var
  lastHintNode : TTreeNode;
  IsDropTarget:Boolean;

  procedure CustomDrawTreeNode(Sender: TCustomTreeView;Node: TTreeNode; State: TCustomDrawState;var DefaultDraw: Boolean) ;
  procedure TreeSelectedNodeChange(Sender: TObject; Node: TTreeNode);
  procedure HandleClick(Sender: TObject) ;
  procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) ;
  procedure HandleDragDrop(Sender, Source: TObject; X, Y: Integer);
  procedure HandleDragOver(Sender, Source: TObject; X, Y: Integer;
                                   State: TDragState; var Accept: Boolean);
  procedure HandleMouseLeave(Sender: TObject) ;
  function NodeHint(tn: TTreeNode): string;
  procedure PopulateMeFromData(NodeTreeString,SelectedNodeString:String);
  procedure selectNode(NodeText:String);
  function AddNewNode(NodeItems:TTreeNodes;ParentNode:TTreeNode; NodeString:string):TTreeNode;
  procedure ExpandTreeNodes(Nodes: TTreeNodes; Level: Integer);
  procedure SetNodeHint(Sender: TObject; Shift: TShiftState; X, Y: Integer) ;
end;

type
  TXTree = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;
    fHandleChange:TEventHandler;
    fHandleDragStart:TEventHandler;
    fHandleDrop:TEventHandler;
    fTreeNodeHint:TTreeNodeHint;
    fTreeNodeDropAccepted:TTreeNodeDropAccepted;

    procedure SetMyEventTypes;

    procedure TreeClick(Sender:TObject);

    function GetTreeData:string;
    function GetSelectedNodeName:string;
    function GetTreeWidth:string;
    function GetTreeHeight:string;

    procedure SetTreeData(NodeTreeString:string);
    procedure SetSelectedNodeName(AValue:string);
    procedure SetTreeWidth(AValue:string);
    procedure SetTreeHeight(AValue:string);

  protected
    { Protected declarations }
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
    property ParentColor;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;

  published
    { Published declarations }

    // Properties defined for this class...
    property SelectedNodeName: String read GetSelectedNodeName write SetSelectedNodeName;
    property TreeData: String read GetTreeData write SetTreeData;
    property TreeHeight: String read GetTreeHeight write SetTreeHeight;
    property TreeWidth: String read GetTreeWidth write SetTreeWidth;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
    property HandleTreeNodeClick: TEventHandler read FHandleChange write FHandleChange;
    property HandleDragStart: TEventHandler read FHandleDragStart write FHandleDragStart;
    property HandleDrop: TEventHandler read FHandleDrop write FHandleDrop;

    property TreeNodeHintFunc: TTreeNodeHint read FTreeNodeHint write FTreeNodeHint;
    property TreeNodeDropAccepted: TTreeNodeDropAccepted read FTreeNodeDropAccepted write FTreeNodeDropAccepted;
  end;


  procedure Register;


{$else}
  type TTreeNodeDropAccepted = function(myTree:TObject; SourceName:String):Boolean of object;
type
  TXTree = class(TWrapperPanel)
  private
    fTreeNodeHint:TTreeNodeHint;
    fTreeNodeDropAccepted:TTreeNodeDropAccepted;
    procedure SetMyEventTypes;

    function GetTreeData:string;
    function GetSelectedNodeName:string;
    function GetTreeWidth:string;
    function GetTreeHeight:string;

    procedure SetTreeData(NodeTreeString:string);
    procedure SetSelectedNodeName(AValue:string);
    procedure SetTreeWidth(AValue:string);
    procedure SetTreeHeight(AValue:string);

  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);

  published
    { Published declarations }
    property SelectedNodeName: String read GetSelectedNodeName write SetSelectedNodeName;
    property TreeData: String read GetTreeData write SetTreeData;
    property TreeHeight: String read GetTreeHeight write SetTreeHeight;
    property TreeWidth: String read GetTreeWidth write SetTreeWidth;

    property TreeNodeHintFunc: TTreeNodeHint read FTreeNodeHint write FTreeNodeHint;
    property TreeNodeDropAccepted: TTreeNodeDropAccepted read FTreeNodeDropAccepted write FTreeNodeDropAccepted;
  end;

const  TreeNodeHighlightColor:String = '#ffff00';          //yellow

  {$endif}


implementation

const MyNodeType='TXTree';


const
  ExampleNodeTree = '["myTreeNamemuchlongernow",["Layout","TestStuff"],"SimpleItems","Collection Items",["Media items","TestMoreStuff"],"Option Popups"]';


procedure TXTree.SetMyEventTypes;
begin
  MyEventTypes.Add('Created');
  MyEventTypes.Add('Click');
  MyEventTypes.Add('TreeNodeClick');
  MyEventTypes.Add('DragStart');
  MyEventTypes.Add('Drop');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xtree_icon.lrs}
  RegisterComponents('Misc',[TXTree]);

  // inherited from TWrapperPanel, not required here
  RegisterPropertyEditor(TypeInfo(TColor), TXTree, 'BgColor', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TXPropertyLink), TXTree, 'Link', THiddenPropertyEditor);
end;

constructor TXTree.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXTree.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXTree.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  myControl:=TmyTreeView.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  TmyTreeView(myControl).DragMode:=dmAutomatic;
  TmyTreeView(myControl).IsDropTarget:=false;

  TmyTreeView(myControl).OnChange := @TmyTreeView(myControl).TreeSelectedNodeChange;
  TmyTreeView(myControl).OnCustomDrawItem:=@TmyTreeView(myControl).CustomDrawTreeNode;
  TmyTreeView(myControl).OnClick := @TmyTreeView(myControl).HandleClick;
  TmyTreeView(myControl).OnMouseMove := @TmyTreeView(myControl).HandleMouseMove;           // set node hint; DragStart
  TmyTreeView(myControl).OnMouseLeave:=@TmyTreeView(myControl).HandleMouseLeave;           // IsDropTarget:=false
  TmyTreeView(myControl).OnDragDrop := @TmyTreeView(myControl).HandleDragDrop;             // 'Drop' - stop drag mode
  TmyTreeView(myControl).OnDragOver := @TmyTreeView(myControl).HandleDragOver;             // is drop allowed

  TmyTreeView(myControl).Options:=TTreeView(myControl).Options - [tvoThemedDraw];
  TmyTreeView(myControl).SelectionColor:=clYellow;  // only works when options are set as above

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

  //self.AutoSize:=false;
  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;


  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  AddLabel(myControl);
  Hint:='';
  SelectedNodeName:='';
  TreeData:=ExampleNodeTree;
  TreeHeight:='150';
  TreeWidth:='200';
  BgColor:=clWhite;

  LabelText:='Tree view';
  LabelPos:='Top';

end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXTree;
  NewNode:TDataNode;
begin
  NewWidget:=TXTree.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
//  NewWidget.Parent:=TWinControl(ParentNode.ScreenObject);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

function TmyTreeView.NodeHint(tn: TTreeNode): string;
var
  MyRecPtr: PNodeDataRec;
begin
  if tn<>nil then
  begin
    MyRecPtr:=tn.Data;
  end;
  if (MyRecPtr<>nil)
//  and (MyRecPtr^ is TNodeDataRec)
  then
    try
    result:=MyRecPtr^.NodeHint
    except
    result:='';
    end
  else
    result:='';
end;

procedure TmyTreeView.HandleDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Src, Dst: TTreeNode;
begin
  Src := TTreeView(Source).Selected;
  Dst := TTreeView(Sender).GetNodeAt(X,Y);
  //Src.MoveTo(Dst, naAdd);
  if (Src<>nil)
  and (Dst<>nil) then
  begin
    CallHandleEvent('Drop',Dst.Text,Sender);
  end;
  self.IsDropTarget:=false;
end;

procedure TmyTreeView.HandleDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Src,DestTreeNode: TTreeNode;
  SourceName, DestName:string;
  i:integer;
begin

  SourceName:=TTreeView(Source).parent.Name;
  DestName:=TTreeView(Sender).parent.Name;


  Accept:=true;
  // Decide whether a drop is allowed here
  if TXTree(self.Parent).TreeNodeDropAccepted<>nil then
    Accept := TXTree(self.Parent).TreeNodeDropAccepted(TXTree(self.Parent),SourceName);

  DestTreeNode := TTreeNode(TTreeView(Sender).GetNodeAt(X, Y)) ;
  Src := TTreeView(Source).Selected;
  if (Src<>nil)
  and (self.IsDropTarget = false)
  then
  begin
    if (Source=Sender) then
      CallHandleEvent('DragStart',Src.Text,Sender);    // pick up source node for dragging
    if (Accept) then
    begin
       self.IsDropTarget:=true;
    end;
  end;

  if (Accept)
  and (DestTreeNode<>nil) then
  begin
    // mark the node as available for drop
    if DestTreeNode.HasChildren then
       TTreeView(Sender).SetInsertMark(DestTreeNode,tvimAsFirstChild)
    else
       TTreeView(Sender).SetInsertMark(DestTreeNode,tvimAsPrevSibling);
    TTreeView(Sender).Update;
  end;
end;

procedure TmyTreeView.SetNodeHint(Sender: TObject; Shift: TShiftState; X, Y: Integer) ;  //
var
  tree: TmyTreeView;
  hoverNode: TTreeNode;
  hitTest : THitTests;
begin
  tree:=TmyTreeView(Sender);

  hoverNode := TTreeNode(tree.GetNodeAt(X, Y)) ;
  hitTest := tree.GetHitTestInfoAt(X, Y) ;

  if (tree.lastHintNode <> hoverNode) then
  begin
    Application.CancelHint;

    if (hitTest <= [htOnItem, htOnIcon, htOnLabel, htOnStateIcon]) then
    begin
      tree.lastHintNode := hoverNode;
      tree.Hint := tree.NodeHint(hoverNode) ;
    end;
  end;
end;

procedure TmyTreeView.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) ;
begin
  if  not (ssLeft in Shift)
  and (self.IsDropTarget = false)   // do nothing if a drag/drop is in progress
  then
  begin
    SetNodeHint(Sender,Shift,X,Y);
  end;
end;

procedure TmyTreeView.HandleClick(Sender: TObject) ;
begin
    self.IsDropTarget:=false;
end;

procedure TmyTreeView.HandleMouseLeave(Sender: TObject) ;
begin
  self.IsDropTarget:=false;

end;

procedure TmyTreeView.selectNode(NodeText:String);
var
  TargetNode:TTreeNode;
begin
  TargetNode:=TTreeNode(self.Items.FindNodeWithText(NodeText));
  if TargetNode<>nil then
  begin
    if TargetNode<>self.Selected then
      self.Select(TargetNode);        // In Lazarus this will automatically open any parent nodes
  end;
end;

procedure TmyTreeView.TreeSelectedNodeChange(Sender: TObject; Node: TTreeNode);
begin
    if Assigned(Node) then
      CallHandleEvent('TreeNodeClick',Node.Text,Sender);
end;

procedure TmyTreeView.CustomDrawTreeNode(Sender: TCustomTreeView;Node: TTreeNode; State: TCustomDrawState;var DefaultDraw: Boolean) ;
begin
  DefaultDraw:=true;
end;

function TmyTreeView.AddNewNode(NodeItems:TTreeNodes;ParentNode:TTreeNode; NodeString:string):TTreeNode;
var
  NewTreeNode:TTreeNode;
  SystemNodeName,myHint:String;
  MyRecPtr: PNodeDataRec;
begin
   New(MyRecPtr);
   if ParentNode<>nil then
     NewTreeNode:=NodeItems.AddChildObject(ParentNode,NodeString,MyRecPtr)
   else
     NewTreeNode:=NodeItems.Add(nil,NodeString);

   // Create the Hint text for this node, if specified.
   if (self.Parent<>nil) and (self.Parent is TXTree) then
     if TXTree(self.Parent).TreeNodeHintFunc<>nil then
       MyRecPtr^.NodeHint := TXTree(self.Parent).TreeNodeHintFunc(NodeString);

   result:=NewTreeNode;
end;

procedure TMyTreeView.ExpandTreeNodes(Nodes: TTreeNodes; Level: Integer);
var
  I: Integer;
begin
  Nodes.BeginUpdate;
  try
    for I := 0 to Nodes.Count - 1 do
      if Nodes[I].Level < Level then
        Nodes[I].Expand(False);
  finally
    Nodes.EndUpdate;
  end;
end;

procedure TMyTreeView.PopulateMeFromData(NodeTreeString,SelectedNodeString:String);
var
   OldNode: TTreeNode;
   OpenBracketsSubStringList:TStringList;
   CloseBracketsSubStringList:TStringList;
   ItemList:TStringList;
   i,j,k,level,OldLevel,LevelOffset,closecount:integer;
   tempstr:string;
begin
  with self.Items do
  begin
    Clear; { remove any existing nodes }
    OpenBracketsSubStringList:=StringToSubStringList(NodeTreeString,'[');

    for i:=0 to OpenBracketsSubStringList.count-1 do
    begin
      tempstr:=OpenBracketsSubStringList[i];
      tempstr:=tempstr;
    end;

    // example tree node definition string ------NodeTree := '["myTreeName",["Layout","TestStuff"] ,"SimpleItems","Collection Items",["Media items" ,"TestMoreStuff"],"Option Popups"]';
    // i counts the open braces to the left
    // closecount counts closing braces to the left
    // the nesting level is the sum of open braces to the left minus closeing braces to the left
    // the first item after an open braces is the parent the rest are its children  so LevelOffset promotes the parent to its correct tree level
    OldLevel:=0;
    closecount:=0;
    for i :=1 to (OpenBracketsSubStringList.count)-1 do
    begin
     // showmessage('OpenBracketSubString = '+OpenBracketsSubStringList[i]);
      tempstr:= OpenBracketsSubStringList[i];
      CloseBracketsSubStringList:=StringToSubStringList(OpenBracketsSubStringList[i],']');

      for j:=0 to CloseBracketsSubStringList.count-1 do
      begin
        tempstr:=CloseBracketsSubStringList[j];
        tempstr:=tempstr;
      end;

      for j:=0 to (CloseBracketsSubStringList.count)-1 do
      begin
//        showmessage(CloseBracketsSubStringList[j]);
        if j>0 then
          closecount:=closecount+1;
        if trim(CloseBracketsSubStringList[j])<>',' then      //???????? dodgy
        begin
        ItemList:=StringToSubStringList(CloseBracketsSubStringList[j],',');
        for k:=0 to (ItemList.count)-1 do
        if trim(ItemList[k])<>'' then
        begin
          tempstr:=itemlist[k];
          ItemList[k]:=trim(ItemList[k]);  //get rid of spaces outside the quotes
          ItemList[k]:=StringReplace(ItemList[k],'"','',[rfReplaceAll]);  // remove remaining quotes
          if (k=0) and (j=0) then LevelOffset:= -1 else LevelOffset:=0;
          Level := i-closecount + LevelOffset;
//          showmessage(' item >'+ItemList[k]+'< is at tree level '+ inttostr(level ));
          if (Level=0) then
          begin
             OldNode:= AddNewNode(self.Items,nil, ItemList[k]); { Add a root node }
             OldNode.expand(true);
          end
          else
          begin
            if Level = OldLevel then
            begin
              OldNode:= AddNewNode(self.Items,TTreeNode(OldNode.Parent), ItemList[k]) ; // add a sibling
            end
            else
            begin
              if Level = OldLevel+1 then   // add a child node
              begin
                OldNode:=AddNewNode(self.Items,OldNode, ItemList[k]);
              end
              else   // go up to the correct parent and add a child node
              Begin
                While Level <> OldLevel+1 do
                begin
                  OldNode:=TTreeNode(OldNode.Parent);
                  OldLevel:=OldLevel-1;
                end;
                OldNode:=AddNewNode(self.Items,OldNode, ItemList[k]);
              end;
            end;
          end;
          OldLevel:=Level;
          if ItemList[k]= SelectedNodeString then
            OldNode.Selected:=true;
          end;
        end;  //k
      end;//j
    end;//i
  end;
  ExpandTreeNodes(self.Items,1);   // by default show the first level
end;


procedure TXTree.TreeClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
     CallHandleEvent('Click',self.myNode.NodeName,self);
end;

//procedure TXTree.SetTreeNodeHint(AValue:TTreeNodeHint);
//begin
//  FtreeNodeHint:=AValue;
//  AddNodeFuncLookup(???,AValue);
//end;

procedure TXTree.SetTreeWidth(AValue:string);
begin
  myNode.SetAttributeValue('TreeWidth',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self.myControl),'TreeWidth','TreeHeight');
end;

procedure TXTree.SetTreeHeight(AValue:string);
begin
  myNode.SetAttributeValue('TreeHeight',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self.myControl),'TreeWidth','TreeHeight');
end;

function SetTreeNodeSelected(TreeComponent:TDataNode;NodeText:string):string;
var
   ATreeView:TTreeView;
   TargetNode:TTreeNode;
begin
   // select the required node in the tree
   //showmessage('select node: '+NameOfDetailsList);
   ATreeView:=TTreeView(TWrapperPanel(TreeComponent.ScreenObject).myControl);
   TargetNode:=TTreeNode(ATreeView.Items.FindNodeWithText(NodeText));
   if TargetNode<>nil then
   begin
     if TargetNode<>ATreeView.Selected then
       ATreeView.Select(TargetNode);        // In Lazarus this will automatically open any parent nodes
   end;
end;

{$else}
function addTreeStyles(dummy:string):string;
begin
  if dummy='notnow' then EXIT;

asm
  try{
      // ----------------------------------------check if the style has already been set
      var x = document.getElementsByTagName("STYLE");
      var StyleIsSet = false;
      if (x.length>0){
        for (var i=0; i<x.length; i++){
          var y= x[i].innerHTML;
          if (y.indexOf("summary.hasChildren") !=-1) { StyleIsSet =true}
        }
      }
      if (StyleIsSet == false){
         //<-------------- Styles to switch the child marker on and off on the summary line ---------------
         var styletext = '<style> summary.noChildren::-webkit-details-marker { display:none; } </style>'
                        + '<style> summary.hasChildren {color:black; }</style> ';
         //----------------------------- now append the style declarations to the head of the HTML page
         document.head.innerHTML = document.head.innerHTML+styletext;
      }
 }catch(err) {alert('Error in XTree.addTreeStyles '+ err.message);}
end;

end;

function HandleTreeNodeDragOver(ob:TObject;str1,str2:string):Boolean;
begin
  // Is there a function in the form called <treename>TreeNodeDropAccepted????  If so, run it.   //!!!!
  //if TXTree(self.Parent).TreeNodeDropAccepted<>nil then
  //  Accept := TXTree(self.Parent).TreeNodeDropAccepted(TXTree(self.Parent),SourceName);
asm
  //alert(ob.id+' dragover '+str1+' '+str2);
  event.preventDefault();
  pas.Events.handleEvent( "DragOver" ,str1,str2,"");      //event type, node id, value
  return true;
end;
end;

//................................................... add Tree Node .......
function addTreeNode(WrapperNodeId,TreeName,parentName,NameOfDetailsList,
                     SummaryText,HasChildren,color,isopen:string;NodeHintFunc:TTreeNodeHint):string;
begin

asm
 try{

 //alert('node '+SummaryText);

  var SystemNodeText='';
  if (Array.isArray(SummaryText)){
    SystemNodeText=SummaryText[0];
    }
  else {
    SystemNodeText=SummaryText;
    }
  var myHint='';
  if (NodeHintFunc!=null) {
     myHint = NodeHintFunc(SystemNodeText);
     //alert('Hint '+myHint);
     }

//alert('addTreeNode '+NameOfDetailsList+' to '+parentName);
  var parent = (document.getElementById(parentName));
  if (parent==null) {alert('cannot find parent '+parentName);}
  else
  {
  var div = document.createElement("div");
  div.style.width  = parent.width;
  div.style.marginLeft = "25px";
  div.id = NameOfDetailsList+'OuterDiv';

  var dragstartstring='event.stopPropagation(); pas.Events.handleEvent( "DragStart" ,"'+NameOfDetailsList+'" ,"'+SummaryText+'",""); ';
  var dragOverEventString='var mynode=pas.HTMLUtils.GetDataNodeFromTreeNode("'+NameOfDetailsList+'");pas.XTree.HandleTreeNodeDragOver(event.target,mynode.NodeName,"'+SummaryText+'");';
  var dropEventString='event.stopPropagation(); pas.Events.handleEvent( "Drop" ,"'+NameOfDetailsList+'" ,"'+SummaryText+'",""); ';
  var dragEventString =  'draggable="true" '
    +" ondragstart = '"+dragstartstring+"' "
    +" ondragover = '"+dragOverEventString+"' "
    +" ondrop =  '"+dropEventString+"' ";

  var ClickEventString = 'event.stopPropagation(); '+
                         'pas.XTree.HandleTreeNodeClick("'+WrapperNodeId+'" ,"'+NameOfDetailsList+'"); '+
                         'pas.Events.handleEvent( "TreeNodeClick" ,"'+NameOfDetailsList+'" ,"'+SummaryText+'",""); '+
                         '';
  if (HasChildren==true){
    if(isopen==true){
      div.innerHTML = "<details open id="+NameOfDetailsList+"><summary id="+NameOfDetailsList+"Summary "
                      +dragEventString
                      +"  onclick ='"+ClickEventString
                      +"'  class='hasChildren ' style='background-color:"+color+";'>"+SummaryText+"</summary></details>";}
      else {
      div.innerHTML = "<details  id="+NameOfDetailsList+"><summary id="+NameOfDetailsList+"Summary "
                      +dragEventString
                      +" onclick ='" +ClickEventString
                      +"'  class='hasChildren ' style='background-color:"+color+";'>"+SummaryText+"</summary></details>";
    }
  }
  else {
    div.innerHTML = "<details id = "+NameOfDetailsList+" ><summary id="+NameOfDetailsList+"Summary "
                     +dragEventString
                     +" onclick ='"+ClickEventString
                     +"'  class='noChildren' style='background-color:"+color+";' >"+SummaryText+"</summary></details>";
  }
   div.title = myHint;

   parent.appendChild(div);
  }
 }catch(err) {alert('Error in XTree.addTreeNode '+ err.message);}
end;

end;

procedure clearTreeNode(parentName:string);
begin
asm
try {
 parent = (document.getElementById(parentName));
 if (parent!=null) {
   //alert('clearTreeNode '+parentName);
   parent.innerHTML = '';}
}catch(err){alert('Error in XTree.clearTreeNode '+ err.message);}
end;
end;

function addnode(WrapperNodeId,TreeName,ParentName,currentNodeTree,SelectedNodeString,IdOfNodeBeingAdded:string;
                 level:integer;normalColor:string;NodeHintFunc:TTreeNodeHint):string;
begin

asm
   try {
   //alert('addnode: '+WrapperNodeId+' '+TreeName+' '+ParentName+' '+IdOfNodeBeingAdded);
       var isopen=false;
       if (level<1) {isopen=true;}  //by default just show the first level of the tree
       var localcurrentNodeTree=currentNodeTree;
       if ((localcurrentNodeTree.length>1)&&(Array.isArray(localcurrentNodeTree))){
       // This node has children so create the parent node ......is it selected?
          if (localcurrentNodeTree==SelectedNodeString)
              { pas.XTree.addTreeNode(WrapperNodeId,TreeName,ParentName,IdOfNodeBeingAdded,localcurrentNodeTree[0],
                                      true,pas.XTree.TreeNodeHighlightColor,isopen,NodeHintFunc);
              }
           else
              { pas.XTree.addTreeNode(WrapperNodeId,TreeName,ParentName,IdOfNodeBeingAdded,localcurrentNodeTree[0],
                                      true,normalColor,isopen,NodeHintFunc);}
       // then make the recursive call for each of its children
           for (var i=1; i<localcurrentNodeTree.length; i++){
             var NameOfChildNode = IdOfNodeBeingAdded+'_'+i
             pas.XTree.addnode(WrapperNodeId,TreeName,IdOfNodeBeingAdded,localcurrentNodeTree[i],
                               NameOfChildNode,NameOfChildNode,level+1,normalColor,NodeHintFunc);}
       }
       else {
       // This node does not have children so just create the node ......is it selected?
           if (localcurrentNodeTree==SelectedNodeString)
              { pas.XTree.addTreeNode(WrapperNodeId,TreeName,ParentName,IdOfNodeBeingAdded,localcurrentNodeTree,
                                      false,pas.XTree.TreeNodeHighlightColor,true,NodeHintFunc);}
           else
              { pas.XTree.addTreeNode(WrapperNodeId,TreeName,ParentName,IdOfNodeBeingAdded,localcurrentNodeTree,
                                      false,normalColor,true,NodeHintFunc);}
       }
    }catch(err){alert('Error in XTree.addnode '+ err.message);}
end;

end;


function SetOpenStatusOfNodeChildren(NodeName,NameOfSelectedNode:string;level:integer):boolean;
var containsSelectedNode:boolean;
begin

asm
try{
  containsSelectedNode = false;
  if (NodeName!='') {
     var parentNode=document.getElementById(NodeName);
     if (parentNode != null){
     if (parentNode.children.length>0) {
        for (var i=0; i<parentNode.children.length; i++) {
          var TempContainsSelectedNode = pas.XTree.SetOpenStatusOfNodeChildren( parentNode.children[i].id,NameOfSelectedNode,level+1);
          if (TempContainsSelectedNode==true){containsSelectedNode = true};
          if ( parentNode.children[i].id== NameOfSelectedNode ){containsSelectedNode = true};
        }
        var test = parentNode.getAttributeNode("open");
        if ((containsSelectedNode == true)||(level < 1) )
          {parentNode.setAttribute("open", "") }
 //       else
 //         {if (test != null){parentNode.removeAttribute("open")}};
     } }
  }
}catch(err) { alert(err.message+'  in XTree.SetOpenStatusOfNodeChildren'); }
end;

result:=  containsSelectedNode;
end;

function GetTreeRootID(NodeID:String):String;
begin
asm
try{
//alert('GetTreeRootID. NodeID='+NodeID);
    var TreeRootID = "";
    var instring = pas.StringUtils.TrimWhiteSpace(NodeID);
    var EndOfTreeRootID = pas.StringUtils.FoundString(instring,"Contents" );
    for (var i=0; i<EndOfTreeRootID-1; i++){ TreeRootID = TreeRootID+instring[i];}
 }catch(err) { alert(err.message+'  in XTree.GetTreeRootID'); }
 return TreeRootID;
end; end;

function OpenAndScrollToSelectedNode(NameOfDetailsList:string):string;
begin asm
try{
  var TreeRootID=pas.XTree.GetTreeRootID(NameOfDetailsList)+'ContentsScroll'; // the tree nodes container
  var root = (document.getElementById(TreeRootID));
  pas.XTree.SetOpenStatusOfNodeChildren(root.id,NameOfDetailsList,0);
  var SelectedNode=document.getElementById(NameOfDetailsList);
  var AlignToTop = false;
  SelectedNode.scrollIntoView(AlignToTop);
}catch(err) { alert(err.message+'  in XTree.OpenAndScrollToSelectedNode'); }
end;end;



function deselectNodeChildren(NodeName,normalColor:string):string;
begin asm
try{
  if (NodeName!='') {
  var parentNode=document.getElementById(NodeName);
  parentNode.style.background=normalColor;

  if (parentNode.children.length>0) {
   // if (pas.Stringutils.FoundString(NodeName,'Navig')<1) {alert('node '+NodeName+' has '+parentNode.children.length+' children');}
    for (var i=0; i<parentNode.children.length; i++) {
      pas.XTree.deselectNodeChildren( parentNode.children[i].id,normalColor);
    }
  }
  }
}catch(err) { alert(err.message+'  in XTree.deselectNodeChildren'); }
end; end;

function clearNodeSelectedMarker(NameOfDetailsList,normalColor:string):string;
begin asm
try{
  //alert('clearNodeSelectedMarker. NameOfDetailsList='+NameOfDetailsList+' normalColor='+normalColor);
     // go down the tree from the root clearing the selected colour

  var TreeRootID=pas.XTree.GetTreeRootID(NameOfDetailsList)+'ContentsScroll';  // the tree nodes container
  //alert('tree root id='+TreeRootID);
  var root = (document.getElementById(TreeRootID));
  if (root==null) {
    alert('root is null')}
  else {
    pas.XTree.deselectNodeChildren(root.id,normalColor); }

}catch(err) { alert(err.message+'  in XTree.clearNodeSelectedMarker'); }
end;end;

function NodeIdFromText(TreeNode:TDataNode;NodeText:String):String;
var
   TreeObject:TObject;
   FoundId:String;
begin
  TreeObject:=TreeNode;
  asm
    var ob = document.getElementById(TreeObject.NodeName+'ContentsScroll');  // the tree nodes container
    if (ob!=null) {
            function checkChildren(obj) {
               for (var i=0; i<obj.children.length; i++) {
                 if (obj.children[i].innerHTML==NodeText) {
                   //alert('found '+NodeText+' at '+ obj.children[i].id);   //Summary level
                   return obj.id;                                         //id is name minus 'Summary' suffix
                 }
                 else {
                   var tmp=checkChildren(obj.children[i]);
                   if (tmp!='') {return tmp;}
                 }
               }
               return '';
            }
      //alert('top text is '+ ob.innerHTML);
      if (ob.innerHTML==NodeText) {
        //alert('top found '+NodeText+' at '+ ob.id);
        FoundId=ob.id;
      }
      else {
        FoundId=checkChildren(ob);
      }
    }
  end;
  result:=FoundId;
end;

procedure SetTreeNodeSelected(TreeComponent:TDataNode;NodeId:string);
var
   normalColor:string;
begin
  normalColor:=TreeComponent.GetAttribute('BgColor',true).AttribValue;
asm
try{
//alert('SetTreeNodeSelected '+ NodeId+' TreeName='+TreeComponent.NodeName);
// NodeId is the name (id) of a tree node
if (NodeId!='') {
  var myself = document.getElementById(NodeId);
  if (myself!=null)
  {
  //alert('node found');
  var HTMLString = myself.innerHTML;
  var hasChildren = pas.StringUtils.FoundString(HTMLString,"hasChildren");

  // go down the tree from the root clearing the selected colour
  pas.XTree.clearNodeSelectedMarker(NodeId,normalColor);  // clear old selected nodes if selecting a new one

//  if ( pas.StringUtils.FoundString(NodeIdt,NavTreeName )>0 )
//      {
//      //  alert('SetTreeNodeSelected.  calling OpenAndScrollToSelectedNode '+NodeId);
      pas.XTree.OpenAndScrollToSelectedNode(NodeId);
//      };
//alert('1. looking for '+NodeId+'Summary');
  // Highlight this selected node in yellow
  var mySummary=document.getElementById(NodeId+'Summary');
  mySummary.style.background=pas.XTree.TreeNodeHighlightColor;

  //alert('2');
  if (hasChildren>0)   // toggle the open attribute
  {
      var test = myself.getAttributeNode("open");
      if (test == null){
         myself.setAttribute("open", "");
         } else {
         myself.removeAttribute("open");}
   }
   //alert('3');
    //**** have to do this to force the document to repaint this element (eg if node has been opened)
    HTMLString = myself.innerHTML;
    myself.innerHTML = HTMLString;
    }
  }
  else alert('Cannot find node '+NodeId);
  }catch(err) { alert(err.message+'  in XTree.SetTreeNodeSelected'); }
end;

end;

constructor TXTree.Create(MyForm:TForm;NodeName:String);
begin
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  LabelText:='Tree view';
  SelectedNodeName:='';
  TreeData:=ExampleNodeTree;
  LabelPos:='Top';
  TreeHeight:='150';
  TreeWidth:='200';
  BgColor:='#ffffff';

end;


function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NodeTree,SelectedNodeString,LabelText,LabelPos,normalColor:string;
  Ht,Wd:String;
  NodeName:string;
  NodeHintFunc:TTreeNodeHint;
begin

  NodeTree:=MyNode.GetAttribute('TreeData',true).AttribValue;
  SelectedNodeString:=MyNode.GetAttribute('SelectedNodeName',true).AttribValue;
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  LabelPos:= UpperCase(MyNode.getAttribute('LabelPos',true).AttribValue);
  normalColor:= MyNode.getAttribute('BgColor',true).AttribValue;
  Wd:=' width:100%; ';
  Ht:=' height:100%; ';
  NodeName:=MyNode.NodeName;

  NodeHintFunc:=TXTree(MyNode).TreeNodeHintFunc;
  //if NodeHintFunc<>nil then showmessage('found node hint func for tree '+ScreenObjectname);

  asm
    try{
    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';
    var wrapper=document.getElementById(ScreenObjectName);

    var MyObjectName=ScreenObjectName+'Contents';
    var localcontainer = document.createElement("div");
    localcontainer.id = MyObjectName;
    localcontainer.style.display="inline-block;";
    localcontainer.style.height="100%";
    localcontainer.style.width="100%";
    wrapper.appendChild(localcontainer);

    pas.XTree.addTreeStyles();// this style script removes the arrowhead on all nodes that do not have children


    // put in a scrollbox to contain all the tree nodes
    //alert('tree container id='+ScreenObjectName+'Contents'+'Scroll');
    var ContainerString = '<div style = "overflow:scroll; display:inline-block;'+
                                'background-color:'+normalColor+';'+Ht+Wd+'" '+
                                'id='+MyObjectName+'Scroll ></div> ';
    localcontainer.insertAdjacentHTML( 'beforeend', ContainerString );

    var TreeName = ScreenObjectName+'ContentsScroll';
    var localNodeTree =JSON.parse(NodeTree);
    //alert('NodeName='+NodeName+' Treename='+TreeName);

    pas.XTree.addnode(NodeName,TreeName,TreeName,localNodeTree,SelectedNodeString,(TreeName+'Node'),
                      0,normalColor,NodeHintFunc) ;


    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';
    if (LabelPos=='LEFT') {
      wrapper.insertAdjacentHTML( 'afterbegin', labelstring );
    }
    else {
      wrapper.insertAdjacentHTML( 'beforeend', labelstring );
    }

  }
  catch(err) { alert(err.message+'  in XTree.CreateWidget');}

end;

  MyNode.ScreenObject:=MyNode;

  TWrapperPanel(myNode).myHeight:=TWrapperPanel(myNode).myHeight;
  TWrapperPanel(myNode).myWidth:=TWrapperPanel(myNode).myWidth;
  TXTree(myNode).TreeHeight:=TXTree(myNode).TreeHeight;
  TXTree(myNode).TreeWidth:=TXTree(myNode).TreeWidth;
  TWrapperPanel(myNode).Alignment:=TWrapperPanel(myNode).Alignment;
  TWrapperPanel(myNode).LabelPos:=TWrapperPanel(myNode).LabelPos;
  TWrapperPanel(myNode).BgColor:=TWrapperPanel(myNode).BgColor;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;
result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXTree.Create(MyForm,NodeName));
end;

procedure HandleTreeNodeClick(WrapperNodeId,SelectedNodeText:String);
var
  myNode:TDataNode;
begin
  //showmessage('HandleTreeNodeClick '+WrapperNodeId+' '+SelectedNodeText);
   myNode:=FindDataNodeById(SystemNodetree,WrapperNodeId,true);
   asm
   $mod.SetTreeNodeSelected(myNode,SelectedNodeText);
   end;
end;

procedure TXTree.SetTreeWidth(AValue:string);
begin
  //showmessage('memo width='+AValue);
  myNode.SetAttributeValue('TreeWidth',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  //  if (ob==null) {alert(this.NodeName+'Contents'+'  not found');}
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXTree.SetTreeHeight(AValue:string);
begin
  myNode.SetAttributeValue('TreeHeight',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;
{$endif}


function TXTree.GetTreeData:string;
begin
  result:=MyNode.getAttribute('TreeData',true).AttribValue;
end;
function TXTree.GetSelectedNodeName:string;
begin
  result:=MyNode.getAttribute('SelectedNodeName',true).AttribValue;
end;
function TXTree.GetTreeHeight:string;
begin
  result:=MyNode.getAttribute('TreeHeight',true).AttribValue;
end;
function TXTree.GetTreeWidth:string;
begin
  result:=MyNode.getAttribute('TreeWidth',true).AttribValue;
end;

procedure TXTree.SetTreeData(NodeTreeString:String);
var
  SelectedNodeString:string;
  myNodeName:String;
  NodeHintFunc:TTreeNodeHint;
begin
  myNode.setAttributeValue('TreeData',NodeTreeString);
  SelectedNodeString:=myNode.getAttribute('SelectedNodeName',false).AttribValue;
  {$ifndef JScript}
  TmyTreeView(myControl).PopulateMeFromData(NodeTreeString,SelectedNodeString);
  {$else}
    myNodeName:=self.NodeName;
    clearTreeNode(myNodeName+'ContentsScroll');
    NodeHintFunc:=TXTree(MyNode).TreeNodeHintFunc;
 //   if NodeHintFunc<>nil then showmessage('found node hint func for tree '+ScreenObjectname);
    asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob!=null) {
      var TreeName = myNodeName+'ContentsScroll';
      var localNodeTree =JSON.parse(NodeTreeString);
      pas.XTree.addnode(myNodeName,TreeName,TreeName,localNodeTree,SelectedNodeString,(TreeName+'Node'),
                        0,this.BgColor,NodeHintFunc) ;
    }
  end;
  {$endif}
end;

procedure TXTree.SetSelectedNodeName(AValue:string);
begin
  myNode.SetAttributeValue('SelectedNodeName',AValue);
  {$ifndef JScript}
  TmyTreeView(myControl).selectNode(AValue);
  {$else}
  //showmessage('SetSelectedNodeName '+AValue);
  if AValue<>'' then
     SetTreeNodeSelected(self,AValue);
  {$endif}
end;


begin
  {$ifndef JScript}
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  SuppressDesignerProperty(TXTree,'BgColor');
{$else}
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
SuppressDesignerProperty('TXTree','BgColor');
  {$endif}
end.

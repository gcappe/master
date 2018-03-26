unit XTable;
{$ifndef JScript}

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, LResources, Forms, Controls, ComCtrls, StdCtrls, Grids,Graphics, Dialogs, ExtCtrls,TypInfo, Propedits,RTTICtrls,
  WrapperPanel,NodeUtils,StringUtils, LazsUtils, Events,XTree;
{$else}
interface
uses
  Classes, SysUtils,TypInfo,
  NodeUtils,StringUtils,HTMLUtils, WrapperPanel,Events;

procedure AddTableStyles(dummy:string);
procedure TableChange(Sender:TObject;NodeName:String) ;

{$endif}

type TTableCellsArray = Array of Array of String;

{$ifndef JScript}
type
  TXTable = class(TWrapperPanel)
  private
    { Private declarations }
    fHandleClick:TEventHandler;

    procedure Tableclick(Sender:TObject);
    procedure TableChange(Sender:TObject);
    //procedure TableCellChange(Sender:TObject;ARow,ACol:longint;const AValue:String);
    procedure SetMyEventTypes;

    function GetReadOnly:Boolean;
    function GetTableWidth:string;
    function GetTableHeight:string;
    function GetTableData:string;
    function GetColWidth:string;
    function GetNumCols:integer;
    function GetNumRows:integer;

    procedure SetReadOnly(AValue:Boolean);
    procedure SetTableWidth(AValue:string);
    procedure SetTableHeight(AValue:string);
    procedure SetTableData(AValue:string);
    procedure SetColWidth(AValue:string);
  protected
    { Protected declarations }
    procedure LinkLoadFromProperty(Sender: TObject);  override;
    procedure LinkSaveToProperty(Sender: TObject);  override;
    procedure DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwner: TComponent;IsDynamic:Boolean); override;
    procedure  PopulateStringGrid(gridData:String);
    function ConstructDataString:String;
    function GetCellValue(row,col:integer):string;
    procedure SetCellValue(row,col:integer;AValue:string);
    property NumCols:integer read GetNumCols;
    property NumRows:integer read GetNumRows;
    function GetCellsAsArray:TTableCellsArray;
  published
    { Published declarations }
    // Properties defined for this class...
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property TableHeight: String read GetTableHeight write SetTableHeight;
    property TableWidth: String read GetTableWidth write SetTableWidth;
    property TableData:String read GetTableData write SetTableData;
    property ColWidth:String read GetColWidth write SetColWidth;

    // Events to be visible in IDE
    property HandleClick: TEventHandler read FHandleClick write FHandleClick;
  end;

procedure Register;

{$else}
type
  TXTable = class(TWrapperPanel)
  private
    procedure SetMyEventTypes;

    function GetReadOnly:Boolean;
    function GetTableWidth:string;
    function GetTableHeight:string;
    function GetTableData:string;
    function GetColWidth:string;
    function GetNumCols:integer;
    function GetNumRows:integer;

    procedure SetReadOnly(AValue:Boolean);
    procedure SetTableWidth(AValue:string);
    procedure SetTableHeight(AValue:string);
    procedure SetTableData(AValue:string);
    procedure SetColWidth(AValue:string);

  protected
    { Protected declarations }
    procedure LinkLoadFromProperty(Sender: TObject);  override;
    procedure LinkSaveToProperty(Sender: TObject);  override;
  public
    { Public declarations }
    constructor Create(MyForm:TForm;NodeName:String);
    function ConstructDataString(ob:TObject):String;
    function GetCellValue(row,col:integer):string;
    procedure SetCellValue(row,col:integer;AValue:string);
    property NumCols:integer read GetNumCols;
    property NumRows:integer read GetNumRows;
    function GetCellsAsArray:TTableCellsArray;

  published
    { Published declarations }
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property TableHeight: String read GetTableHeight write SetTableHeight;
    property TableWidth: String read GetTableWidth write SetTableWidth;
    property TableData:String read GetTableData write SetTableData;
    property ColWidth:String read GetColWidth write SetColWidth;

  end;
{$endif}

implementation

const MyNodeType='TXTable';

procedure TXTable.SetMyEventTypes;
begin
  MyEventTypes.Add('Click');
  MyEventTypes.Add('Change');
end;

{$ifndef JScript}
procedure Register;
begin
  {$I xtable_icon.lrs}
  RegisterComponents('Misc',[TXTable]);

  // inherited from TWrapperPanel, not required here

  // suppress some of the link properties
  RegisterPropertyEditor(TypeInfo(TAliasStrings), TXPropertyLink, 'AliasValues', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String), TXPropertyLink, 'TIElementName', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TPropertyLinkOptions), TXPropertyLink, 'Options', THiddenPropertyEditor);
end;

constructor TXTable.Create(TheOwner:TComponent);
begin
  inherited Create(TheOwner,false);
  DoConstructor(TheOwner,false);
end;

constructor TXTable.Create(TheOwner:TComponent;IsDynamic:Boolean);
begin
  inherited Create(TheOwner,IsDynamic);
  DoConstructor(TheOwner,IsDynamic);
end;

procedure TXTable.DoConstructor(TheOwner:TComponent;IsDynamic:Boolean);
begin
  self.BorderSpacing.Around:=glbBorderSpacing;

  myControl:=TStringGrid.Create(self);
  myControl.Parent:=self;

  myControl.SetSubComponent(true);  // Tell the IDE to store the modified properties
  // Make sure the embedded component can not be selected/deleted within the IDE
  myControl.ControlStyle := myControl.ControlStyle - [csNoDesignSelectable];

  TStringGrid(myControl).OnEditingDone:=@self.myeditingDone;
  myControl.OnClick:=@self.TableClick;
  //TStringGrid(myControl).OnSetEditText:=@self.TableCellChange;
  TStringGrid(myControl).OnExit:=@self.TableChange;

  self.SetMyEventTypes;
  self.myNode:=CreateComponentDataNode(self.Name,MyNodeType, self.myEventTypes, self,TheOwner,IsDynamic);

  self.ParentColor:=true;
  // Setting IsContainer false will prevent designer dropping new child controls into this one.
  self.IsContainer:=false;

  TStringGrid(myControl).FixedRows:=1;
  TStringGrid(myControl).Options:=[goColSizing,goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,goSmoothScroll,goEditing];
  //  default - scrollbars will appear as required
  //PopulateStringGrid(TempStringGrid,gridData);

  // cycle through the getters and setters AFTER creating data node, to synchronise
  // node attributes and set defaults for published properties
  AddLabel(myControl);
  Hint:='';
  TableWidth:='300';
  TableHeight:='300';
  ReadOnly:=False;
  TableData:='[["a","b","c"],[1,2,3]]';
  ColWidth:='40';

  LabelText:='Table';
  LabelPos:='Top';


end;

function CreateWidget(ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  NewWidget:TXTable;
  NewNode:TDataNode;
begin
  NewWidget:=TXTable.Create(ParentNode.MyForm,true);
  NewWidget.Name:=ScreenObjectName;
  InsertUnderParent(TWinControl(NewWidget),TWinControl(ParentNode.ScreenObject), position);
  NewNode:=NewWidget.myNode;
  AddChildToParentNode(ParentNode,NewNode,position);
  result:=NewNode;
end;

//procedure TXTable.TableCellChange(Sender:TObject;ARow,ACol:longint;const AValue:String) ;
//begin
//  showmessage('table change event');
//  CallHandleEvent('Change','',Sender);
//
//end;
procedure TXTable.TableChange(Sender:TObject) ;
var
  newData:String;
begin
  //showmessage('table change event');
  // get the table data contents
  newData:=self.ConstructDataString;
  // update the node attribute value
  if newData<>self.TableData then
  begin
    myNode.SetAttributeValue('TableData',newData);
    CallHandleEvent('Change',newData,Sender);
  end;
end;

procedure TXTable.TableClick(Sender: TObject) ;
begin
  if not (csDesigning in componentState) then
    CallHandleEvent('Click',self.myNode.NodeName,self);
end;
procedure TXTable.LinkLoadFromProperty(Sender: TObject);
var
  TxtVal:string;
  NewValue:Boolean;
begin
  if Sender=nil then ;
  if (Link.Editor=nil) then exit;
  inherited  LinkLoadFromProperty(Sender);

  TxtVal:=Link.GetAsText;
//!!!! sort this out
//  NewValue:= MyStrToBool(Link.GetAsText);
//  if NewValue<>TCheckBox(myControl).Checked then
//  begin
//     TCheckBox(myControl).Checked:=NewValue;
//  //   showmessage('checkbox. LinkLoadFromProperty '+TxtVal);
//  end;

end;

procedure TXTable.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link.Editor=nil then exit;
 // showmessage('checkbox. LinkSaveToProperty '+myBoolToStr(self.myCheckBox.Checked));
 // Link.SetAsText(myBoolToStr(TCheckBox(myControl).Checked));

end;

procedure TXTable.SetTableWidth(AValue:string);
begin
  myNode.SetAttributeValue('TableWidth',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self.myControl),'TableWidth','TableHeight');
end;

procedure TXTable.SetTableHeight(AValue:string);
begin
  myNode.SetAttributeValue('TableHeight',AValue);
  SetMyHeightWidth(self.myNode,TWinControl(self.myControl),'TableWidth','TableHeight');
end;

procedure TXTable.SetColWidth(AValue:string);
var
  i:integer;
begin
  myNode.SetAttributeValue('ColWidth',AValue);
  //showmessage('colcount='+inttostr(TStringGrid(myControl).ColCount));
  for i:=0 to TStringGrid(myControl).ColCount-1 do
      TStringGrid(myControl).ColWidths[i]:=strtoint(AValue);
end;

procedure  TXTable.PopulateStringGrid(gridData:String);
var
    subitem: String;
    TempTree:TMyTreeView;
    RootNode,CurrentNode:TTreeNode;
    i,j,RowCount,Colcount:integer;
begin
  gridData:= StringReplace(gridData,'[','[Dummy,',[rfReplaceAll]); // leading elements inside braces are not parent child so add dummy elements to reuse the tree string parse algortithm
  TempTree:= TMyTreeView.Create(nil);               //(CreateTree(nil,'',-1,gridData,''));
  TempTree.PopulateMeFromData(gridData,'');
 //showmessage(' > '+testData+' <'+inttostr(TempTree.Items[0].count ));
  RootNode:= TempTree.Items[0];

  // Find the size of the StringGrid
  RowCount:= RootNode.Count;
  Colcount :=0;
  for i:=0 to  RootNode.Count-1 do
  begin
    CurrentNode:= RootNode.items[i];
    //showmessage(CurrentNode.text);
    if CurrentNode.Count>Colcount then colcount:=CurrentNode.Count;
  end;
  TStringGrid(myControl).RowCount:=RowCount;
  TStringGrid(myControl).ColCount:= colcount;
  TStringGrid(myControl).FixedRows:=1;
  TStringGrid(myControl).FixedCols:=0;
  //showmessage(inttostr(CurrentStringGrid.colCount)+'   ' +inttostr(CurrentStringGrid.RowCount));
  for i:=0 to  RootNode.Count-1 do
  begin
     CurrentNode:= RootNode.items[i];
     for j:=0 to CurrentNode.Count-1 do
     begin
       subitem := CurrentNode.items[j].Text;
       TStringGrid(myControl).Cells[j, i] :=  subitem;
     end;
  end;

end;

function TXTable.GetCellsAsArray:TTableCellsArray;
var
  i,j:integer;
  myArray:TTableCellsArray;
begin
  SetLength(myArray, TStringGrid(myControl).RowCount);
  for i:= 0 to TStringGrid(myControl).RowCount-1 do
  begin
    setlength(myArray[i],TStringGrid(myControl).ColCount);
    for j:=0 to TStringGrid(myControl).ColCount-1 do
    begin
      myArray[i,j]:=TStringGrid(myControl).Cells[j, i];
    end;
  end;
  result:=myArray;
end;

function TXTable.ConstructDataString:String;
// Generate the data in string form from the table cell values
var
    i,j:integer;
    dataStr:String;
begin
  dataStr:='[';
  for i:= 0 to TStringGrid(myControl).RowCount-1 do
  begin
    if i>0 then dataStr:=dataStr+',';
    dataStr:=dataStr+'[';
    for j:=0 to TStringGrid(myControl).ColCount-1 do
    begin
      if j>0 then dataStr:=dataStr+',';
      dataStr:=dataStr+TStringGrid(myControl).Cells[j, i];
    end;
    dataStr:=dataStr+']';
  end;
  dataStr:=dataStr+']';
  result:=dataStr;
end;

function TXTable.GetNumCols:integer;
begin
  result:=TStringGrid(myControl).ColCount;
end;

function TXTable.GetNumRows:integer;
begin
  result:=TStringGrid(myControl).RowCount;
end;

function TXTable.GetCellValue(row,col:integer):string;
begin
  result:=TStringGrid(myControl).Cells[col,row];
end;
procedure TXTable.SetCellValue(row,col:integer;AValue:string);
begin
  TStringGrid(myControl).Cells[col,row]:=AValue;
  self.TableChange(TStringGrid(myControl));
end;

{$else}
constructor TXTable.Create(MyForm:TForm;NodeName:String);
begin
  //showmessage('create table node');
  inherited Create(NodeName);
  self.NodeType:=MyNodeType;
  self.MyForm:=MyForm;

  self.SetMyEventTypes;
  self.IsContainer:=false;

  Hint:='';
  TableWidth:='300';
  TableHeight:='300';
  TableData:='[["a","b","c"],[1,2,3]]';
  LabelText:='Table';
  LabelPos:='Top';
  ReadOnly:=False;
end;


procedure TableChange(Sender:TObject; NodeName:String) ;
var
  newData:String;
  myNode:TDataNode;
begin
  //showmessage('table change event');
  myNode:=FindDataNodeById(SystemNodeTree,NodeName,true);
  // get the table data contents
  newData:=TXTable(myNode).ConstructDataString(Sender);
  // update the node attribute value
  if newData<>TXTable(myNode).TableData then
  begin
    myNode.SetAttributeValue('TableData',newData);
    HandleEvent('Change',myNode.NodeName,newData);
  end;
end;

function CreateWidget(MyNode, ParentNode:TDataNode;ScreenObjectName:string;position:integer):TDataNode;
var
  Checked,ReadOnly,LabelText,LabelPos:string;
  OnChangeString, OnFocusOutString, OnClickString:String;
begin
  //showmessage('create table widget');
  LabelText:= MyNode.getAttribute('LabelText',true).AttribValue;
  ReadOnly:= MyNode.getAttribute('ReadOnly',true).AttribValue;

  OnClickString:='onclick="event.stopPropagation();pas.Events.handleEvent(''Click'','''+ScreenObjectName+''', '''', '''');' +
                          '" ';
  OnFocusOutString:='onfocusout="pas.XTable.TableChange(this,'''+ScreenObjectName+''');"';
  asm
    try{
    pas.XTable.AddTableStyles('');

    var wrapper = pas.HTMLUtils.CreateWrapperDiv(MyNode,ParentNode,'UI',ScreenObjectName,$impl.MyNodeType,position);
    wrapper.style.overflow = 'hidden';
    wrapper.style.overflow = 'hidden';
    wrapper.style.display = 'flex';
    var goright =  'flex-e'+'nd';

    var HTMLString='';
    var NodeIDString = "'"+ScreenObjectName+"'";
    var MyObjectName=ScreenObjectName+'Contents';

    var ReadOnlyString = '';
    if (ReadOnly=='False') { ReadOnlyString = ' contenteditable ';}

    var labelstring='<label for="'+MyObjectName+'" id="'+MyObjectName+'Lbl'+'">'+LabelText+'</label>';

    var TableString = '<table id='+MyObjectName+ ' '+
                       OnClickString +
                       OnFocusOutString +
                       ' style="display:inline-block;" '+
                       ReadOnlyString+' >' +
                         '<tr>'+
                           '<th>1</th>'+
                           '<th>2</th>'+
                           '<th>3</th>'+
                         '</tr>'+
                       '</table> ';

    HTMLString = labelstring+TableString;

    var wrapper=document.getElementById(ScreenObjectName);
    wrapper.insertAdjacentHTML('beforeend', HTMLString);
  }
  catch(err) { alert(err.message+'  in XCheckBox.CreateXCheckBox');}

end;

  MyNode.ScreenObject:=MyNode;
  TXTable(myNode).myHeight:=TXTable(myNode).myHeight;
  TXTable(myNode).myWidth:=TXTable(myNode).myWidth;
  TXTable(myNode).TableHeight:=TXTable(myNode).TableHeight;
  TXTable(myNode).TableWidth:=TXTable(myNode).TableWidth;
  TXTable(myNode).Alignment:=TXTable(myNode).Alignment;
  TXTable(myNode).LabelPos:=TXTable(myNode).LabelPos;
  TXTable(myNode).LabelText:=TXTable(myNode).LabelText;
  TXTable(myNode).ReadOnly:=TXTable(myNode).ReadOnly;
  TXTable(myNode).TableData:=TXTable(myNode).TableData;
  TWrapperPanel(myNode).Hint:=TWrapperPanel(myNode).Hint;
result:=myNode;
end;

function CreateinterfaceObj(MyForm:TForm;NodeName:String):TObject;
begin
  result:=TObject(TXTable.Create(MyForm,NodeName));
end;

procedure TXTable.LinkLoadFromProperty(Sender: TObject);
begin
  inherited  LinkLoadFromProperty(Sender);
end;

procedure TXTable.LinkSaveToProperty(Sender: TObject);
begin
  if Sender=nil then ;
  if Link=nil then exit;
  if Link.TIObject=nil then exit;
//  showmessage('linksavetoproperty. '+Link.TIPropertyName+' '));

  SetStringProp(Link.TIObject,Link.TIPropertyName,self.TableData);
end;

procedure TXTable.SetTableWidth(AValue:string);
begin
  //showmessage('Table width='+AValue);
  myNode.SetAttributeValue('TableWidth',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'W',AValue);
  end;
end;

procedure TXTable.SetTableHeight(AValue:string);
begin
  myNode.SetAttributeValue('TableHeight',AValue);
  asm
  var ob = document.getElementById(this.NodeName+'Contents');
  pas.HTMLUtils.SetHeightWidthHTML(this,ob,'H',AValue);
  end;
end;

procedure TXTable.SetColWidth(AValue:string);
begin
  //showmessage('Col width='+AValue);
  myNode.SetAttributeValue('ColWidth',AValue);
  //rebuild the data to incorporate col width
  self.TableData:=self.TableData;
end;

function TXTable.GetCellsAsArray:TTableCellsArray;
var
  i,j:integer;
  myArray:TTableCellsArray;
begin
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
  //alert('ob is '+ob.id);
    for (var i = 0, row; row = ob.rows[i]; i++) {
      if (i>0) { dataStr=dataStr+',';}
      dataStr=dataStr+'[';
      for (var j = 0, col; col = row.cells[j]; j++) {
        if (j>0) { dataStr=dataStr+','; }
          dataStr=dataStr+ row.cells[j].innerHTML;
      }
      dataStr=dataStr+']';
    }
  end;
  result:=myArray;
end;
function TXTable.ConstructDataString(ob:TObject):String;
// Generate the data is string form from the table cell values
var
    i,j:integer;
    dataStr:String;
begin
  dataStr:='[';
  asm
 //  var ob = document.getElementById(this.NodeName+'Contents');
 //alert('ob is '+ob.id);
    for (var i = 0, row; row = ob.rows[i]; i++) {
      if (i>0) { dataStr=dataStr+',';}
      dataStr=dataStr+'[';
      for (var j = 0, col; col = row.cells[j]; j++) {
        if (j>0) { dataStr=dataStr+','; }
          dataStr=dataStr+ row.cells[j].innerHTML;
      }
      dataStr=dataStr+']';
    }
  end;
  dataStr:=dataStr+']';
  result:=dataStr;
end;

function TXTable.GetNumCols:integer;
var
    num:integer;
begin
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob.rows.length > 0) {
      num=ob.rows[0].cells.length;
    }
    else {num=0};
  end;
  result:=num;
end;

function TXTable.GetNumRows:integer;
var
    num:integer;
begin
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    num=ob.rows.length
  end;
  result:=num;
end;

function TXTable.GetCellValue(row,col:integer):string;
var
  myval:string;
begin
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob.rows.length > row) {
      if (ob.rows[row].cells.length > col) {
        myval = ob.rows[row].cells[col].innerHTML;
      }
    }
  end;
  result:=myval;
end;
procedure TXTable.SetCellValue(row,col:integer;AValue:string);
begin
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if (ob.rows.length > row) {
      if (ob.rows[row].cells.length > col) {
        ob.rows[row].cells[col].innerHTML = AValue;
        pas.XTable.TableChange(ob,this.NodeName);
      }
    }
  end;
end;

procedure AddTableStyles(dummy:string);
begin
  if dummy='notnow' then EXIT;

  asm
    try{
        // ----------------------------------------check if the style has already been set
        //alert('AddTableStyles');
        var x = document.getElementsByTagName("STYLE");
        var StyleIsSet = false;
        if (x.length>0){
          for (var i=0; i<x.length; i++){
            var y= x[i].innerHTML;
            if (y.indexOf("table") !=-1) { StyleIsSet =true}
          }
        }
        if (StyleIsSet == false){
          var styletext = '<style>'+
                        'table, th, td { '+
                         ' background-color:#FFFFFF; '+
                         ' border: 1px solid black; '+
                         ' border-collapse: collapse; '+
                         '}'+
                        'th { '+
                         ' background-color:#DDDDDD; '+
                         ' text-align: left;'+
                          '}'+
                       '}</style>';
        //alert(styletext);
        //----------------------------- now append the style declarations to the head of the HTML page
           document.head.innerHTML = document.head.innerHTML+styletext;
        }
   }catch(err) {alert('Error in XTable.AddTableStyles '+ err.message);}
  end;

end;

{$endif}

function TXTable.GetReadOnly:Boolean;
begin
  result:=MyStrToBool(MyNode.getAttribute('ReadOnly',true).AttribValue);
end;

function TXTable.GetTableHeight:string;
begin
  result:=MyNode.getAttribute('TableHeight',true).AttribValue;
end;
function TXTable.GetTableWidth:string;
begin
  result:=MyNode.getAttribute('TableWidth',true).AttribValue;
end;
function TXTable.GetTableData:string;
begin
  result:=MyNode.getAttribute('TableData',true).AttribValue;
end;
function TXTable.GetColWidth:string;
begin
  result:=MyNode.getAttribute('ColWidth',true).AttribValue;
end;

procedure TXTable.SetReadOnly(AValue:Boolean);
begin
  myNode.SetAttributeValue('ReadOnly',myBoolToStr(AValue),'Boolean');
  {$ifndef JScript}
  TStringGrid(myControl).Enabled:=not AValue;
  {$else}
  asm
    var ob = document.getElementById(this.NodeName);
    if (ob!=null) {
    if (AValue==true) {ob.disabled = true}
    else {ob.disabled = false }
    }
  end;
  {$endif}
end;

procedure TXTable.SetTableData(AValue:String);
var
  cw:String;
begin
  myNode.SetAttributeValue('TableData',AValue,'String');
  {$ifndef JScript}
  self.PopulateStringGrid(AValue);
  {$else}
  cw:=myNode.GetAttribute('ColWidth',true).AttribValue;
  asm
    var ob = document.getElementById(this.NodeName+'Contents');
    if ((ob!=null)&&(AValue!='')) {
      var localtestdata=JSON.parse(AValue);
      var RowCount =localtestdata.length;
      var ColCount = 0;
      for ( var i = 0; i < RowCount; i++ ) {
        if(localtestdata[i].length> ColCount)
          {ColCount = localtestdata[i].length }
      }
      //alert('rows='+RowCount+' cols='+ColCount);
      ob.innerHTML='';
      if (RowCount>0) {

        // first row is headers
        var toprow=document.createElement("tr");
        for (var j=0; j<ColCount; j++) {
          var hdr=document.createElement("th");
          hdr.style.width=cw+'px';
          var textnode=document.createTextNode(localtestdata[0][j]);
          hdr.appendChild(textnode);
          toprow.appendChild(hdr);
        }
        ob.appendChild(toprow);

        for (i = 1; i < RowCount; i++ ) {
           var row=document.createElement("tr");
           for (j=0; j<ColCount; j++) {
             var cell = document.createElement("td");
             //alert('cell value is '+localtestdata[i][j]);
             textnode=document.createTextNode(localtestdata[i][j]);
             cell.appendChild(textnode);
             row.appendChild(cell);
           }
           ob.appendChild(row);
        }
      }
    }

  end;
  {$endif}
end;

begin
  {$ifndef JScript}
  SuppressDesignerProperty(TXTable,'BgColor');
  AddNodeFuncLookup(MyNodeType,@CreateWidget);
  {$else}
  SuppressDesignerProperty('TXTable','BgColor');
  AddNodeFuncLookup(MyNodeType,@CreateinterfaceObj,@CreateWidget);
  {$endif}
end.


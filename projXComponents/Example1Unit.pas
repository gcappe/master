unit Example1Unit;

{$mode objfpc}{$H+}

interface

{$ifndef JScript}
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLIntf, ExtCtrls, Menus, ComCtrls, TypInfo, LazIDEIntf,
  StringUtils, NodeUtils, LazsUtils,
  UtilsJSCompile, Events, Types, LResources,
  {$ifdef Chromium}
  cef3lcl,
  {$endif}
{$else}
uses
  Classes, SysUtils, StringUtils, NodeUtils, HTMLUtils,
{$endif}
  Popup, XScrollBox, XButton, XEditBox, XCheckBox, XHBox, XHyperLink,
  XTabControl, XVBox, XMemo, XRadioBtns;


{$ifdef JScript}
procedure InitialisePage(dummy:string);
{$endif}

{ TExample1Form }


type
TExample1Form = class(TForm)

  {$ifndef JScript}
  // Lazarus-only Form properties...
  MainMenu1: TMainMenu;
  CompileToJS: TMenuItem;
  {$endif}
  MyRootDiv: TXScrollBox;
  XButton1: TXButton;
  XButton2: TXButton;
  XCheckBox1: TXCheckBox;
  XEditBox1: TXEditBox;
  XHBox1: TXHBox;
  XHyperLink1: TXHyperLink;
  XRadioBtns1: TXRadioBtns;
  XRadioBtns2: TXRadioBtns;
  XTabControl1: TXTabControl;
  XTabSheet1: TXTabSheet;
  XTabSheet2: TXTabSheet;
  XVBox1: TXVBox;
  XVBox2: TXVBox;


  {$ifndef JScript}
  // Lazarus-only methods...
  procedure CompileToJSClick(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure FormResize(Sender: TObject);
  {$endif}

  // Common Event Handlers - created at design time along with X components...
  procedure DummyPositionMarker;   // DO not delete this line.
  procedure XButton1HandleButtonClick(nodeID: AnsiString; myNode: TDataNode;
    myValue: AnsiString);
  procedure XButton2HandleButtonClick(nodeID: AnsiString; myNode: TDataNode;
    myValue: AnsiString);
  procedure XCheckBox1HandleClick(nodeID: AnsiString; myNode: TDataNode;
    myValue: AnsiString);
  procedure XRadioBtns1HandleChange(nodeID: AnsiString; myNode: TDataNode;
    myValue: AnsiString);
  procedure XRadioBtns2HandleChange(nodeID: AnsiString; myNode: TDataNode;
    myValue: AnsiString);

private
  { private declarations }
public
  { public declarations }
end;


var
Example1Form: TExample1Form;

implementation

{$R *.lfm}

procedure TExample1Form.DummyPositionMarker;     // do not delete this procedure
begin
end;

procedure TExample1Form.XButton1HandleButtonClick(nodeID: AnsiString;
  myNode: TDataNode; myValue: AnsiString);
begin
  if XEditBox1.ReadOnly = true then
  begin
    XEditBox1.ReadOnly:=false;
    XButton1.myCaption:='Disable EditBox';
  end
  else
  begin
    XEditBox1.ReadOnly:=true;
    XButton1.myCaption:='Enable EditBox';
  end;
end;

procedure TExample1Form.XButton2HandleButtonClick(nodeID: AnsiString;
  myNode: TDataNode; myValue: AnsiString);
begin
  if XHyperLink1.IsVisible then
  begin
    XHyperLink1.IsVisible:=false;
    XButton2.myCaption:='Show BBC Link';
  end
  else
  begin
    XHyperLink1.IsVisible:=true;
    XButton2.myCaption:='Hide BBC Link';
  end;
end;

procedure TExample1Form.XCheckBox1HandleClick(nodeID: AnsiString;
  myNode: TDataNode; myValue: AnsiString);
begin
  if XCheckBox1.Checked then
  begin
    XHBox1.IsVisible:=false;
  end
  else
  begin
    XHBox1.IsVisible:=true;
  end;
end;

procedure TExample1Form.XRadioBtns1HandleChange(nodeID: AnsiString;
  myNode: TDataNode; myValue: AnsiString);
begin
  {$ifdef JScript}
  if myValue='0' then
  begin
    XVBox1.BgColor:='#8080FF';
  end
  else if myValue='1' then
    XVBox1.BgColor:='#6666AA'
  else
    XVBox1.BgColor:='#000077';
  {$else}
  if myValue='0' then
    XVBox1.BgColor:=$FF8080
  else if myValue='1' then
    XVBox1.BgColor:=$AA6666
  else
    XVBox1.BgColor:=$770000;
  {$endif}
end;

procedure TExample1Form.XRadioBtns2HandleChange(nodeID: AnsiString;
  myNode: TDataNode; myValue: AnsiString);
begin
  {$ifdef JScript}
  if myValue='0' then
  begin
    XVBox2.BgColor:='#FFFF00';
  end
  else if myValue='1' then
    XVBox2.BgColor:='#999900'
  else
    XVBox2.BgColor:='#444400';
  {$else}
  if myValue='0' then
    XVBox2.BgColor:=$00FFFF
  else if myValue='1' then
    XVBox2.BgColor:=$009999
  else
    XVBox2.BgColor:=$004444;
  {$endif}
end;

{$ifndef JScript}

{ TForm1 }

procedure TExample1Form.FormCreate(Sender: TObject);
var
  myNode:TDataNode;
begin
  MainForm:=self;
  SystemNodeTree.ScreenObject:=nil;       // root node has no screen object.

  myNode:=DoXFormCreated(self);

end;

procedure TExample1Form.CompileToJSClick(Sender: TObject);
begin
  CompileJSandExecute;
end;

procedure TExample1Form.FormResize(Sender: TObject);
begin
  DoFormResize(self, MyRootDiv);
end;

{$else}
procedure InitialisePage(dummy:string);
begin
  if dummy='notnow' then exit;

  StartingUp:=true;// suppress event handlers while starting up
  loadingSystem:=false;

  setPasteCutAreaSize();

  // this include file contains create statements for all the interface objects in main form and popup forms
  // Form (popup) nodes are added as children of UIRootNode.
  {$I systemintface.inc}
  MainForm:=Example1Form;
  UIRootNode.MyForm:=nil;

  // this include file contains the system description to be loaded at startup.
  {$I systemnodetree.inc}
  XMLToNodeTree(LoadedSystemString);   //! has been saved by the CompileToJS button

  StartingUp:=false;

end;


{$endif}


begin

  {$ifndef JScript}
    MainUnitName:='Example1Unit';
    // Load the needed RTL units from the Lazarus resource file rtl.lrs
    // resource file was created with ...  lazres rtl.lrs C:/pas2js/packages/rtl/system.pas ...etc...
    {$I rtl.lrs}

  {$Else}
    InitialisePage('notnow');
     asm
     try{
        // now do any Javascript specific start up code
        pas.HTMLUtils.addHandVBoxStyles();
        }catch(err) { alert(err.message+' in StartupCode');}
     end;
  {$endif}

end.


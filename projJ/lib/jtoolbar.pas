unit jToolBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, JElement, JPanel;

type
  TWToolBar = class(TCustomControl)
  private
    ToolBarItems: Array of TWPanel;
  public
    constructor Create(parent: TCustomControl); virtual;
    Procedure AddMenu(menuText, GotoForm, color: string);
    Procedure SetActiveMenu(formname: string);
  end;

implementation

uses JApplication;

{ TWMenuBar }

constructor TWToolBar.Create(parent: TCustomControl);
begin
  inherited Create('div', parent);
end;

procedure TWToolBar.AddMenu(menuText, GotoForm, color: string);
  var
    Panel0 : TWPanel;

  procedure Panel0Click(Sender:TObject);
    procedure postMessage(aList : TJSValueDynArray; aValue : JSValue); external name 'window.postMessage';
  begin
  if TJSArray(Application.FormNames).IndexOf((Sender as TWPanel).tag) > -1 then     //if form
    Application.GoToForm((Sender as TWPanel).tag) else               //then gotoform
    postMessage([self.Handle.id,'click',GoToForm],'*');   //else send message
  end;

begin
//
  Panel0 := TWPanel.Create(self);
  Panel0.SetBounds(20 + ((TJSArray(ToolBarItems).Length) * 100), 14, 90, 26);
  Panel0.SetinnerHtml(menuText);
  Panel0.setProperty('color', color);
  Panel0.setProperty('cursor','pointer');
  Panel0.SetProperty('font-size', '0.9em');

  TJSArray(ToolBarItems).push(Panel0);
  Panel0.Tag := GotoForm;
  Panel0.OnClick := @Panel0Click;
end;

procedure TWToolBar.SetActiveMenu(FormName: String);
var
  i: integer;

begin
//
  for i := 0 to TJSArray(ToolBarItems).Length - 1 do begin
    ToolBarItems[i].setProperty('font-weight', 'normal');
    ToolBarItems[i].setProperty('text-decoration', 'none');
    If ToolBarItems[i].Tag = FormName then
    begin
      ToolBarItems[i].setProperty('font-weight', 'bold');
      ToolBarItems[i].setProperty('text-decoration', 'underline');
    end;
  end;
end;

end.


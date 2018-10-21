unit jDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement, JPanel, JButton;

type
  TWDialog = class(TCustomControl)
  protected
    DialogBox : TWPanel;
    CloseButton: TWButton;
    procedure ArrangeElements;
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure OpenDialog(DialogMessage: String);
  end;

implementation



{ TWDialog }

constructor TWDialog.Create(parent: TCustomControl);
  procedure doCloseButtonOnClick(sender:TObject);
  begin
    self.SetProperty('display','none');
  end;

begin
  inherited Create('div', parent);
  self.SetProperty('display','none');
  self.SetProperty('background-color', 'rgb(0,0,0)');
  self.SetProperty('background-color', 'rgba(0,0,0,0.4)');
  TJSHTMLElement(self.Handle).style.setProperty('width', '100%');
  TJSHTMLElement(self.Handle).style.setProperty('height', '100%');

  DialogBox := TWPanel.Create(self);
  DialogBox.SetProperty('background-color', 'whitesmoke');
  DialogBox.SetProperty('margin', '10% 5% 0% 10%');
  DialogBox.SetProperty('border', '1px solid #888');
  DialogBox.SetProperty('width', '80%');
  DialogBox.SetProperty('height', '30%');

  CloseButton := TWButton.Create(DialogBox);
  CloseButton.SetinnerHTML('x');
  CloseButton.SetAttribute('style', 'margin: 2px 2px; float: right; cursor: pointer;');

  CloseButton.OnClick :=  @doCloseButtonOnClick;
end;

procedure TWDialog.OpenDialog(DialogMessage: String);
begin
  ArrangeElements;
  // todo : set title
  self.SetProperty('display','inline-block');
end;

procedure TWDialog.ArrangeElements;
var
  d: TJSHTMLCollection;
  i, j, x, y, z: Integer;
  TempArray : Array of JSValue;
begin
  //move all children of self, except dialogbox, to dialogbox
  //so this component can be invoked as if it is a normal form
  d := self.Handle.children;
  for i := 0 to d.length -1 do
    TJSArray(TempArray).push(d[i]);

  z := 0;
  for j := 0 to TJSArray(TempArray).length -1 do
    If TJSElement(TempArray[j]).id <> DialogBox.Handle.id then begin  //omit DialogBox

      // set child.top at least to CloseButton.height so as not to obscure close button
      x := strtoInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('top'),'px'));
      If x <= 30 then
        TJSHTMLElement(TempArray[j]).style.setProperty('top', inttostr(x+30) + 'px');

      // set height of dialogbox depending on lowest child-bottom
      y := strtoInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('top'),'px')) +
               strtoInt(StrBefore(TJSHTMLElement(TempArray[j]).style.getPropertyValue('height'),'px'));
      If y > z then z := y;

      // move all elements (if any) from self to DialogBox
      DialogBox.Handle.appendChild(TJSNode(TempArray[j]));
    end;

  If z > 0 then
    TJSHTMLElement(DialogBox.Handle).style.setProperty('height', inttostr(z) + 'px');
end;

end.


unit JSelect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, JS, Web, JElement, JListBox, JPanel, JImage;

type
  TWSelect = class(TCustomControl)
  private
    ListBox: TWListBox;
    Panel: TWPanel;
    Chevron: TWImage;
  public
    constructor Create(parent: TCustomControl); virtual;
    Procedure Add(item: TCustomControl);
    Value : String;
  end;

implementation



{ TWSelect }

constructor TWSelect.Create(parent: TCustomControl);
  procedure doPanelClick(sender: TObject);
  begin
    ListBox.SetProperty('display','inline-block');
  end;

  procedure doOnReadyExecute(sender: TObject);
  begin
    Panel.SetBounds(0,0,width-2,20);
    Chevron.SetBounds(width-22,2,16,16);
    Chevron.SetProperty('max-height','16px');
    Chevron.SetProperty('max-width','16px');
    ListBox.Width := self.Width;
    ListBox.Height := self.Height - 22;
  end;

begin
  inherited Create('div', parent);

  Panel := TWPanel.Create(self);
  Panel.OnClick := @doPanelClick;
  Panel.SetProperty('border','1px solid silver');
  Panel.SetinnerHTML('select...');

  Chevron := TWImage.Create(self);
  Chevron.SetAttribute('src','images/chevron-down.png');
  Chevron.OnClick := Panel.OnClick;

  ListBox := TWListBox.Create(self);
  Listbox.SetProperty('display','none');
  ListBox.Top := 22;

  //self.Observe;
  self.OnReadyExecute := @doOnReadyExecute;
end;

procedure TWSelect.Add(item: TCustomControl);
  procedure postMessage(aList : TJSValueDynArray; aValue : JSValue); external name 'window.postMessage';
  procedure doSelectOnClick(Sender:TObject);
  begin
    Panel.SetInnerHTML( (Sender as TCustomControl).tag);
    Value := (Sender as TCustomControl).tag;
    postMessage([self.Handle.id,'click',value],'*');
    Listbox.SetProperty('display','none');
  end;
begin
//
  ListBox.Add(item);
  Item.OnClick := @doSelectOnClick;

end;

end.


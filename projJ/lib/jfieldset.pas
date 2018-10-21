unit jFieldSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWFieldSet = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    Legend: String;
    Title : TCustomControl;
  end;

implementation



{ TWFieldSet }

constructor TWFieldSet.Create(parent: TCustomControl);

  procedure doFieldSetOnReadyExecute(sender: TObject);
  var
    i: integer;
    d: TJSHTMLCollection;
  begin
    //construct legend when applicable
    If self.legend <> '' then
    begin
      Title := TCustomControl.Create('legend',self);
      Title.Handle.innerHTML := self.Legend;
      Title.Handle.removeAttribute('style');
    end;

    d := self.Handle.children;
    for i := 0 to d.length -1 do begin
      If TJSHTMLElement(d[i]).style.getPropertyValue('height') = '0px' then
      begin
        TJSHTMLElement(d[i]).style.setProperty('left', '10px');
        TJSHTMLElement(d[i]).style.setProperty('top', inttostr(30 + (i*34)) + 'px');
        TJSHTMLElement(d[i]).style.setProperty('width', inttostr(self.width-4) + 'px');
        TJSHTMLElement(d[i]).style.setProperty('height', '30px');
      end;
    end;

  end;

begin
  inherited Create('fieldset', parent);
  SetProperty('border','1px solid silver');

  //self.Observe;
  self.OnReadyExecute := @doFieldSetOnReadyExecute;
end;

end.


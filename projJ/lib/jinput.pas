unit jInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWInput = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;


implementation

{ TWInput }

constructor TWInput.Create(parent: TCustomControl);
begin
  inherited Create('input', parent);
  self.SetAttribute('type','text');
end;

end.


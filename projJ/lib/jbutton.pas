unit jButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, jElement;

type
  TWButton = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;

implementation

constructor TWButton.Create(parent: TCustomControl);
begin
  inherited Create('button', parent);
  SetProperty('color','white');
  SetProperty('border-radius', '4px');
  SetProperty('background', '#699BCE');
  SetProperty('cursor','pointer');
  SetProperty('box-shadow','0 -1px 1px 0 rgba(0, 0, 0, 0.25) inset, 0 1px 1px 0 rgba(0, 0, 0, 0.10) inset;)');

end;

end.


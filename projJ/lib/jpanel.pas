unit jPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWPanel = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;


implementation

{ TWPanel }

constructor TWPanel.Create(parent: TCustomControl);
begin
  inherited Create('div', parent);
end;

end.


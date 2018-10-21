unit jImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWImage = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;

implementation


{ TWImage }

constructor TWImage.Create(parent: TCustomControl);
begin
  inherited Create('img', parent);
end;


end.


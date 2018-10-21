unit jCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWCanvas = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    ctx : TJSCanvasRenderingContext2D;
  end;

implementation



{ TWCanvas }

constructor TWCanvas.Create(parent: TCustomControl);
begin
  inherited Create('canvas', parent);

  ctx := TJSHTMLCanvasElement(self.Handle).getContextAs2DContext('2d');        //(self.AsChild).getContext('2d');
end;

end.


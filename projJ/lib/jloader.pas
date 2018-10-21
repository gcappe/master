unit JLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWLoader = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;

implementation

{ TWLoader }

constructor TWLoader.Create(parent: TCustomControl);
var
  s: String;

begin
  inherited Create('div', parent);
  setProperty('border','6px solid #f3f3f3');
  setProperty('border-radius','50%');
  setProperty('border-top','6px solid #3498db');
  setProperty('-webkit-animation','spin 2s linear infinite');
  setProperty('animation','spin 2s linear infinite');

  s := '@-webkit-keyframes spin {0% { -webkit-transform: rotate(0deg); }100% { -webkit-transform: rotate(360deg); }}';

  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);

end;

end.


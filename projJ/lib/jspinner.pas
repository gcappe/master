unit jSpinner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement;

type
  TWSpinner = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
  end;

implementation



{ TWSpinner }

constructor TWSpinner.Create(parent: TCustomControl);
var
  s: String;

begin
  inherited Create('div', parent);
  setProperty('-webkit-animation','sk-rotate 2.0s infinite linear');
  setProperty('animation','sk-rotate 2.0s infinite linear');
(*
  s :=
  '.dot1'+
  '{'+
  'width: 60%;'+
  'height: 60%;'+
  'display: inline-block;'+
  'position: absolute;'+
  'top: 0;'+
  'background-color: #699BCE;'+
  'border-radius: 100%;'+
  '-webkit-animation: sk-bounce 2.0s infinite ease-in-out;'+
  'animation: sk-bounce 2.0s infinite ease-in-out;'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
//
  s :=
  '.dot2'+
  '{'+
  'width: 60%;'+
  'height: 60%;'+
  'display: inline-block;'+
  'position: absolute;'+
  'top: 0;'+
  'background-color: #699BCE;'+
  'border-radius: 100%;'+
  '-webkit-animation: sk-bounce 2.0s infinite ease-in-out;'+
  'animation: sk-bounce 2.0s infinite ease-in-out;'+
  'top: auto;'+
  'bottom: 0;'+
  '-webkit-animation-delay: -1.0s;'+
  'animation-delay: -1.0s;'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);

//
  s :=
  '@-webkit-keyframes sk-rotate {'+
  '100% {'+
  '-webkit-transform: rotate(360deg)'+
  '}'+
  '}';

  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
//
  s :=
  '@keyframes sk-rotate {'+
  '100% {'+
  'transform: rotate(360deg);'+
  '-webkit-transform: rotate(360deg)'+
  '}'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
//
  s :=
  '@-webkit-keyframes sk-bounce {'+
  '0%,'+
  '100% {'+
  '-webkit-transform: scale(0.0)'+
  '}'+
  '50% {'+
  '-webkit-transform: scale(1.0)'+
  '}'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
//
  s :=
  '@keyframes sk-bounce {'+
  '0%,'+
  '100% {'+
  'transform: scale(0.0);'+
  '-webkit-transform: scale(0.0);'+
  '}'+
  '50% {'+
  'transform: scale(1.0);'+
  '-webkit-transform: scale(1.0);'+
  '}'+
  '}';
  TJSCSSStyleSheet(document.styleSheets[0]).insertRule(s, 0);
*)
//
  self.SetinnerHTML('<div class="dot1"></div><div class="dot2"></div>');

end;

end.


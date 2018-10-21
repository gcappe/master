unit jStringTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web, JElement, JPanel;

type
  TWStrTable = class(TCustomControl)
  public
    constructor Create(parent: TCustomControl); virtual;
    procedure AddColumn(title: string; colwidth: integer);
    procedure AddCell(row, column: integer; cell: String);
    ColumnCount : integer;
    RowCount: integer;
    ColumnWidths : array of integer;
    TableRow : String;
    TitleRow : String;
  end;

implementation



{ TWTable }

constructor TWStrTable.Create(parent: TCustomControl);
begin
  inherited Create('table', parent);
  ColumnCount := 0;
  RowCount := 0;
  setProperty('transform','none');
end;

procedure TWStrTable.AddColumn(title: string; colwidth: integer);
begin
//
//add columnwidth to array
  TJSArray(ColumnWidths).push(colwidth);

  Inc(ColumnCount);
//when inserting the first column, create a tr row element
  If ColumnCount = 1 then
    TitleRow := '<tr>';

//delete tr closure if it exists
  TitleRow := StrBefore(TitleRow, '</tr>');

  TitleRow := TitleRow + '<th style="width:' + inttostr(colwidth)
                       + 'px;text-align:left;border:1px solid lightgrey">' + title + '</th></tr>';

  self.SetInnerHtml(TitleRow);

end;

procedure TWStrTable.AddCell(row, column: integer; cell: String);
begin
//
//when inserting the first cell in a row, create a tr row element
  If Column = 1 then begin
    TableRow := '<tr>';
    Inc(RowCount);
  end;

  If odd(RowCount)
    then TableRow := TableRow + '<td style="background-color:whitesmoke">' + cell + '</td>'
    else TableRow := TableRow + '<td style="background-color:white">' + cell + '</td>';

//when inserting the last cell in a row,
  If Column = ColumnCount then
  begin
    TableRow := TableRow + '</tr>';
    self.SetInnerHtml(self.GetinnerHTML + TableRow);
  end;

end;

end.


unit FormHistogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries,
  TAChartUtils, ImageProcessing;

type

  { TFormHist }

  TFormHist = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Chart1LineSeries4: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure ShowHistogram(const histData: THistogramData);
  end;

var
  FormHist: TFormHist;

implementation

{$R *.lfm}

{ TFormHist }

procedure TFormHist.FormCreate(Sender: TObject);
begin
  // Configurar el gráfico
  Chart1LineSeries1.Title := 'Intensidad';
  Chart1LineSeries2.Title := 'Rojo';
  Chart1LineSeries3.Title := 'Verde';
  Chart1LineSeries4.Title := 'Azul';
  
  // Colores de las series
  Chart1LineSeries1.SeriesColor := clGray;
  Chart1LineSeries2.SeriesColor := clRed;
  Chart1LineSeries3.SeriesColor := clLime;
  Chart1LineSeries4.SeriesColor := clBlue;
  
  // Configurar ejes
  try
    Chart1.AxisList[1].Range.UseMin := True;
    Chart1.AxisList[1].Range.Min := 0;
    Chart1.AxisList[1].Range.UseMax := True;
    Chart1.AxisList[1].Range.Max := 255;
  except
  end;
end;

procedure TFormHist.ShowHistogram(const histData: THistogramData);
var
  i: Integer;
begin
  // Limpiar series
  Chart1LineSeries1.Clear;
  Chart1LineSeries2.Clear;
  Chart1LineSeries3.Clear;
  Chart1LineSeries4.Clear;
  
  // Agregar datos
  for i := 0 to 255 do
  begin
    Chart1LineSeries2.AddXY(i, histData.Red[i]);
    Chart1LineSeries3.AddXY(i, histData.Green[i]);
    Chart1LineSeries4.AddXY(i, histData.Blue[i]);
    Chart1LineSeries1.AddXY(i, histData.Intensity[i]);
  end;
  
  Chart1.Invalidate;
end;

end.

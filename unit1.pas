unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, ExtDlgs, StdCtrls, TAGraph, TASeries, TAChartUtils, Math;

type

  //definir tipos propios
   //MATRGB= Array of Array of Array of byte;
   RGB_MATRIX = Array of Array of Array of byte;
   HSV_MATRIX = Array of Array of Array of byte;
   GRAY_SCALE_MATRIX = Array of Array of Byte;

  { TForm1 }

  TForm1 = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1BarSeries1: TBarSeries;
    Chart1BarSeries2: TBarSeries;
    Chart1BarSeries3: TBarSeries;
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    Shape1: TShape;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure hideLabelShape(Sender: TObject);
    //procedure callGenerate
    procedure MenuItem2Click(Sender: TObject); // Abrir imagen
    procedure MenuItem4Click(Sender: TObject); // generar histograma
    procedure MenuItem6Click(Sender: TObject); // Botón de escala de grises
    //procedure MenuItem7Click(Sender: TObject); // HSV
    procedure MenuItem8Click(Sender: TObject); // Botón de restaurar
  private

  public

    //procedure copiaItoM(Al,An: Integer; B: Tbitmap;  var M:RGB_MATRIX);   //copiar de bitmap a matriz con scanline
    //procedure copiaMtoI(Al,An: Integer; M:RGB_MATRIX; var B:Tbitmap  );   //copiar de matriz a la imagen con scanline

    // Procedimiento para convertir un valor de RGB a HSV
    procedure RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
    // Copiar una imagen a una matriz
    procedure copyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap; var matrix:RGB_MATRIX);

    // Copiar una matriz a una imagen
    procedure copyMatrixToImage(imageHeight, imageWidth: Integer; matrix: RGB_MATRIX; var B: TBitmap);


    // RGB a HSV
    procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer; const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);
    // HSV a RGB
    //procedure HSVMatrixToRGBMatrix(imageHeight, imageWidth: Integer; const HSV: HSV_MATRIX; var RGB: RGB_MATRIX);
    // Escala de grises
    procedure mediumRangeGrayScale(imageHeight, imageWidth: Integer; var matrix: RGB_MATRIX; var CONVERTED_GRAY_MATRIX: RGB_MATRIX; B: TBitmap);
    //Generar histograma
    procedure generateHistogram(imageHeight, imageWidth: Integer; const matrix: RGB_MATRIX);

  end;

var
  Form1: TForm1;

  IMG_HEIGHT, IMG_WIDTH, COLOR_MODE: Integer;
  //MAT: RGB_MATRIX ;  //del tipo propio para alamacenar R,G,B
  MATRIX, ORIGINAL_MATRIX: RGB_MATRIX;
  CONVERTED_HSV_MATRIX:  HSV_MATRIX;
  GRAY_SCALE_VALUES: GRAY_SCALE_MATRIX;
  CONVERTED_GRAY_MATRIX: RGB_MATRIX;
  BMAP: TBitmap;   //para acceso a imagenes bmp

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.copyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap; var matrix:RGB_MATRIX);
var
  i, j, k: Integer;
  P: PByte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
  begin
    Exit;
  end;

  B.BeginUpdate;
  try
    for i := 0 to imageHeight - 1 do
    begin
      P := B.ScanLine[i];
      for j := 0 to imageWidth - 1 do
      begin
        k := 3 * j;
        matrix[j, i, 0] := P[k + 2]; // R
        matrix[j, i, 1] := P[k + 1]; // G
        matrix[j, i, 2] := P[k + 0]; // B
      end; // j
    end; // i
  finally
    B.EndUpdate;
  end;
end;

procedure TForm1.copyMatrixToImage(imageHeight, imageWidth: Integer; matrix:RGB_MATRIX; var B:TBitmap);
var
  i, j, k: Integer;
  P: PByte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
  begin
    Exit;
  end;

  B.BeginUpdate;
  try
    for i := 0 to imageHeight - 1 do
    begin
      P := B.ScanLine[i];
      for j := 0 to imageWidth - 1 do
      begin
        k := 3 * j;
        P[k + 2] := matrix[j, i, 0]; // R
        P[k + 1] := matrix[j, i, 1]; // G
        P[k + 0] := matrix[j, i, 2]; // B
      end; // j
    end; // i
  finally
    B.EndUpdate;
  end;

  // HEIGHT := B.Height;
  // WIDTH := B.Width;
end;

procedure TForm1.RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
var
  rf, gf, bf, cmax, cmin, delta, H, S, V: Double;
begin
  // 1. Normalizar valores RGB de 0..255 a 0..1
  rf := r / 255.0;
  gf := g / 255.0;
  bf := b / 255.0;

  cmax := Max(rf, Max(gf, bf));
  cmin := Min(rf, Min(gf, bf));
  delta := cmax - cmin;

  // 2. Calcular Valor (V) -> Rango 0..1
  V := cmax;

  // 3. Calcular Saturación (S) -> Rango 0..1
  if cmax = 0 then
    S := 0
  else
    S := delta / cmax;

  // 4. Calcular Matiz (H) -> Rango 0..360 grados
  if delta = 0 then
    H := 0
  else
  begin
    if cmax = rf then
      H := (gf - bf) / delta
    else if cmax = gf then
      H := 2.0 + (bf - rf) / delta
    else
      H := 4.0 + (rf - gf) / delta;

    H := H * 60; // Convertir a grados
    if H < 0 then
      H := H + 360;
  end;

  // 5. Convertir todo a Byte (0..255)
  // H se normaliza dividiendo por 360 y multiplicando por 255
  Hb := Round((H / 360) * 255);
  // S y V ya están entre 0 y 1, solo se multiplican por 255
  Sb := Round(S * 255);
  Vb := Round(V * 255);
end;



procedure TForm1.RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer; const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);

var
  i, j: Integer;
  r, g, b, h, s, v: Byte;
begin
  SetLength(HSV, imageWidth, imageHeight, 3);
  for i := 0 to imageHeight - 1 do
  begin
    for j := 0 to imageWidth - 1 do
    begin
      r := RGB[j, i, 0];
      g := RGB[j, i, 1];
      b := RGB[j, i, 2];
      RGBToHSVByte(r, g, b, h, s, v);
      HSV[j, i, 0] := h;
      HSV[j, i, 1] := s;
      HSV[j, i, 2] := v;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //WIDTH := 0;
  //HEIGHT := 0;
  BMAP := TBitmap.Create;  //Instanciar-crear objeto de la clase Tbitmap
  Image1.OnMouseLeave := @hideLabelShape;
//   MenuItem4.OnClick := @generateHistogram;
   COLOR_MODE := 0;
end;


procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  // Protección básica: dimensiones válidas
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
  if (X < 0) or (X >= IMG_WIDTH) or (Y < 0) or (Y >= IMG_HEIGHT) then Exit;

  // Actualizar paneles solo si existen
  if StatusBar1.Panels.Count > 1 then
    StatusBar1.Panels[1].Text := IntToStr(X);
  if StatusBar1.Panels.Count > 2 then
    StatusBar1.Panels[2].Text := IntToStr(Y);

  if (Length(MATRIX) > 0) and (Length(MATRIX[0]) > 0) then
  begin
    if StatusBar1.Panels.Count > 4 then
      StatusBar1.Panels[4].Text := IntToStr(MATRIX[X, Y, 0]) + ',' + IntToStr(MATRIX[X, Y, 1]) + ',' + IntToStr(MATRIX[X, Y, 2]);
  end;

    if (Length(CONVERTED_HSV_MATRIX) > 0) and (Length(CONVERTED_HSV_MATRIX[0]) > 0) then
  begin
    if StatusBar1.Panels.Count > 8 then
      StatusBar1.Panels[8].Text := IntToStr(CONVERTED_HSV_MATRIX[X, Y, 0]) + ', ' + IntToStr(CONVERTED_HSV_MATRIX[X, Y, 1]) + ', ' + IntToStr(CONVERTED_HSV_MATRIX[X, Y, 2]);
  end;

  // Mostrar color (solo si hay matrices válidas)
  Label1.Visible := True;
  Shape1.Visible := True;
  if COLOR_MODE = 1 then
  begin
    if (Length(MATRIX) > 0) and (Length(MATRIX[0]) > 0) then
      Shape1.Brush.Color := RGBToColor(MATRIX[X, Y, 0], MATRIX[X, Y, 1], MATRIX[X, Y, 2])
    else
      Shape1.Brush.Color := clBtnFace;
  end
  else
  begin
    if (Length(CONVERTED_GRAY_MATRIX) > 0) and (Length(CONVERTED_GRAY_MATRIX[0]) > 0) then
      Shape1.Brush.Color := RGBToColor(CONVERTED_GRAY_MATRIX[X, Y, 0], CONVERTED_GRAY_MATRIX[X, Y, 1], CONVERTED_GRAY_MATRIX[X, Y, 2])
    else
      Shape1.Brush.Color := clBtnFace;
  end;

end;

procedure TForm1.mediumRangeGrayScale(imageHeight, imageWidth: Integer; var matrix: RGB_MATRIX; var CONVERTED_GRAY_MATRIX: RGB_MATRIX; B: TBitmap);
var
  i, j: Integer;
  red, green, blue, gray: Byte;
  minimumValue, maximumValue: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
  begin
    Exit;
  end;

  // CORRECCIÓN 1: Inicializar la matriz DE DESTINO, no la global auxiliar
  SetLength(CONVERTED_GRAY_MATRIX, imageWidth, imageHeight, 3);

  // Opcional: Si también necesitas la matriz de solo valores de gris (1 canal)
  SetLength(GRAY_SCALE_VALUES, imageWidth, imageHeight);

  for i := 0 to imageWidth - 1 do
  begin
    for j := 0 to imageHeight - 1 do
    begin
      red := matrix[i, j, 0];
      green := matrix[i, j, 1];
      blue := matrix[i, j, 2];

      // Rango Medio
      maximumValue := Max(red, Max(green, blue));
      minimumValue := Min(red, Min(green, blue));
      gray := (maximumValue + minimumValue) div 2;

      // Guardar en matriz de valores simples (para histogramas futuros)
      GRAY_SCALE_VALUES[i, j] := gray;

      // CORRECCIÓN 2: Escribir en la matriz que acabamos de inicializar
      CONVERTED_GRAY_MATRIX[i, j, 0] := gray;
      CONVERTED_GRAY_MATRIX[i, j, 1] := gray;
      CONVERTED_GRAY_MATRIX[i, j, 2] := gray;
    end;
  end;

  // Pasar los datos de la NUEVA matriz gris al Bitmap para visualizar
  copyMatrixToImage(imageHeight, imageWidth, CONVERTED_GRAY_MATRIX, B);
end;

procedure TForm1.hideLabelShape(Sender: TObject);
begin
  Label1.Visible := False;
  Shape1.Visible := False;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
    if OpenPictureDialog1.Execute then
    begin
     SetLength(MATRIX, 0, 0, 0);
     SetLength(ORIGINAL_MATRIX, 0, 0, 0);
     SetLength(CONVERTED_GRAY_MATRIX, 0, 0, 0);
     SetLength(CONVERTED_HSV_MATRIX, 0, 0, 0);

    Image1.Enabled := True;
    BMAP.LoadFromFile(OpenPictureDialog1.FileName);
    IMG_HEIGHT := BMAP.Height;
    IMG_WIDTH := BMAP.Width;

     if BMAP.PixelFormat <> pf24bit then   //garantizar 8 bits por canal
     begin
       BMAP.PixelFormat := pf24bit;
     end;

    StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
    SetLength(MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    SetLength(ORIGINAL_MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    copyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, MATRIX);  //copiar (TPicture)contenido de bitmap a MAT
    copyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, ORIGINAL_MATRIX); // respaldar matriz

    Image1.Picture.Assign(BMAP);  //ver imagen
    RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
     COLOR_MODE := 1;
  end;
end;
procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  if (IMG_WIDTH > 0) AND (IMG_HEIGHT > 0) then
  begin
  generateHistogram(IMG_HEIGHT, IMG_WIDTH, MATRIX);
   Chart1.Visible := True;
   ShowMessage('Histograma generado');
  end
  else
  ShowMessage('Primero carga una imagen');
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
var
  GRAY_MATRIX: GRAY_SCALE_MATRIX;
begin
  if (IMG_WIDTH > 0) AND (IMG_HEIGHT > 0) then
  begin
   mediumRangeGrayScale(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_GRAY_MATRIX, BMAP);
   Image1.Picture.Assign(BMAP);
   COLOR_MODE := 2;
  end;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
var
  i, j, k: Integer;
begin
  if (IMG_WIDTH > 0) AND (IMG_HEIGHT > 0) then
  begin
    copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, ORIGINAL_MATRIX, BMAP);

    SetLength(MATRIX,IMG_WIDTH,IMG_HEIGHT,3);
   //copyMatrixToImage(HEIGHT, WIDTH, MATRIX, BMAP);  //copiar (TPicture)contenido de bitmap a MAT
   for i := 0 to IMG_WIDTH - 1 do
    begin
      for j := 0 to IMG_HEIGHT - 1 do
       begin
         for k := 0 to 2 do
          MATRIX[i, j, k] := ORIGINAL_MATRIX[i, j, k];
       end;
    end;

   Image1.Picture.Assign(BMAP);  //visulaizar imagen
  RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
   COLOR_MODE := 1;
   ShowMessage('Imagen restaurada');
  end
  else
   ShowMessage('No hay imagen para restaurar');
end;

procedure TForm1.generateHistogram(imageHeight, imageWidth: Integer; const matrix: RGB_MATRIX);
var
  x, y, i: Integer;
  histR, histG, histB, histI: Array [0..255] of Integer;
  r, g, b, intensity: Byte;
begin
  // Limpiar histogramas
  for i := 0 to 255 do
  begin
    histR[i] := 0;
    histG[i] := 0;
    histB[i] := 0;
    histI[i] := 0;
  end;

  // Calcular frecuencias
  for x := 0 to imageWidth - 1 do
  begin
    for y := 0 to imageHeight - 1 do
    begin
      r := matrix[x, y, 0];
      g := matrix[x, y, 1];
      b := matrix[x, y, 2];

      Inc(histR[r]);
      Inc(histG[g]);
      Inc(histB[b]);

      intensity := (r + g + b) div 3;
      Inc(histI[intensity]);
    end;
  end;

  // Limpiar series
  Chart1BarSeries1.Clear;
  Chart1BarSeries2.Clear;
  Chart1BarSeries3.Clear;
  Chart1LineSeries1.Clear;

  // Asegurar rango del eje X para 0..255 (valores de intensidad)
  try
    Chart1.AxisList[1].Range.UseMin := True;
    Chart1.AxisList[1].Range.Min := 0;
    Chart1.AxisList[1].Range.UseMax := True;
    Chart1.AxisList[1].Range.Max := 255;
  except
    // si no está disponible, no bloquear la generación
  end;

  // Llenar series
  for i := 0 to 255 do
  begin
    Chart1BarSeries1.AddXY(i, histR[i]);
    Chart1BarSeries2.AddXY(i, histG[i]);
    Chart1BarSeries3.AddXY(i, histB[i]);
    Chart1LineSeries1.AddXY(i, histI[i]);
  end;

  // Forzar redibujado
  Chart1.Invalidate;
end;

end.

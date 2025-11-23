unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, ExtDlgs, StdCtrls, TAGraph, TASeries, Math;

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
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Chart1LineSeries4: TLineSeries;
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
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    Shape1: TShape;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure hideLabelShape(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject); // Abrir imagen
    procedure MenuItem6Click(Sender: TObject); // Botón de escala de grises
    procedure MenuItem8Click(Sender: TObject); // Botón de restaurar
  private

  public

    //procedure copiaItoM(Al,An: Integer; B: Tbitmap;  var M:RGB_MATRIX);   //copiar de bitmap a matriz con scanline
    //procedure copiaMtoI(Al,An: Integer; M:RGB_MATRIX; var B:Tbitmap  );   //copiar de matriz a la imagen con scanline

    // Procedimiento para copiar una imagen a una matriz
    procedure copyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap; var matrix:RGB_MATRIX);

    // Procedimiento para copiar una matriz a una imagen
    procedure copyMatrixToImage(imageHeight, imageWidth: Integer; matrix:RGB_MATRIX; var B:TBitmap);

    // Procedimiento para convertir un valor de RGB a HSV
    procedure RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
    procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer; const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);

    // Histograma y grises
    procedure mediumRangeGrayScale(imageHeight, imageWidth: Integer; var matrix: RGB_MATRIX; var CONVERTED_GRAY_MATRIX: RGB_MATRIX; B: TBitmap);
//    procedure generateHistogram;
  end;

var
  Form1: TForm1;

  HEIGHT, WIDTH, COLOR_MODE: Integer;
  //MAT: RGB_MATRIX ;  //del tipo propio para alamacenar R,G,B
  MATRIX: RGB_MATRIX;
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
  for i:=0 to imageHeight - 1 do
  begin
    B.BeginUpdate;
    P := B.ScanLine[i];
    B.EndUpdate;

    for j := 0 to imageWidth - 1 do
    begin
      k:= 3 * j;
      matrix[j, i, 0] := P [k + 2]; // R
      matrix[j, i, 1] := P [k + 1]; // G
      matrix[j, i, 2] := P [k + 0]; // B
    end; // j
  end; // i
end;

procedure TForm1.copyMatrixToImage(imageHeight, imageWidth: Integer; matrix:RGB_MATRIX; var B:TBitmap);
var
  i, j, k: Integer;
  P: PByte;
begin
  for i:=0 to imageHeight - 1 do
  begin
    B.BeginUpdate;
    P:= B.ScanLine[i];
    B.EndUpdate;
    for j:=0 to imageWidth - 1 do
    begin
      k:= 3 * j;
      P[k + 2]:= matrix[j, i, 0]; // R
      P[k + 1]:= matrix[j, i, 1]; // G
      P[k + 0]:= matrix[j, i, 2]; // B
    end; // j
  end; // i

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
   BMAP:=Tbitmap.Create;  //Instanciar-crear objeto de la clase Tbitmap
   Image1.OnMouseLeave := @hideLabelShape;
   COLOR_MODE := 0;
end;


procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  
  StatusBar1.Panels[1].Text:=IntToStr(X);
  StatusBar1.Panels[2].Text:=IntToStr(Y);
  StatusBar1.Panels[4].Text:= IntToStr(MATRIX[x,y,0])+','+IntToStr(MATRIX[x,y,1])+','+IntToStr(MATRIX[x,y,2]);
  StatusBar1.Panels[8].Text:= IntToStr(CONVERTED_HSV_MATRIX[x,y,0])+','+IntToStr(CONVERTED_HSV_MATRIX[x,y,1])+','+IntToStr(CONVERTED_HSV_MATRIX[x,y,2]);

  // Mostrar color
  Label1.Visible := True;
  Shape1.Visible := True;
  if COLOR_MODE = 1 then
    Shape1.Brush.Color := RGBToColor(MATRIX[x, y, 0], MATRIX[x, y, 1], MATRIX[x, y, 2])
  else
    Shape1.Brush.Color := RGBToColor(CONVERTED_GRAY_MATRIX[x, y, 0], CONVERTED_GRAY_MATRIX[x, y, 1], CONVERTED_GRAY_MATRIX[x, y, 2]);


end;

procedure TForm1.mediumRangeGrayScale(imageHeight, imageWidth: Integer; var matrix: RGB_MATRIX; var CONVERTED_GRAY_MATRIX: RGB_MATRIX; B: TBitmap);
var
  i, j: Integer;
  red, green, blue, gray: Byte;
  minimumValue, maximumValue: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then Exit;

  // CORRECCIÓN 1: Inicializar la matriz DE DESTINO, no la global auxiliar
  SetLength(CONVERTED_GRAY_MATRIX, imageWidth, imageHeight, 3);

  // Opcional: Si también necesitas la matriz de solo valores de gris (1 canal)
  SetLength(GRAY_SCALE_VALUES, imageWidth, imageHeight);

  for i := 0 to imageWidth - 1 do
  begin
    for j := 0 to imageHeight - 1 do
    begin
      red   := matrix[i, j, 0];
      green := matrix[i, j, 1];
      blue  := matrix[i, j, 2];

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
     Image1.Enabled:=True;
     BMAP.LoadFromFile(OpenPictureDialog1.FileName);
     HEIGHT:=BMAP.Height;
     WIDTH:=BMAP.Width;

     if BMAP.PixelFormat<> pf24bit then   //garantizar 8 bits por canal
     begin
      BMAP.PixelFormat:=pf24bit;
     end;
     StatusBar1.Panels[6].Text:=IntToStr(HEIGHT)+'x'+IntToStr(WIDTH);
     SetLength(MATRIX,WIDTH,HEIGHT,3);
     copyImageToMatrix(HEIGHT,WIDTH,BMAP,MATRIX);  //copiar (TPicture)contenido de bitmap a MAT
     Image1.Picture.Assign(BMAP);  //visulaizar imagen
     RGBMatrixToHSVMatrix(HEIGHT, WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
     COLOR_MODE := 1
  end;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
var
  GRAY_MATRIX: GRAY_SCALE_MATRIX;
begin
  if (WIDTH > 0) then
  begin
   mediumRangeGrayScale(HEIGHT, WIDTH, MATRIX, CONVERTED_GRAY_MATRIX, BMAP);
   Image1.Picture.Assign(BMAP);
   COLOR_MODE := 2;
  end;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  SetLength(MATRIX,WIDTH,HEIGHT,3);
  copyMatrixToImage(HEIGHT, WIDTH, MATRIX, BMAP);  //copiar (TPicture)contenido de bitmap a MAT
  Image1.Picture.Assign(BMAP);  //visulaizar imagen
  COLOR_MODE := 1
end;

end.


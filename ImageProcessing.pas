unit ImageProcessing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

type
  RGB_MATRIX = array of array of array of byte;
  HSV_MATRIX = array of array of array of byte;
  GRAY_SCALE_MATRIX = array of array of Byte;
  BYTE_LUT = array[0..255] of Byte;
  
  THistogramData = record
    Red: array[0..255] of Integer;
    Green: array[0..255] of Integer;
    Blue: array[0..255] of Integer;
    Intensity: array[0..255] of Integer;
  end;

{ Funciones de conversión de imágenes }
procedure CopyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap;
  var matrix: RGB_MATRIX);
procedure CopyMatrixToImage(imageHeight, imageWidth: Integer; 
  const matrix: RGB_MATRIX; var B: TBitmap);

{ Funciones de conversión de color }
procedure RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
procedure HSVByteToRGB(Hb, Sb, Vb: Byte; out r, g, b: Byte);
procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
  const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);

{ Funciones de procesamiento }
procedure BinarizeMatrix(const src: RGB_MATRIX; var dst: RGB_MATRIX;
  imageHeight, imageWidth: Integer; modeIndex: Integer; threshold: Byte);
procedure MediumRangeGrayScale(imageHeight, imageWidth: Integer;
  const matrix: RGB_MATRIX; var grayMatrix: RGB_MATRIX);
procedure ApplyGammaCorrection(var matrix: RGB_MATRIX; imageHeight, 
  imageWidth: Integer; gamma: Double);
procedure ApplyContrast(var matrix: RGB_MATRIX; imageHeight, imageWidth: Integer;
  methodIndex: Integer; percentile: Double);

// Non-destructive version: apply contrast reading from src and writing to dst
procedure ApplyContrastMatrix(const src: RGB_MATRIX; var dst: RGB_MATRIX; imageHeight, imageWidth: Integer;
  methodIndex: Integer; percentile: Double);

{ Funciones de análisis }
procedure GenerateHistogram(imageHeight, imageWidth: Integer; 
  const matrix: RGB_MATRIX; out histData: THistogramData);
procedure BuildGammaLUT(gamma: Double; var lut: BYTE_LUT);
function PercentileFromHist(const hist: array of Integer; total: Integer; 
  p: Double): Integer;


var
  histDataGlobal: THistogramData;
  
implementation

{ Implementación de funciones de conversión }

procedure CopyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap;
  var matrix: RGB_MATRIX);
var
  i, j, k: Integer;
  P: PByte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;

  SetLength(matrix, imageWidth, imageHeight, 3);
  B.BeginUpdate;
  try
    for i := 0 to imageHeight - 1 do
    begin
      P := B.ScanLine[i];
      for j := 0 to imageWidth - 1 do
      begin
        k := 3 * j;
        matrix[j, i, 0] := P[k + 2];  // R
        matrix[j, i, 1] := P[k + 1];  // G
        matrix[j, i, 2] := P[k + 0];  // B
      end;
    end;
  finally
    B.EndUpdate;
  end;
end;

procedure ApplyContrastMatrix(const src: RGB_MATRIX; var dst: RGB_MATRIX; imageHeight, imageWidth: Integer;
  methodIndex: Integer; percentile: Double);
var
  hist: array[0..255] of Integer;
  total, lowVal, highVal: Integer;
  i, x, y, k: Integer;
  lut: array[0..255] of Byte;
  HSVtmp: HSV_MATRIX;
  r, g, b: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;

  // Ensure destination size
  SetLength(dst, imageWidth, imageHeight, 3);

  if methodIndex in [0, 1] then
  begin
    // Convert source RGB to HSV temporary
    RGBMatrixToHSVMatrix(imageHeight, imageWidth, src, HSVtmp);

    for i := 0 to 255 do
      hist[i] := 0;
    for x := 0 to imageWidth - 1 do
      for y := 0 to imageHeight - 1 do
        Inc(hist[HSVtmp[x, y, 2]]);

    total := imageWidth * imageHeight;

    if methodIndex = 0 then
    begin
      lowVal := 255;
      highVal := 0;
      for i := 0 to 255 do
        if hist[i] > 0 then
        begin
          if i < lowVal then lowVal := i;
          if i > highVal then highVal := i;
        end;
    end
    else
    begin
      lowVal := PercentileFromHist(hist, total, percentile);
      highVal := PercentileFromHist(hist, total, 1.0 - percentile);
    end;

    if highVal <= lowVal then
      for i := 0 to 255 do
        lut[i] := i
    else
      for i := 0 to 255 do
        if i <= lowVal then
          lut[i] := 0
        else if i >= highVal then
          lut[i] := 255
        else
          lut[i] := Byte(Round((i - lowVal) * 255 / (highVal - lowVal)));

    for x := 0 to imageWidth - 1 do
      for y := 0 to imageHeight - 1 do
      begin
        HSVtmp[x, y, 2] := lut[HSVtmp[x, y, 2]];
        HSVByteToRGB(HSVtmp[x, y, 0], HSVtmp[x, y, 1], HSVtmp[x, y, 2], r, g, b);
        dst[x, y, 0] := r;
        dst[x, y, 1] := g;
        dst[x, y, 2] := b;
      end;
  end;
end;

procedure CopyMatrixToImage(imageHeight, imageWidth: Integer; 
  const matrix: RGB_MATRIX; var B: TBitmap);
var
  i, j, k: Integer;
  P: PByte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;

  B.BeginUpdate;
  try
    for i := 0 to imageHeight - 1 do
    begin
      P := B.ScanLine[i];
      for j := 0 to imageWidth - 1 do
      begin
        k := 3 * j;
        P[k + 2] := matrix[j, i, 0];  // R
        P[k + 1] := matrix[j, i, 1];  // G
        P[k + 0] := matrix[j, i, 2];  // B
      end;
    end;
  finally
    B.EndUpdate;
  end;
end;

procedure RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
var
  rf, gf, bf, cmax, cmin, delta, H, S, V: Double;
begin
  rf := r / 255.0;
  gf := g / 255.0;
  bf := b / 255.0;

  cmax := Max(rf, Max(gf, bf));
  cmin := Min(rf, Min(gf, bf));
  delta := cmax - cmin;

  V := cmax;
  if cmax = 0 then
    S := 0
  else
    S := delta / cmax;

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

    H := H * 60;
    if H < 0 then
      H := H + 360;
  end;

  Hb := Round((H / 360) * 255);
  Sb := Round(S * 255);
  Vb := Round(V * 255);
end;

procedure HSVByteToRGB(Hb, Sb, Vb: Byte; out r, g, b: Byte);
var
  H, S, V: Double;
  C, X, m: Double;
  Hp: Double;
  Rp, Gp, Bp: Double;
begin
  H := (Hb / 255.0) * 360.0;
  S := Sb / 255.0;
  V := Vb / 255.0;
  C := V * S;
  Hp := H / 60.0;
  X := C * (1 - Abs(Frac(Hp) * 2 - 1));
  
  if (Hp >= 0) and (Hp < 1) then
  begin
    Rp := C; Gp := X; Bp := 0;
  end
  else if (Hp >= 1) and (Hp < 2) then
  begin
    Rp := X; Gp := C; Bp := 0;
  end
  else if (Hp >= 2) and (Hp < 3) then
  begin
    Rp := 0; Gp := C; Bp := X;
  end
  else if (Hp >= 3) and (Hp < 4) then
  begin
    Rp := 0; Gp := X; Bp := C;
  end
  else if (Hp >= 4) and (Hp < 5) then
  begin
    Rp := X; Gp := 0; Bp := C;
  end
  else
  begin
    Rp := C; Gp := 0; Bp := X;
  end;
  
  m := V - C;
  r := Byte(Round((Rp + m) * 255));
  g := Byte(Round((Gp + m) * 255));
  b := Byte(Round((Bp + m) * 255));
end;

procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
  const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);
var
  i, j: Integer;
  r, g, b, h, s, v: Byte;
begin
  SetLength(HSV, imageWidth, imageHeight, 3);
  for i := 0 to imageHeight - 1 do
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

{ Funciones de procesamiento }

procedure BinarizeMatrix(const src: RGB_MATRIX; var dst: RGB_MATRIX;
  imageHeight, imageWidth, modeIndex: Integer; threshold: Byte);
var
  x, y: Integer;
  r, g, b: Byte;
  val: Integer;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
    
  SetLength(dst, imageWidth, imageHeight, 3);
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
    begin
      r := src[x, y, 0];
      g := src[x, y, 1];
      b := src[x, y, 2];
      
      case modeIndex of
        0: val := (Integer(r) + Integer(g) + Integer(b)) div 3;
        1: val := Round(0.299 * r + 0.587 * g + 0.114 * b);
        2: val := r;
        3: val := g;
        4: val := b;
      else
        val := (Integer(r) + Integer(g) + Integer(b)) div 3;
      end;

      if val >= threshold then
      begin
        dst[x, y, 0] := 255;
        dst[x, y, 1] := 255;
        dst[x, y, 2] := 255;
      end
      else
      begin
        dst[x, y, 0] := 0;
        dst[x, y, 1] := 0;
        dst[x, y, 2] := 0;
      end;
    end;
end;

procedure MediumRangeGrayScale(imageHeight, imageWidth: Integer;
  const matrix: RGB_MATRIX; var grayMatrix: RGB_MATRIX);
var
  i, j: Integer;
  red, green, blue, gray: Byte;
  minimumValue, maximumValue: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
    
  SetLength(grayMatrix, imageWidth, imageHeight, 3);
  for i := 0 to imageWidth - 1 do
    for j := 0 to imageHeight - 1 do
    begin
      red := matrix[i, j, 0];
      green := matrix[i, j, 1];
      blue := matrix[i, j, 2];
      maximumValue := Max(red, Max(green, blue));
      minimumValue := Min(red, Min(green, blue));
      gray := (maximumValue + minimumValue) div 2;
      grayMatrix[i, j, 0] := gray;
      grayMatrix[i, j, 1] := gray;
      grayMatrix[i, j, 2] := gray;
    end;
end;

procedure BuildGammaLUT(gamma: Double; var lut: BYTE_LUT);
var
  i: Integer;
  v: Double;
begin
  if gamma <= 0 then
    gamma := 1.0;
  for i := 0 to 255 do
  begin
    if i = 0 then
      lut[i] := 0
    else
    begin
      v := Power((i / 255.0), gamma);
      v := v * 255.0;
      if v < 0.0 then v := 0.0;
      if v > 255.0 then v := 255.0;
      lut[i] := Byte(Round(v));
    end;
  end;
end;

procedure ApplyGammaCorrection(var matrix: RGB_MATRIX; imageHeight, 
  imageWidth: Integer; gamma: Double);
var
  x, y, k: Integer;
  lut: BYTE_LUT;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
    
  BuildGammaLUT(gamma, lut);
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      for k := 0 to 2 do
        matrix[x, y, k] := lut[matrix[x, y, k]];
end;

function PercentileFromHist(const hist: array of Integer; total: Integer; 
  p: Double): Integer;
var
  i: Integer;
  cum: Int64;
  target: Int64;
begin
  if total <= 0 then Exit(0);
  if p <= 0 then Exit(0);
  if p >= 1 then Exit(255);
  
  target := Round(p * total);
  cum := 0;
  for i := 0 to High(hist) do
  begin
    cum := cum + hist[i];
    if cum >= target then
      Exit(i);
  end;
  Result := High(hist);
end;

procedure ApplyContrast(var matrix: RGB_MATRIX; imageHeight, imageWidth: Integer;
  methodIndex: Integer; percentile: Double);
var
  hist: array[0..255] of Integer;
  total, lowVal, highVal: Integer;
  i, x, y, k: Integer;
  lut: array[0..255] of Byte;
  HSVtmp: HSV_MATRIX;
  r, g, b: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;

  if methodIndex in [0, 1] then
  begin
    // Aplicar en luminancia (HSV) using non-destructive helper
    ApplyContrastMatrix(matrix, matrix, imageHeight, imageWidth, methodIndex, percentile);
  end;
end;

procedure GenerateHistogram(imageHeight, imageWidth: Integer; 
  const matrix: RGB_MATRIX; out histData: THistogramData);
var
  x, y, i: Integer;
  r, g, b, intensity: Byte;
begin
  // Inicializar histogramas
  for i := 0 to 255 do
  begin
    histData.Red[i] := 0;
    histData.Green[i] := 0;
    histData.Blue[i] := 0;
    histData.Intensity[i] := 0;
  end;

  // Calcular frecuencias
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
    begin
      r := matrix[x, y, 0];
      g := matrix[x, y, 1];
      b := matrix[x, y, 2];
      Inc(histData.Red[r]);
      Inc(histData.Green[g]);
      Inc(histData.Blue[b]);
      intensity := (r + g + b) div 3;
      Inc(histData.Intensity[intensity]);
    end;
end;

end.

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, ExtDlgs, StdCtrls, TAGraph, TASeries, TAChartUtils, Math;

type

  RGB_MATRIX = array of array of array of byte;
  HSV_MATRIX = array of array of array of byte;
  GRAY_SCALE_MATRIX = array of array of Byte;
  BYTE_LUT = array[0..255] of Byte;

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
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem8: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    Shape1: TShape;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Button1: TButton;
    ToolButtonSave: TToolButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure hideLabelShape(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure Edit2Exit(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
  private
    PendingGammaPreview: Boolean;
    PendingContrastPreview: Boolean;
  public
    procedure binarizeMatrix(const src: RGB_MATRIX; var dst: RGB_MATRIX;
      imageHeight, imageWidth: Integer; modeIndex: Integer; threshold: Byte);
    procedure previewBinarize;
    procedure applyBinarize;
    procedure RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
    procedure copyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap;
      var matrix: RGB_MATRIX);
    procedure copyMatrixToImage(imageHeight, imageWidth: Integer; matrix: RGB_MATRIX;
      var B: TBitmap);
    procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
      const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);
    procedure mediumRangeGrayScale(imageHeight, imageWidth: Integer;
      var matrix: RGB_MATRIX; var CONVERTED_GRAY_MATRIX: RGB_MATRIX; B: TBitmap);
    procedure generateHistogram(imageHeight, imageWidth: Integer; const matrix: RGB_MATRIX);
    procedure BuildGammaLUT(gamma: Double; var lut: BYTE_LUT);
    procedure PreviewGamma;
    procedure ApplyGamma;
    procedure PreviewContrast;
    procedure ApplyContrast;
  end;

var
  Form1: TForm1;
  IMG_HEIGHT, IMG_WIDTH, COLOR_MODE: Integer;
  MATRIX, ORIGINAL_MATRIX: RGB_MATRIX;
  CONVERTED_HSV_MATRIX: HSV_MATRIX;
  GRAY_SCALE_VALUES: GRAY_SCALE_MATRIX;
  CONVERTED_GRAY_MATRIX: RGB_MATRIX;
  BMAP: TBitmap;

implementation

{$R *.lfm}

procedure TForm1.copyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap;
  var matrix: RGB_MATRIX);
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
        matrix[j, i, 0] := P[k + 2];
        matrix[j, i, 1] := P[k + 1];
        matrix[j, i, 2] := P[k + 0];
      end;
    end;
  finally
    B.EndUpdate;
  end;
end;

procedure TForm1.copyMatrixToImage(imageHeight, imageWidth: Integer; matrix: RGB_MATRIX;
  var B: TBitmap);
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
        P[k + 2] := matrix[j, i, 0];
        P[k + 1] := matrix[j, i, 1];
        P[k + 0] := matrix[j, i, 2];
      end;
    end;
  finally
    B.EndUpdate;
  end;
end;

procedure TForm1.RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
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

procedure TForm1.RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  BMAP := TBitmap.Create;
  Image1.OnMouseLeave := @hideLabelShape;
  COLOR_MODE := 0;
  Timer1.Interval := 150;
  Timer1.Enabled := False;
  Timer1.OnTimer := @Timer1Timer;
  TrackBar1.OnChange := @TrackBar1Change;
  MenuItem7.OnClick := @MenuItem7Click;
  Button1.Enabled := False;
  MenuItem10.OnClick := @MenuItem10Click;
  TrackBar2.OnChange := @TrackBar2Change;
  Button3.OnClick := @Button3Click;
  Button2.OnClick := @Button2Click;
  Edit1.OnExit := @Edit1Exit;
  if Assigned(MenuItem9) then
    MenuItem9.OnClick := @MenuItem9Click;
  if Assigned(ToolButtonSave) then
    ToolButtonSave.OnClick := @MenuItem9Click;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Button1.Enabled := True;
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  applyBinarize;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Label2.Caption := 'Umbral: ' + IntToStr(TrackBar1.Position);
  Timer1.Enabled := False;
  Timer1.Enabled := True;
  Button1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if PendingGammaPreview then
  begin
    PendingGammaPreview := False;
    PreviewGamma;
    Exit;
  end;

  if PendingContrastPreview then
  begin
    PendingContrastPreview := False;
    PreviewContrast;
    Exit;
  end;

  previewBinarize;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  TrackBar1.Visible := True;
  TrackBar1.Max := 255;
  if TrackBar1.Position = 0 then
    TrackBar1.Position := 128;
  ComboBox1.Visible := True;
  if (ComboBox1.ItemIndex < 0) and (ComboBox1.Items.Count > 0) then
    ComboBox1.ItemIndex := 0;
  Label2.Visible := True;
  Label2.Caption := 'Umbral: ' + IntToStr(TrackBar1.Position);
  Button1.Visible := True;
  Button1.Enabled := (IMG_WIDTH > 0) and (IMG_HEIGHT > 0);
  if (IMG_WIDTH > 0) and (IMG_HEIGHT > 0) then
    previewBinarize
  else
    ShowMessage('Coloca o abre una imagen antes de aplicar la binarización.');
end;

procedure TForm1.binarizeMatrix(const src: RGB_MATRIX; var dst: RGB_MATRIX;
  imageHeight, imageWidth: Integer; modeIndex: Integer; threshold: Byte);
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

procedure TForm1.previewBinarize;
var
  dst: RGB_MATRIX;
  tmpBmp: TBitmap;
  modeIndex: Integer;
  threshold: Byte;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
    Exit;
  modeIndex := 0;
  TrackBar1.Visible := True;
  Button1.Enabled := True;
  Button1.Visible := True;
  threshold := 0;
  if Assigned(TrackBar1) then
    threshold := Byte(TrackBar1.Position);

  binarizeMatrix(MATRIX, dst, IMG_HEIGHT, IMG_WIDTH, modeIndex, threshold);
  tmpBmp := TBitmap.Create;
  try
    tmpBmp.PixelFormat := pf24bit;
    tmpBmp.SetSize(IMG_WIDTH, IMG_HEIGHT);
    copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, dst, tmpBmp);
    Image1.Picture.Assign(tmpBmp);
  finally
    tmpBmp.Free;
  end;
end;

procedure TForm1.applyBinarize;
var
  dst: RGB_MATRIX;
  modeIndex: Integer;
  threshold: Byte;
  x, y, k: Integer;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
    Exit;
  modeIndex := ComboBox1.ItemIndex;
  threshold := Byte(TrackBar1.Position);
  binarizeMatrix(MATRIX, dst, IMG_HEIGHT, IMG_WIDTH, modeIndex, threshold);
  SetLength(MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
  for x := 0 to IMG_WIDTH - 1 do
    for y := 0 to IMG_HEIGHT - 1 do
      for k := 0 to 2 do
        MATRIX[x, y, k] := dst[x, y, k];

  copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
  ShowMessage('Binarización aplicada');
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
    Exit;
  if (X < 0) or (X >= IMG_WIDTH) or (Y < 0) or (Y >= IMG_HEIGHT) then
    Exit;
  if StatusBar1.Panels.Count > 1 then
    StatusBar1.Panels[1].Text := IntToStr(X);
  if StatusBar1.Panels.Count > 2 then
    StatusBar1.Panels[2].Text := IntToStr(Y);
  if (Length(MATRIX) > 0) and (Length(MATRIX[0]) > 0) then
    if StatusBar1.Panels.Count > 4 then
      StatusBar1.Panels[4].Text := IntToStr(MATRIX[X, Y, 0]) + ',' + IntToStr(MATRIX[X, Y, 1]) + ',' + IntToStr(MATRIX[X, Y, 2]);
  if (Length(CONVERTED_HSV_MATRIX) > 0) and (Length(CONVERTED_HSV_MATRIX[0]) > 0) then
    if StatusBar1.Panels.Count > 8 then
      StatusBar1.Panels[8].Text := IntToStr(CONVERTED_HSV_MATRIX[X, Y, 0]) + ', ' + IntToStr(CONVERTED_HSV_MATRIX[X, Y, 1]) + ', ' + IntToStr(CONVERTED_HSV_MATRIX[X, Y, 2]);

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

procedure TForm1.mediumRangeGrayScale(imageHeight, imageWidth: Integer;
  var matrix: RGB_MATRIX; var CONVERTED_GRAY_MATRIX: RGB_MATRIX; B: TBitmap);
var
  i, j: Integer;
  red, green, blue, gray: Byte;
  minimumValue, maximumValue: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
  SetLength(CONVERTED_GRAY_MATRIX, imageWidth, imageHeight, 3);
  SetLength(GRAY_SCALE_VALUES, imageWidth, imageHeight);
  for i := 0 to imageWidth - 1 do
    for j := 0 to imageHeight - 1 do
    begin
      red := matrix[i, j, 0];
      green := matrix[i, j, 1];
      blue := matrix[i, j, 2];
      maximumValue := Max(red, Max(green, blue));
      minimumValue := Min(red, Min(green, blue));
      gray := (maximumValue + minimumValue) div 2;
      GRAY_SCALE_VALUES[i, j] := gray;
      CONVERTED_GRAY_MATRIX[i, j, 0] := gray;
      CONVERTED_GRAY_MATRIX[i, j, 1] := gray;
      CONVERTED_GRAY_MATRIX[i, j, 2] := gray;
    end;
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
    if BMAP.PixelFormat <> pf24bit then
      BMAP.PixelFormat := pf24bit;
    StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
    SetLength(MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    SetLength(ORIGINAL_MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    copyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, MATRIX);
    copyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, ORIGINAL_MATRIX);
    Image1.Picture.Assign(BMAP);
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
    SetLength(MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    for i := 0 to IMG_WIDTH - 1 do
      for j := 0 to IMG_HEIGHT - 1 do
        for k := 0 to 2 do
          MATRIX[i, j, k] := ORIGINAL_MATRIX[i, j, k];
    Image1.Picture.Assign(BMAP);
    RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
    COLOR_MODE := 1;
    ShowMessage('Imagen restaurada');
    if ComboBox1.Visible then
      ComboBox1.Visible := False;
    if TrackBar1.Visible then
      TrackBar1.Visible := False;
    if Label2.Visible then
      Label2.Visible := False;
    if Button1.Visible then
      Button1.Visible := False;
  end
  else
    ShowMessage('No hay imagen para restaurar');
end;

procedure TForm1.generateHistogram(imageHeight, imageWidth: Integer; const matrix: RGB_MATRIX);
var
  x, y, i: Integer;
  histR, histG, histB, histI: array[0..255] of Integer;
  r, g, b, intensity: Byte;
begin
  for i := 0 to 255 do
  begin
    histR[i] := 0;
    histG[i] := 0;
    histB[i] := 0;
    histI[i] := 0;
  end;

  for x := 0 to imageWidth - 1 do
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

  try
    Chart1.AxisList[1].Range.UseMin := True;
    Chart1.AxisList[1].Range.Min := 0;
    Chart1.AxisList[1].Range.UseMax := True;
    Chart1.AxisList[1].Range.Max := 255;
  except
  end;

  for i := 0 to 255 do
  begin
    Chart1LineSeries2.AddXY(i, histR[i]);
    Chart1LineSeries3.AddXY(i, histG[i]);
    Chart1LineSeries4.AddXY(i, histB[i]);
    Chart1LineSeries1.AddXY(i, histI[i]);
  end;

  Chart1.Invalidate;
end;

procedure TForm1.BuildGammaLUT(gamma: Double; var lut: BYTE_LUT);
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
      if v < 0.0 then
        v := 0.0;
      if v > 255.0 then
        v := 255.0;
      lut[i] := Byte(Round(v));
    end;
  end;
end;

procedure TForm1.PreviewGamma;
var
  dst: RGB_MATRIX;
  tmpBmp: TBitmap;
  x, y, k: Integer;
  lut: BYTE_LUT;
  gammaVal: Double;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
    Exit;
  gammaVal := TrackBar2.Position / 100.0;
  BuildGammaLUT(gammaVal, lut);
  SetLength(dst, IMG_WIDTH, IMG_HEIGHT, 3);
  for x := 0 to IMG_WIDTH - 1 do
    for y := 0 to IMG_HEIGHT - 1 do
      for k := 0 to 2 do
        dst[x, y, k] := lut[MATRIX[x, y, k]];

  tmpBmp := TBitmap.Create;
  try
    tmpBmp.PixelFormat := pf24bit;
    tmpBmp.SetSize(IMG_WIDTH, IMG_HEIGHT);
    copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, dst, tmpBmp);
    Image1.Picture.Assign(tmpBmp);
  finally
    tmpBmp.Free;
  end;
end;

procedure TForm1.ApplyGamma;
var
  x, y, k: Integer;
  lut: BYTE_LUT;
  gammaVal: Double;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
    Exit;
  gammaVal := TrackBar2.Position / 100.0;
  BuildGammaLUT(gammaVal, lut);
  for x := 0 to IMG_WIDTH - 1 do
    for y := 0 to IMG_HEIGHT - 1 do
      for k := 0 to 2 do
        MATRIX[x, y, k] := lut[MATRIX[x, y, k]];
  copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
  ShowMessage('Filtro gamma aplicado');
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
    Rp := C;
    Gp := X;
    Bp := 0;
  end
  else if (Hp >= 1) and (Hp < 2) then
  begin
    Rp := X;
    Gp := C;
    Bp := 0;
  end
  else if (Hp >= 2) and (Hp < 3) then
  begin
    Rp := 0;
    Gp := C;
    Bp := X;
  end
  else if (Hp >= 3) and (Hp < 4) then
  begin
    Rp := 0;
    Gp := X;
    Bp := C;
  end
  else if (Hp >= 4) and (Hp < 5) then
  begin
    Rp := X;
    Gp := 0;
    Bp := C;
  end
  else
  begin
    Rp := C;
    Gp := 0;
    Bp := X;
  end;
  m := V - C;
  r := Byte(Round((Rp + m) * 255));
  g := Byte(Round((Gp + m) * 255));
  b := Byte(Round((Bp + m) * 255));
end;

function PercentileFromHistLocal(const hist: array of Integer; total: Integer; p: Double): Integer;
var
  i: Integer;
  cum: Int64;
  target: Int64;
begin
  if total <= 0 then
    Exit(0);
  if p <= 0 then
    Exit(0);
  if p >= 1 then
    Exit(255);
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

procedure TForm1.PreviewContrast;
var
  dst: RGB_MATRIX;
  tmpBmp: TBitmap;
  methodIndex: Integer;
  applyModeLuminance: Boolean;
  pval: Double;
  hist: array[0..255] of Integer;
  total, lowVal, highVal: Integer;
  i, x, y, k: Integer;
  lut: array[0..255] of Byte;
  HSVtmp: HSV_MATRIX;
  r, g, b: Byte;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
    Exit;
  methodIndex := ComboBox2.ItemIndex;
  applyModeLuminance := True;
  pval := StrToFloatDef(Edit2.Text, TrackBar3.Position / 100.0);
  if pval > 1.0 then
    pval := pval / 100.0;
  if pval < 0 then
    pval := 0;
  SetLength(dst, IMG_WIDTH, IMG_HEIGHT, 3);

  if methodIndex in [0, 1] then
  begin
    if applyModeLuminance then
    begin
      RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, HSVtmp);
      for i := 0 to 255 do
        hist[i] := 0;
      for x := 0 to IMG_WIDTH - 1 do
        for y := 0 to IMG_HEIGHT - 1 do
          Inc(hist[HSVtmp[x, y, 2]]);
      total := IMG_WIDTH * IMG_HEIGHT;
      if methodIndex = 0 then
      begin
        lowVal := 255;
        highVal := 0;
        for i := 0 to 255 do
          if hist[i] > 0 then
          begin
            if i < lowVal then
              lowVal := i;
            if i > highVal then
              highVal := i;
          end;
      end
      else
      begin
        lowVal := PercentileFromHistLocal(hist, total, pval);
        highVal := PercentileFromHistLocal(hist, total, 1.0 - pval);
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

      for x := 0 to IMG_WIDTH - 1 do
        for y := 0 to IMG_HEIGHT - 1 do
        begin
          HSVtmp[x, y, 2] := lut[HSVtmp[x, y, 2]];
          HSVByteToRGB(HSVtmp[x, y, 0], HSVtmp[x, y, 1], HSVtmp[x, y, 2], r, g, b);
          dst[x, y, 0] := r;
          dst[x, y, 1] := g;
          dst[x, y, 2] := b;
        end;
    end
    else
    begin
      for k := 0 to 2 do
      begin
        for i := 0 to 255 do
          hist[i] := 0;
        for x := 0 to IMG_WIDTH - 1 do
          for y := 0 to IMG_HEIGHT - 1 do
            Inc(hist[MATRIX[x, y, k]]);
        total := IMG_WIDTH * IMG_HEIGHT;
        if methodIndex = 0 then
        begin
          lowVal := 255;
          highVal := 0;
          for i := 0 to 255 do
            if hist[i] > 0 then
            begin
              if i < lowVal then
                lowVal := i;
              if i > highVal then
                highVal := i;
            end;
        end
        else
        begin
          lowVal := PercentileFromHistLocal(hist, total, pval);
          highVal := PercentileFromHistLocal(hist, total, 1.0 - pval);
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

        for x := 0 to IMG_WIDTH - 1 do
          for y := 0 to IMG_HEIGHT - 1 do
            dst[x, y, k] := lut[MATRIX[x, y, k]];
      end;
    end;
  end;

  tmpBmp := TBitmap.Create;
  try
    tmpBmp.PixelFormat := pf24bit;
    tmpBmp.SetSize(IMG_WIDTH, IMG_HEIGHT);
    copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, dst, tmpBmp);
    Image1.Picture.Assign(tmpBmp);
  finally
    tmpBmp.Free;
  end;
end;

procedure TForm1.ApplyContrast;
var
  methodIndex: Integer;
  applyModeLuminance: Boolean;
  pval: Double;
  hist: array[0..255] of Integer;
  total, lowVal, highVal: Integer;
  i, x, y, k: Integer;
  lut: array[0..255] of Byte;
  HSVtmp: HSV_MATRIX;
  r, g, b: Byte;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
    Exit;
  methodIndex := ComboBox2.ItemIndex;
  applyModeLuminance := True;
  pval := StrToFloatDef(Edit2.Text, TrackBar3.Position / 100.0);
  if pval > 1.0 then
    pval := pval / 100.0;
  if pval < 0 then
    pval := 0;

  if methodIndex in [0, 1] then
  begin
    if applyModeLuminance then
    begin
      RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, HSVtmp);
      for i := 0 to 255 do
        hist[i] := 0;
      for x := 0 to IMG_WIDTH - 1 do
        for y := 0 to IMG_HEIGHT - 1 do
          Inc(hist[HSVtmp[x, y, 2]]);
      total := IMG_WIDTH * IMG_HEIGHT;
      if methodIndex = 0 then
      begin
        lowVal := 255;
        highVal := 0;
        for i := 0 to 255 do
          if hist[i] > 0 then
          begin
            if i < lowVal then
              lowVal := i;
            if i > highVal then
              highVal := i;
          end;
      end
      else
      begin
        lowVal := PercentileFromHistLocal(hist, total, pval);
        highVal := PercentileFromHistLocal(hist, total, 1.0 - pval);
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

      for x := 0 to IMG_WIDTH - 1 do
        for y := 0 to IMG_HEIGHT - 1 do
        begin
          HSVtmp[x, y, 2] := lut[HSVtmp[x, y, 2]];
          HSVByteToRGB(HSVtmp[x, y, 0], HSVtmp[x, y, 1], HSVtmp[x, y, 2], r, g, b);
          MATRIX[x, y, 0] := r;
          MATRIX[x, y, 1] := g;
          MATRIX[x, y, 2] := b;
        end;
    end
    else
    begin
      for k := 0 to 2 do
      begin
        for i := 0 to 255 do
          hist[i] := 0;
        for x := 0 to IMG_WIDTH - 1 do
          for y := 0 to IMG_HEIGHT - 1 do
            Inc(hist[MATRIX[x, y, k]]);
        total := IMG_WIDTH * IMG_HEIGHT;
        if methodIndex = 0 then
        begin
          lowVal := 255;
          highVal := 0;
          for i := 0 to 255 do
            if hist[i] > 0 then
            begin
              if i < lowVal then
                lowVal := i;
              if i > highVal then
                highVal := i;
            end;
        end
        else
        begin
          lowVal := PercentileFromHistLocal(hist, total, pval);
          highVal := PercentileFromHistLocal(hist, total, 1.0 - pval);
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

        for x := 0 to IMG_WIDTH - 1 do
          for y := 0 to IMG_HEIGHT - 1 do
            MATRIX[x, y, k] := lut[MATRIX[x, y, k]];
      end;
    end;
  end;

  copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
  ShowMessage('Contraste aplicado');
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin
  if TrackBar3.Position = 0 then
    TrackBar3.Position := 10;
  Edit2.Text := FormatFloat('0.00', TrackBar3.Position / 100.0);
  TrackBar3.Visible := True;
  Edit2.Visible := True;
  Label4.Visible := True;
  Button4.Visible := True;
  Button5.Visible := True;
  ComboBox2.Visible := True;
  if (IMG_WIDTH > 0) and (IMG_HEIGHT > 0) then
    ShowMessage('Ajusta el recorte y pulsa "Vista previa" para ver resultados.')
  else
    ShowMessage('Carga una imagen antes de usar Contraste automático.');
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  Edit2.Text := FormatFloat('0.00', TrackBar3.Position / 100.0);
  PendingContrastPreview := True;
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TForm1.Edit2Exit(Sender: TObject);
var
  v: Double;
begin
  v := StrToFloatDef(Edit2.Text, TrackBar3.Position / 100.0);
  if v < 0.0 then
    v := 0.0;
  if v > 50.0 then
    v := 50.0;
  TrackBar3.Position := Round(v * 100);
  Edit2.Text := FormatFloat('0.00', TrackBar3.Position / 100.0);
  PendingContrastPreview := True;
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  PreviewContrast;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ApplyContrast;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  TrackBar2.Visible := True;
  Edit1.Visible := True;
  Label3.Visible := True;
  Button2.Visible := True;
  Button3.Visible := True;
  if TrackBar2.Position = 0 then
    TrackBar2.Position := 100;
  Edit1.Text := FormatFloat('0.00', TrackBar2.Position / 100.0);
  if (IMG_WIDTH > 0) and (IMG_HEIGHT > 0) then
    ShowMessage('Ajusta gamma y usa "Vista previa" antes de aplicar.')
  else
    ShowMessage('Carga una imagen antes de usar el filtro gamma.');
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  Edit1.Text := FormatFloat('0.00', TrackBar2.Position / 100.0);
  PendingGammaPreview := True;
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TForm1.Edit1Exit(Sender: TObject);
var
  v: Double;
begin
  v := StrToFloatDef(Edit1.Text, TrackBar2.Position / 100.0);
  if v < 0.1 then
    v := 0.1;
  if v > 10.0 then
    v := 10.0;
  TrackBar2.Position := Round(v * 100);
  Edit1.Text := FormatFloat('0.00', TrackBar2.Position / 100.0);
  PendingGammaPreview := True;
  Timer1.Enabled := False;
  Timer1.Enabled := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  PreviewGamma;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ApplyGamma;
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
var
  outName: string;
  tmpBmp: TBitmap;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('No hay imagen para guardar.');
    Exit;
  end;
  if SavePictureDialog = nil then
  begin
    ShowMessage('Save dialog no disponible.');
    Exit;
  end;
  SavePictureDialog.Filter := 'Bitmap (*.bmp)|*.bmp';
  SavePictureDialog.DefaultExt := 'bmp';
  if not SavePictureDialog.Execute then
    Exit;
  outName := SavePictureDialog.FileName;
  if ExtractFileExt(outName) = '' then
    outName := outName + '.bmp';
  tmpBmp := TBitmap.Create;
  try
    if (Image1.Picture.Graphic <> nil) then
      tmpBmp.Assign(Image1.Picture.Graphic)
    else
      tmpBmp.Assign(BMAP);
    tmpBmp.PixelFormat := pf24bit;
    try
      tmpBmp.SaveToFile(outName);
      ShowMessage('Imagen guardada: ' + outName)
    except
      on E: Exception do
        ShowMessage('Error guardando la imagen: ' + E.Message);
    end;
  finally
    tmpBmp.Free;
  end;
end;

end.
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, ExtDlgs, StdCtrls, TAGraph, TASeries, TAChartUtils, Math;

type

  RGB_MATRIX = Array of Array of Array of byte;
  HSV_MATRIX = Array of Array of Array of byte;
  GRAY_SCALE_MATRIX = Array of Array of Byte;
  BYTE_LUT = array[0..255] of Byte;

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
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem8: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    Shape1: TShape;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Button1: TButton;
    ToolButtonSave: TToolButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject;
Shift: TShiftState;
X, Y: Integer);
    procedure hideLabelShape(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure Edit2Exit(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
  private
    PendingGammaPreview: Boolean;
    PendingContrastPreview: Boolean;
  public
    procedure binarizeMatrix(const src: RGB_MATRIX;
var dst: RGB_MATRIX;
imageHeight, imageWidth: Integer;
modeIndex: Integer;
threshold: Byte);
    procedure previewBinarize;
    procedure applyBinarize;
    procedure RGBToHSVByte(r, g, b: Byte;
out Hb, Sb, Vb: Byte);
    procedure copyImageToMatrix(imageHeight, imageWidth: Integer;
B: TBitmap;
var matrix:RGB_MATRIX);
    procedure copyMatrixToImage(imageHeight, imageWidth: Integer;
matrix: RGB_MATRIX;
var B: TBitmap);
    procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
const RGB: RGB_MATRIX;
var HSV: HSV_MATRIX);
    procedure mediumRangeGrayScale(imageHeight, imageWidth: Integer;
var matrix: RGB_MATRIX;
var CONVERTED_GRAY_MATRIX: RGB_MATRIX;
B: TBitmap);
    procedure generateHistogram(imageHeight, imageWidth: Integer;
const matrix: RGB_MATRIX);
    procedure BuildGammaLUT(gamma: Double;
var lut: BYTE_LUT);
    procedure PreviewGamma;
    procedure ApplyGamma;
    procedure PreviewContrast;
    procedure ApplyContrast;
  end;

var
  Form1: TForm1;
  IMG_HEIGHT, IMG_WIDTH, COLOR_MODE: Integer;
  MATRIX, ORIGINAL_MATRIX: RGB_MATRIX;
  CONVERTED_HSV_MATRIX:  HSV_MATRIX;
  GRAY_SCALE_VALUES: GRAY_SCALE_MATRIX;
  CONVERTED_GRAY_MATRIX: RGB_MATRIX;
  BMAP: TBitmap;

implementation

{$R *.lfm}

procedure TForm1.copyImageToMatrix(imageHeight, imageWidth: Integer;
B: TBitmap;
var matrix:RGB_MATRIX);
var
  i, j, k: Integer;
  P: PByte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then Exit;
  B.BeginUpdate;
  try
    for i := 0 to imageHeight - 1 do
    begin
      P := B.ScanLine[i];
      for j := 0 to imageWidth - 1 do
      begin
        k := 3 * j;
        matrix[j, i, 0] := P[k + 2];
        matrix[j, i, 1] := P[k + 1];
        matrix[j, i, 2] := P[k + 0];
      end;
    end;
  finally
    B.EndUpdate;
  end;
end;

procedure TForm1.copyMatrixToImage(imageHeight, imageWidth: Integer;
matrix:RGB_MATRIX;
var B:TBitmap);
var
  i, j, k: Integer;
  P: PByte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then Exit;
  B.BeginUpdate;
  try
    for i := 0 to imageHeight - 1 do
    begin
      P := B.ScanLine[i];
      for j := 0 to imageWidth - 1 do
      begin
        k := 3 * j;
        P[k + 2] := matrix[j, i, 0];
        P[k + 1] := matrix[j, i, 1];
        P[k + 0] := matrix[j, i, 2];
      end;
    end;
  finally
    B.EndUpdate;
  end;
end;

procedure TForm1.RGBToHSVByte(r, g, b: Byte;
out Hb, Sb, Vb: Byte);
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
  if cmax = 0 then S := 0 else S := delta / cmax;
  if delta = 0 then H := 0 else
  begin
    if cmax = rf then H := (gf - bf) / delta
    else if cmax = gf then H := 2.0 + (bf - rf) / delta
    else H := 4.0 + (rf - gf) / delta;
    H := H * 60;
if H < 0 then H := H + 360;
  end;
  Hb := Round((H / 360) * 255);
Sb := Round(S * 255);
Vb := Round(V * 255);
end;

procedure TForm1.RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
const RGB: RGB_MATRIX;
var HSV: HSV_MATRIX);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  BMAP := TBitmap.Create;
  Image1.OnMouseLeave := @hideLabelShape;
  COLOR_MODE := 0;
  Timer1.Interval := 150;
Timer1.Enabled := False;
Timer1.OnTimer := @Timer1Timer;
  TrackBar1.OnChange := @TrackBar1Change;
MenuItem7.OnClick := @MenuItem7Click;
  Button1.Enabled := False;
  MenuItem10.OnClick := @MenuItem10Click;
TrackBar2.OnChange := @TrackBar2Change;
  Button3.OnClick := @Button3Click;
Button2.OnClick := @Button2Click;
Edit1.OnExit := @Edit1Exit;
  if Assigned(MenuItem9) then MenuItem9.OnClick := @MenuItem9Click;
  if Assigned(ToolButtonSave) then ToolButtonSave.OnClick := @MenuItem9Click;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  Button1.Enabled := True;
Timer1.Enabled := False;
Timer1.Enabled := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin applyBinarize;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Label2.Caption := 'Umbral: ' + IntToStr(TrackBar1.Position);
  Timer1.Enabled := False;
Timer1.Enabled := True;
Button1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if PendingGammaPreview then begin PendingGammaPreview := False;
PreviewGamma;
Exit;
end;
  if PendingContrastPreview then begin PendingContrastPreview := False;
PreviewContrast;
Exit;
end;
  previewBinarize;
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  TrackBar1.Visible := True;
TrackBar1.Max := 255;
if TrackBar1.Position = 0 then TrackBar1.Position := 128;
  ComboBox1.Visible := True;
if (ComboBox1.ItemIndex < 0) and (ComboBox1.Items.Count > 0) then ComboBox1.ItemIndex := 0;
  Label2.Visible := True;
Label2.Caption := 'Umbral: ' + IntToStr(TrackBar1.Position);
  Button1.Visible := True;
Button1.Enabled := (IMG_WIDTH > 0) and (IMG_HEIGHT > 0);
  if (IMG_WIDTH > 0) and (IMG_HEIGHT > 0) then previewBinarize else ShowMessage('Coloca o abre una imagen antes de aplicar la binarización.');
end;

procedure TForm1.binarizeMatrix(const src: RGB_MATRIX;
var dst: RGB_MATRIX;
imageHeight, imageWidth: Integer;
modeIndex: Integer;
threshold: Byte);
var x, y: Integer;
r, g, b: Byte;
val: Integer;
begin
  if (imageWidth = 0) or (imageHeight = 0) then Exit;
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
      else val := (Integer(r) + Integer(g) + Integer(b)) div 3;
end;
      if val >= threshold then begin dst[x, y, 0] := 255;
dst[x, y, 1] := 255;
dst[x, y, 2] := 255;
end
      else begin dst[x, y, 0] := 0;
dst[x, y, 1] := 0;
dst[x, y, 2] := 0;
end;
    end;
end;

procedure TForm1.previewBinarize;
var dst: RGB_MATRIX;
tmpBmp: TBitmap;
modeIndex: Integer;
threshold: Byte;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
  modeIndex := 0;
TrackBar1.Visible := True;
Button1.Enabled := True;
Button1.Visible := True;
  threshold := 0;
if Assigned(TrackBar1) then threshold := Byte(TrackBar1.Position);
  binarizeMatrix(MATRIX, dst, IMG_HEIGHT, IMG_WIDTH, modeIndex, threshold);
  tmpBmp := TBitmap.Create;
  try tmpBmp.PixelFormat := pf24bit;
tmpBmp.SetSize(IMG_WIDTH, IMG_HEIGHT);
    copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, dst, tmpBmp);
Image1.Picture.Assign(tmpBmp);
  finally tmpBmp.Free;
end;
end;

procedure TForm1.applyBinarize;
var dst: RGB_MATRIX;
modeIndex: Integer;
threshold: Byte;
x, y, k: Integer;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
  modeIndex := ComboBox1.ItemIndex;
threshold := Byte(TrackBar1.Position);
  binarizeMatrix(MATRIX, dst, IMG_HEIGHT, IMG_WIDTH, modeIndex, threshold);
  SetLength(MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
  for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do for k := 0 to 2 do MATRIX[x, y, k] := dst[x, y, k];
  copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
Image1.Picture.Assign(BMAP);
  RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
ShowMessage('Binarización aplicada');
end;

procedure TForm1.Image1MouseMove(Sender: TObject;
Shift: TShiftState;
X, Y: Integer);
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
  if (X < 0) or (X >= IMG_WIDTH) or (Y < 0) or (Y >= IMG_HEIGHT) then Exit;
  if StatusBar1.Panels.Count > 1 then StatusBar1.Panels[1].Text := IntToStr(X);
  if StatusBar1.Panels.Count > 2 then StatusBar1.Panels[2].Text := IntToStr(Y);
  if (Length(MATRIX) > 0) and (Length(MATRIX[0]) > 0) then if StatusBar1.Panels.Count > 4 then StatusBar1.Panels[4].Text := IntToStr(MATRIX[X, Y, 0]) + ',' + IntToStr(MATRIX[X, Y, 1]) + ',' + IntToStr(MATRIX[X, Y, 2]);
  if (Length(CONVERTED_HSV_MATRIX) > 0) and (Length(CONVERTED_HSV_MATRIX[0]) > 0) then if StatusBar1.Panels.Count > 8 then StatusBar1.Panels[8].Text := IntToStr(CONVERTED_HSV_MATRIX[X, Y, 0]) + ', ' + IntToStr(CONVERTED_HSV_MATRIX[X, Y, 1]) + ', ' + IntToStr(CONVERTED_HSV_MATRIX[X, Y, 2]);
  Label1.Visible := True;
Shape1.Visible := True;
  if COLOR_MODE = 1 then
    if (Length(MATRIX) > 0) and (Length(MATRIX[0]) > 0) then Shape1.Brush.Color := RGBToColor(MATRIX[X, Y, 0], MATRIX[X, Y, 1], MATRIX[X, Y, 2]) else Shape1.Brush.Color := clBtnFace
  else
    if (Length(CONVERTED_GRAY_MATRIX) > 0) and (Length(CONVERTED_GRAY_MATRIX[0]) > 0) then Shape1.Brush.Color := RGBToColor(CONVERTED_GRAY_MATRIX[X, Y, 0], CONVERTED_GRAY_MATRIX[X, Y, 1], CONVERTED_GRAY_MATRIX[X, Y, 2]) else Shape1.Brush.Color := clBtnFace;
end;

procedure TForm1.mediumRangeGrayScale(imageHeight, imageWidth: Integer;
var matrix: RGB_MATRIX;
var CONVERTED_GRAY_MATRIX: RGB_MATRIX;
B: TBitmap);
var i, j: Integer;
red, green, blue, gray: Byte;
minimumValue, maximumValue: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then Exit;
  SetLength(CONVERTED_GRAY_MATRIX, imageWidth, imageHeight, 3);
SetLength(GRAY_SCALE_VALUES, imageWidth, imageHeight);
  for i := 0 to imageWidth - 1 do for j := 0 to imageHeight - 1 do begin
    red := matrix[i, j, 0];
green := matrix[i, j, 1];
blue := matrix[i, j, 2];
    maximumValue := Max(red, Max(green, blue));
minimumValue := Min(red, Min(green, blue));
    gray := (maximumValue + minimumValue) div 2;
GRAY_SCALE_VALUES[i, j] := gray;
    CONVERTED_GRAY_MATRIX[i, j, 0] := gray;
CONVERTED_GRAY_MATRIX[i, j, 1] := gray;
CONVERTED_GRAY_MATRIX[i, j, 2] := gray;
  end;
  copyMatrixToImage(imageHeight, imageWidth, CONVERTED_GRAY_MATRIX, B);
end;

procedure TForm1.hideLabelShape(Sender: TObject);
begin Label1.Visible := False;
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
    if BMAP.PixelFormat <> pf24bit then BMAP.PixelFormat := pf24bit;
    StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
    SetLength(MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
SetLength(ORIGINAL_MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    copyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, MATRIX);
copyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, ORIGINAL_MATRIX);
    Image1.Picture.Assign(BMAP);
RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
COLOR_MODE := 1;
  end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin if (IMG_WIDTH > 0) AND (IMG_HEIGHT > 0) then begin generateHistogram(IMG_HEIGHT, IMG_WIDTH, MATRIX);
Chart1.Visible := True;
ShowMessage('Histograma generado');
end else ShowMessage('Primero carga una imagen');
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin if (IMG_WIDTH > 0) AND (IMG_HEIGHT > 0) then begin mediumRangeGrayScale(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_GRAY_MATRIX, BMAP);
Image1.Picture.Assign(BMAP);
COLOR_MODE := 2;
end;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
var i, j, k: Integer;
begin
  if (IMG_WIDTH > 0) AND (IMG_HEIGHT > 0) then
  begin
    copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, ORIGINAL_MATRIX, BMAP);
    SetLength(MATRIX,IMG_WIDTH,IMG_HEIGHT,3);
    for i := 0 to IMG_WIDTH - 1 do for j := 0 to IMG_HEIGHT - 1 do for k := 0 to 2 do MATRIX[i, j, k] := ORIGINAL_MATRIX[i, j, k];
    Image1.Picture.Assign(BMAP);
RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
COLOR_MODE := 1;
ShowMessage('Imagen restaurada');
    if ComboBox1.Visible then ComboBox1.Visible := False;
if TrackBar1.Visible then TrackBar1.Visible := False;
if Label2.Visible then Label2.Visible := False;
if Button1.Visible then Button1.Visible := False;
  end else ShowMessage('No hay imagen para restaurar');
end;

procedure TForm1.generateHistogram(imageHeight, imageWidth: Integer;
const matrix: RGB_MATRIX);
var x, y, i: Integer;
histR, histG, histB, histI: Array [0..255] of Integer;
r, g, b, intensity: Byte;
begin
  Chart1LineSeries2.Clear;
Chart1LineSeries3.Clear;
Chart1LineSeries4.Clear;
Chart1LineSeries1.Clear;
  for i := 0 to 255 do begin histR[i] := 0;
histG[i] := 0;
histB[i] := 0;
histI[i] := 0;
end;
  for x := 0 to imageWidth - 1 do for y := 0 to imageHeight - 1 do begin r := matrix[x, y, 0];
g := matrix[x, y, 1];
b := matrix[x, y, 2];
Inc(histR[r]);
Inc(histG[g]);
Inc(histB[b]);
intensity := (r + g + b) div 3;
Inc(histI[intensity]);
end;
  try Chart1.AxisList[1].Range.UseMin := True;
Chart1.AxisList[1].Range.Min := 0;
Chart1.AxisList[1].Range.UseMax := True;
Chart1.AxisList[1].Range.Max := 255;
except end;
  for i := 0 to 255 do begin Chart1LineSeries2.AddXY(i, histR[i]);
Chart1LineSeries3.AddXY(i, histG[i]);
Chart1LineSeries4.AddXY(i, histB[i]);
Chart1LineSeries1.AddXY(i, histI[i]);
end;
  Chart1.Invalidate;
end;

procedure TForm1.BuildGammaLUT(gamma: Double;
var lut: BYTE_LUT);
var i: Integer;
v: Double;
begin if gamma <= 0 then gamma := 1.0;
for i := 0 to 255 do begin if i = 0 then lut[i] := 0 else begin v := Power((i / 255.0), gamma);
v := v * 255.0;
if v < 0.0 then v := 0.0;
if v > 255.0 then v := 255.0;
lut[i] := Byte(Round(v));
end;
end;
end;

procedure TForm1.PreviewGamma;
var dst: RGB_MATRIX;
tmpBmp: TBitmap;
x, y, k: Integer;
lut: BYTE_LUT;
gammaVal: Double;
begin if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
gammaVal := TrackBar2.Position / 100.0;
BuildGammaLUT(gammaVal, lut);
SetLength(dst, IMG_WIDTH, IMG_HEIGHT, 3);
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do for k := 0 to 2 do dst[x, y, k] := lut[MATRIX[x, y, k]];
tmpBmp := TBitmap.Create;
try tmpBmp.PixelFormat := pf24bit;
tmpBmp.SetSize(IMG_WIDTH, IMG_HEIGHT);
copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, dst, tmpBmp);
Image1.Picture.Assign(tmpBmp);
finally tmpBmp.Free;
end;
end;

procedure TForm1.ApplyGamma;
var x, y, k: Integer;
lut: BYTE_LUT;
gammaVal: Double;
begin if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
gammaVal := TrackBar2.Position / 100.0;
BuildGammaLUT(gammaVal, lut);
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do for k := 0 to 2 do MATRIX[x, y, k] := lut[MATRIX[x, y, k]];
copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
Image1.Picture.Assign(BMAP);
RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
ShowMessage('Filtro gamma aplicado');
end;

procedure HSVByteToRGB(Hb, Sb, Vb: Byte;
out r, g, b: Byte);
var H, S, V: Double;
C, X, m: Double;
Hp: Double;
Rp, Gp, Bp: Double;
begin H := (Hb / 255.0) * 360.0;
S := Sb / 255.0;
V := Vb / 255.0;
C := V * S;
Hp := H / 60.0;
X := C * (1 - Abs(Frac(Hp) * 2 - 1));
  if (Hp >= 0) and (Hp < 1) then begin Rp := C;
Gp := X;
Bp := 0;
end
  else if (Hp >= 1) and (Hp < 2) then begin Rp := X;
Gp := C;
Bp := 0;
end
  else if (Hp >= 2) and (Hp < 3) then begin Rp := 0;
Gp := C;
Bp := X;
end
  else if (Hp >= 3) and (Hp < 4) then begin Rp := 0;
Gp := X;
Bp := C;
end
  else if (Hp >= 4) and (Hp < 5) then begin Rp := X;
Gp := 0;
Bp := C;
end
  else begin Rp := C;
Gp := 0;
Bp := X;
end;
m := V - C;
r := Byte(Round((Rp + m) * 255));
g := Byte(Round((Gp + m) * 255));
b := Byte(Round((Bp + m) * 255));
end;

function PercentileFromHistLocal(const hist: array of Integer;
total: Integer;
p: Double): Integer;
var i: Integer;
cum: Int64;
target: Int64;
begin if total <= 0 then Exit(0);
if p <= 0 then Exit(0);
if p >= 1 then Exit(255);
target := Round(p * total);
cum := 0;
for i := 0 to High(hist) do begin cum := cum + hist[i];
if cum >= target then Exit(i);
end;
Result := High(hist);
end;

procedure TForm1.PreviewContrast;
var dst: RGB_MATRIX;
tmpBmp: TBitmap;
methodIndex: Integer;
applyModeLuminance: Boolean;
pval: Double;
hist: array[0..255] of Integer;
total, lowVal, highVal: Integer;
i, x, y, k: Integer;
lut: array[0..255] of Byte;
HSVtmp: HSV_MATRIX;
r, g, b: Byte;
begin
if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
  methodIndex := ComboBox2.ItemIndex;
applyModeLuminance := True;
pval := StrToFloatDef(Edit2.Text, TrackBar3.Position / 100.0);
if pval > 1.0 then pval := pval / 100.0;
if pval < 0 then pval := 0;
SetLength(dst, IMG_WIDTH, IMG_HEIGHT, 3);
  if methodIndex in [0,1] then begin
    if applyModeLuminance then begin
      RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, HSVtmp);
      for i := 0 to 255 do hist[i] := 0;
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do Inc(hist[HSVtmp[x, y, 2]]);
total := IMG_WIDTH * IMG_HEIGHT;
      if methodIndex = 0 then begin lowVal := 255;
highVal := 0;
for i := 0 to 255 do if hist[i] > 0 then begin if i < lowVal then lowVal := i;
if i > highVal then highVal := i;
end;
end else begin lowVal := PercentileFromHistLocal(hist, total, pval);
highVal := PercentileFromHistLocal(hist, total, 1.0 - pval);
end;
      if highVal <= lowVal then for i := 0 to 255 do lut[i] := i else for i := 0 to 255 do begin if i <= lowVal then lut[i] := 0 else if i >= highVal then lut[i] := 255 else lut[i] := Byte(Round((i - lowVal) * 255 / (highVal - lowVal)));
end;
      for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do begin HSVtmp[x, y, 2] := lut[HSVtmp[x, y, 2]];
HSVByteToRGB(HSVtmp[x, y, 0], HSVtmp[x, y, 1], HSVtmp[x, y, 2], r, g, b);
dst[x, y, 0] := r;
dst[x, y, 1] := g;
dst[x, y, 2] := b;
end;
    end else begin
      for k := 0 to 2 do begin for i := 0 to 255 do hist[i] := 0;
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do Inc(hist[MATRIX[x, y, k]]);
total := IMG_WIDTH * IMG_HEIGHT;
if methodIndex = 0 then begin lowVal := 255;
highVal := 0;
for i := 0 to 255 do if hist[i] > 0 then begin if i < lowVal then lowVal := i;
if i > highVal then highVal := i;
end;
end else begin lowVal := PercentileFromHistLocal(hist, total, pval);
highVal := PercentileFromHistLocal(hist, total, 1.0 - pval);
end;
if highVal <= lowVal then for i := 0 to 255 do lut[i] := i else for i := 0 to 255 do if i <= lowVal then lut[i] := 0 else if i >= highVal then lut[i] := 255 else lut[i] := Byte(Round((i - lowVal) * 255 / (highVal - lowVal)));
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do dst[x, y, k] := lut[MATRIX[x, y, k]];
end;
end;
  tmpBmp := TBitmap.Create;
  try tmpBmp.PixelFormat := pf24bit;
    tmpBmp.SetSize(IMG_WIDTH, IMG_HEIGHT);
    copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, dst, tmpBmp);
    Image1.Picture.Assign(tmpBmp);
  finally tmpBmp.Free;
  end;
end;

procedure TForm1.ApplyContrast;
var methodIndex: Integer;
applyModeLuminance: Boolean;
pval: Double;
hist: array[0..255] of Integer;
total, lowVal, highVal: Integer;
i, x, y, k: Integer;
lut: array[0..255] of Byte;
HSVtmp: HSV_MATRIX;
r, g, b: Byte;
begin if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then Exit;
methodIndex := ComboBox2.ItemIndex;
applyModeLuminance := True;
pval := StrToFloatDef(Edit2.Text, TrackBar3.Position / 100.0);
if pval > 1.0 then pval := pval / 100.0;
if pval < 0 then pval := 0;
  if methodIndex in [0,1] then begin if applyModeLuminance then begin RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, HSVtmp);
for i := 0 to 255 do hist[i] := 0;
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do Inc(hist[HSVtmp[x, y, 2]]);
total := IMG_WIDTH * IMG_HEIGHT;
if methodIndex = 0 then begin lowVal := 255;
highVal := 0;
for i := 0 to 255 do if hist[i] > 0 then begin if i < lowVal then lowVal := i;
if i > highVal then highVal := i;
end;
end else begin lowVal := PercentileFromHistLocal(hist, total, pval);
highVal := PercentileFromHistLocal(hist, total, 1.0 - pval);
end;
if highVal <= lowVal then for i := 0 to 255 do lut[i] := i else for i := 0 to 255 do if i <= lowVal then lut[i] := 0 else if i >= highVal then lut[i] := 255 else lut[i] := Byte(Round((i - lowVal) * 255 / (highVal - lowVal)));
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do begin HSVtmp[x, y, 2] := lut[HSVtmp[x, y, 2]];
HSVByteToRGB(HSVtmp[x, y, 0], HSVtmp[x, y, 1], HSVtmp[x, y, 2], r, g, b);
MATRIX[x, y, 0] := r;
MATRIX[x, y, 1] := g;
MATRIX[x, y, 2] := b;
end;
end else begin for k := 0 to 2 do begin for i := 0 to 255 do hist[i] := 0;
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do Inc(hist[MATRIX[x, y, k]]);
total := IMG_WIDTH * IMG_HEIGHT;
if methodIndex = 0 then begin lowVal := 255;
highVal := 0;
for i := 0 to 255 do if hist[i] > 0 then begin if i < lowVal then lowVal := i;
if i > highVal then highVal := i;
end;
end else begin lowVal := PercentileFromHistLocal(hist, total, pval);
highVal := PercentileFromHistLocal(hist, total, 1.0 - pval);
end;
if highVal <= lowVal then for i := 0 to 255 do lut[i] := i else for i := 0 to 255 do if i <= lowVal then lut[i] := 0 else if i >= highVal then lut[i] := 255 else lut[i] := Byte(Round((i - lowVal) * 255 / (highVal - lowVal)));
for x := 0 to IMG_WIDTH - 1 do for y := 0 to IMG_HEIGHT - 1 do MATRIX[x, y, k] := lut[MATRIX[x, y, k]];
end;
end;
copyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
Image1.Picture.Assign(BMAP);
RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
ShowMessage('Contraste aplicado');
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin if TrackBar3.Position = 0 then TrackBar3.Position := 10;
Edit2.Text := FormatFloat('0.00', TrackBar3.Position / 100.0);
TrackBar3.Visible := True;
Edit2.Visible := True;
Label4.Visible := True;
Button4.Visible := True;
Button5.Visible := True;
ComboBox2.Visible := True;
if (IMG_WIDTH > 0) and (IMG_HEIGHT > 0) then ShowMessage('Ajusta el recorte y pulsa "Vista previa" para ver resultados.') else ShowMessage('Carga una imagen antes de usar Contraste automático.');
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin Edit2.Text := FormatFloat('0.00', TrackBar3.Position / 100.0);
PendingContrastPreview := True;
Timer1.Enabled := False;
Timer1.Enabled := True;
end;

procedure TForm1.Edit2Exit(Sender: TObject);
var v: Double;
begin v := StrToFloatDef(Edit2.Text, TrackBar3.Position / 100.0);
if v < 0.0 then v := 0.0;
if v > 50.0 then v := 50.0;
TrackBar3.Position := Round(v * 100);
Edit2.Text := FormatFloat('0.00', TrackBar3.Position / 100.0);
PendingContrastPreview := True;
Timer1.Enabled := False;
Timer1.Enabled := True;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin PreviewContrast;
end;
procedure TForm1.Button4Click(Sender: TObject);
begin ApplyContrast;
end;
procedure TForm1.ComboBox2Change(Sender: TObject);
begin end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin TrackBar2.Visible := True;
Edit1.Visible := True;
Label3.Visible := True;
Button2.Visible := True;
Button3.Visible := True;
if TrackBar2.Position = 0 then TrackBar2.Position := 100;
Edit1.Text := FormatFloat('0.00', TrackBar2.Position / 100.0);
if (IMG_WIDTH > 0) and (IMG_HEIGHT > 0) then ShowMessage('Ajusta gamma y usa "Vista previa" antes de aplicar.') else ShowMessage('Carga una imagen antes de usar el filtro gamma.');
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin Edit1.Text := FormatFloat('0.00', TrackBar2.Position / 100.0);
PendingGammaPreview := True;
Timer1.Enabled := False;
Timer1.Enabled := True;
end;

procedure TForm1.Edit1Exit(Sender: TObject);
var v: Double;
begin v := StrToFloatDef(Edit1.Text, TrackBar2.Position / 100.0);
if v < 0.1 then v := 0.1;
if v > 10.0 then v := 10.0;
TrackBar2.Position := Round(v * 100);
Edit1.Text := FormatFloat('0.00', TrackBar2.Position / 100.0);
PendingGammaPreview := True;
Timer1.Enabled := False;
Timer1.Enabled := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin PreviewGamma;
end;
procedure TForm1.Button2Click(Sender: TObject);
begin ApplyGamma;
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
var outName: string;
tmpBmp: TBitmap;
begin if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then begin ShowMessage('No hay imagen para guardar.');
Exit;
end;
if SavePictureDialog = nil then begin ShowMessage('Save dialog no disponible.');
Exit;
end;
SavePictureDialog.Filter := 'Bitmap (*.bmp)|*.bmp';
SavePictureDialog.DefaultExt := 'bmp';
if not SavePictureDialog.Execute then Exit;
outName := SavePictureDialog.FileName;
if ExtractFileExt(outName) = '' then outName := outName + '.bmp';
tmpBmp := TBitmap.Create;
try if (Image1.Picture.Graphic <> nil) then tmpBmp.Assign(Image1.Picture.Graphic) else tmpBmp.Assign(BMAP);
tmpBmp.PixelFormat := pf24bit;
try tmpBmp.SaveToFile(outName);
ShowMessage('Imagen guardada: ' + outName) except on E: Exception do ShowMessage('Error guardando la imagen: ' + E.Message);
end;
finally tmpBmp.Free;
end;
end;

end.
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
    procedure Image1MouseMove(Sender: TObject;
Shift: TShiftState;
X, Y: Integer
      );
    procedure hideLabelShape(Sender: TObject);
    //procedure callGenerate
    procedure MenuItem2Click(Sender: TObject);
// Abrir imagen
    procedure MenuItem4Click(Sender: TObject);
// generar histograma
    procedure MenuItem6Click(Sender: TObject);
// Botón de escala de grises
    //procedure MenuItem7Click(Sender: TObject);
// HSV
    procedure MenuItem8Click(Sender: TObject);
// Botón de restaurar
  private

  public

    //procedure copiaItoM(Al,An: Integer;
B: Tbitmap;
 var M:RGB_MATRIX);
  //copiar de bitmap a matriz con scanline
    //procedure copiaMtoI(Al,An: Integer;
M:RGB_MATRIX;
var B:Tbitmap  );
  //copiar de matriz a la imagen con scanline

    // Procedimiento para convertir un valor de RGB a HSV
    procedure RGBToHSVByte(r, g, b: Byte;
out Hb, Sb, Vb: Byte);
    // Copiar una imagen a una matriz
    procedure copyImageToMatrix(imageHeight, imageWidth: Integer;
B: TBitmap;
var matrix:RGB_MATRIX);

    // Copiar una matriz a una imagen
    procedure copyMatrixToImage(imageHeight, imageWidth: Integer;
matrix: RGB_MATRIX;
var B: TBitmap);


    // RGB a HSV
    procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
const RGB: RGB_MATRIX;
var HSV: HSV_MATRIX);
    // HSV a RGB
    //procedure HSVMatrixToRGBMatrix(imageHeight, imageWidth: Integer;
const HSV: HSV_MATRIX;
var RGB: RGB_MATRIX);
    // Escala de grises
    procedure mediumRangeGrayScale(imageHeight, imageWidth: Integer;
var matrix: RGB_MATRIX;
var CONVERTED_GRAY_MATRIX: RGB_MATRIX;
B: TBitmap);
    //Generar histograma
    procedure generateHistogram(imageHeight, imageWidth: Integer;
const matrix: RGB_MATRIX);

  end;

var
  Form1: TForm1;

  IMG_HEIGHT, IMG_WIDTH, COLOR_MODE: Integer;
  //MAT: RGB_MATRIX ;
 //del tipo propio para alamacenar R,G,B
  MATRIX, ORIGINAL_MATRIX: RGB_MATRIX;
  CONVERTED_HSV_MATRIX:  HSV_MATRIX;
  GRAY_SCALE_VALUES: GRAY_SCALE_MATRIX;
  CONVERTED_GRAY_MATRIX: RGB_MATRIX;
  BMAP: TBitmap;
  //para acceso a imagenes bmp

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.copyImageToMatrix(imageHeight, imageWidth: Integer;
B: TBitmap;
var matrix:RGB_MATRIX);
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
        matrix[j, i, 0] := P[k + 2];
// R
        matrix[j, i, 1] := P[k + 1];
// G
        matrix[j, i, 2] := P[k + 0];
// B
      end;
// j
    end;
// i
  finally
    B.EndUpdate;
  end;
end;

procedure TForm1.copyMatrixToImage(imageHeight, imageWidth: Integer;
matrix:RGB_MATRIX;
var B:TBitmap);
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
        P[k + 2] := matrix[j, i, 0];
// R
        P[k + 1] := matrix[j, i, 1];
// G
        P[k + 0] := matrix[j, i, 2];
// B
      end;
// j
    end;
// i
  finally
    B.EndUpdate;
  end;

  // HEIGHT := B.Height;
  // WIDTH := B.Width;
end;

procedure TForm1.RGBToHSVByte(r, g, b: Byte;
out Hb, Sb, Vb: Byte);
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

    H := H * 60;
// Convertir a grados
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



procedure TForm1.RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
const RGB: RGB_MATRIX;
var HSV: HSV_MATRIX);

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
  BMAP := TBitmap.Create;
 //Instanciar-crear objeto de la clase Tbitmap
  Image1.OnMouseLeave := @hideLabelShape;
//   MenuItem4.OnClick := @generateHistogram;
   COLOR_MODE := 0;
end;


procedure TForm1.Image1MouseMove(Sender: TObject;
Shift: TShiftState;
X,
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

  // Mostrar color 
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

procedure TForm1.mediumRangeGrayScale(imageHeight, imageWidth: Integer;
var matrix: RGB_MATRIX;
var CONVERTED_GRAY_MATRIX: RGB_MATRIX;
B: TBitmap);
var
  i, j: Integer;
  red, green, blue, gray: Byte;
  minimumValue, maximumValue: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
  begin
    Exit;
  end;

  SetLength(CONVERTED_GRAY_MATRIX, imageWidth, imageHeight, 3);
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

      // Guardar en matriz 
      GRAY_SCALE_VALUES[i, j] := gray;
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
    copyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, MATRIX);
 //copiar (TPicture)contenido de bitmap a MAT
    copyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, ORIGINAL_MATRIX);
// respaldar matriz

    Image1.Picture.Assign(BMAP);
 //ver imagen
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
   //copyMatrixToImage(HEIGHT, WIDTH, MATRIX, BMAP);
 //copiar (TPicture)contenido de bitmap a MAT
   for i := 0 to IMG_WIDTH - 1 do
    begin
      for j := 0 to IMG_HEIGHT - 1 do
       begin
         for k := 0 to 2 do
          MATRIX[i, j, k] := ORIGINAL_MATRIX[i, j, k];
       end;
    end;

   Image1.Picture.Assign(BMAP);
 //visulaizar imagen
  RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
   COLOR_MODE := 1;
   ShowMessage('Imagen restaurada');
  end
  else
   ShowMessage('No hay imagen para restaurar');
end;

procedure TForm1.generateHistogram(imageHeight, imageWidth: Integer;
const matrix: RGB_MATRIX);
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

  try
    Chart1.AxisList[1].Range.UseMin := True;
    Chart1.AxisList[1].Range.Min := 0;
    Chart1.AxisList[1].Range.UseMax := True;
    Chart1.AxisList[1].Range.Max := 255;
  except
  end;

  for i := 0 to 255 do
  begin
    Chart1BarSeries1.AddXY(i, histR[i]);
    Chart1BarSeries2.AddXY(i, histG[i]);
    Chart1BarSeries3.AddXY(i, histB[i]);
    Chart1LineSeries1.AddXY(i, histI[i]);
  end;

  Chart1.Invalidate;
end;

end.

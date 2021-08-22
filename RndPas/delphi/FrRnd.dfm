object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 89
    Width = 635
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitTop = 120
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 635
    Height = 89
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Input')
    ParentFont = False
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 92
    Width = 635
    Height = 207
    Align = alClient
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 633
      Height = 35
      Align = alTop
      TabOrder = 0
      object Bevel1: TBevel
        Left = 169
        Top = -8
        Width = 9
        Height = 50
      end
      object BEvalL: TButton
        Left = 264
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Eval &Lisp'
        TabOrder = 3
        OnClick = BEvalLClick
      end
      object BParseL: TButton
        Left = 183
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Parse L&isp'
        TabOrder = 2
        OnClick = BParseLClick
      end
      object BEvalRnd: TButton
        Left = 88
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Eval &Rnd'
        TabOrder = 1
        OnClick = BEvalRndClick
      end
      object BParseRnd: TButton
        Left = 7
        Top = 5
        Width = 75
        Height = 25
        Caption = '&Parse Rnd'
        TabOrder = 0
        OnClick = BParseRndClick
      end
    end
    object Memo2: TMemo
      Left = 1
      Top = 36
      Width = 633
      Height = 170
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'Output'
        '')
      ParentFont = False
      TabOrder = 1
    end
  end
end

object Form2: TForm2
  Left = 0
  Height = 299
  Top = 0
  Width = 635
  Caption = 'Form2'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  LCLVersion = '1.6.0.4'
  object Splitter1: TSplitter
    Cursor = crVSplit
    Left = 0
    Height = 3
    Top = 89
    Width = 635
    Align = alTop
    ResizeAnchor = akTop
  end
  object Memo1: TMemo
    Left = 0
    Height = 89
    Top = 0
    Width = 635
    Align = alTop
    Lines.Strings = (
      'Input'
    )
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Height = 207
    Top = 92
    Width = 635
    Align = alClient
    ClientHeight = 207
    ClientWidth = 635
    TabOrder = 1
    object Panel2: TPanel
      Left = 1
      Height = 35
      Top = 1
      Width = 633
      Align = alTop
      ClientHeight = 35
      ClientWidth = 633
      TabOrder = 0
      object BEval: TButton
        Left = 88
        Height = 25
        Top = 5
        Width = 75
        Caption = '&Eval'
        OnClick = BEvalClick
        TabOrder = 0
      end
      object BParse: TButton
        Left = 8
        Height = 25
        Top = 5
        Width = 75
        Caption = '&Parse'
        OnClick = BParseClick
        TabOrder = 1
      end
    end
    object Memo2: TMemo
      Left = 1
      Height = 170
      Top = 36
      Width = 633
      Align = alClient
      Lines.Strings = (
        'Output'
        ''
      )
      TabOrder = 1
    end
  end
end

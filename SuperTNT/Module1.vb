Module Module1
    Public ch1, ch2 As Integer                      ' 1P / 2P 角色選擇 (Tag)
    Public replay1 As Integer                       ' replay
    Public mode As Integer                          ' mode
    Public music As Integer                         ' music (Tag)
    Public began As Integer                         ' 開始
    Public player As New WMPLib.WindowsMediaPlayer  ' 音樂
    Public admin As Boolean                         ' 權限 (布林值)
    Public open As Boolean                          ' 開場動畫開啟與否 (布林值)
    Public map As Integer                           ' 地圖 (Tag)
    Public random As Boolean                        ' 隨機地圖 (Tag)
    Public invincible As Integer                    ' 無敵 (Tag)
    Public gaintime As Integer                      ' 一般模式 加時/減時
    Public dancetime As Integer                     ' 跳舞模式 加時/減時
    Public border As Boolean                        ' 是否縮邊 (布林值)
    Public map_style As Integer                     ' 地圖風格
    Public mc As Integer                            ' 是否minecraft化 (布林值)
End Module

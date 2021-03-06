Public Class Form9
    Dim x1, y1, xy1 As Integer                  ' 1P 座標
    Dim x2, y2, xy2 As Integer                  ' 2P 座標
    Dim tnt1xy As Integer                       ' 1P 2顆TNT xy
    Dim tnt2xy As Integer                       ' 2P 2顆TNT xy
    Dim tnt1_tick1, tnt1_tick2 As Integer       ' 1P 2顆TNT的Timer
    Dim tnt2_tick1, tnt2_tick2 As Integer       ' 2P 2顆TNT的Timer
    Dim tnt11, tnt12 As Integer                 ' 1P 2顆TNT的暫存座標
    Dim tnt21, tnt22 As Integer                 ' 2P 2顆TNT的暫存座標
    Dim un1, udlr1 As Integer                   ' 1P 走路方向判斷 (udlr = up / down / left / right)
    Dim un2, udlr2 As Integer                   ' 2P 走路方向判斷 (udlr = up / down / left / right)
    Dim f3, f12, esc As Integer                 ' 開發者模式啟動(F12需權限)
    Dim endcount, score1, score2 As Integer     ' 分數計算
    Dim death1, death2 As Integer               ' 1P / 2P 死亡 (Tag)
    Dim exer1, exer2 As Integer                 ' 1P / 2P 放TNT能穿越1次TNT (Tag)
    Dim endy As Integer                         ' 結束(Tag)
    Dim op As Integer                           ' 密碼暫存
    Dim block(4) As Integer                     ' 方塊樣式
    Dim talk As Integer                         ' 對話參數
    Dim light As Boolean = True                 ' 是否有光
    Dim i As Integer                            ' Timer計時暫存用
    Dim count As Boolean = True                 ' Timer計時暫存用
    Dim end_type As Integer                     ' 結局

    '------------------------------------------1P-控制--------------------------------------------------------------------------------
    '1P角色控制(上)(function)
    Private Sub Character1_up()
        If ch1 = 0 Then
            PictureBox401.Image = ImageList1.Images(0)
        ElseIf ch1 = 1 Then
            PictureBox401.Image = ImageList3.Images(0)
        ElseIf ch1 = 2 Then
            PictureBox401.Image = ImageList4.Images(0)
        ElseIf ch1 = 3 Then
            PictureBox401.Image = RR1.Image
        ElseIf ch1 = 4 Then
            PictureBox401.Image = mach1.Image
        ElseIf ch1 = 5 Then
            PictureBox401.Image = sp1.Image
        ElseIf ch1 = 6 Then
            PictureBox401.Image = yfs1.Image
        End If
        PictureBox401.Top -= PictureBox104.Height
        y1 -= 1
        udlr1 = 0
        un1 = 1
        If PictureBox401.Top < 0 Then
            back1()
        End If
        Call detection1()
        If exer1 > 0 Then
            exer1 -= 1
        End If
    End Sub
    '1P角色控制(下)(function)
    Private Sub Character1_down()
        If ch1 = 0 Then
            PictureBox401.Image = ImageList1.Images(1)
        ElseIf ch1 = 1 Then
            PictureBox401.Image = ImageList3.Images(1)
        ElseIf ch1 = 2 Then
            PictureBox401.Image = ImageList4.Images(1)
        ElseIf ch1 = 3 Then
            PictureBox401.Image = RR2.Image
        ElseIf ch1 = 4 Then
            PictureBox401.Image = mach2.Image
        ElseIf ch1 = 5 Then
            PictureBox401.Image = sp2.Image
        ElseIf ch1 = 6 Then
            PictureBox401.Image = yfs2.Image
        End If
        PictureBox401.Top += PictureBox104.Height
        y1 += 1
        udlr1 = 0
        un1 = -1
        Call detection1()
        If exer1 > 0 Then
            exer1 -= 1
        End If
    End Sub
    '1P角色控制(左)(function)
    Private Sub Character1_left()
        If ch1 = 0 Then
            PictureBox401.Image = ImageList1.Images(2)
        ElseIf ch1 = 1 Then
            PictureBox401.Image = ImageList3.Images(2)
        ElseIf ch1 = 2 Then
            PictureBox401.Image = ImageList4.Images(2)
        ElseIf ch1 = 3 Then
            PictureBox401.Image = RR3.Image
        ElseIf ch1 = 4 Then
            PictureBox401.Image = mach3.Image
        ElseIf ch1 = 5 Then
            PictureBox401.Image = sp3.Image
        ElseIf ch1 = 6 Then
            PictureBox401.Image = yfs3.Image
        End If
        PictureBox401.Left -= PictureBox104.Width
        x1 -= 1
        udlr1 = 1
        un1 = 1
        If PictureBox401.Left < 0 Then
            back1()
        End If
        Call detection1()
        If exer1 > 0 Then
            exer1 -= 1
        End If
    End Sub
    '1P角色控制(右)(function)
    Private Sub Character1_right()
        If ch1 = 0 Then
            PictureBox401.Image = ImageList1.Images(3)
        ElseIf ch1 = 1 Then
            PictureBox401.Image = ImageList3.Images(3)
        ElseIf ch1 = 2 Then
            PictureBox401.Image = ImageList4.Images(3)
        ElseIf ch1 = 3 Then
            PictureBox401.Image = RR4.Image
        ElseIf ch1 = 4 Then
            PictureBox401.Image = mach4.Image
        ElseIf ch1 = 5 Then
            PictureBox401.Image = sp4.Image
        ElseIf ch1 = 6 Then
            PictureBox401.Image = yfs4.Image
        End If
        PictureBox401.Left += PictureBox104.Width
        x1 += 1
        If PictureBox401.Left > Me.Width - 20 - PictureBox401.Width Then
            back1()
        End If
        udlr1 = 1
        un1 = -1
        Call detection1()
        If exer1 > 0 Then
            exer1 -= 1
        End If
    End Sub
    '---------------------------------------------------------------------------------------------------------------------------------

    '1P死亡判斷(function)
    Private Sub die1()
        If invincible = 2 Or invincible = 0 Then
            If mode = 1 Then
                If xy1 = tnt1xy - 100 Or xy1 = tnt1xy + 100 Or xy1 = tnt1xy Or xy1 = tnt1xy + 200 Or xy1 = tnt1xy - 200 Then
                    If tnt1_tick1 = 5 Then
                        death1 = 1
                    End If
                    If tnt1_tick2 = 5 Then
                        death1 = 1
                    End If
                End If
                If xy1 >= tnt1xy - 2 And xy1 <= tnt1xy + 2 Then
                    If tnt1_tick1 = 5 Then
                        death1 = 1
                    End If
                    If tnt1_tick2 = 5 Then
                        death1 = 1
                    End If
                End If
                If xy1 = tnt2xy - 100 Or xy1 = tnt2xy + 100 Or xy1 = tnt2xy Or xy1 = tnt2xy + 200 Or xy1 = tnt2xy - 200 Then
                    If tnt2_tick1 = 5 Then
                        death1 = 1
                    End If
                    If tnt2_tick2 = 5 Then
                        death1 = 1
                    End If
                End If
                If xy1 >= tnt2xy - 2 And xy1 <= tnt2xy + 2 Then
                    If tnt2_tick1 = 5 Then
                        death1 = 1
                    End If
                    If tnt2_tick2 = 5 Then
                        death1 = 1
                    End If
                End If
            ElseIf mode = 0 Then
                If xy1 = tnt1xy - 100 Or xy1 = tnt1xy + 100 Or xy1 = tnt1xy Then
                    If tnt1_tick1 = 5 Then
                        death1 = 1
                    End If
                    If tnt1_tick2 = 5 Then
                        death1 = 1
                    End If
                End If
                If xy1 >= tnt1xy - 1 And xy1 <= tnt1xy + 1 Then
                    If tnt1_tick1 = 5 Then
                        death1 = 1
                    End If
                    If tnt1_tick2 = 5 Then
                        death1 = 1
                    End If
                End If
                If xy1 = tnt2xy - 100 Or xy1 = tnt2xy + 100 Or xy1 = tnt2xy Then
                    If tnt2_tick1 = 5 Then
                        death1 = 1
                    End If
                    If tnt2_tick2 = 5 Then
                        death1 = 1
                    End If
                End If
                If xy1 >= tnt2xy - 1 And xy1 <= tnt2xy + 1 Then
                    If tnt2_tick1 = 5 Then
                        death1 = 1
                    End If
                    If tnt2_tick2 = 5 Then
                        death1 = 1
                    End If
                End If
            End If
        End If
    End Sub

    '開發人員模式F12(function)
    Private Sub Engineering_mode()
        If f3 = 1 Then
            Label1.Text = "x1: " & x1
            Label2.Text = "y1: " & y1
            Label3.Text = "tnt1xy: " & tnt1xy
            Label4.Text = "tnt1_tick1: " & tnt1_tick1 & " (tick/s)"
            Label5.Text = "tnt1_tick2:" & tnt1_tick2 & " (tick/s)"
            Label12.Text = "xy1:" & xy1 & " (x1 * 100 + y1)"
            Label13.Text = "udlr: " & udlr1
            Label14.Text = "un1: " & un1
            Label15.Text = "xy1: " & xy1
            Label16.Text = "map_style: " & map_style
            Label17.Text = "map:" & map
            Label12.Text = "xy2:" & xy2 & " (x2 * 100 + y2)"
            Label1.Visible = True
            Label2.Visible = True
            Label3.Visible = True
            Label4.Visible = True
            Label5.Visible = True
            Label12.Visible = True
            Label13.Visible = True
            Label14.Visible = True
            Label15.Visible = True
            Label16.Visible = True
            Label17.Visible = True
            Label1.BringToFront()
            Label2.BringToFront()
            Label3.BringToFront()
            Label4.BringToFront()
            Label5.BringToFront()
            Label12.BringToFront()
            Label13.BringToFront()
            Label14.BringToFront()
            Label15.BringToFront()
            Label16.BringToFront()
            Label17.BringToFront()
        ElseIf f3 = 0 Then
            Label1.Visible = False
            Label2.Visible = False
            Label3.Visible = False
            Label4.Visible = False
            Label5.Visible = False
            Label12.Visible = False
            Label13.Visible = False
            Label14.Visible = False
            Label15.Visible = False
            Label16.Visible = False
            Label17.Visible = False
        End If
    End Sub

    'ESC(function)
    Private Sub esc_mode()
        If esc = 1 Then
            If endy = 0 Then
                endy = 1
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
            End If
            Timer6.Enabled = False
            PictureBox136.BringToFront()
            Button1.BringToFront()
            Button2.BringToFront()
            Button3.BringToFront()
            PictureBox136.Visible = True
            If endy < 2 Then
                Label20.BringToFront()
                Label20.Visible = True
            End If
            Button1.Visible = True
            Button2.Visible = True
            Button3.Visible = True
        ElseIf esc = 0 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
            endy = 0
            Timer6.Enabled = True
            PictureBox136.Visible = False
            Button1.Visible = False
            Button2.Visible = False
            Button3.Visible = False
            Label20.Visible = False
        End If
    End Sub

    'minecraft化
    Private Sub minecraft()
        map_style = map
        If map_style = 0 Then
            block(3) = 17
            block(2) = 364
        ElseIf map_style = 1 Then
            block(3) = 325
            block(2) = 440
        ElseIf map_style = 2 Then
            block(3) = 325
            block(2) = 440
        ElseIf map_style = 21 Then
            block(3) = 325
            block(2) = 440
            block(1) = 319
        ElseIf map_style = 22 Then
            If paper1 = True Then
                block(3) = 474
                block(2) = 462
                block(1) = 432
            Else
                block(3) = 277
                block(2) = 15
                block(1) = 432
            End If
        ElseIf map_style = 23 Then
            block(3) = 277
            block(2) = 15
            block(1) = 312
        ElseIf map_style = 24 Then
            block(3) = 368
            block(2) = 318
            block(1) = 254
        ElseIf map_style = 11 Then
            block(3) = 325
            block(2) = 440
        ElseIf map_style = 12 Then
            block(3) = 325
            block(2) = 440
        ElseIf map_style = 13 Then
            block(3) = 325
            block(2) = 319
            block(1) = 440
        ElseIf map_style = 14 Then
            block(3) = 18
            block(2) = 312
            block(3) = 474
            block(1) = 4
        ElseIf map_style = 15 Then
            block(3) = 478
            block(2) = 110
        ElseIf map_style = 16 Or map_style = 17 Or map_style = 18 Then
            block(2) = 319
            block(3) = 319
            block(1) = 166
        ElseIf map_style = 19 Or map_style = 20 Then
            block(2) = 166
            block(3) = 319
        ElseIf map_style = 30 Then
            block(2) = 462
            block(3) = 18
            block(1) = 467
        ElseIf map_style = 31 Then
            block(2) = 337
            block(3) = 325
            block(1) = 254
        ElseIf map_style = 32 Then
            block(2) = 4
            block(3) = 17
            block(1) = 78
        ElseIf map_style = 33 Then
            block(3) = 325
            block(2) = 440
        ElseIf map_style = 100 Then
            block(3) = 73
        ElseIf map_style = 40 Then
            block(2) = 127
            block(3) = 478
            block(1) = 491
        ElseIf map_style = 41 Then
            block(2) = 39
            block(3) = 404
            block(1) = 343
        End If
    End Sub
    '需觸發事件
    Private Sub eventes()
        If map = 24 Then
            If xy1 = 212 And udlr1 = 1 And un1 = 1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\chestopen.wav", AudioPlayMode.Background)
                talk = 10
                chat_events()
            End If
        ElseIf map = 0 Then
            If cd_1 = True Then
                If xy1 = 1004 And udlr1 = 1 And un1 = -1 Then
                    cd_1 = False
                    cd1_item.Visible = False
                    talk = 40
                    chat_events()
                End If
            End If
        ElseIf map = 14 Then
            If xy1 = 305 And udlr1 = 1 And un1 = 1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\chestopen.wav", AudioPlayMode.Background)
                talk = 10
                chat_events()
            End If
        ElseIf map = 16 Then
            If xy1 = 506 And udlr1 = 1 And un1 = 1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\chestopen.wav", AudioPlayMode.Background)
                paper3.Visible = True
            End If
        ElseIf map = 17 Then
            If xy1 = 506 And udlr1 = 1 And un1 = 1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\chestopen.wav", AudioPlayMode.Background)
                paper4.Visible = True
            End If
        ElseIf map = 18 Then
            If xy1 = 506 And udlr1 = 1 And un1 = 1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\chestopen.wav", AudioPlayMode.Background)
                paper5.Visible = True
            End If
        ElseIf map = 19 Then
            If xy1 = 706 And have_sword = False Then
                sword.Visible = True
                have_sword = True
                PictureBox209.Visible = False
            End If
            If xy1 = 708 And udlr1 = 1 And un1 = -1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\chestopen.wav", AudioPlayMode.Background)
                paper6.Visible = True
            End If
        ElseIf map = 30 Then
            If xy1 = 605 And udlr1 = 0 And un1 = 1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\chestopen.wav", AudioPlayMode.Background)
                paper7.Visible = True
            End If
        ElseIf map = 31 Then
            If xy1 = 712 And udlr1 = 0 And un1 = -1 And Jimmy_bar.Visible = True Then
                PictureBox202.Visible = False
                PictureBox235.Visible = False
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\stone1.wav", AudioPlayMode.Background)
            End If
            If xy1 = 812 And udlr1 = 0 And un1 = -1 And Jimmy_bar.Visible = True Then
                PictureBox202.Visible = False
                PictureBox235.Visible = False
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\stone1.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 32 Then
            If xy1 = 1102 And udlr1 = 0 And un1 = 1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\chestopen.wav", AudioPlayMode.Background)
                Jimmy_bar.Visible = True
            End If
        End If
        tnt11 = x1 * 100 + y1
        exer1 = 1
        Label4.Text = "tnt1_tick:" & tnt1_tick1 & " (tick/s)"
        tnt1xy = tnt11
        Label3.Text = "tnt1xy: " & tnt1xy
        Call tnt_1()
        tnt11 = 0
    End Sub

    '自動觸發事件
    Private Sub events_auto()
        If map = 0 Then
            If xy1 = 601 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
                If back = True Then
                    end_type = 1
                    x1 = 100
                    y1 = 100
                    map_clear()
                    map = 100
                    map_change()
                Else
                    x1 = 6
                    y1 = 13
                    map_clear()
                    map = 1
                    map_change()
                End If
            End If
            If xy1 = 608 And udlr1 = 0 And un1 = -1 Then
                talk = 1
                chat_events()
            End If
            If cd_1 = False Then
                If xy1 = 1004 And udlr1 = 1 And un1 = -1 Then
                    talk = 10
                    chat_events()
                End If
            End If
            If xy1 = 1008 And udlr1 = 1 And un1 = -1 Then
                talk = 20
                chat_events()
            End If
            If xy1 = 1009 And udlr1 = 1 And un1 = -1 Then
                talk = 20
                chat_events()
            End If
            If xy1 = 1011 And udlr1 = 1 And un1 = -1 Then
                talk = 30
                chat_events()
            End If
        ElseIf map = 1 Then
            If xy1 = 613 And udlr1 = 0 And un1 = -1 Then
                x1 = 6
                y1 = 1
                map_clear()
                map = 0
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
            End If
            If xy1 = 107 Or xy1 = 106 And udlr1 = 1 And un1 = 1 Then
                x1 = 12
                y1 = 6
                map_clear()
                map = 2
                map_change()
            End If
        ElseIf map = 2 Then
            If xy1 = 1205 Or xy1 = 1206 Or xy1 = 1207 And udlr1 = 0 And un1 = -1 Then
                x1 = 1
                y1 = 7
                map_clear()
                map = 1
                map_change()
            End If
            If xy1 = 101 Or xy1 = 102 Or xy1 = 103 Or xy1 = 104 And udlr1 = 1 And un1 = 1 Then
                x1 = 12
                y1 = 6
                map_clear()
                map = 21
                map_change()
            End If
            If xy1 = 111 Or xy1 = 112 Or xy1 = 113 And udlr1 = 1 And un1 = 1 Then
                If cd1_played = True Then
                    x1 = 12
                    y1 = 6
                    map_clear()
                    map = 11
                    map_change()
                Else
                    x1 = 12
                    y1 = 6
                    map_clear()
                    map = 2
                    map_change()
                End If
            End If
        ElseIf map = 21 Then
            If xy1 = 1205 Or xy1 = 1206 Or xy1 = 1207 And udlr1 = 1 And un1 = -1 Then
                x1 = 1
                y1 = 3
                map_clear()
                map = 2
                map_change()
            End If
            If xy1 = 507 And udlr1 = 1 And un1 = 1 Then
                x1 = 12
                y1 = 7
                map_clear()
                map = 22
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 22 Then
            If paper1 = True Then
                If xy1 = 1207 And udlr1 = 1 And un1 = -1 Then
                    x1 = 5
                    y1 = 7
                    map_clear()
                    map = 24
                    map_change()
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
                    player.settings.setMode("loop", True)
                    player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm_story_mode.mp3"
                End If
            Else
                If xy1 = 1207 And udlr1 = 1 And un1 = -1 Then
                    x1 = 5
                    y1 = 7
                    map_clear()
                    map = 21
                    map_change()
                    If paper1 = True Then
                    Else
                        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
                        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\thunder1.wav", AudioPlayMode.Background)
                    End If
                End If
            End If
            If xy1 = 202 And udlr1 = 0 And un1 = 1 Then
                x1 = 2
                y1 = 2
                map_clear()
                map = 23
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\ladder3.wav", AudioPlayMode.WaitToComplete)
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\ladder4.wav", AudioPlayMode.WaitToComplete)
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\ladder5.wav", AudioPlayMode.WaitToComplete)
            End If

        ElseIf map = 23 Then
            If xy1 = 201 And udlr1 = 0 And un1 = 1 Then
                x1 = 2
                y1 = 3
                map_clear()
                map = 22
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\ladder3.wav", AudioPlayMode.WaitToComplete)
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\ladder4.wav", AudioPlayMode.WaitToComplete)
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\ladder5.wav", AudioPlayMode.WaitToComplete)
                If paper1 = True Then
                    player.settings.setMode("loop", False)
                    player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\girl_sing.mp3"
                End If
            End If
            If xy1 = 1107 And udlr1 = 1 And un1 = -1 Then
                talk = 1
                chat_events()
            End If
        ElseIf map = 24 Then
            If xy1 = 908 And udlr1 = 1 And un1 = 1 Then
                talk = 1
                chat_events()
            End If
            If xy1 = 507 And udlr1 = 1 And un1 = 1 Then
                paper1 = False
                x1 = 12
                y1 = 7
                map_clear()
                map = 22
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 11 Then
            If xy1 = 1205 Or xy1 = 1206 Or xy1 = 1207 And udlr1 = 1 And un1 = -1 Then
                ghost.Visible = False
                ghost_move.Enabled = False
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm_story_mode.mp3"
                x1 = 1
                y1 = 9
                map_clear()
                map = 12
                map_change()
            End If
            If x1 = 5 Then
                ghost.Left = -25
                ghost.Visible = True
                ghost_move.Enabled = True
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\ghost_chasing.mp3"
            End If
        ElseIf map = 12 Then
            If xy1 = 1202 Or xy1 = 1203 Or xy1 = 1204 And udlr1 = 1 And un1 = -1 Then
                x1 = 1
                y1 = 9
                map_clear()
                map = 13
                map_change()
            End If
        ElseIf map = 13 Then
            If xy1 = 704 And udlr1 = 0 And un1 = 1 Then
                x1 = 6
                y1 = 13
                map_clear()
                map = 14
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
            If xy1 = 1207 And udlr1 = 1 And un1 = -1 Then
                x1 = 1
                y1 = 8
                map_clear()
                map = 15
                map_change()
            End If
            If xy1 = 1208 And udlr1 = 1 And un1 = -1 Then
                x1 = 1
                y1 = 8
                map_clear()
                map = 15
                map_change()
            End If
        ElseIf map = 14 Then
            If xy1 = 613 And udlr1 = 0 And un1 = -1 Then
                x1 = 7
                y1 = 5
                map_clear()
                map = 13
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
            End If
            If xy1 = 403 And udlr1 = 1 And un1 = 1 And PictureBox57.Visible = True Then
                talk = 1
                chat_events()
            End If
            If xy1 = 703 And udlr1 = 0 And un1 = 1 Then
                talk = 20
                chat_events()
            End If
            If xy1 = 1103 And udlr1 = 0 And un1 = 1 Then
                talk = 20
                chat_events()
            End If
            If xy1 = 1006 And udlr1 = 0 And un1 = -1 And waiter_talk = True Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_openning.wav", AudioPlayMode.Background)
                waiting.Enabled = True
            End If
        ElseIf map = 15 Then
            If xy1 = 204 And udlr1 = 0 And un1 = 1 Then
                x1 = 6
                y1 = 9
                map_clear()
                map = 16
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
            If xy1 = 604 And udlr1 = 0 And un1 = 1 Then
                x1 = 6
                y1 = 9
                map_clear()
                map = 17
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
            If xy1 = 1004 And udlr1 = 0 And un1 = 1 Then
                x1 = 6
                y1 = 9
                map_clear()
                map = 18
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
            If xy1 = 209 And udlr1 = 0 And un1 = -1 Then
                x1 = 6
                y1 = 5
                map_clear()
                map = 19
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
            If xy1 = 609 And udlr1 = 0 And un1 = -1 Then
                x1 = 6
                y1 = 5
                map_clear()
                map = 20
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
            If xy1 = 1207 And udlr1 = 1 And un1 = -1 Then
                x1 = 2
                y1 = 7
                map_clear()
                map = 30
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_open.wav", AudioPlayMode.Background)
            End If
            If xy1 = 813 And udlr1 = 0 And un1 = -1 Then
                x1 = 3
                y1 = 1
                map_clear()
                map = 31
                map_change()
            End If
            If xy1 = 913 And udlr1 = 0 And un1 = -1 Then
                x1 = 3
                y1 = 1
                map_clear()
                map = 31
                map_change()
            End If
            If xy1 = 1013 And udlr1 = 0 And un1 = -1 Then
                x1 = 3
                y1 = 1
                map_clear()
                map = 31
                map_change()
            End If
            If xy1 = 1113 And udlr1 = 0 And un1 = -1 Then
                x1 = 3
                y1 = 1
                map_clear()
                map = 31
                map_change()
            End If
        ElseIf map = 16 Then
            If xy1 = 610 And udlr1 = 0 And un1 = -1 Then
                x1 = 2
                y1 = 5
                map_clear()
                map = 15
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 17 Then
            If xy1 = 610 And udlr1 = 0 And un1 = -1 Then
                x1 = 6
                y1 = 5
                map_clear()
                map = 15
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 18 Then
            If xy1 = 610 And udlr1 = 0 And un1 = -1 Then
                x1 = 10
                y1 = 5
                map_clear()
                map = 15
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 19 Then
            If xy1 = 604 And udlr1 = 0 And un1 = 1 Then
                x1 = 2
                y1 = 8
                map_clear()
                map = 15
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
            End If
            If y1 > 5 And seen_the_woman = False Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\scream1.wav", AudioPlayMode.Background)
                i = 0
                scream.Enabled = True
            End If
        ElseIf map = 20 Then
            If xy1 = 604 And udlr1 = 0 And un1 = 1 Then
                x1 = 6
                y1 = 8
                map_clear()
                map = 15
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 30 Then
            If xy1 = 107 And udlr1 = 1 And un1 = 1 Then
                x1 = 11
                y1 = 7
                map_clear()
                map = 15
                map_change()
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 31 Then
            If xy1 = 201 And udlr1 = 0 And un1 = 1 Then
                If goddad_talk = True Then
                    x1 = 12
                    y1 = 6
                    map_clear()
                    map = 40
                    map_change()
                Else
                    x1 = 9
                    y1 = 13
                    map_clear()
                    map = 15
                    map_change()
                End If
            End If
            If xy1 = 301 And udlr1 = 0 And un1 = 1 Then
                If goddad_talk = True Then
                    x1 = 12
                    y1 = 6
                    map_clear()
                    map = 40
                    map_change()
                Else
                    x1 = 9
                    y1 = 13
                    map_clear()
                    map = 15
                    map_change()
                End If
            End If
            If xy1 = 401 And udlr1 = 0 And un1 = 1 Then
                If goddad_talk = True Then
                    x1 = 12
                    y1 = 6
                    map_clear()
                    map = 40
                    map_change()
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\news1.wav", AudioPlayMode.Background)
                Else
                    x1 = 9
                    y1 = 13
                    map_clear()
                    map = 15
                    map_change()
                End If
            End If
            If xy1 = 1006 And udlr1 = 1 And un1 = -1 Then
                x1 = 1
                y1 = 7
                map_clear()
                map = 32
                map_change()
            End If
            If xy1 = 813 And udlr1 = 0 And un1 = -1 Then
                x1 = 7
                y1 = 1
                map_clear()
                map = 33
                map_change()
            End If
            If xy1 = 713 And udlr1 = 0 And un1 = -1 Then
                x1 = 6
                y1 = 1
                map_clear()
                map = 33
                map_change()
            End If
        ElseIf map = 32 Then
            If xy1 = 107 And udlr1 = 1 And un1 = 1 Then
                x1 = 9
                y1 = 6
                map_clear()
                map = 31
                map_change()
            End If
            If xy1 = 907 And udlr1 = 1 And un1 = -1 Then
                talk = 1
                chat_events()
            End If
        ElseIf map = 33 Then
            If back = False Then
                If xy1 = 613 And udlr1 = 0 And un1 = -1 Then
                    x1 = 100
                    y1 = 100
                    map_clear()
                    map = 100
                    map_change()
                End If
                If xy1 = 713 And udlr1 = 0 And un1 = -1 Then
                    end_type = 0
                    x1 = 100
                    y1 = 100
                    map_clear()
                    map = 100
                    map_change()
                End If
            Else
                If xy1 = 613 And udlr1 = 0 And un1 = -1 Then
                    x1 = 6
                    y1 = 1
                    map_clear()
                    map = 0
                    map_change()
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
                End If
                If xy1 = 713 And udlr1 = 0 And un1 = -1 Then
                    x1 = 6
                    y1 = 1
                    map_clear()
                    map = 0
                    map_change()
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
                End If
            End If
            If udlr1 = 0 And un1 = 1 Then
                back = True
            End If
        ElseIf map = 40 Then
            If xy1 = 201 And udlr1 = 0 And un1 = 1 Then
                x1 = 7
                y1 = 9
                map_clear()
                map = 41
                map_change()
                endy = 1
                item = 41
                talk = 1
                chat_events()
            End If
        End If
    End Sub

    '對話事件
    Private Sub chat_events()
        chat_bar.Visible = True
        chater.Visible = True
        Label_chat.Visible = True
        Label_hit.Visible = True
        chat_bar.BringToFront()
        chater.BringToFront()
        Label_chat.BringToFront()
        Label_hit.BringToFront()
        endy = 1
        If map = 0 Then
            If talk = 1 Then
                chater.Image = cheif.Image
                Label_chat.Text = "<鎮長>" & vbCrLf & "我有件事要拜託你..."
            ElseIf talk = 2 Then
                Label_chat.Text = "<鎮長>" & vbCrLf & "最近鎮上常常發生命案"
            ElseIf talk = 3 Then
                Label_chat.Text = "<鎮長>" & vbCrLf & "你幫我調查一下"
            ElseIf talk = 4 Then
                Label_chat.Text = "<鎮長>" & vbCrLf & "當然，獎賞是會有的"
            ElseIf talk = 5 Then
                chat_end()
            End If
            If talk = 10 Then
                chater.Image = self.Image
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "這是一個唱片機"
            ElseIf talk = 11 Then
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "不過裡面沒有唱片..."
            ElseIf talk = 12 Then
                chat_end()
            End If
            If talk = 20 Then
                chater.Image = self.Image
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "這是一個釀造台"
            ElseIf talk = 21 Then
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "好像在釀造飛濺型的藥水"
            ElseIf talk = 22 Then
                chat_end()
            End If
            If talk = 30 Then
                chater.Image = self.Image
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "熔爐內似乎沒有東西"
            ElseIf talk = 31 Then
                chat_end()
            End If
            If talk = 40 Then
                chater.Image = cd_player.Image
                Label_chat.Text = "<唱片機>" & vbCrLf & "播放中..."
                If back = True Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\diff.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\news1.wav", AudioPlayMode.Background)
                End If
            ElseIf talk = 41 Then
            ElseIf talk = 42 Then
                If back = False Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\cdplayer_end.wav", AudioPlayMode.Background)
                End If
                cd1_played = True
                cd_1 = True
                cd1_item.Visible = True
                chat_end()
            End If
        ElseIf map = 23 Then
            If talk = 1 Then
                chater.Image = self.Image
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "有一張紙"
            ElseIf talk = 2 Then
                chater.Image = paper.Image
                Label_chat.Text = "[紙上寫著]" & vbCrLf & "5/20 最近都沒人來借書，大家都跑到哪裡去了?" & vbCrLf & vbCrLf & "5/23 最近常常停電，大概是昨天打雷的關係吧!"
            ElseIf talk = 3 Then
                paper1 = True
                chat_end()
            End If
        End If
        If map = 24 Then
            If talk = 1 Then
                chater.Image = libraier.Image
                Label_chat.Text = "<圖書館管理員>" & vbCrLf & "每天出來吹吹風，看看我種的甘蔗"
            ElseIf talk = 2 Then
                Label_chat.Text = "<圖書館管理員>" & vbCrLf & "這樣的感覺多好啊!"
            ElseIf talk = 3 Then
                Label_chat.Text = "<圖書館管理員>" & vbCrLf & "在這個鎮發現了礦坑之後"
            ElseIf talk = 4 Then
                Label_chat.Text = "<圖書館管理員>" & vbCrLf & "鎮上湧入了一堆人，好像變得比較繁榮"
            ElseIf talk = 5 Then
                Label_chat.Text = "<圖書館管理員>" & vbCrLf & "希望現在這種生活還能繼續下去"
            ElseIf talk = 6 Then
                paper1 = True
                chat_end()
            End If
            If talk = 10 Then
                chater.Image = self.Image
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "這是什麼東西?"
                cd_1 = True
                cd1_item.Visible = True
            ElseIf talk = 11 Then
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "看起來像一張唱片"
            ElseIf talk = 12 Then
                chater.Image = cd1.Image
                Label_chat.Text = "<唱片>" & vbCrLf & "看起來有點老舊的唱片"
            ElseIf talk = 13 Then
                item = 0
                chat_end()
            End If
        End If
        If map = 14 Then
            If talk = 1 Then
                chater.Image = waiter_ghost.Image
                Label_chat.Text = "<曾經的服務生>" & vbCrLf & "那一天晚上"
            ElseIf talk = 2 Then
                Label_chat.Text = "<曾經的服務生>" & vbCrLf & "我聽到一種詭異的開門聲"
            ElseIf talk = 3 Then
                Label_chat.Text = "<曾經的服務生>" & vbCrLf & "那時最好不要動...直到他關門"
            ElseIf talk = 4 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\sud_2.wav", AudioPlayMode.Background)
                PictureBox57.Visible = False
                waiter_talk = True
                chat_end()
            End If
            If talk = 10 Then
                chater.Image = paper.Image
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "看起來像一張藥方" & vbCrLf & "(點擊物品欄內物品以開啟)"
                paper2 = True
                paper2_item.Visible = True
            ElseIf talk = 11 Then
                chat_end()
            End If
            If talk = 20 Then
                chater.Image = self.Image
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "熔爐內似乎沒有東西"
            ElseIf talk = 21 Then
                chat_end()
            End If
        End If
        If map = 32 Then
            If talk = 1 Then
                chater.Image = goddad.Image
                Label_chat.Text = "<神父>" & vbCrLf & "你有什麼困擾?"
            ElseIf talk = 2 Then
                chater.Image = self.Image
                Label_chat.Text = "<" & player_name & ">" & vbCrLf & "我在調查一件命案，但不知道要如何是好?"
            ElseIf talk = 3 Then
                chater.Image = goddad.Image
                Label_chat.Text = "<神父>" & vbCrLf & "跟著你心中的聲音..."
            ElseIf talk = 4 Then
                Label_chat.Text = "<神父>" & vbCrLf & "你不怕，就往回走。"
            ElseIf talk = 5 Then
                Label_chat.Text = "<神父>" & vbCrLf & "如果你害怕，你會找到路離開的，但..."
            ElseIf talk = 6 Then
                Label_chat.Text = "<神父>" & vbCrLf & "不要回頭!"
            ElseIf talk = 7 Then
                goddad_talk = True
                chat_end()
            End If
        End If
        If item = 1 Then
            If talk = 1 Then
                chater.Image = cd1.Image
                Label_chat.Text = "<唱片>" & vbCrLf & "看起來有點老舊的唱片"
            ElseIf talk = 2 Then
                item = 0
                chat_end()
            End If
        End If
        If item = 2 Then
            If talk = 100 Then
                chater.Image = paper.Image
                Label_chat.Text = "<藥方>"
                paper2 = False
                paper2_item.Visible = False
                pap_text.Text = "死而復生" & vbCrLf & vbCrLf & "蒐集:" & vbCrLf & "野生三葉青、五味子、山茱萸、連翹、天冬各一筲，蘄蛇一條，小米數斗" & vbCrLf & vbCrLf & "方法:" & vbCrLf & "把藥材倒進器皿，加入冷水蓋過藥材約半扠，讓藥材浸水待約6、7個時辰，再加" & vbCrLf & "入水淹過藥材半扠。 " & vbCrLf & "開始煎煮藥材，頭先用大火將水煮沸，然後切至文火煎煮中藥" & vbCrLf & "第一次煎好的是【藥頭】，再煎煮第二次是【藥尾】，加入冷水蓋過藥材約半扠" & vbCrLf & vbCrLf & "第二次就不用浸泡了、先用大火將水煮沸，然後切至文火煎煮中藥，藥水煎至小碗約8分" & vbCrLf & "藥材與死者同浸於一桶，以米覆其蓋，埋於地底，兩年之後取出。" & vbCrLf & vbCrLf & "注意:" & vbCrLf & "死而復生後，死者無法自行造血，故每當月圓之時，便會嗜人血以延其性命"
                pap_.Visible = True
                pap_text.Visible = True
            ElseIf talk = 101 Then
                pap_.Visible = False
                pap_text.Visible = False
                paper2 = True
                paper2_item.Visible = True
                item = 0
                chat_end()
            End If
        End If
        If item = 3 Then
            If talk = 1 Then
                chater.Image = paper.Image
                Label_chat.Text = "<殘餘的日記>"
                paper3.Visible = False
                pap_text.Text = "5月15日" & vbCrLf & vbCrLf & "對面那戶怎麼都沒看見她出來買菜?" & vbCrLf & vbCrLf & "5月17日" & vbCrLf & "最近有些人都沒來礦坑了，或許是這裡快要沒得挖了。"
                pap_.Visible = True
                pap_text.Visible = True
            ElseIf talk = 2 Then
                paper3.Visible = True
                chat_end()
            End If
        End If
        If item = 4 Then
            If talk = 1 Then
                chater.Image = paper.Image
                Label_chat.Text = "<殘餘的日記>"
                paper4.Visible = False
                pap_text.Text = "5月14日" & vbCrLf & "礦場老闆今天難得沒來巡視，挖得就特別輕鬆!" & vbCrLf & vbCrLf & "5月15日" & vbCrLf & "最近怎麼那麼幸運，回家時月亮這麼亮，連煤燈都不用浪費了。"
                pap_.Visible = True
                pap_text.Visible = True
            ElseIf talk = 2 Then
                paper4.Visible = True
                chat_end()
            End If
        End If
        If item = 5 Then
            If talk = 1 Then
                chater.Image = paper.Image
                Label_chat.Text = "<殘餘的日記>"
                paper5.Visible = False
                pap_text.Text = "4月22日" & vbCrLf & "靠近酒館的那一戶好像怪怪的，他丈夫不是兩年前死" & vbCrLf & "在礦場，現在在守寡嗎?怎麼還聽到一男一女在說話，該不會..." & vbCrLf & vbCrLf & "5月14日" & vbCrLf & "明明才剛找到新礦源，怎麼人愈來愈少了?"
                pap_.Visible = True
                pap_text.Visible = True
            ElseIf talk = 2 Then
                paper5.Visible = True
                chat_end()
            End If
        End If
        If item = 6 Then
            If talk = 1 Then
                chater.Image = paper.Image
                Label_chat.Text = "<殘餘的日記>"
                paper6.Visible = False
                pap_text.Text = "4月21日" & vbCrLf & "終於過了兩年了，今晚看看那個藥方是否靈驗!" & vbCrLf & vbCrLf & "5月12日" & vbCrLf & "雖然藥方靈驗了，但最近總覺得他變得怪怪的..."
                pap_.Visible = True
                pap_text.Visible = True
            ElseIf talk = 2 Then
                paper6.Visible = True
                chat_end()
            End If
        End If
        If item = 7 Then
            If talk = 1 Then
                chater.Image = paper.Image
                Label_chat.Text = "<殘餘的日記>"
                paper7.Visible = False
                pap_text.Text = "3月18日" & vbCrLf & "最近煤炭的產量有下滑的趨勢，可能要開始找新的礦區了!" & vbCrLf & vbCrLf & "4月20日" & vbCrLf & "發了! 發了! 這種地層居然挖得到黃金!"
                pap_.Visible = True
                pap_text.Visible = True
            ElseIf talk = 2 Then
                paper7.Visible = True
                chat_end()
            End If
        End If
        If item = 8 Then
            If talk = 1 Then
                chater.Image = sword.Image
                Label_chat.Text = "<刀>" & vbCrLf & "一把鋒利的刀"
            ElseIf talk = 2 Then
                item = 0
                chat_end()
            End If
        End If
        If item = 9 Then
            If talk = 1 Then
                chater.Image = Jimmy_bar.Image
                Label_chat.Text = "<鐵撬>" & vbCrLf & "看似耐用的鐵撬，可以用來破壞鐵欄杆" & vbCrLf & "(不知道為何會出現在教堂內)"
            ElseIf talk = 2 Then
                item = 0
                chat_end()
            End If
        End If
        If item = 41 Then
            If have_sword = True Then
                If talk = 1 Then
                    chater.Image = self.Image
                    Label_chat.Text = "<" & player_name & ">" & vbCrLf & "終於找到幕後黑手了"
                ElseIf talk = 2 Then
                    chater.Image = monster.Image
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "現在不是月圓之夜，我不會傷害你"
                ElseIf talk = 3 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "先聽我解釋解釋"
                ElseIf talk = 4 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "在2年前，一場礦坑的大爆炸使我喪命"
                ElseIf talk = 5 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "我的妻子因為太思念我了"
                ElseIf talk = 6 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "於是找到能讓人起死回生的方法，使我有了第二生命"
                ElseIf talk = 7 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "但是每當月圓之時"
                ElseIf talk = 8 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "我就會感覺到我需要更多的血液"
                ElseIf talk = 9 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "之後便開始失去意識地嗜血"
                ElseIf talk = 10 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "但我本質是善良的..."
                ElseIf talk = 11 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "所以你能放下你的刀子"
                ElseIf talk = 12 Then
                    Label_chat.Text = "<復活礦工>" & vbCrLf & "相信我，並讓我有治療的機會嗎?"
                    yes.Visible = True
                    no.Visible = True
                    yes.BringToFront()
                    no.BringToFront()
                    talk -= 1
                ElseIf talk = 13 Then
                    talk = 0
                End If
            Else
                true_die()
            End If

        End If
    End Sub
    '對話結束
    Private Sub chat_end()
        item = 0
        pap_.Visible = False
        pap_text.Visible = False
        chat_bar.Visible = False
        chater.Visible = False
        Label_chat.Visible = False
        Label_hit.Visible = False
        endy = 0
        talk = 0
        un1 = un1 * -1
    End Sub

    '黑霧
    Private Sub dark_effect()
        If light = False Then
            boder1.Top = PictureBox401.Top - boder1.Height - PictureBox104.Height * 2
            boder2.Top = PictureBox401.Top + PictureBox401.Height + PictureBox104.Height * 2
            boder3.Left = PictureBox401.Left - boder1.Width - PictureBox104.Width * 2
            boder4.Left = PictureBox401.Left + PictureBox401.Width + PictureBox104.Width * 2
            boder1.Visible = True
            boder2.Visible = True
            boder3.Visible = True
            boder4.Visible = True
            boder1.BringToFront()
            boder2.BringToFront()
            boder3.BringToFront()
            boder4.BringToFront()
            up_to_form()
        Else
            boder1.Visible = False
            boder2.Visible = False
            boder3.Visible = False
            boder4.Visible = False
        End If
    End Sub

    '重製地圖
    Private Sub map_clear()
        PictureBox104.Visible = False
        PictureBox2.Visible = False
        PictureBox3.Visible = False
        PictureBox4.Visible = False
        PictureBox5.Visible = False
        PictureBox6.Visible = False
        PictureBox7.Visible = False
        PictureBox8.Visible = False
        PictureBox9.Visible = False
        PictureBox10.Visible = False
        PictureBox11.Visible = False
        PictureBox12.Visible = False
        PictureBox13.Visible = False
        PictureBox26.Visible = False
        PictureBox25.Visible = False
        PictureBox24.Visible = False
        PictureBox23.Visible = False
        PictureBox22.Visible = False
        PictureBox21.Visible = False
        PictureBox20.Visible = False
        PictureBox19.Visible = False
        PictureBox18.Visible = False
        PictureBox17.Visible = False
        PictureBox16.Visible = False
        PictureBox15.Visible = False
        PictureBox14.Visible = False
        PictureBox59.Visible = False
        PictureBox58.Visible = False
        PictureBox57.Visible = False
        PictureBox56.Visible = False
        PictureBox55.Visible = False
        PictureBox54.Visible = False
        PictureBox53.Visible = False
        PictureBox52.Visible = False
        PictureBox51.Visible = False
        PictureBox50.Visible = False
        PictureBox49.Visible = False
        PictureBox48.Visible = False
        PictureBox47.Visible = False
        PictureBox92.Visible = False
        PictureBox91.Visible = False
        PictureBox90.Visible = False
        PictureBox89.Visible = False
        PictureBox88.Visible = False
        PictureBox87.Visible = False
        PictureBox86.Visible = False
        PictureBox85.Visible = False
        PictureBox84.Visible = False
        PictureBox83.Visible = False
        PictureBox82.Visible = False
        PictureBox81.Visible = False
        PictureBox80.Visible = False
        PictureBox148.Visible = False
        PictureBox147.Visible = False
        PictureBox146.Visible = False
        PictureBox145.Visible = False
        PictureBox1.Visible = False
        PictureBox123.Visible = False
        PictureBox122.Visible = False
        PictureBox121.Visible = False
        PictureBox120.Visible = False
        PictureBox119.Visible = False
        PictureBox118.Visible = False
        PictureBox117.Visible = False
        PictureBox116.Visible = False
        PictureBox181.Visible = False
        PictureBox180.Visible = False
        PictureBox179.Visible = False
        PictureBox178.Visible = False
        PictureBox177.Visible = False
        PictureBox176.Visible = False
        PictureBox175.Visible = False
        PictureBox174.Visible = False
        PictureBox173.Visible = False
        PictureBox172.Visible = False
        PictureBox171.Visible = False
        PictureBox170.Visible = False
        PictureBox169.Visible = False
        PictureBox214.Visible = False
        PictureBox213.Visible = False
        PictureBox212.Visible = False
        PictureBox211.Visible = False
        PictureBox210.Visible = False
        PictureBox209.Visible = False
        PictureBox208.Visible = False
        PictureBox207.Visible = False
        PictureBox206.Visible = False
        PictureBox205.Visible = False
        PictureBox204.Visible = False
        PictureBox203.Visible = False
        PictureBox202.Visible = False
        PictureBox247.Visible = False
        PictureBox246.Visible = False
        PictureBox245.Visible = False
        PictureBox244.Visible = False
        PictureBox243.Visible = False
        PictureBox242.Visible = False
        PictureBox241.Visible = False
        PictureBox240.Visible = False
        PictureBox239.Visible = False
        PictureBox238.Visible = False
        PictureBox237.Visible = False
        PictureBox236.Visible = False
        PictureBox235.Visible = False
        PictureBox280.Visible = False
        PictureBox279.Visible = False
        PictureBox278.Visible = False
        PictureBox277.Visible = False
        PictureBox276.Visible = False
        PictureBox275.Visible = False
        PictureBox274.Visible = False
        PictureBox273.Visible = False
        PictureBox272.Visible = False
        PictureBox271.Visible = False
        PictureBox270.Visible = False
        PictureBox269.Visible = False
        PictureBox268.Visible = False
        PictureBox313.Visible = False
        PictureBox312.Visible = False
        PictureBox311.Visible = False
        PictureBox310.Visible = False
        PictureBox309.Visible = False
        PictureBox308.Visible = False
        PictureBox307.Visible = False
        PictureBox306.Visible = False
        PictureBox305.Visible = False
        PictureBox304.Visible = False
        PictureBox303.Visible = False
        PictureBox302.Visible = False
        PictureBox301.Visible = False
        PictureBox346.Visible = False
        PictureBox345.Visible = False
        PictureBox344.Visible = False
        PictureBox343.Visible = False
        PictureBox342.Visible = False
        PictureBox341.Visible = False
        PictureBox340.Visible = False
        PictureBox339.Visible = False
        PictureBox338.Visible = False
        PictureBox337.Visible = False
        PictureBox336.Visible = False
        PictureBox335.Visible = False
        PictureBox334.Visible = False
        PictureBox379.Visible = False
        PictureBox378.Visible = False
        PictureBox377.Visible = False
        PictureBox376.Visible = False
        PictureBox375.Visible = False
        PictureBox374.Visible = False
        PictureBox373.Visible = False
        PictureBox372.Visible = False
        PictureBox371.Visible = False
        PictureBox370.Visible = False
        PictureBox369.Visible = False
        PictureBox368.Visible = False
        PictureBox367.Visible = False
    End Sub
    '重新載入地圖
    Private Sub map_change()
        minecraft()
        maptype()
        PictureBox401.Left = PictureBox104.Width * (x1 - 1) + 3
        PictureBox401.Top = PictureBox104.Height * (y1 - 1) + 3
        If map = 0 Then
            If back = True Then
                light = False
            Else
                light = True
            End If

            'Me.BackColor = Color.FromArgb(255, 192, 128)
            Me.BackColor = Color.Peru
            Me.BackgroundImage = ImageList9.Images(318)
        ElseIf map = 1 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 2 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 21 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 22 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            If paper1 = True Then
                Me.BackgroundImage = ImageList9.Images(481)
            Else
                Me.BackgroundImage = ImageList9.Images(319)
            End If
        ElseIf map = 23 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(319)
        ElseIf map = 24 Then
            light = True
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(148)
        ElseIf map = 11 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 12 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 13 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 14 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(490)
        ElseIf map = 15 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 16 Or map = 17 Or map = 18 Or map = 19 Or map = 20 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(318)
        ElseIf map = 30 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(487)
        ElseIf map = 31 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 32 Then
            light = True
            Me.BackColor = Color.White
            Me.BackgroundImage = ImageList9.Images(336)
        ElseIf map = 33 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 100 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = Nothing
            Me.BackColor = Color.White
        ElseIf map = 40 Then
            light = False
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
        ElseIf map = 41 Then
            light = True
            Me.BackColor = Color.Silver
            Me.BackgroundImage = ImageList9.Images(403)
        End If
        dark_effect()
        where()
    End Sub

    '1P牆壁擋玩家執行(function back)
    Private Sub back1()
        If f12 = 0 Then
            If udlr1 = 0 Then
                PictureBox401.Top = PictureBox401.Top + un1 * PictureBox104.Height
                y1 = y1 + un1
            ElseIf udlr1 = 1 Then
                PictureBox401.Left = PictureBox401.Left + un1 * PictureBox104.Width
                x1 = x1 + un1
            End If
        End If
        If exer1 > 0 Then
            exer1 += 1
        End If
    End Sub

    'TNT擋玩家偵測 + 呼叫 function back (function)
    Private Sub tntwall()
        If exer1 = 0 Then
            If xy1 = tnt11 Then
                Call back1()
            End If
            If xy1 = tnt12 Then
                Call back1()
            End If
        End If
        If exer1 = 0 Or exer1 = 1 Then
            If xy1 = tnt21 Then
                Call back1()
            End If
            If xy1 = tnt22 Then
                Call back1()
            End If
        End If
    End Sub

    '1P牆壁擋玩家偵測 + 呼叫 function back (function)
    Private Sub detection1()
        xy1 = x1 * 100 + y1
        If f12 = 0 Then
            If map = 0 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 407 And PictureBox86.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 507 And PictureBox122.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 607 And PictureBox175.Visible = True Then
                    Call back1()
                End If
                If xy1 = 609 And PictureBox173.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 707 And PictureBox208.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 807 And PictureBox241.Visible = True Then
                    Call back1()
                End If
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1105 And PictureBox342.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1106 And PictureBox341.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1108 And PictureBox339.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 1 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 507 And PictureBox122.Visible = True Then
                    Call back1()
                End If
                If xy1 = 508 And PictureBox121.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 606 And PictureBox176.Visible = True Then
                    Call back1()
                End If
                If xy1 = 607 And PictureBox175.Visible = True Then
                    Call back1()
                End If
                If xy1 = 608 And PictureBox174.Visible = True Then
                    Call back1()
                End If
                If xy1 = 609 And PictureBox173.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 706 And PictureBox209.Visible = True Then
                    Call back1()
                End If
                If xy1 = 707 And PictureBox208.Visible = True Then
                    Call back1()
                End If
                If xy1 = 708 And PictureBox207.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 807 And PictureBox241.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 2 Then
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 207 And PictureBox20.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 407 And PictureBox86.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 705 And PictureBox210.Visible = True Then
                    Call back1()
                End If
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 21 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 207 And PictureBox20.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 405 And PictureBox88.Visible = True Then
                    Call back1()
                End If
                If xy1 = 406 And PictureBox87.Visible = True Then
                    Call back1()
                End If
                If xy1 = 407 And PictureBox86.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 505 And PictureBox1.Visible = True Then
                    Call back1()
                End If
                If xy1 = 506 And PictureBox123.Visible = True Then
                    Call back1()
                End If
                If xy1 = 508 And PictureBox121.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 709 And PictureBox206.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 908 And PictureBox273.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1108 And PictureBox339.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 22 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 406 And PictureBox87.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 506 And PictureBox123.Visible = True Then
                    Call back1()
                End If
                If xy1 = 508 And PictureBox121.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 606 And PictureBox176.Visible = True Then
                    Call back1()
                End If
                If xy1 = 607 And PictureBox175.Visible = True Then
                    Call back1()
                End If
                If xy1 = 608 And PictureBox174.Visible = True Then
                    Call back1()
                End If
                If xy1 = 610 And PictureBox172.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 706 And PictureBox209.Visible = True Then
                    Call back1()
                End If
                If xy1 = 708 And PictureBox207.Visible = True Then
                    Call back1()
                End If
                If xy1 = 709 And PictureBox206.Visible = True Then
                    Call back1()
                End If
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 908 And PictureBox273.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1006 And PictureBox308.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1007 And PictureBox307.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1008 And PictureBox306.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1106 And PictureBox341.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 23 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 406 And PictureBox87.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 506 And PictureBox123.Visible = True Then
                    Call back1()
                End If
                If xy1 = 508 And PictureBox121.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 606 And PictureBox176.Visible = True Then
                    Call back1()
                End If
                If xy1 = 608 And PictureBox174.Visible = True Then
                    Call back1()
                End If
                If xy1 = 610 And PictureBox172.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 706 And PictureBox209.Visible = True Then
                    Call back1()
                End If
                If xy1 = 708 And PictureBox207.Visible = True Then
                    Call back1()
                End If
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 908 And PictureBox273.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1006 And PictureBox308.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1007 And PictureBox307.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1008 And PictureBox306.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 24 Then
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If

                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 207 And PictureBox20.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 405 And PictureBox88.Visible = True Then
                    Call back1()
                End If
                If xy1 = 406 And PictureBox87.Visible = True Then
                    Call back1()
                End If
                If xy1 = 407 And PictureBox86.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 505 And PictureBox1.Visible = True Then
                    Call back1()
                End If
                If xy1 = 506 And PictureBox123.Visible = True Then
                    Call back1()
                End If
                If xy1 = 508 And PictureBox121.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1006 And PictureBox308.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1105 And PictureBox342.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1106 And PictureBox341.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1107 And PictureBox340.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1108 And PictureBox339.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 11 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 609 And PictureBox173.Visible = True Then
                    Call back1()
                End If
                If xy1 = 610 And PictureBox172.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 709 And PictureBox206.Visible = True Then
                    Call back1()
                End If
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 12 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 607 And PictureBox175.Visible = True Then
                    Call back1()
                End If
                If xy1 = 608 And PictureBox174.Visible = True Then
                    Call back1()
                End If
                If xy1 = 609 And PictureBox173.Visible = True Then
                    Call back1()
                End If
                If xy1 = 610 And PictureBox172.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 706 And PictureBox209.Visible = True Then
                    Call back1()
                End If
                If xy1 = 707 And PictureBox208.Visible = True Then
                    Call back1()
                End If
                If xy1 = 708 And PictureBox207.Visible = True Then
                    Call back1()
                End If
                If xy1 = 709 And PictureBox206.Visible = True Then
                    Call back1()
                End If
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 807 And PictureBox241.Visible = True Then
                    Call back1()
                End If
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 907 And PictureBox274.Visible = True Then
                    Call back1()
                End If
                If xy1 = 908 And PictureBox273.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1006 And PictureBox308.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1007 And PictureBox307.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1008 And PictureBox306.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1105 And PictureBox342.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1106 And PictureBox341.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1107 And PictureBox340.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1108 And PictureBox339.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 13 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 207 And PictureBox20.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 507 And PictureBox122.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 605 And PictureBox177.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 708 And PictureBox207.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 14 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 406 And PictureBox87.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 506 And PictureBox123.Visible = True Then
                    Call back1()
                End If
                If xy1 = 507 And PictureBox122.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 606 And PictureBox176.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 705 And PictureBox210.Visible = True Then
                    Call back1()
                End If
                If xy1 = 706 And PictureBox209.Visible = True Then
                    Call back1()
                End If
                If xy1 = 707 And PictureBox208.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1105 And PictureBox342.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1106 And PictureBox341.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 15 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 610 And PictureBox172.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 709 And PictureBox206.Visible = True Then
                    Call back1()
                End If
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 16 Or map = 17 Or map = 18 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 207 And PictureBox20.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 405 And PictureBox88.Visible = True Then
                    Call back1()
                End If
                If xy1 = 406 And PictureBox87.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 505 And PictureBox1.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 605 And PictureBox177.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 705 And PictureBox210.Visible = True Then
                    Call back1()
                End If
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 807 And PictureBox241.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 907 And PictureBox274.Visible = True Then
                    Call back1()
                End If
                If xy1 = 908 And PictureBox273.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1006 And PictureBox308.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1007 And PictureBox307.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1008 And PictureBox306.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1105 And PictureBox342.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1106 And PictureBox341.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1107 And PictureBox340.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1108 And PictureBox339.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 19 Or map = 20 Then
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 207 And PictureBox20.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 407 And PictureBox86.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 609 And PictureBox173.Visible = True Then
                    Call back1()
                End If
                If xy1 = 610 And PictureBox172.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 709 And PictureBox206.Visible = True Then
                    Call back1()
                End If
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 907 And PictureBox274.Visible = True Then
                    Call back1()
                End If
                If xy1 = 908 And PictureBox273.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1006 And PictureBox308.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1007 And PictureBox307.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1008 And PictureBox306.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 30 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 610 And PictureBox172.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 807 And PictureBox241.Visible = True Then
                    Call back1()
                End If
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1007 And PictureBox307.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 31 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 505 And PictureBox1.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 605 And PictureBox177.Visible = True Then
                    Call back1()
                End If
                If xy1 = 606 And PictureBox176.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1007 And PictureBox307.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1008 And PictureBox306.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1105 And PictureBox342.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1106 And PictureBox341.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1107 And PictureBox340.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1108 And PictureBox339.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 32 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 405 And PictureBox88.Visible = True Then
                    Call back1()
                End If
                If xy1 = 406 And PictureBox87.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 605 And PictureBox177.Visible = True Then
                    Call back1()
                End If
                If xy1 = 606 And PictureBox176.Visible = True Then
                    Call back1()
                End If
                If xy1 = 608 And PictureBox174.Visible = True Then
                    Call back1()
                End If
                If xy1 = 609 And PictureBox173.Visible = True Then
                    Call back1()
                End If
                If xy1 = 610 And PictureBox172.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1007 And PictureBox307.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 33 Then
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 405 And PictureBox88.Visible = True Then
                    Call back1()
                End If
                If xy1 = 406 And PictureBox87.Visible = True Then
                    Call back1()
                End If
                If xy1 = 407 And PictureBox86.Visible = True Then
                    Call back1()
                End If
                If xy1 = 408 And PictureBox85.Visible = True Then
                    Call back1()
                End If
                If xy1 = 409 And PictureBox84.Visible = True Then
                    Call back1()
                End If
                If xy1 = 410 And PictureBox83.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 505 And PictureBox1.Visible = True Then
                    Call back1()
                End If
                If xy1 = 506 And PictureBox123.Visible = True Then
                    Call back1()
                End If
                If xy1 = 507 And PictureBox122.Visible = True Then
                    Call back1()
                End If
                If xy1 = 508 And PictureBox121.Visible = True Then
                    Call back1()
                End If
                If xy1 = 509 And PictureBox120.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 511 And PictureBox118.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 807 And PictureBox241.Visible = True Then
                    Call back1()
                End If
                If xy1 = 808 And PictureBox240.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 810 And PictureBox238.Visible = True Then
                    Call back1()
                End If
                If xy1 = 811 And PictureBox237.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 907 And PictureBox274.Visible = True Then
                    Call back1()
                End If
                If xy1 = 908 And PictureBox273.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 100 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 613 And PictureBox169.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 40 Then
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 407 And PictureBox86.Visible = True Then
                    Call back1()
                End If
                If xy1 = 411 And PictureBox82.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 510 And PictureBox119.Visible = True Then
                    Call back1()
                End If
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 608 And PictureBox174.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
                    Call back1()
                End If
                If xy1 = 612 And PictureBox170.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 809 And PictureBox239.Visible = True Then
                    Call back1()
                End If
                If xy1 = 812 And PictureBox236.Visible = True Then
                    Call back1()
                End If
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 911 And PictureBox270.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1009 And PictureBox305.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1010 And PictureBox304.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1108 And PictureBox339.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 41 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 103 And PictureBox3.Visible = True Then
                    Call back1()
                End If
                If xy1 = 104 And PictureBox4.Visible = True Then
                    Call back1()
                End If
                If xy1 = 105 And PictureBox5.Visible = True Then
                    Call back1()
                End If
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 111 And PictureBox11.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 113 And PictureBox13.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
                    Call back1()
                End If
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 206 And PictureBox21.Visible = True Then
                    Call back1()
                End If
                If xy1 = 207 And PictureBox20.Visible = True Then
                    Call back1()
                End If
                If xy1 = 208 And PictureBox19.Visible = True Then
                    Call back1()
                End If
                If xy1 = 209 And PictureBox18.Visible = True Then
                    Call back1()
                End If
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
                    Call back1()
                End If
                If xy1 = 213 And PictureBox14.Visible = True Then
                    Call back1()
                End If
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 302 And PictureBox58.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
                    Call back1()
                End If
                If xy1 = 304 And PictureBox56.Visible = True Then
                    Call back1()
                End If
                If xy1 = 305 And PictureBox55.Visible = True Then
                    Call back1()
                End If
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
                    Call back1()
                End If
                If xy1 = 311 And PictureBox49.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 401 And PictureBox92.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
                    Call back1()
                End If
                If xy1 = 403 And PictureBox90.Visible = True Then
                    Call back1()
                End If
                If xy1 = 404 And PictureBox89.Visible = True Then
                    Call back1()
                End If
                If xy1 = 405 And PictureBox88.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
                    Call back1()
                End If
                If xy1 = 501 And PictureBox148.Visible = True Then
                    Call back1()
                End If
                If xy1 = 502 And PictureBox147.Visible = True Then
                    Call back1()
                End If
                If xy1 = 503 And PictureBox146.Visible = True Then
                    Call back1()
                End If
                If xy1 = 504 And PictureBox145.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
                    Call back1()
                End If
                If xy1 = 603 And PictureBox179.Visible = True Then
                    Call back1()
                End If
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 701 And PictureBox214.Visible = True Then
                    Call back1()
                End If
                If xy1 = 702 And PictureBox213.Visible = True Then
                    Call back1()
                End If
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
                    Call back1()
                End If
                If xy1 = 713 And PictureBox202.Visible = True Then
                    Call back1()
                End If
                If xy1 = 801 And PictureBox247.Visible = True Then
                    Call back1()
                End If
                If xy1 = 802 And PictureBox246.Visible = True Then
                    Call back1()
                End If
                If xy1 = 803 And PictureBox245.Visible = True Then
                    Call back1()
                End If
                If xy1 = 804 And PictureBox244.Visible = True Then
                    Call back1()
                End If
                If xy1 = 805 And PictureBox243.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
                    Call back1()
                End If
                If xy1 = 903 And PictureBox278.Visible = True Then
                    Call back1()
                End If
                If xy1 = 904 And PictureBox277.Visible = True Then
                    Call back1()
                End If
                If xy1 = 905 And PictureBox276.Visible = True Then
                    Call back1()
                End If
                If xy1 = 913 And PictureBox268.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1001 And PictureBox313.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1003 And PictureBox311.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1004 And PictureBox310.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1005 And PictureBox309.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1006 And PictureBox308.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1101 And PictureBox346.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1105 And PictureBox342.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1106 And PictureBox341.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1107 And PictureBox340.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1108 And PictureBox339.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1109 And PictureBox338.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1201 And PictureBox379.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1202 And PictureBox378.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1203 And PictureBox377.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1209 And PictureBox371.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1210 And PictureBox370.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If

            End If
            If y1 = 14 Then
                Call back1()
            End If
            If x1 = 13 Then
                Call back1()
            End If
        End If
    End Sub

    '1PTNT炸[可破壞牆壁]判斷&執行(function)
    Private Sub tnt_1()
        If map = 0 Then
            If tnt1xy = 203 Or tnt1xy = 303 Or tnt1xy = 103 Or tnt1xy = 204 Or tnt1xy = 202 Then

                If PictureBox24.Visible = True Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\glass1.wav", AudioPlayMode.Background)
                End If
                PictureBox24.Visible = False
            End If
            If tnt1xy = 212 Or tnt1xy = 312 Or tnt1xy = 112 Or tnt1xy = 213 Or tnt1xy = 211 Then

                If PictureBox15.Visible = True Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\glass2.wav", AudioPlayMode.Background)
                End If
                PictureBox15.Visible = False
            End If
            If tnt1xy = 1012 Or tnt1xy = 1112 Or tnt1xy = 912 Or tnt1xy = 1013 Or tnt1xy = 1011 Then

                If PictureBox302.Visible = True Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\glass3.wav", AudioPlayMode.Background)
                End If
                PictureBox302.Visible = False
            End If
        ElseIf map = 22 Then
            If tnt1xy = 206 Or tnt1xy = 306 Or tnt1xy = 106 Or tnt1xy = 207 Or tnt1xy = 205 Then
                PictureBox21.Visible = False
                sounds()
            End If
            If tnt1xy = 208 Or tnt1xy = 308 Or tnt1xy = 108 Or tnt1xy = 209 Or tnt1xy = 207 Then
                PictureBox19.Visible = False
                sounds()
            End If
            If tnt1xy = 211 Or tnt1xy = 311 Or tnt1xy = 111 Or tnt1xy = 212 Or tnt1xy = 210 Then
                PictureBox16.Visible = False
                sounds()
            End If
            If tnt1xy = 305 Or tnt1xy = 405 Or tnt1xy = 205 Or tnt1xy = 306 Or tnt1xy = 304 Then
                PictureBox55.Visible = False
                sounds()
            End If
            If tnt1xy = 403 Or tnt1xy = 503 Or tnt1xy = 303 Or tnt1xy = 404 Or tnt1xy = 402 Then
                PictureBox90.Visible = False
                sounds()
            End If
            If tnt1xy = 411 Or tnt1xy = 511 Or tnt1xy = 311 Or tnt1xy = 412 Or tnt1xy = 410 Then
                PictureBox82.Visible = False
                sounds()
            End If
            If tnt1xy = 607 Or tnt1xy = 707 Or tnt1xy = 507 Or tnt1xy = 608 Or tnt1xy = 606 Then
                PictureBox175.Visible = False
                sounds()
            End If
            If tnt1xy = 709 Or tnt1xy = 809 Or tnt1xy = 609 Or tnt1xy = 710 Or tnt1xy = 708 Then
                PictureBox206.Visible = False
                sounds()
            End If
            If tnt1xy = 805 Or tnt1xy = 905 Or tnt1xy = 705 Or tnt1xy = 806 Or tnt1xy = 804 Then
                PictureBox243.Visible = False
                sounds()
            End If
            If tnt1xy = 811 Or tnt1xy = 911 Or tnt1xy = 711 Or tnt1xy = 812 Or tnt1xy = 810 Then
                PictureBox237.Visible = False
                sounds()
            End If
            If tnt1xy = 903 Or tnt1xy = 1003 Or tnt1xy = 803 Or tnt1xy = 904 Or tnt1xy = 902 Then
                PictureBox278.Visible = False
                sounds()
            End If
            If tnt1xy = 905 Or tnt1xy = 1005 Or tnt1xy = 805 Or tnt1xy = 906 Or tnt1xy = 904 Then
                PictureBox276.Visible = False
                sounds()
            End If
            If tnt1xy = 1007 Or tnt1xy = 1107 Or tnt1xy = 907 Or tnt1xy = 1008 Or tnt1xy = 1006 Then
                PictureBox307.Visible = False
                sounds()
            End If
            If tnt1xy = 1106 Or tnt1xy = 1206 Or tnt1xy = 1006 Or tnt1xy = 1107 Or tnt1xy = 1105 Then
                PictureBox341.Visible = False
                sounds()
            End If
            If tnt1xy = 1110 Or tnt1xy = 1210 Or tnt1xy = 1010 Or tnt1xy = 1111 Or tnt1xy = 1109 Then
                PictureBox337.Visible = False
                sounds()
            End If
        End If
    End Sub

    '按鍵盤時動作(包含上下左右,F3,F12,ESC)+(邊界擋玩家)(keydown)
    Private Sub Form1_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        If endy = 0 Then
            If e.KeyCode = Keys.Left Then
                Call Character1_left()
            ElseIf e.KeyCode = Keys.Right Then
                Call Character1_right()
            ElseIf e.KeyCode = Keys.Up Then
                Call Character1_up()
            ElseIf e.KeyCode = Keys.Down Then
                Character1_down()
            ElseIf e.KeyCode = Keys.Space Then
                eventes()
                '執行時可按F3查看參數
            ElseIf e.KeyCode = Keys.F3 Then
                f3 += 1
                If f3 > 1 Then
                    f3 = 0
                End If
            ElseIf e.KeyCode = Keys.F12 Then
                If admin = True Then
                    f12 += 1
                    f3 = 1
                    If f12 > 1 Then
                        f12 = 0
                        f3 = 0
                    End If
                Else
                    op = Val(InputBox("你即將啟動開發人員模式", "admin", "ch12xy2music"))
                    If op = ch1 & ch2 & xy2 & music Then
                        admin = True
                        MsgBox("成功開啟開發人員模式", 4 + 48, "開發人員模式")
                    End If
                End If
            End If
            xy1 = x1 * 100 + y1
            xy2 = x2 * 100 + y2

            dark_effect()
            Engineering_mode()
            tntwall()
            where()
        End If
        If e.KeyCode = Keys.Escape Then
            If endy < 2 Then
                esc += 1
                If esc > 1 Then
                    esc = 0
                End If
                esc_mode()
            End If
        End If
        If talk > 0 Then
            talk += 1
            chat_events()
        End If
        If talk = 0 Then
            events_auto()
        End If
        pap_text.BringToFront()
        PictureBox136.BringToFront()
        Button1.BringToFront()
        Button2.BringToFront()
        Button3.BringToFront()
    End Sub

    '載入遊戲時初始化(onload)
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        cd_1 = False
        paper1 = False
        cd1_played = False
        waiter_talk = False
        paper2 = False
        have_sword = False
        seen_the_woman = False
        goddad_talk = False
        back = False

        If player_name = "" Then
            player_name = "自己"
        End If
        Me.BackColor = Color.Peru
        Me.BackgroundImage = ImageList9.Images(318)
        music = 0
        map = 0
        minecraft()
        ch1 = 0
        PictureBox401.Image = ImageList1.Images(1)
        PictureBox136.Visible = False
        Button1.Visible = False
        Button2.Visible = False
        Button3.Visible = False
        PictureBox401.Left = PictureBox104.Width * (6 - 1) + 3
        PictureBox401.Top = PictureBox104.Height * (5 - 1) + 3
        x1 = 6
        y1 = 5
        x2 = 20
        y2 = 20
        xy1 = x1 * 100 + y1
        xy2 = x2 * 100 + y2
        f3 = 0
        f12 = 0
        PictureBox401.BringToFront()
        PictureBox400.BringToFront()
        PictureBox392.BringToFront()
        PictureBox393.BringToFront()
        PictureBox398.BringToFront()
        PictureBox399.BringToFront()

        PictureBox380.SendToBack()
        PictureBox382.SendToBack()
        PictureBox383.SendToBack()
        PictureBox384.SendToBack()
        PictureBox385.SendToBack()
        PictureBox386.SendToBack()
        PictureBox387.SendToBack()
        PictureBox388.SendToBack()

        PictureBox381.SendToBack()
        PictureBox389.SendToBack()
        PictureBox390.SendToBack()
        PictureBox391.SendToBack()
        PictureBox396.SendToBack()
        PictureBox397.SendToBack()
        PictureBox394.SendToBack()
        PictureBox395.SendToBack()

        player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm_story_mode.mp3"
        If music = 0 Then
            Dim i As Integer
            Randomize()
            i = Int((6 - 0 + 1) * Rnd() + 0)
        ElseIf music = 1 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\Dies Irae.mp3"
        ElseIf music = 2 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\+9.mp3"
        ElseIf music = 3 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\apple.mp3"
        End If
        maptype()
        where()
        Timer6.Enabled = True
    End Sub
    '死亡特效
    Private Sub Timer5_Tick(sender As System.Object, e As System.EventArgs) Handles Timer5.Tick
        Dim i As Integer
        i += 1
        If i < Me.Height + 100 Then
            If death1 >= 1 Then
                PictureBox401.Top += 10
            End If
            If death2 >= 1 Then
                PictureBox400.Top += 10
            End If
        End If
        If PictureBox400.Top > Me.Height Or PictureBox401.Top > Me.Height Then
            esc = 1
            Call esc_mode()
        End If
    End Sub
    'end
    Private Sub Button2_Click(sender As System.Object, e As System.EventArgs) Handles Button2.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        start.Close()
    End Sub
    'replay
    Private Sub Button1_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        replay1 = 6
        Me.Close()
    End Sub
    'back
    Private Sub Button3_Click(sender As System.Object, e As System.EventArgs) Handles Button3.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        Form2.Show()
        Me.Close()

    End Sub

    '地圖
    Private Sub maptype()
        If map = 0 Then
            PictureBox181.Visible = True

            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox122.Visible = True
            PictureBox116.Visible = True
            PictureBox175.Visible = True
            PictureBox173.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox208.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox241.Visible = True
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True
            If back = True Then
                PictureBox173.Visible = False
                PictureBox24.Visible = False
                PictureBox311.Visible = False
                PictureBox15.Visible = False
                PictureBox302.Visible = False
                PictureBox176.Visible = True
                PictureBox177.Visible = True
                PictureBox178.Visible = True
                PictureBox179.Visible = True
                PictureBox180.Visible = True
                PictureBox176.Image = ImageList9.Images(473)
                PictureBox177.Image = ImageList9.Images(472)
                PictureBox178.Image = ImageList9.Images(473)
                PictureBox179.Image = ImageList9.Images(472)
                PictureBox180.Image = ImageList9.Images(473)
            End If


            PictureBox181.Image = ImageList9.Images(119)

            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox25.Image = ImageList9.Images(15)
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(15)
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox91.Image = ImageList9.Images(15)
            PictureBox86.Image = ImageList9.Images(4)
            PictureBox85.Image = ImageList9.Images(4)
            PictureBox84.Image = ImageList9.Images(4)
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(15)
            PictureBox122.Image = ImageList9.Images(82)
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox175.Image = ImageList9.Images(141)
            PictureBox173.Image = cheif.Image
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(15)
            PictureBox208.Image = ImageList9.Images(147)
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(15)
            PictureBox241.Image = ImageList9.Images(4)
            PictureBox240.Image = ImageList9.Images(4)
            PictureBox239.Image = ImageList9.Images(4)
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(15)
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(15)
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(15)
            PictureBox344.Image = ImageList9.Images(32)
            PictureBox343.Image = ImageList9.Images(260)
            PictureBox342.Image = ImageList9.Images(147)
            PictureBox341.Image = ImageList9.Images(147)
            PictureBox339.Image = ImageList9.Images(16)
            PictureBox338.Image = ImageList9.Images(16)
            PictureBox336.Image = ImageList9.Images(166)
            PictureBox335.Image = ImageList9.Images(167)
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
            If back = True Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\sud_3.wav", AudioPlayMode.Background)
            End If
        ElseIf map = 1 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox122.Visible = True
            PictureBox121.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox176.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox242.Visible = True
            PictureBox241.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox104.Image = ImageList9.Images(block(2))
            PictureBox2.Image = ImageList9.Images(block(2))
            PictureBox3.Image = ImageList9.Images(block(2))
            PictureBox4.Image = ImageList9.Images(block(2))
            PictureBox10.Image = ImageList9.Images(block(2))
            PictureBox11.Image = ImageList9.Images(block(2))
            PictureBox12.Image = ImageList9.Images(block(2))
            PictureBox26.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox18.Image = ImageList9.Images(block(2))
            PictureBox17.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox58.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox51.Image = ImageList9.Images(block(2))
            PictureBox50.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox48.Image = ImageList9.Images(block(2))
            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox84.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox121.Image = ImageList9.Images(block(2))
            PictureBox120.Image = ImageList9.Images(block(2))
            PictureBox181.Image = ImageList9.Images(block(2))
            PictureBox175.Image = ImageList9.Images(block(2))
            PictureBox174.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox208.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox346.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox337.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox379.Image = ImageList9.Images(block(2))
            PictureBox378.Image = ImageList9.Images(block(2))
            PictureBox377.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox369.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox23.Image = ImageList9.Images(block(3))
            PictureBox19.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox56.Image = ImageList9.Images(block(3))
            PictureBox52.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox90.Image = ImageList9.Images(block(3))
            PictureBox85.Image = ImageList9.Images(block(3))
            PictureBox82.Image = ImageList9.Images(block(3))
            PictureBox81.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox122.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox180.Image = ImageList9.Images(block(3))
            PictureBox176.Image = ImageList9.Images(block(3))
            PictureBox173.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox209.Image = ImageList9.Images(block(3))
            PictureBox207.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox242.Image = ImageList9.Images(block(3))
            PictureBox241.Image = ImageList9.Images(block(3))
            PictureBox236.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox270.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(block(3))
            PictureBox304.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox338.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 2 Then
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox26.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox210.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox243.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox6.Image = ImageList9.Images(block(2))
            PictureBox7.Image = ImageList9.Images(block(2))
            PictureBox8.Image = ImageList9.Images(block(2))
            PictureBox9.Image = ImageList9.Images(block(2))
            PictureBox20.Image = ImageList9.Images(block(2))
            PictureBox19.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox53.Image = ImageList9.Images(block(2))
            PictureBox52.Image = ImageList9.Images(block(2))
            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox80.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox181.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox179.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox169.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox213.Image = ImageList9.Images(block(2))
            PictureBox212.Image = ImageList9.Images(block(2))
            PictureBox211.Image = ImageList9.Images(block(2))
            PictureBox204.Image = ImageList9.Images(block(2))
            PictureBox203.Image = ImageList9.Images(block(2))
            PictureBox202.Image = ImageList9.Images(block(2))
            PictureBox247.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox245.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox237.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox278.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox310.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox301.Image = ImageList9.Images(block(2))
            PictureBox346.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox344.Image = ImageList9.Images(block(2))
            PictureBox337.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox334.Image = ImageList9.Images(block(2))
            PictureBox379.Image = ImageList9.Images(block(2))
            PictureBox378.Image = ImageList9.Images(block(2))
            PictureBox377.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox369.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox21.Image = ImageList9.Images(block(3))
            PictureBox18.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox54.Image = ImageList9.Images(block(3))
            PictureBox51.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox91.Image = ImageList9.Images(block(3))
            PictureBox86.Image = ImageList9.Images(block(3))
            PictureBox85.Image = ImageList9.Images(block(3))
            PictureBox81.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox178.Image = ImageList9.Images(block(3))
            PictureBox171.Image = ImageList9.Images(block(3))
            PictureBox210.Image = ImageList9.Images(block(3))
            PictureBox205.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox238.Image = ImageList9.Images(block(3))
            PictureBox276.Image = ImageList9.Images(block(3))
            PictureBox272.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox305.Image = ImageList9.Images(block(3))
            PictureBox343.Image = ImageList9.Images(block(3))
            PictureBox338.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
        ElseIf map = 21 Then
            PictureBox122.Visible = True

            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox89.Visible = True
            PictureBox88.Visible = True
            PictureBox87.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox145.Visible = True
            PictureBox1.Visible = True
            PictureBox123.Visible = True
            PictureBox121.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox211.Visible = True
            PictureBox206.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox242.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox273.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox343.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox376.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True



            PictureBox122.Image = ImageList9.Images(116)

            PictureBox3.Image = ImageList9.Images(block(1))
            PictureBox4.Image = ImageList9.Images(block(1))
            PictureBox5.Image = ImageList9.Images(block(1))
            PictureBox6.Image = ImageList9.Images(block(1))
            PictureBox7.Image = ImageList9.Images(block(1))
            PictureBox8.Image = ImageList9.Images(block(1))
            PictureBox9.Image = ImageList9.Images(block(1))
            PictureBox10.Image = ImageList9.Images(block(1))
            PictureBox24.Image = ImageList9.Images(block(1))
            PictureBox23.Image = ImageList9.Images(block(1))
            PictureBox22.Image = ImageList9.Images(block(1))
            PictureBox21.Image = ImageList9.Images(block(1))
            PictureBox20.Image = ImageList9.Images(block(1))
            PictureBox19.Image = ImageList9.Images(block(1))
            PictureBox18.Image = ImageList9.Images(block(1))
            PictureBox17.Image = ImageList9.Images(block(1))
            PictureBox57.Image = ImageList9.Images(block(1))
            PictureBox56.Image = ImageList9.Images(block(1))
            PictureBox55.Image = ImageList9.Images(block(1))
            PictureBox54.Image = ImageList9.Images(block(1))
            PictureBox53.Image = ImageList9.Images(block(1))
            PictureBox52.Image = ImageList9.Images(block(1))
            PictureBox51.Image = ImageList9.Images(block(1))
            PictureBox50.Image = ImageList9.Images(block(1))
            PictureBox89.Image = ImageList9.Images(block(1))
            PictureBox88.Image = ImageList9.Images(block(1))
            PictureBox87.Image = ImageList9.Images(block(1))
            PictureBox86.Image = ImageList9.Images(block(1))
            PictureBox85.Image = ImageList9.Images(block(1))
            PictureBox84.Image = ImageList9.Images(block(1))
            PictureBox83.Image = ImageList9.Images(block(1))
            PictureBox145.Image = ImageList9.Images(block(1))
            PictureBox1.Image = ImageList9.Images(block(1))
            PictureBox123.Image = ImageList9.Images(block(1))
            PictureBox121.Image = ImageList9.Images(block(1))
            PictureBox120.Image = ImageList9.Images(block(1))
            PictureBox119.Image = ImageList9.Images(block(1))
            PictureBox2.Image = ImageList9.Images(block(2))
            PictureBox11.Image = ImageList9.Images(block(2))
            PictureBox12.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox169.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox211.Image = ImageList9.Images(block(2))
            PictureBox206.Image = ImageList9.Images(block(2))
            PictureBox203.Image = ImageList9.Images(block(2))
            PictureBox202.Image = ImageList9.Images(block(2))
            PictureBox247.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox242.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox301.Image = ImageList9.Images(block(2))
            PictureBox338.Image = ImageList9.Images(block(2))
            PictureBox337.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox334.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox369.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox170.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox204.Image = ImageList9.Images(block(3))
            PictureBox245.Image = ImageList9.Images(block(3))
            PictureBox237.Image = ImageList9.Images(block(3))
            PictureBox278.Image = ImageList9.Images(block(3))
            PictureBox271.Image = ImageList9.Images(block(3))
            PictureBox310.Image = ImageList9.Images(block(3))
            PictureBox305.Image = ImageList9.Images(block(3))
            PictureBox343.Image = ImageList9.Images(block(3))
            PictureBox339.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
        ElseIf map = 22 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox21.Visible = True
            PictureBox19.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox52.Visible = True
            PictureBox50.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox87.Visible = True
            PictureBox85.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox145.Visible = True
            PictureBox123.Visible = True
            PictureBox121.Visible = True
            PictureBox119.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox178.Visible = True
            PictureBox176.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox172.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox211.Visible = True
            PictureBox209.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True
            PictureBox205.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox244.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox240.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox273.Visible = True
            PictureBox271.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox310.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox304.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox341.Visible = True
            PictureBox337.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox25.Image = ImageList9.Images(261)
            PictureBox21.Image = ImageList9.Images(block(1))
            PictureBox19.Image = ImageList9.Images(block(1))
            PictureBox16.Image = ImageList9.Images(block(1))
            PictureBox55.Image = ImageList9.Images(block(1))
            PictureBox90.Image = ImageList9.Images(block(1))
            PictureBox82.Image = ImageList9.Images(block(1))
            PictureBox175.Image = ImageList9.Images(block(1))
            PictureBox206.Image = ImageList9.Images(block(1))
            PictureBox243.Image = ImageList9.Images(block(1))
            PictureBox237.Image = ImageList9.Images(block(1))
            PictureBox278.Image = ImageList9.Images(block(1))
            PictureBox276.Image = ImageList9.Images(block(1))
            PictureBox307.Image = ImageList9.Images(block(1))
            PictureBox341.Image = ImageList9.Images(block(1))
            PictureBox337.Image = ImageList9.Images(block(1))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox58.Image = ImageList9.Images(block(2))
            PictureBox56.Image = ImageList9.Images(block(2))
            PictureBox54.Image = ImageList9.Images(block(2))
            PictureBox52.Image = ImageList9.Images(block(2))
            PictureBox50.Image = ImageList9.Images(block(2))
            PictureBox48.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox81.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox123.Image = ImageList9.Images(block(2))
            PictureBox121.Image = ImageList9.Images(block(2))
            PictureBox119.Image = ImageList9.Images(block(2))
            PictureBox117.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox178.Image = ImageList9.Images(block(2))
            PictureBox176.Image = ImageList9.Images(block(2))
            PictureBox174.Image = ImageList9.Images(block(2))
            PictureBox172.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox213.Image = ImageList9.Images(block(2))
            PictureBox211.Image = ImageList9.Images(block(2))
            PictureBox209.Image = ImageList9.Images(block(2))
            PictureBox207.Image = ImageList9.Images(block(2))
            PictureBox205.Image = ImageList9.Images(block(2))
            PictureBox203.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox242.Image = ImageList9.Images(block(2))
            PictureBox240.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox310.Image = ImageList9.Images(block(2))
            PictureBox308.Image = ImageList9.Images(block(2))
            PictureBox306.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(261)
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 23 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox87.Visible = True
            PictureBox85.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox123.Visible = True
            PictureBox121.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox176.Visible = True
            PictureBox174.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox209.Visible = True
            PictureBox207.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox242.Visible = True
            PictureBox240.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox275.Visible = True
            PictureBox273.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True

            PictureBox243.Visible = True
            PictureBox276.Visible = True
            PictureBox175.Visible = True
            PictureBox206.Visible = True



            PictureBox243.Image = ImageList9.Images(432)
            PictureBox276.Image = ImageList9.Images(432)
            PictureBox175.Image = ImageList9.Images(432)
            PictureBox206.Image = ImageList9.Images(432)

            PictureBox57.Image = ImageList9.Images(block(1))
            PictureBox56.Image = ImageList9.Images(block(1))
            PictureBox55.Image = ImageList9.Images(block(1))
            PictureBox54.Image = ImageList9.Images(block(1))
            PictureBox53.Image = ImageList9.Images(block(1))
            PictureBox52.Image = ImageList9.Images(block(1))
            PictureBox51.Image = ImageList9.Images(block(1))
            PictureBox50.Image = ImageList9.Images(block(1))
            PictureBox49.Image = ImageList9.Images(block(1))
            PictureBox90.Image = ImageList9.Images(block(1))
            PictureBox82.Image = ImageList9.Images(block(1))
            PictureBox146.Image = ImageList9.Images(block(1))
            PictureBox118.Image = ImageList9.Images(block(1))
            PictureBox179.Image = ImageList9.Images(block(1))
            PictureBox171.Image = ImageList9.Images(block(1))
            PictureBox212.Image = ImageList9.Images(block(1))
            PictureBox204.Image = ImageList9.Images(block(1))
            PictureBox245.Image = ImageList9.Images(block(1))
            PictureBox237.Image = ImageList9.Images(block(1))
            PictureBox278.Image = ImageList9.Images(block(1))
            PictureBox270.Image = ImageList9.Images(block(1))
            PictureBox311.Image = ImageList9.Images(block(1))
            PictureBox310.Image = ImageList9.Images(block(1))
            PictureBox309.Image = ImageList9.Images(block(1))
            PictureBox308.Image = ImageList9.Images(block(1))
            PictureBox307.Image = ImageList9.Images(block(1))
            PictureBox306.Image = ImageList9.Images(block(1))
            PictureBox305.Image = ImageList9.Images(block(1))
            PictureBox304.Image = ImageList9.Images(block(1))
            PictureBox303.Image = ImageList9.Images(block(1))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox123.Image = ImageList9.Images(block(2))
            PictureBox121.Image = ImageList9.Images(block(2))
            PictureBox119.Image = ImageList9.Images(block(2))
            PictureBox178.Image = ImageList9.Images(block(2))
            PictureBox176.Image = ImageList9.Images(block(2))
            PictureBox174.Image = ImageList9.Images(block(2))
            PictureBox172.Image = ImageList9.Images(block(2))
            PictureBox211.Image = ImageList9.Images(block(2))
            PictureBox209.Image = ImageList9.Images(block(2))
            PictureBox207.Image = ImageList9.Images(block(2))
            PictureBox205.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox242.Image = ImageList9.Images(block(2))
            PictureBox240.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(261)
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(140)
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 24 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox88.Visible = True
            PictureBox87.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox1.Visible = True
            PictureBox123.Visible = True
            PictureBox121.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True

            PictureBox240.Visible = True
            PictureBox122.Visible = True


            PictureBox122.Image = ImageList9.Images(119)
            PictureBox240.Image = libraier.Image

            PictureBox339.Image = ImageList9.Images(block(1))
            PictureBox338.Image = ImageList9.Images(block(1))
            PictureBox3.Image = ImageList9.Images(block(2))
            PictureBox4.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(2))
            PictureBox6.Image = ImageList9.Images(block(2))
            PictureBox7.Image = ImageList9.Images(block(2))
            PictureBox8.Image = ImageList9.Images(block(2))
            PictureBox9.Image = ImageList9.Images(block(2))
            PictureBox10.Image = ImageList9.Images(block(2))
            PictureBox11.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox23.Image = ImageList9.Images(block(2))
            PictureBox22.Image = ImageList9.Images(block(2))
            PictureBox21.Image = ImageList9.Images(block(2))
            PictureBox20.Image = ImageList9.Images(block(2))
            PictureBox19.Image = ImageList9.Images(block(2))
            PictureBox18.Image = ImageList9.Images(block(2))
            PictureBox17.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox56.Image = ImageList9.Images(block(2))
            PictureBox55.Image = ImageList9.Images(block(2))
            PictureBox54.Image = ImageList9.Images(block(2))
            PictureBox53.Image = ImageList9.Images(block(2))
            PictureBox52.Image = ImageList9.Images(block(2))
            PictureBox51.Image = ImageList9.Images(block(2))
            PictureBox50.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox88.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox86.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox84.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox1.Image = ImageList9.Images(block(2))
            PictureBox123.Image = ImageList9.Images(block(2))
            PictureBox121.Image = ImageList9.Images(block(2))
            PictureBox120.Image = ImageList9.Images(block(2))
            PictureBox119.Image = ImageList9.Images(block(2))
            PictureBox118.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(460)
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox278.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox276.Image = ImageList9.Images(block(3))
            PictureBox269.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(block(3))
            PictureBox311.Image = ImageList9.Images(block(3))
            PictureBox310.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox308.Image = ImageList9.Images(block(3))
            PictureBox303.Image = ImageList9.Images(block(3))
            PictureBox302.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox343.Image = ImageList9.Images(block(3))
            PictureBox342.Image = ImageList9.Images(block(3))
            PictureBox341.Image = ImageList9.Images(block(3))
            PictureBox340.Image = ImageList9.Images(block(3))
            PictureBox337.Image = ImageList9.Images(block(3))
            PictureBox336.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 11 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox173.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox206.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox104.Image = ImageList9.Images(block(2))
            PictureBox2.Image = ImageList9.Images(block(2))
            PictureBox3.Image = ImageList9.Images(block(2))
            PictureBox10.Image = ImageList9.Images(block(2))
            PictureBox11.Image = ImageList9.Images(block(2))
            PictureBox12.Image = ImageList9.Images(block(2))
            PictureBox13.Image = ImageList9.Images(block(2))
            PictureBox26.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox17.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox14.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox58.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox50.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox48.Image = ImageList9.Images(block(2))
            PictureBox47.Image = ImageList9.Images(block(2))
            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox81.Image = ImageList9.Images(block(2))
            PictureBox80.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox119.Image = ImageList9.Images(block(2))
            PictureBox118.Image = ImageList9.Images(block(2))
            PictureBox117.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox181.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox179.Image = ImageList9.Images(block(2))
            PictureBox172.Image = ImageList9.Images(block(2))
            PictureBox171.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox169.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox213.Image = ImageList9.Images(block(2))
            PictureBox212.Image = ImageList9.Images(block(2))
            PictureBox205.Image = ImageList9.Images(block(2))
            PictureBox204.Image = ImageList9.Images(block(2))
            PictureBox203.Image = ImageList9.Images(block(2))
            PictureBox202.Image = ImageList9.Images(block(2))
            PictureBox247.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox245.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox237.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox278.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox301.Image = ImageList9.Images(block(2))
            PictureBox346.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox344.Image = ImageList9.Images(block(2))
            PictureBox337.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox334.Image = ImageList9.Images(block(2))
            PictureBox379.Image = ImageList9.Images(block(2))
            PictureBox378.Image = ImageList9.Images(block(2))
            PictureBox377.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox369.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox23.Image = ImageList9.Images(block(3))
            PictureBox18.Image = ImageList9.Images(block(3))
            PictureBox56.Image = ImageList9.Images(block(3))
            PictureBox51.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox84.Image = ImageList9.Images(block(3))
            PictureBox145.Image = ImageList9.Images(block(3))
            PictureBox120.Image = ImageList9.Images(block(3))
            PictureBox178.Image = ImageList9.Images(block(3))
            PictureBox173.Image = ImageList9.Images(block(3))
            PictureBox211.Image = ImageList9.Images(block(3))
            PictureBox206.Image = ImageList9.Images(block(3))
            PictureBox244.Image = ImageList9.Images(block(3))
            PictureBox239.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox272.Image = ImageList9.Images(block(3))
            PictureBox310.Image = ImageList9.Images(block(3))
            PictureBox305.Image = ImageList9.Images(block(3))
            PictureBox343.Image = ImageList9.Images(block(3))
            PictureBox338.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
        ElseIf map = 12 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox241.Visible = True
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox274.Visible = True
            PictureBox273.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox104.Image = ImageList9.Images(block(2))
            PictureBox2.Image = ImageList9.Images(block(2))
            PictureBox3.Image = ImageList9.Images(block(2))
            PictureBox4.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(2))
            PictureBox12.Image = ImageList9.Images(block(2))
            PictureBox13.Image = ImageList9.Images(block(2))
            PictureBox26.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox23.Image = ImageList9.Images(block(2))
            PictureBox22.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox14.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox58.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox56.Image = ImageList9.Images(block(2))
            PictureBox48.Image = ImageList9.Images(block(2))
            PictureBox47.Image = ImageList9.Images(block(2))
            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox81.Image = ImageList9.Images(block(2))
            PictureBox80.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox119.Image = ImageList9.Images(block(2))
            PictureBox118.Image = ImageList9.Images(block(2))
            PictureBox117.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox181.Image = ImageList9.Images(block(2))
            PictureBox173.Image = ImageList9.Images(block(2))
            PictureBox172.Image = ImageList9.Images(block(2))
            PictureBox171.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox169.Image = ImageList9.Images(block(2))
            PictureBox208.Image = ImageList9.Images(block(2))
            PictureBox207.Image = ImageList9.Images(block(2))
            PictureBox206.Image = ImageList9.Images(block(2))
            PictureBox205.Image = ImageList9.Images(block(2))
            PictureBox204.Image = ImageList9.Images(block(2))
            PictureBox203.Image = ImageList9.Images(block(2))
            PictureBox202.Image = ImageList9.Images(block(2))
            PictureBox242.Image = ImageList9.Images(block(2))
            PictureBox241.Image = ImageList9.Images(block(2))
            PictureBox240.Image = ImageList9.Images(block(2))
            PictureBox239.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox237.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox274.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox272.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox308.Image = ImageList9.Images(block(2))
            PictureBox307.Image = ImageList9.Images(block(2))
            PictureBox306.Image = ImageList9.Images(block(2))
            PictureBox305.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox301.Image = ImageList9.Images(block(2))
            PictureBox341.Image = ImageList9.Images(block(2))
            PictureBox340.Image = ImageList9.Images(block(2))
            PictureBox339.Image = ImageList9.Images(block(2))
            PictureBox338.Image = ImageList9.Images(block(2))
            PictureBox337.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox334.Image = ImageList9.Images(block(2))
            PictureBox374.Image = ImageList9.Images(block(2))
            PictureBox373.Image = ImageList9.Images(block(2))
            PictureBox372.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox369.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox21.Image = ImageList9.Images(block(3))
            PictureBox16.Image = ImageList9.Images(block(3))
            PictureBox55.Image = ImageList9.Images(block(3))
            PictureBox49.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox83.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox120.Image = ImageList9.Images(block(3))
            PictureBox180.Image = ImageList9.Images(block(3))
            PictureBox175.Image = ImageList9.Images(block(3))
            PictureBox174.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox209.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox276.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox342.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
        ElseIf map = 13 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox83.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox122.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox207.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox238.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox275.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox305.Visible = True
            PictureBox302.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True

            If paper2_item.Visible = False Then
                PictureBox373.Visible = True
                PictureBox372.Visible = True
            End If

            PictureBox211.Visible = True


            PictureBox211.Image = ImageList9.Images(116)


            PictureBox104.Image = ImageList9.Images(block(1))
            PictureBox2.Image = ImageList9.Images(block(1))
            PictureBox3.Image = ImageList9.Images(block(1))
            PictureBox4.Image = ImageList9.Images(block(1))
            PictureBox5.Image = ImageList9.Images(block(1))
            PictureBox6.Image = ImageList9.Images(block(1))
            PictureBox12.Image = ImageList9.Images(block(1))
            PictureBox13.Image = ImageList9.Images(block(1))
            PictureBox22.Image = ImageList9.Images(block(1))
            PictureBox21.Image = ImageList9.Images(block(1))
            PictureBox15.Image = ImageList9.Images(block(1))
            PictureBox14.Image = ImageList9.Images(block(1))
            PictureBox47.Image = ImageList9.Images(block(1))
            PictureBox83.Image = ImageList9.Images(block(1))
            PictureBox122.Image = ImageList9.Images(block(1))
            PictureBox177.Image = ImageList9.Images(block(1))
            PictureBox207.Image = ImageList9.Images(block(1))
            PictureBox238.Image = ImageList9.Images(block(1))
            PictureBox275.Image = ImageList9.Images(block(1))
            PictureBox305.Image = ImageList9.Images(block(1))
            PictureBox26.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox23.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox58.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox56.Image = ImageList9.Images(block(2))
            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox181.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox179.Image = ImageList9.Images(block(2))
            PictureBox178.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox213.Image = ImageList9.Images(block(2))
            PictureBox212.Image = ImageList9.Images(block(2))
            PictureBox247.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox245.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox278.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox310.Image = ImageList9.Images(block(2))
            PictureBox346.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox344.Image = ImageList9.Images(block(2))
            PictureBox343.Image = ImageList9.Images(block(2))
            PictureBox379.Image = ImageList9.Images(block(2))
            PictureBox378.Image = ImageList9.Images(block(2))
            PictureBox377.Image = ImageList9.Images(block(2))
            PictureBox376.Image = ImageList9.Images(block(2))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox20.Image = ImageList9.Images(block(3))
            PictureBox16.Image = ImageList9.Images(block(3))
            PictureBox55.Image = ImageList9.Images(block(3))
            PictureBox54.Image = ImageList9.Images(block(3))
            PictureBox48.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox302.Image = ImageList9.Images(block(3))
            PictureBox336.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(466)
            PictureBox372.Image = ImageList9.Images(466)
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
        ElseIf map = 14 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox18.Visible = True
            PictureBox16.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox57.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox87.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox123.Visible = True
            PictureBox122.Visible = True
            PictureBox120.Visible = True
            PictureBox118.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox176.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox210.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox242.Visible = True
            PictureBox239.Visible = True
            PictureBox237.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox338.Visible = True
            PictureBox336.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox18.Image = ImageList9.Images(block(1))
            PictureBox16.Image = ImageList9.Images(block(1))
            PictureBox53.Image = ImageList9.Images(block(1))
            PictureBox122.Image = ImageList9.Images(block(1))
            PictureBox120.Image = ImageList9.Images(block(1))
            PictureBox118.Image = ImageList9.Images(block(1))
            PictureBox208.Image = ImageList9.Images(block(1))
            PictureBox239.Image = ImageList9.Images(block(1))
            PictureBox237.Image = ImageList9.Images(block(1))
            PictureBox338.Image = ImageList9.Images(block(1))
            PictureBox336.Image = ImageList9.Images(block(1))
            PictureBox21.Image = ImageList9.Images(block(2))
            PictureBox54.Image = ImageList9.Images(block(2))
            PictureBox51.Image = ImageList9.Images(block(2))
            PictureBox50.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox84.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox123.Image = ImageList9.Images(block(2))
            PictureBox176.Image = ImageList9.Images(block(2))
            PictureBox209.Image = ImageList9.Images(block(2))
            PictureBox242.Image = ImageList9.Images(block(2))
            PictureBox276.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox272.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox305.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox342.Image = ImageList9.Images(block(2))
            PictureBox341.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox22.Image = ImageList9.Images(460)
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox57.Image = waiter_ghost.Image
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(166)
            PictureBox210.Image = ImageList9.Images(80)
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(166)
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 15 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox173.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox206.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox23.Image = ImageList9.Images(block(2))
            PictureBox18.Image = ImageList9.Images(block(2))
            PictureBox178.Image = ImageList9.Images(block(2))
            PictureBox173.Image = ImageList9.Images(block(2))
            PictureBox310.Image = ImageList9.Images(block(2))
            PictureBox373.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox25.Image = ImageList9.Images(block(3))
            PictureBox24.Image = ImageList9.Images(block(3))
            PictureBox17.Image = ImageList9.Images(block(3))
            PictureBox16.Image = ImageList9.Images(block(3))
            PictureBox15.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox57.Image = ImageList9.Images(block(3))
            PictureBox56.Image = ImageList9.Images(block(3))
            PictureBox51.Image = ImageList9.Images(block(3))
            PictureBox50.Image = ImageList9.Images(block(3))
            PictureBox49.Image = ImageList9.Images(block(3))
            PictureBox48.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox91.Image = ImageList9.Images(block(3))
            PictureBox90.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox84.Image = ImageList9.Images(block(3))
            PictureBox83.Image = ImageList9.Images(block(3))
            PictureBox82.Image = ImageList9.Images(block(3))
            PictureBox81.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox145.Image = ImageList9.Images(block(3))
            PictureBox120.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox118.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox180.Image = ImageList9.Images(block(3))
            PictureBox179.Image = ImageList9.Images(block(3))
            PictureBox172.Image = ImageList9.Images(block(3))
            PictureBox171.Image = ImageList9.Images(block(3))
            PictureBox170.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox212.Image = ImageList9.Images(block(3))
            PictureBox211.Image = ImageList9.Images(block(3))
            PictureBox206.Image = ImageList9.Images(block(3))
            PictureBox205.Image = ImageList9.Images(block(3))
            PictureBox204.Image = ImageList9.Images(block(3))
            PictureBox203.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(block(3))
            PictureBox245.Image = ImageList9.Images(block(3))
            PictureBox244.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox278.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(block(3))
            PictureBox311.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox343.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 16 Or map = 17 Or map = 18 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox88.Visible = True
            PictureBox87.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox1.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox210.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox241.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox274.Visible = True
            PictureBox273.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True



            If map = 16 Then
                PictureBox87.Image = ImageList9.Images(460)
                PictureBox242.Image = ImageList9.Images(166)
                PictureBox241.Image = ImageList9.Images(81)

                PictureBox85.Visible = True
                PictureBox120.Visible = True
                PictureBox121.Visible = True
                PictureBox85.Image = ImageList9.Images(470)
                PictureBox121.Image = ImageList9.Images(472)
                PictureBox120.Image = ImageList9.Images(473)
            ElseIf map = 17 Then
                PictureBox87.Image = ImageList9.Images(460)
                PictureBox242.Image = ImageList9.Images(467)
                PictureBox241.Image = ImageList9.Images(81)

                PictureBox240.Visible = True
                PictureBox240.Image = ImageList9.Images(470)
                PictureBox122.Visible = True
                PictureBox122.Image = ImageList9.Images(470)
            ElseIf map = 18 Then
                PictureBox87.Image = ImageList9.Images(460)
                PictureBox242.Image = ImageList9.Images(467)
                PictureBox241.Image = ImageList9.Images(80)

                PictureBox209.Visible = True
                PictureBox209.Image = ImageList9.Images(470)
                PictureBox175.Visible = True
                PictureBox175.Image = ImageList9.Images(472)
                PictureBox174.Visible = True
                PictureBox174.Image = ImageList9.Images(473)
                PictureBox173.Visible = True
                PictureBox173.Image = ImageList9.Images(472)
            End If


            PictureBox172.Image = ImageList9.Images(110)
            PictureBox104.Image = ImageList9.Images(block(2))
            PictureBox2.Image = ImageList9.Images(block(2))
            PictureBox3.Image = ImageList9.Images(block(2))
            PictureBox4.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(2))
            PictureBox6.Image = ImageList9.Images(block(2))
            PictureBox7.Image = ImageList9.Images(block(2))
            PictureBox8.Image = ImageList9.Images(block(2))
            PictureBox9.Image = ImageList9.Images(block(2))
            PictureBox10.Image = ImageList9.Images(block(2))
            PictureBox11.Image = ImageList9.Images(block(2))
            PictureBox12.Image = ImageList9.Images(block(2))
            PictureBox13.Image = ImageList9.Images(block(2))
            PictureBox26.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox23.Image = ImageList9.Images(block(2))
            PictureBox22.Image = ImageList9.Images(block(2))
            PictureBox21.Image = ImageList9.Images(block(2))
            PictureBox20.Image = ImageList9.Images(block(2))
            PictureBox19.Image = ImageList9.Images(block(2))
            PictureBox18.Image = ImageList9.Images(block(2))
            PictureBox17.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox14.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox58.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox56.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox48.Image = ImageList9.Images(block(2))
            PictureBox47.Image = ImageList9.Images(block(2))
            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox81.Image = ImageList9.Images(block(2))
            PictureBox80.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox118.Image = ImageList9.Images(block(2))
            PictureBox117.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox181.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox179.Image = ImageList9.Images(block(2))
            PictureBox178.Image = ImageList9.Images(block(2))
            PictureBox171.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox169.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox213.Image = ImageList9.Images(block(2))
            PictureBox212.Image = ImageList9.Images(block(2))
            PictureBox211.Image = ImageList9.Images(block(2))
            PictureBox204.Image = ImageList9.Images(block(2))
            PictureBox203.Image = ImageList9.Images(block(2))
            PictureBox202.Image = ImageList9.Images(block(2))
            PictureBox247.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox245.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox237.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox278.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox310.Image = ImageList9.Images(block(2))
            PictureBox309.Image = ImageList9.Images(block(2))
            PictureBox308.Image = ImageList9.Images(block(2))
            PictureBox307.Image = ImageList9.Images(block(2))
            PictureBox306.Image = ImageList9.Images(block(2))
            PictureBox305.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox301.Image = ImageList9.Images(block(2))
            PictureBox346.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox344.Image = ImageList9.Images(block(2))
            PictureBox343.Image = ImageList9.Images(block(2))
            PictureBox342.Image = ImageList9.Images(block(2))
            PictureBox341.Image = ImageList9.Images(block(2))
            PictureBox340.Image = ImageList9.Images(block(2))
            PictureBox339.Image = ImageList9.Images(block(2))
            PictureBox338.Image = ImageList9.Images(block(2))
            PictureBox337.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox334.Image = ImageList9.Images(block(2))
            PictureBox379.Image = ImageList9.Images(block(2))
            PictureBox378.Image = ImageList9.Images(block(2))
            PictureBox377.Image = ImageList9.Images(block(2))
            PictureBox376.Image = ImageList9.Images(block(2))
            PictureBox375.Image = ImageList9.Images(block(2))
            PictureBox374.Image = ImageList9.Images(block(2))
            PictureBox373.Image = ImageList9.Images(block(2))
            PictureBox372.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox369.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox55.Image = ImageList9.Images(block(3))
            PictureBox54.Image = ImageList9.Images(block(3))
            PictureBox53.Image = ImageList9.Images(block(3))
            PictureBox52.Image = ImageList9.Images(block(3))
            PictureBox51.Image = ImageList9.Images(block(3))
            PictureBox50.Image = ImageList9.Images(block(3))
            PictureBox88.Image = ImageList9.Images(block(3))
            PictureBox83.Image = ImageList9.Images(block(3))
            PictureBox1.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox177.Image = ImageList9.Images(block(3))
            PictureBox210.Image = ImageList9.Images(block(3))
            PictureBox205.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox238.Image = ImageList9.Images(block(3))
            PictureBox276.Image = ImageList9.Images(block(3))
            PictureBox275.Image = ImageList9.Images(block(3))
            PictureBox274.Image = ImageList9.Images(block(3))
            PictureBox273.Image = ImageList9.Images(block(3))
            PictureBox272.Image = ImageList9.Images(block(3))
            PictureBox271.Image = ImageList9.Images(block(3))
        ElseIf map = 19 Or map = 20 Then
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox179.Visible = True
            PictureBox173.Visible = True
            PictureBox172.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox206.Visible = True
            PictureBox205.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox274.Visible = True
            PictureBox273.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            If have_sword = False And map = 19 Then
                PictureBox209.Visible = True
            End If
            If map = 19 Then
                PictureBox177.Image = ImageList9.Images(473)
                PictureBox174.Image = ImageList9.Images(465)
                PictureBox176.Image = ImageList9.Images(472)
                PictureBox210.Image = ImageList9.Images(470)
                PictureBox240.Image = ImageList9.Images(460)

                PictureBox177.Visible = True
                PictureBox174.Visible = True
                PictureBox176.Visible = True
                PictureBox210.Visible = True
                If seen_the_woman = True Then
                    PictureBox174.Visible = False
                End If
            ElseIf map = 20 Then
                PictureBox208.Visible = True
                PictureBox208.Image = ImageList9.Images(470)
                PictureBox176.Visible = True
                PictureBox176.Image = ImageList9.Images(470)
                PictureBox240.Image = ImageList9.Images(302)
            End If

            PictureBox178.Visible = True

            PictureBox178.Image = ImageList9.Images(110)



            PictureBox86.Image = ImageList9.Images(167)
            PictureBox85.Image = ImageList9.Images(81)
            PictureBox209.Image = ImageList9.Images(471)
            PictureBox24.Image = ImageList9.Images(block(3))
            PictureBox23.Image = ImageList9.Images(block(3))
            PictureBox22.Image = ImageList9.Images(block(3))
            PictureBox21.Image = ImageList9.Images(block(3))
            PictureBox20.Image = ImageList9.Images(block(3))
            PictureBox19.Image = ImageList9.Images(block(3))
            PictureBox18.Image = ImageList9.Images(block(3))
            PictureBox17.Image = ImageList9.Images(block(3))
            PictureBox57.Image = ImageList9.Images(block(3))
            PictureBox56.Image = ImageList9.Images(block(3))
            PictureBox55.Image = ImageList9.Images(block(3))
            PictureBox54.Image = ImageList9.Images(block(3))
            PictureBox53.Image = ImageList9.Images(block(3))
            PictureBox52.Image = ImageList9.Images(block(3))
            PictureBox51.Image = ImageList9.Images(block(3))
            PictureBox50.Image = ImageList9.Images(block(3))
            PictureBox90.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox84.Image = ImageList9.Images(block(3))
            PictureBox83.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox145.Image = ImageList9.Images(block(3))
            PictureBox120.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox179.Image = ImageList9.Images(block(3))
            PictureBox173.Image = ImageList9.Images(block(3))
            PictureBox172.Image = ImageList9.Images(block(3))
            PictureBox212.Image = ImageList9.Images(block(3))
            PictureBox211.Image = ImageList9.Images(block(3))
            PictureBox206.Image = ImageList9.Images(block(3))
            PictureBox205.Image = ImageList9.Images(block(3))
            PictureBox245.Image = ImageList9.Images(block(3))
            PictureBox244.Image = ImageList9.Images(block(3))
            PictureBox239.Image = ImageList9.Images(block(3))
            PictureBox238.Image = ImageList9.Images(block(3))
            PictureBox278.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox276.Image = ImageList9.Images(block(3))
            PictureBox275.Image = ImageList9.Images(block(3))
            PictureBox274.Image = ImageList9.Images(block(3))
            PictureBox273.Image = ImageList9.Images(block(3))
            PictureBox272.Image = ImageList9.Images(block(3))
            PictureBox271.Image = ImageList9.Images(block(3))
            PictureBox311.Image = ImageList9.Images(block(3))
            PictureBox310.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox308.Image = ImageList9.Images(block(3))
            PictureBox307.Image = ImageList9.Images(block(3))
            PictureBox306.Image = ImageList9.Images(block(3))
            PictureBox305.Image = ImageList9.Images(block(3))
            PictureBox304.Image = ImageList9.Images(block(3))
        ElseIf map = 30 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox118.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox241.Visible = True
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox276.Visible = True
            PictureBox272.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox309.Visible = True
            PictureBox307.Visible = True
            PictureBox305.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True

            PictureBox52.Visible = True
            PictureBox123.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True

            PictureBox52.Image = ImageList9.Images(470)
            PictureBox123.Image = ImageList9.Images(470)
            PictureBox208.Image = ImageList9.Images(473)
            PictureBox207.Image = ImageList9.Images(472)
            PictureBox206.Image = ImageList9.Images(473)



            PictureBox50.Image = ImageList9.Images(80)
            PictureBox89.Image = ImageList9.Images(16)
            PictureBox83.Image = ImageList9.Images(141)
            PictureBox145.Image = ImageList9.Images(block(1))
            PictureBox178.Image = ImageList9.Images(460)
            PictureBox172.Image = ImageList9.Images(147)
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox58.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox48.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox81.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox118.Image = ImageList9.Images(block(2))
            PictureBox117.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox179.Image = ImageList9.Images(block(2))
            PictureBox171.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox213.Image = ImageList9.Images(block(2))
            PictureBox212.Image = ImageList9.Images(block(2))
            PictureBox204.Image = ImageList9.Images(block(2))
            PictureBox203.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox245.Image = ImageList9.Images(block(2))
            PictureBox237.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox278.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox344.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(312)
            PictureBox242.Image = ImageList9.Images(312)
            PictureBox241.Image = ImageList9.Images(312)
            PictureBox240.Image = ImageList9.Images(312)
            PictureBox239.Image = ImageList9.Images(312)
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox276.Image = ImageList9.Images(312)
            PictureBox272.Image = ImageList9.Images(312)
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(312)
            PictureBox307.Image = ImageList9.Images(4)
            PictureBox305.Image = ImageList9.Images(312)
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 31 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox1.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox176.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True

            PictureBox308.Visible = True

            PictureBox308.Image = ImageList9.Images(107)



            PictureBox202.Image = ImageList9.Images(block(1))
            PictureBox235.Image = ImageList9.Images(block(1))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox310.Image = ImageList9.Images(block(2))
            PictureBox309.Image = ImageList9.Images(block(2))
            PictureBox307.Image = ImageList9.Images(block(2))
            PictureBox306.Image = ImageList9.Images(block(2))
            PictureBox305.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox344.Image = ImageList9.Images(block(2))
            PictureBox343.Image = ImageList9.Images(block(2))
            PictureBox342.Image = ImageList9.Images(block(2))
            PictureBox341.Image = ImageList9.Images(418)
            PictureBox340.Image = ImageList9.Images(block(2))
            PictureBox339.Image = ImageList9.Images(block(2))
            PictureBox338.Image = ImageList9.Images(block(2))
            PictureBox337.Image = ImageList9.Images(block(2))
            PictureBox378.Image = ImageList9.Images(block(2))
            PictureBox377.Image = ImageList9.Images(block(2))
            PictureBox376.Image = ImageList9.Images(block(2))
            PictureBox375.Image = ImageList9.Images(block(2))
            PictureBox374.Image = ImageList9.Images(block(2))
            PictureBox373.Image = ImageList9.Images(block(2))
            PictureBox372.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox19.Image = ImageList9.Images(block(3))
            PictureBox18.Image = ImageList9.Images(block(3))
            PictureBox17.Image = ImageList9.Images(block(3))
            PictureBox16.Image = ImageList9.Images(block(3))
            PictureBox15.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox50.Image = ImageList9.Images(block(3))
            PictureBox49.Image = ImageList9.Images(block(3))
            PictureBox48.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox81.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox145.Image = ImageList9.Images(block(3))
            PictureBox1.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox180.Image = ImageList9.Images(block(3))
            PictureBox179.Image = ImageList9.Images(block(3))
            PictureBox178.Image = ImageList9.Images(block(3))
            PictureBox177.Image = ImageList9.Images(block(3))
            PictureBox176.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 32 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox88.Visible = True
            PictureBox87.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox176.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox312.Image = ImageList9.Images(block(1))
            PictureBox311.Image = ImageList9.Images(block(1))
            PictureBox310.Image = ImageList9.Images(block(1))
            PictureBox309.Image = ImageList9.Images(block(1))
            PictureBox308.Image = ImageList9.Images(block(1))
            PictureBox306.Image = ImageList9.Images(block(1))
            PictureBox305.Image = ImageList9.Images(block(1))
            PictureBox304.Image = ImageList9.Images(block(1))
            PictureBox303.Image = ImageList9.Images(block(1))
            PictureBox302.Image = ImageList9.Images(block(1))
            PictureBox345.Image = ImageList9.Images(block(1))
            PictureBox344.Image = ImageList9.Images(block(1))
            PictureBox343.Image = ImageList9.Images(block(1))
            PictureBox342.Image = ImageList9.Images(block(1))
            PictureBox341.Image = ImageList9.Images(block(1))
            PictureBox340.Image = ImageList9.Images(block(1))
            PictureBox339.Image = ImageList9.Images(block(1))
            PictureBox338.Image = ImageList9.Images(block(1))
            PictureBox337.Image = ImageList9.Images(block(1))
            PictureBox336.Image = ImageList9.Images(block(1))
            PictureBox335.Image = ImageList9.Images(block(1))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox23.Image = ImageList9.Images(block(2))
            PictureBox22.Image = ImageList9.Images(block(2))
            PictureBox21.Image = ImageList9.Images(block(2))
            PictureBox19.Image = ImageList9.Images(block(2))
            PictureBox18.Image = ImageList9.Images(block(2))
            PictureBox17.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox88.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox84.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox81.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox179.Image = ImageList9.Images(block(2))
            PictureBox178.Image = ImageList9.Images(block(2))
            PictureBox177.Image = ImageList9.Images(block(2))
            PictureBox176.Image = ImageList9.Images(block(2))
            PictureBox174.Image = ImageList9.Images(block(2))
            PictureBox173.Image = ImageList9.Images(block(2))
            PictureBox172.Image = ImageList9.Images(block(2))
            PictureBox171.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox245.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox243.Image = ImageList9.Images(block(2))
            PictureBox242.Image = ImageList9.Images(block(2))
            PictureBox240.Image = ImageList9.Images(block(2))
            PictureBox239.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox237.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox346.Image = ImageList9.Images(460)
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox307.Image = goddad.Image
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        ElseIf map = 33 Then
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox88.Visible = True
            PictureBox87.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox1.Visible = True
            PictureBox123.Visible = True
            PictureBox122.Visible = True
            PictureBox121.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox241.Visible = True
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox274.Visible = True
            PictureBox273.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True




            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox88.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox86.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox84.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox81.Image = ImageList9.Images(block(2))
            PictureBox80.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox278.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox276.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox274.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox272.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox145.Image = ImageList9.Images(block(3))
            PictureBox1.Image = ImageList9.Images(block(3))
            PictureBox123.Image = ImageList9.Images(block(3))
            PictureBox122.Image = ImageList9.Images(block(3))
            PictureBox121.Image = ImageList9.Images(block(3))
            PictureBox120.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox118.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(block(3))
            PictureBox245.Image = ImageList9.Images(block(3))
            PictureBox244.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox242.Image = ImageList9.Images(block(3))
            PictureBox241.Image = ImageList9.Images(block(3))
            PictureBox240.Image = ImageList9.Images(block(3))
            PictureBox239.Image = ImageList9.Images(block(3))
            PictureBox238.Image = ImageList9.Images(block(3))
            PictureBox237.Image = ImageList9.Images(block(3))
            PictureBox236.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
        ElseIf map = 100 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
            story_end()
        ElseIf map = 40 Then
            PictureBox2.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox10.Visible = True
            PictureBox12.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox18.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox54.Visible = True
            PictureBox48.Visible = True
            PictureBox92.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox87.Visible = True
            PictureBox86.Visible = True
            PictureBox82.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox123.Visible = True
            PictureBox119.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox179.Visible = True
            PictureBox176.Visible = True
            PictureBox174.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox209.Visible = True
            PictureBox204.Visible = True
            PictureBox202.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox239.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox275.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox308.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox341.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox374.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox26.Image = ImageList9.Images(343)
            PictureBox25.Image = ImageList9.Images(343)
            PictureBox24.Image = ImageList9.Images(343)
            PictureBox23.Image = ImageList9.Images(343)
            PictureBox22.Image = ImageList9.Images(343)
            PictureBox21.Image = ImageList9.Images(492)
            PictureBox54.Image = ImageList9.Images(block(1))
            PictureBox87.Image = ImageList9.Images(block(1))
            PictureBox123.Image = ImageList9.Images(block(1))
            PictureBox176.Image = ImageList9.Images(block(1))
            PictureBox209.Image = ImageList9.Images(block(1))
            PictureBox242.Image = ImageList9.Images(block(1))
            PictureBox275.Image = ImageList9.Images(block(1))
            PictureBox308.Image = ImageList9.Images(block(1))
            PictureBox341.Image = ImageList9.Images(block(1))
            PictureBox374.Image = ImageList9.Images(block(1))
            PictureBox2.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(2))
            PictureBox6.Image = ImageList9.Images(block(2))
            PictureBox7.Image = ImageList9.Images(block(2))
            PictureBox8.Image = ImageList9.Images(block(2))
            PictureBox10.Image = ImageList9.Images(block(2))
            PictureBox12.Image = ImageList9.Images(block(2))
            PictureBox18.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox15.Image = ImageList9.Images(block(2))
            PictureBox14.Image = ImageList9.Images(block(2))
            PictureBox48.Image = ImageList9.Images(block(2))
            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox86.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox80.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox119.Image = ImageList9.Images(block(2))
            PictureBox117.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox179.Image = ImageList9.Images(block(2))
            PictureBox174.Image = ImageList9.Images(block(2))
            PictureBox171.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox213.Image = ImageList9.Images(block(2))
            PictureBox204.Image = ImageList9.Images(block(2))
            PictureBox202.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox245.Image = ImageList9.Images(block(2))
            PictureBox243.Image = ImageList9.Images(block(2))
            PictureBox239.Image = ImageList9.Images(block(2))
            PictureBox236.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox301.Image = ImageList9.Images(block(2))
            PictureBox346.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox344.Image = ImageList9.Images(block(2))
            PictureBox338.Image = ImageList9.Images(block(2))
            PictureBox337.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox334.Image = ImageList9.Images(block(2))
            PictureBox379.Image = ImageList9.Images(block(2))
            PictureBox378.Image = ImageList9.Images(block(2))
            PictureBox377.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox369.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox271.Image = ImageList9.Images(block(3))
            PictureBox270.Image = ImageList9.Images(block(3))
            PictureBox269.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox311.Image = ImageList9.Images(block(3))
            PictureBox305.Image = ImageList9.Images(block(3))
            PictureBox343.Image = ImageList9.Images(block(3))
            PictureBox339.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
        ElseIf map = 41 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
            PictureBox12.Visible = True
            PictureBox13.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox14.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox88.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox243.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox346.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox379.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True

            PictureBox209.Visible = True

            PictureBox209.Image = ImageList9.Images(468)




            PictureBox204.Image = ImageList9.Images(block(1))
            PictureBox203.Image = ImageList9.Images(block(1))
            PictureBox202.Image = ImageList9.Images(block(1))
            PictureBox20.Image = ImageList9.Images(block(2))
            PictureBox56.Image = ImageList9.Images(block(2))
            PictureBox54.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox178.Image = ImageList9.Images(block(2))
            PictureBox212.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox308.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox3.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox11.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox13.Image = ImageList9.Images(block(3))
            PictureBox26.Image = ImageList9.Images(block(3))
            PictureBox25.Image = ImageList9.Images(block(3))
            PictureBox24.Image = ImageList9.Images(block(3))
            PictureBox23.Image = ImageList9.Images(block(3))
            PictureBox22.Image = ImageList9.Images(block(3))
            PictureBox21.Image = ImageList9.Images(block(3))
            PictureBox19.Image = ImageList9.Images(block(3))
            PictureBox18.Image = ImageList9.Images(block(3))
            PictureBox17.Image = ImageList9.Images(block(3))
            PictureBox16.Image = ImageList9.Images(block(3))
            PictureBox15.Image = ImageList9.Images(block(3))
            PictureBox14.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox57.Image = ImageList9.Images(block(3))
            PictureBox55.Image = ImageList9.Images(block(3))
            PictureBox53.Image = ImageList9.Images(block(3))
            PictureBox52.Image = ImageList9.Images(block(3))
            PictureBox49.Image = ImageList9.Images(block(3))
            PictureBox48.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox91.Image = ImageList9.Images(block(3))
            PictureBox90.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox88.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox180.Image = ImageList9.Images(block(3))
            PictureBox179.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox211.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(block(3))
            PictureBox245.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox278.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox276.Image = ImageList9.Images(block(3))
            PictureBox268.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(block(3))
            PictureBox311.Image = ImageList9.Images(block(3))
            PictureBox310.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox302.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox343.Image = ImageList9.Images(block(3))
            PictureBox342.Image = ImageList9.Images(block(3))
            PictureBox341.Image = ImageList9.Images(block(3))
            PictureBox340.Image = ImageList9.Images(block(3))
            PictureBox339.Image = ImageList9.Images(block(3))
            PictureBox338.Image = ImageList9.Images(block(3))
            PictureBox337.Image = ImageList9.Images(block(3))
            PictureBox336.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
            PictureBox334.Image = ImageList9.Images(block(3))
            PictureBox379.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox374.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
            PictureBox370.Image = ImageList9.Images(block(3))
            PictureBox369.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox367.Image = ImageList9.Images(block(3))
        End If
    End Sub
    '死亡
    Private Sub ending2()
        If endy = 0 Then
            If death1 >= 1 Then
                endy = 2
                PictureBox401.Image = ImageList3.Images(4)
                PictureBox401.Top -= 50
                Timer5.Enabled = True
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\sud_3.wav", AudioPlayMode.Background)
                player.close()
            End If
        End If
    End Sub
    '物件移到最上層
    Private Sub up_to_form()
        PictureBox124.BringToFront()
        place.BringToFront()
        pap_.BringToFront()
        pap_text.BringToFront()
        hit.BringToFront()

        paper3.BringToFront()
        paper4.BringToFront()
        paper5.BringToFront()
        paper6.BringToFront()
        paper7.BringToFront()
        sword.BringToFront()
        Jimmy_bar.BringToFront()
        cd1_item.BringToFront()
        paper2_item.BringToFront()
        killed.BringToFront()
        yes.BringToFront()
        no.BringToFront()
        yes_pic.BringToFront()
        pic_no.BringToFront()

    End Sub
    '地點
    Private Sub where()
        If map = 0 Then
            place.Text = "鎮長辦公室"
        ElseIf map = 1 Then
            place.Text = "道路"
        ElseIf map = 2 Then
            place.Text = "分岔道路"
        ElseIf map = 21 Then
            place.Text = "圖書館外"
        ElseIf map = 22 Then
            place.Text = "圖書館"
        ElseIf map = 23 Then
            place.Text = "圖書館2F"
        ElseIf map = 24 Then
            place.Text = "圖書館外"
        ElseIf map = 11 Then
            place.Text = "道路"
        ElseIf map = 12 Then
            place.Text = "道路"
        ElseIf map = 13 Then
            place.Text = "酒館外"
        ElseIf map = 14 Then
            place.Text = "酒館"
        ElseIf map = 15 Then
            place.Text = "街道"
        ElseIf map = 16 And map = 17 And map = 18 And map = 19 And map = 20 Then
            place.Text = "工人住宅"
        ElseIf map = 30 Then
            place.Text = "辦公室"
        ElseIf map = 31 Then
            place.Text = "教堂外"
        ElseIf map = 32 Then
            place.Text = "教堂"
        ElseIf map = 33 Then
            place.Text = "道路"
        ElseIf map = 100 Then
            If end_type = 0 Then
                place.Text = "結局1-逃出"
            ElseIf end_type = 1 Then
                place.Text = "結局2-迷惘"
            End If
        ElseIf map = 40 Then
            place.Text = "鐵道"
        ElseIf map = 41 Then
            place.Text = "廢棄礦坑"
        End If
    End Sub
    'cd1
    Private Sub cd1_item_Click(sender As System.Object, e As System.EventArgs) Handles cd1_item.Click
        If endy = 0 Then
            item = 1
            talk = 1
            chat_events()
        End If

    End Sub
    'ghost_move
    Private Sub ghost_move_Tick(sender As System.Object, e As System.EventArgs) Handles ghost_move.Tick
        ghost.Left += 7
        If PictureBox401.Left <= ghost.Left + ghost.Width Then
            death1 = 1
            ending2()
            ghost_move.Enabled = False
            ghost.Visible = False
        End If
    End Sub
    'paper2
    Private Sub paper2_item_Click(sender As System.Object, e As System.EventArgs) Handles paper2_item.Click
        If paper2 = True And endy = 0 Then
            item = 2
            talk = 100
            chat_events()
        End If
    End Sub
    'waiting
    Private Sub waiting_Tick(sender As System.Object, e As System.EventArgs) Handles waiting.Tick
        i += 1
        If count = False Then
            killed.Visible = False
            waiting.Enabled = False
            waiter_talk = False
        End If
        If i < 15 And xy1 <> 1006 And count = True Then
            killed.Left = 0
            killed.Top = 0
            killed.Visible = True
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\sud_3.wav", AudioPlayMode.Background)
            count = False
        End If
        If i > 14 And count = True Then
            killed.Visible = False
            waiting.Enabled = False
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\door_close.wav", AudioPlayMode.Background)
        End If
    End Sub
    'paper3
    Private Sub paper3_Click(sender As System.Object, e As System.EventArgs) Handles paper3.Click
        If endy = 0 Then
            item = 3
            talk = 1
            chat_events()
        End If

    End Sub
    'paper4
    Private Sub paper4_Click(sender As System.Object, e As System.EventArgs) Handles paper4.Click
        If endy = 0 Then
            item = 4
            talk = 1
            chat_events()
        End If

    End Sub
    'paper5
    Private Sub paper5_Click(sender As System.Object, e As System.EventArgs) Handles paper5.Click
        If endy = 0 Then
            item = 5
            talk = 1
            chat_events()
        End If

    End Sub
    'scream
    Private Sub scream_Tick(sender As System.Object, e As System.EventArgs) Handles scream.Tick
        i += 1
        If i = 1 Then
            PictureBox174.Visible = False
            seen_the_woman = True
            scream.Enabled = False
        End If
    End Sub
    'web sound
    Private Sub sounds()
        Randomize()
        i = Int((3 - 0 + 1) * Rnd() + 0)
        If i = 0 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\cloth1.wav", AudioPlayMode.Background)
        ElseIf i = 1 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\cloth2.wav", AudioPlayMode.Background)
        ElseIf i = 2 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\cloth3.wav", AudioPlayMode.Background)
        End If
        i = 0
    End Sub
    'paper6
    Private Sub paper6_Click(sender As System.Object, e As System.EventArgs) Handles paper6.Click
        If endy = 0 Then
            item = 6
            talk = 1
            chat_events()
        End If
    End Sub
    'paper7
    Private Sub paper7_Click(sender As System.Object, e As System.EventArgs) Handles paper7.Click
        If endy = 0 Then
            item = 7
            talk = 1
            chat_events()
        End If
    End Sub
    '刀
    Private Sub sword_Click(sender As System.Object, e As System.EventArgs) Handles sword.Click
        If endy = 0 Then
            item = 8
            talk = 1
            chat_events()
        End If
    End Sub
    '鐵撬
    Private Sub Jimmy_bar_Click(sender As System.Object, e As System.EventArgs) Handles Jimmy_bar.Click
        If endy = 0 Then
            item = 9
            talk = 1
            chat_events()
        End If
    End Sub
    '結局
    Private Sub story_end()
        Dim F As New Font("微軟正黑體", 20, FontStyle.Bold)
        If end_type = 0 Then
            pap_text.BackColor = Color.White
            pap_text.Font = F
            pap_text.Visible = True
            pap_text.BringToFront()
            pap_text.Text = "結局1 --- 逃出" & vbCrLf & vbCrLf & "你選擇放棄繼續追查，並成功的逃出" & vbCrLf & "這個小鎮，到安全的地方去了" & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & "(按Esc進入選單)"
        ElseIf end_type = 1 Then
            pap_text.BackColor = Color.White
            pap_text.Font = F
            pap_text.Visible = True
            pap_text.BringToFront()
            pap_text.Text = "結局2 --- 迷惘" & vbCrLf & vbCrLf & "在離開中的猶豫，讓你和這個小鎮的" & vbCrLf & "時間一起凍結了。" & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & "(按Esc進入選單)"
        ElseIf end_type = 2 Then
            pap_text.BackColor = Color.White
            pap_text.Font = F
            pap_text.Visible = True
            pap_text.BringToFront()
            pap_text.Text = "結局3 --- 真相" & vbCrLf & vbCrLf & "在2年的復活過程中，他早已喪失了" & vbCrLf & "靈魂，所謂的復活其實只是寄生蟲對" & vbCrLf & "屍體的控制，他只是種肉體復活的" & vbCrLf & "假象。而月圓之時只是少數案例的巧" & vbCrLf & "合，而被民間寫入藥方，其實只要時" & vbCrLf & "候到了，消化完藥草的寄生蟲便會" & vbCrLf & "開始工作。" & vbCrLf & "(按Esc進入選單)"
        ElseIf end_type = 3 Then
            pap_text.BackColor = Color.White
            pap_text.Font = F
            pap_text.Visible = True
            pap_text.BringToFront()
            pap_text.Text = "結局4 --- 死亡" & vbCrLf & vbCrLf & "你也成了他血液的一部份了" & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & "(按Esc進入選單)"

        End If
    End Sub
    'yes
    Private Sub yes_Click(sender As System.Object, e As System.EventArgs) Handles yes.Click
        true_die()
    End Sub
    'no
    Private Sub no_Click(sender As System.Object, e As System.EventArgs) Handles no.Click
        yes.Visible = False
        no.Visible = False
        chat_end()
        item = 0
        end_type = 2
        i = 0
        pic_no.Visible = True
        Timer7.Enabled = True
    End Sub
    'true_die
    Private Sub true_die()
        yes.Visible = False
        no.Visible = False
        chat_end()
        item = 0
        end_type = 3
        i = 0
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\story_mode\sud_3.wav", AudioPlayMode.Background)
        yes_pic.Visible = True
        Timer7.Enabled = True
    End Sub
    'timer_7
    Private Sub Timer7_Tick(sender As System.Object, e As System.EventArgs) Handles Timer7.Tick
        i += 1
        If i > 0 Then
            If end_type = 2 Then
                pic_no.Visible = False
            ElseIf end_type = 3 Then
                yes_pic.Visible = False
            End If
            If i > 1 Then
                x1 = 100
                y1 = 100
                map_clear()
                map = 100
                map_change()
                Timer7.Enabled = False
            End If
        End If
    End Sub
End Class

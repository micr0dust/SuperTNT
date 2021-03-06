Public Class Form8
    Dim x1, y1, xy1 As Integer                  '1P 座標
    Dim x2, y2, xy2 As Integer                  '2P 座標
    Dim tnt1xy As Integer                       '1P 2顆TNT xy
    Dim tnt2xy As Integer                       '2P 2顆TNT xy
    Dim tnt1_tick1, tnt1_tick2 As Integer       '1P 2顆TNT的Timer
    Dim tnt2_tick1, tnt2_tick2 As Integer       '2P 2顆TNT的Timer
    Dim tnt11, tnt12 As Integer                 '1P 2顆TNT的暫存座標
    Dim tnt21, tnt22 As Integer                 '2P 2顆TNT的暫存座標
    Dim un1, udlr1 As Integer                   '1P 走路方向判斷 (udlr = up / down / left / right)
    Dim un2, udlr2 As Integer                   '2P 走路方向判斷 (udlr = up / down / left / right)
    Dim f3, f12, esc As Integer                  '開發者模式啟動(F12需權限)
    Dim endcount, score1, score2 As Integer     '分數計算
    Dim death1, death2 As Integer               ' 1P / 2P 死亡 (Tag)
    Dim exer1, exer2 As Integer                 ' 1P / 2P 放TNT能穿越1次TNT (Tag)
    Dim endy As Integer                         '結束(Tag)
    Dim op As Integer                           '密碼暫存
    Dim m, s As Integer                         '時間
    Dim block(4) As Integer                     '方塊樣式

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

    '------------------------------------------2P-控制--------------------------------------------------------------------------------
    '2P角色控制(上)(function)
    Private Sub Character2_up()
        If ch2 = 0 Then
            PictureBox400.Image = ImageList1.Images(0)
        ElseIf ch2 = 1 Then
            PictureBox400.Image = ImageList3.Images(0)
        ElseIf ch2 = 2 Then
            PictureBox400.Image = ImageList4.Images(0)
        ElseIf ch2 = 3 Then
            PictureBox400.Image = RR1.Image
        ElseIf ch2 = 4 Then
            PictureBox400.Image = mach1.Image
        ElseIf ch2 = 5 Then
            PictureBox400.Image = sp1.Image
        ElseIf ch2 = 6 Then
            PictureBox400.Image = yfs1.Image
        End If
        PictureBox400.Top -= PictureBox104.Height
        y2 -= 1
        udlr2 = 0
        un2 = 1
        If PictureBox400.Top < 0 Then
            back2()
        End If
        Call detection2()
        If exer2 > 0 Then
            exer2 -= 1
        End If
    End Sub
    '2P角色控制(下)(function)
    Private Sub Character2_down()
        If ch2 = 0 Then
            PictureBox400.Image = ImageList1.Images(1)
        ElseIf ch2 = 1 Then
            PictureBox400.Image = ImageList3.Images(1)
        ElseIf ch2 = 2 Then
            PictureBox400.Image = ImageList4.Images(1)
        ElseIf ch2 = 3 Then
            PictureBox400.Image = RR2.Image
        ElseIf ch2 = 4 Then
            PictureBox400.Image = mach2.Image
        ElseIf ch2 = 5 Then
            PictureBox400.Image = sp2.Image
        ElseIf ch2 = 6 Then
            PictureBox400.Image = yfs2.Image
        End If
        PictureBox400.Top += PictureBox104.Height
        y2 += 1
        udlr2 = 0
        un2 = -1
        Call detection2()
        If exer2 > 0 Then
            exer2 -= 1
        End If
    End Sub
    '2P角色控制(左)(function)
    Private Sub Character2_left()
        If ch2 = 0 Then
            PictureBox400.Image = ImageList1.Images(2)
        ElseIf ch2 = 1 Then
            PictureBox400.Image = ImageList3.Images(2)
        ElseIf ch2 = 2 Then
            PictureBox400.Image = ImageList4.Images(2)
        ElseIf ch2 = 3 Then
            PictureBox400.Image = RR3.Image
        ElseIf ch2 = 4 Then
            PictureBox400.Image = mach3.Image
        ElseIf ch2 = 5 Then
            PictureBox400.Image = sp3.Image
        ElseIf ch2 = 6 Then
            PictureBox400.Image = yfs3.Image
        End If
        PictureBox400.Left -= PictureBox104.Width
        x2 -= 1
        udlr2 = 1
        un2 = 1
        If PictureBox400.Left < 0 Then
            back2()
        End If
        Call detection2()
        If exer2 > 0 Then
            exer2 -= 1
        End If
    End Sub
    '2P角色控制(右)(function)
    Private Sub Character2_right()
        If ch2 = 0 Then
            PictureBox400.Image = ImageList1.Images(3)
        ElseIf ch2 = 1 Then
            PictureBox400.Image = ImageList3.Images(3)
        ElseIf ch2 = 2 Then
            PictureBox400.Image = ImageList4.Images(3)
        ElseIf ch2 = 3 Then
            PictureBox400.Image = RR4.Image
        ElseIf ch2 = 4 Then
            PictureBox400.Image = mach4.Image
        ElseIf ch2 = 5 Then
            PictureBox400.Image = sp4.Image
        ElseIf ch2 = 6 Then
            PictureBox400.Image = yfs4.Image
        End If
        PictureBox400.Left += PictureBox104.Width
        x2 += 1
        If PictureBox400.Left > Me.Width - 20 - PictureBox400.Width Then
            back2()
        End If
        udlr2 = 1
        un2 = -1
        Call detection2()
        If exer2 > 0 Then
            exer2 -= 1
        End If
    End Sub
    '---------------------------------------------------------------------------------------------------------------------------------

    '地圖分數上限
    Private Sub map_point()
        If map = 0 Then
            If endcount = 18 Then
                Call ending1()
            End If
        ElseIf map = 1 Then
            If endcount = 19 Then
                Call ending1()
            End If
        ElseIf map = 2 Then
            If endcount = 12 Then
                Call ending1()
            End If
        ElseIf map = 3 Then
            If endcount = 29 Then
                Call ending1()
            End If
        ElseIf map = 4 Then
            If endcount = 80 Then
                Call ending1()
            End If
        ElseIf map = 5 Then
            If endcount = 16 Then
                Call ending1()
            End If
        ElseIf map = 6 Then
            If endcount = 33 Then
                Call ending1()
            End If
        End If
    End Sub
    '1P爆炸特效(function)
    Private Sub ex1()
        Dim i As Integer
        Randomize()
        i = Int((3 - 0 + 1) * Rnd() + 0)
        If i = 0 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode1.wav", AudioPlayMode.Background)
        ElseIf i = 1 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode2.wav", AudioPlayMode.Background)
        ElseIf i = 2 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode3.wav", AudioPlayMode.Background)
        ElseIf i = 3 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode4.wav", AudioPlayMode.Background)
        End If
        If mode = 0 Then
            PictureBox380.Visible = True
            PictureBox381.Visible = True
            PictureBox382.Visible = True
            PictureBox383.Visible = True
            If tnt1_tick1 = 5 Then
                PictureBox380.Left = PictureBox392.Left + PictureBox104.Width
                PictureBox381.Left = PictureBox392.Left - PictureBox104.Width
                PictureBox382.Left = PictureBox392.Left
                PictureBox383.Left = PictureBox392.Left
                PictureBox380.Top = PictureBox392.Top
                PictureBox381.Top = PictureBox392.Top
                PictureBox382.Top = PictureBox392.Top + PictureBox104.Height
                PictureBox383.Top = PictureBox392.Top - PictureBox104.Height
            ElseIf tnt1_tick2 = 5 Then
                PictureBox380.Left = PictureBox393.Left + PictureBox104.Width
                PictureBox381.Left = PictureBox393.Left - PictureBox104.Width
                PictureBox382.Left = PictureBox393.Left
                PictureBox383.Left = PictureBox393.Left
                PictureBox380.Top = PictureBox393.Top
                PictureBox381.Top = PictureBox393.Top
                PictureBox382.Top = PictureBox393.Top + PictureBox104.Height
                PictureBox383.Top = PictureBox393.Top - PictureBox104.Height
            End If
        ElseIf mode = 1 Then
            PictureBox380.Visible = True
            PictureBox381.Visible = True
            PictureBox382.Visible = True
            PictureBox383.Visible = True
            PictureBox384.Visible = True
            PictureBox385.Visible = True
            PictureBox386.Visible = True
            PictureBox387.Visible = True
            If tnt1_tick1 = 5 Then
                PictureBox380.Left = PictureBox392.Left + PictureBox104.Width
                PictureBox381.Left = PictureBox392.Left - PictureBox104.Width
                PictureBox382.Left = PictureBox392.Left
                PictureBox383.Left = PictureBox392.Left
                PictureBox380.Top = PictureBox392.Top
                PictureBox381.Top = PictureBox392.Top
                PictureBox382.Top = PictureBox392.Top + PictureBox104.Height
                PictureBox383.Top = PictureBox392.Top - PictureBox104.Height

                PictureBox384.Left = PictureBox392.Left + PictureBox104.Width * 2
                PictureBox385.Left = PictureBox392.Left - PictureBox104.Width * 2
                PictureBox386.Left = PictureBox392.Left
                PictureBox387.Left = PictureBox392.Left
                PictureBox384.Top = PictureBox392.Top
                PictureBox385.Top = PictureBox392.Top
                PictureBox386.Top = PictureBox392.Top + PictureBox104.Height * 2
                PictureBox387.Top = PictureBox392.Top - PictureBox104.Height * 2
            ElseIf tnt1_tick2 = 5 Then
                PictureBox380.Left = PictureBox393.Left + PictureBox104.Width
                PictureBox381.Left = PictureBox393.Left - PictureBox104.Width
                PictureBox382.Left = PictureBox393.Left
                PictureBox383.Left = PictureBox393.Left
                PictureBox380.Top = PictureBox393.Top
                PictureBox381.Top = PictureBox393.Top
                PictureBox382.Top = PictureBox393.Top + PictureBox104.Height
                PictureBox383.Top = PictureBox393.Top - PictureBox104.Height

                PictureBox384.Left = PictureBox393.Left + PictureBox104.Width * 2
                PictureBox385.Left = PictureBox393.Left - PictureBox104.Width * 2
                PictureBox386.Left = PictureBox393.Left
                PictureBox387.Left = PictureBox393.Left
                PictureBox384.Top = PictureBox393.Top
                PictureBox385.Top = PictureBox393.Top
                PictureBox386.Top = PictureBox393.Top + PictureBox104.Height * 2
                PictureBox387.Top = PictureBox393.Top - PictureBox104.Height * 2
            End If
        End If
        
    End Sub
    '2P爆炸特效(function)
    Private Sub ex2()
        Dim i As Integer
        Randomize()
        i = Int((3 - 0 + 1) * Rnd() + 0)
        If i = 0 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode1.wav", AudioPlayMode.Background)
        ElseIf i = 1 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode2.wav", AudioPlayMode.Background)
        ElseIf i = 2 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode3.wav", AudioPlayMode.Background)
        ElseIf i = 3 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode4.wav", AudioPlayMode.Background)
        End If
        If mode = 1 Then
            PictureBox388.Visible = True
            PictureBox389.Visible = True
            PictureBox390.Visible = True
            PictureBox391.Visible = True
            PictureBox394.Visible = True
            PictureBox395.Visible = True
            PictureBox396.Visible = True
            PictureBox397.Visible = True
            If tnt2_tick1 = 5 Then
                PictureBox388.Left = PictureBox398.Left + PictureBox104.Width
                PictureBox389.Left = PictureBox398.Left - PictureBox104.Width
                PictureBox390.Left = PictureBox398.Left
                PictureBox391.Left = PictureBox398.Left

                PictureBox388.Top = PictureBox398.Top
                PictureBox389.Top = PictureBox398.Top
                PictureBox390.Top = PictureBox398.Top + PictureBox104.Height
                PictureBox391.Top = PictureBox398.Top - PictureBox104.Height

                PictureBox394.Left = PictureBox398.Left + PictureBox104.Width * 2
                PictureBox395.Left = PictureBox398.Left - PictureBox104.Width * 2
                PictureBox396.Left = PictureBox398.Left
                PictureBox397.Left = PictureBox398.Left

                PictureBox394.Top = PictureBox398.Top
                PictureBox395.Top = PictureBox398.Top
                PictureBox396.Top = PictureBox398.Top + PictureBox104.Height * 2
                PictureBox397.Top = PictureBox398.Top - PictureBox104.Height * 2
            ElseIf tnt2_tick2 = 5 Then
                PictureBox388.Left = PictureBox399.Left + PictureBox104.Width
                PictureBox389.Left = PictureBox399.Left - PictureBox104.Width
                PictureBox390.Left = PictureBox399.Left
                PictureBox391.Left = PictureBox399.Left
                PictureBox388.Top = PictureBox399.Top
                PictureBox389.Top = PictureBox399.Top
                PictureBox390.Top = PictureBox399.Top + PictureBox104.Height
                PictureBox391.Top = PictureBox399.Top - PictureBox104.Height

                PictureBox394.Left = PictureBox399.Left + PictureBox104.Width * 2
                PictureBox395.Left = PictureBox399.Left - PictureBox104.Width * 2
                PictureBox396.Left = PictureBox399.Left
                PictureBox397.Left = PictureBox399.Left
                PictureBox394.Top = PictureBox399.Top
                PictureBox395.Top = PictureBox399.Top
                PictureBox396.Top = PictureBox399.Top + PictureBox104.Height * 2
                PictureBox397.Top = PictureBox399.Top - PictureBox104.Height * 2
            End If
        ElseIf mode = 0 Then
            PictureBox388.Visible = True
            PictureBox389.Visible = True
            PictureBox390.Visible = True
            PictureBox391.Visible = True
            If tnt2_tick1 = 5 Then
                PictureBox388.Left = PictureBox398.Left + PictureBox104.Width
                PictureBox389.Left = PictureBox398.Left - PictureBox104.Width
                PictureBox390.Left = PictureBox398.Left
                PictureBox391.Left = PictureBox398.Left

                PictureBox388.Top = PictureBox398.Top
                PictureBox389.Top = PictureBox398.Top
                PictureBox390.Top = PictureBox398.Top + PictureBox104.Height
                PictureBox391.Top = PictureBox398.Top - PictureBox104.Height
            ElseIf tnt2_tick2 = 5 Then
                PictureBox388.Left = PictureBox399.Left + PictureBox104.Width
                PictureBox389.Left = PictureBox399.Left - PictureBox104.Width
                PictureBox390.Left = PictureBox399.Left
                PictureBox391.Left = PictureBox399.Left
                PictureBox388.Top = PictureBox399.Top
                PictureBox389.Top = PictureBox399.Top
                PictureBox390.Top = PictureBox399.Top + PictureBox104.Height
                PictureBox391.Top = PictureBox399.Top - PictureBox104.Height
            End If
        End If
        
    End Sub

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
    '2P死亡判斷(function)
    Private Sub die2()
        If invincible = 1 Or invincible = 0 Then
            If mode = 1 Then
                If xy2 = tnt1xy - 100 Or xy2 = tnt1xy + 100 Or xy2 = tnt1xy Or xy2 = tnt1xy + 200 Or xy2 = tnt1xy - 200 Then
                    If tnt1_tick1 = 5 Then
                        death2 = 1
                    End If
                    If tnt1_tick2 = 5 Then
                        death2 = 1
                    End If
                End If
                If xy2 >= tnt1xy - 2 And xy2 <= tnt1xy + 2 Then
                    If tnt1_tick1 = 5 Then
                        death2 = 1
                    End If
                    If tnt1_tick2 = 5 Then
                        death2 = 1
                    End If
                End If
                If xy2 = tnt2xy - 100 Or xy2 = tnt2xy + 100 Or xy2 = tnt2xy Or xy2 = tnt2xy + 200 Or xy2 = tnt2xy - 200 Then
                    If tnt2_tick1 = 5 Then
                        death2 = 1
                    End If
                    If tnt2_tick2 = 5 Then
                        death2 = 1
                    End If
                End If
                If xy2 >= tnt2xy - 2 And xy2 <= tnt2xy + 2 Then
                    If tnt2_tick1 = 5 Then
                        death2 = 1
                    End If
                    If tnt2_tick2 = 5 Then
                        death2 = 1
                    End If
                ElseIf mode = 0 Then
                    If xy2 = tnt1xy - 100 Or xy2 = tnt1xy + 100 Or xy2 = tnt1xy Then
                        If tnt1_tick1 = 5 Then
                            death2 = 1
                        End If
                        If tnt1_tick2 = 5 Then
                            death2 = 1
                        End If
                    End If
                    If xy2 >= tnt1xy - 1 And xy2 <= tnt1xy + 1 Then
                        If tnt1_tick1 = 5 Then
                            death2 = 1
                        End If
                        If tnt1_tick2 = 5 Then
                            death2 = 1
                        End If
                    End If
                    If xy2 = tnt2xy - 100 Or xy2 = tnt2xy + 100 Or xy2 = tnt2xy Then
                        If tnt2_tick1 = 5 Then
                            death2 = 1
                        End If
                        If tnt2_tick2 = 5 Then
                            death2 = 1
                        End If
                    End If
                    If xy2 >= tnt2xy - 1 And xy2 <= tnt2xy + 1 Then
                        If tnt2_tick1 = 5 Then
                            death2 = 1
                        End If
                        If tnt2_tick2 = 5 Then
                            death2 = 1
                        End If
                    End If
                End If
            ElseIf mode = 0 Then
                If xy2 = tnt1xy - 100 Or xy2 = tnt1xy + 100 Or xy2 = tnt1xy Then
                    If tnt1_tick1 = 5 Then
                        death2 = 1
                    End If
                    If tnt1_tick2 = 5 Then
                        death2 = 1
                    End If
                End If
                If xy2 >= tnt1xy - 1 And xy2 <= tnt1xy + 1 Then
                    If tnt1_tick1 = 5 Then
                        death2 = 1
                    End If
                    If tnt1_tick2 = 5 Then
                        death2 = 1
                    End If
                End If
                If xy2 = tnt2xy - 100 Or xy2 = tnt2xy + 100 Or xy2 = tnt2xy Then
                    If tnt2_tick1 = 5 Then
                        death2 = 1
                    End If
                    If tnt2_tick2 = 5 Then
                        death2 = 1
                    End If
                End If
                If xy2 >= tnt2xy - 1 And xy2 <= tnt2xy + 1 Then
                    If tnt2_tick1 = 5 Then
                        death2 = 1
                    End If
                    If tnt2_tick2 = 5 Then
                        death2 = 1
                    End If
                End If
            End If
        End If
    End Sub

    '遊戲結束(時間到)[比分數](function)
    Private Sub ending3()
        If endy = 0 Then
            If score1 < score2 Then
                Label8.Text = "WIN"
                Label9.Text = "LOSS"
                endy = 2
                player.close()
            ElseIf score1 > score2 Then
                Label9.Text = "WIN"
                Label8.Text = "LOSS"
                endy = 2
                player.close()
            Else
                Label9.Text = "DRAW"
                Label8.Text = "DRAW"
                endy = 2
                player.close()
            End If
            esc = 1
            Call esc_mode()
        End If
    End Sub

    '遊戲結束(吃完[?])[比分數](function)
    Private Sub ending1()
        If endy = 0 Then
            If score1 > score2 Then
                Label8.Text = "WIN"
                Label9.Text = "LOSS"
                endy = 2
                player.close()
            ElseIf score1 < score2 Then
                Label9.Text = "WIN"
                Label8.Text = "LOSS"
                endy = 2
                player.close()
            Else
                Label9.Text = "DRAW"
                Label8.Text = "DRAW"
                endy = 2
                player.close()
            End If
            esc = 1
            Call esc_mode()
        End If
    End Sub

    '遊戲結束[死亡](function)
    Private Sub ending2()
        If endy = 0 Then
            If death1 >= 1 And death2 = 0 Then
                Label8.Text = "LOSS"
                Label9.Text = "WIN"
                endy = 2
                PictureBox401.Image = ImageList3.Images(4)
                PictureBox401.Top -= 50
                Timer5.Enabled = True
                If music = 2 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\+9die.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\dead.wav", AudioPlayMode.Background)
                End If
                player.close()
            ElseIf death2 >= 1 And death1 = 0 Then
                Label9.Text = "LOSS"
                Label8.Text = "WIN"
                endy = 2
                PictureBox400.Image = ImageList3.Images(4)
                PictureBox400.Top -= 50
                Timer5.Enabled = True
                If music = 2 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\+9die.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\dead.wav", AudioPlayMode.Background)
                End If
                player.close()
            ElseIf death2 >= 1 And death1 >= 1 Then
                Label9.Text = "LOSS"
                Label8.Text = "LOSS"
                endy = 2
                PictureBox401.Image = ImageList3.Images(4)
                PictureBox400.Image = ImageList3.Images(4)
                PictureBox401.Top -= 50
                PictureBox400.Top -= 50
                If music = 2 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\+9die.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\dead.wav", AudioPlayMode.Background)
                End If

                Timer5.Enabled = True
                player.close()
            End If
        End If
    End Sub

    '1p分數+1(function)
    Private Sub point1()
        score1 += 1
        Label6.Text = score1
        endcount += 1
        map_point()
    End Sub
    '2p分數+1(function)
    Private Sub point2()
        score2 += 1
        Label7.Text = score2
        endcount += 1
        map_point()
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
            Label13.Text = "x2: " & x2
            Label14.Text = "y2: " & y2
            Label15.Text = "tnt2xy: " & tnt2xy
            Label16.Text = "tnt2_tick1: " & tnt2_tick1 & " (tick/s)"
            Label17.Text = "tnt2_tick2:" & tnt2_tick2 & " (tick/s)"
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
        Dim choose_b As Integer
        Randomize()
        map_style = Int((19 - 0 + 1) * Rnd() + 0)

        If map_style = 0 Then
            Randomize()
            choose_b = Int((1 - 0 + 1) * Rnd() + 0)
            If choose_b = 0 Then
                block(3) = 412
            Else
                block(3) = 415
            End If
            Me.BackColor = Color.Peru
            Me.BackgroundImage = ImageList9.Images(318)
            block(2) = 279
            Randomize()
            choose_b = Int((6 - 0 + 1) * Rnd() + 0)
            If choose_b = 0 Then
                block(1) = 138
            ElseIf choose_b = 1 Then
                block(1) = 39
            ElseIf choose_b = 2 Then
                block(1) = 222
            ElseIf choose_b = 3 Then
                block(1) = 147
            ElseIf choose_b = 4 Then
                block(1) = 256
            ElseIf choose_b = 5 Then
                block(1) = 263
            Else
                block(1) = 365
            End If
        ElseIf map_style = 1 Then
            Randomize()
            choose_b = Int((1 - 0 + 1) * Rnd() + 0)
            If choose_b = 0 Then
                block(1) = 457
            Else
                block(1) = 221
            End If
            Randomize()
            choose_b = Int((1 - 0 + 1) * Rnd() + 0)
            If choose_b = 0 Then
                Me.BackColor = Color.LightSteelBlue
            Else
                Me.BackColor = Color.Wheat
            End If
            block(2) = 324
            block(3) = 325
        ElseIf map_style = 2 Then
            block(1) = 220
            Me.BackColor = Color.Tan
            block(2) = 458
            block(3) = 301
        ElseIf map_style = 3 Then
            Randomize()
            choose_b = Int((1 - 0 + 1) * Rnd() + 0)
            If choose_b = 0 Then
                block(3) = 312
            Else
                block(3) = 313
            End If
            Me.BackColor = Color.Silver
            block(2) = 314
            block(1) = 413
        ElseIf map_style = 4 Then
            block(3) = 15
            Me.BackgroundImage = ImageList9.Images(318)
            Me.BackColor = Color.Peru
            block(2) = 141
            block(1) = 16
        ElseIf map_style = 5 Then
            block(3) = 3
            Me.BackgroundImage = ImageList9.Images(42)
            Me.BackColor = Color.Gray
            block(2) = 287
            block(1) = 425
        ElseIf map_style = 6 Then
            block(3) = 355
            Me.BackgroundImage = ImageList9.Images(371)
            Me.BackColor = Color.NavajoWhite
            block(2) = 86
            block(1) = 283
        ElseIf map_style = 7 Then
            block(3) = 18
            Me.BackgroundImage = ImageList9.Images(229)
            Me.BackColor = Color.Gray
            block(2) = 326
            block(1) = 327
        ElseIf map_style = 8 Then
            Randomize()
            choose_b = Int((4 - 0 + 1) * Rnd() + 0)
            If choose_b = 0 Then
                block(2) = 436
            ElseIf choose_b = 1 Then
                block(2) = 437
            ElseIf choose_b = 2 Then
                block(2) = 438
            ElseIf choose_b = 3 Then
                block(2) = 439
            ElseIf choose_b = 4 Then
                block(2) = 440
            End If
            Randomize()
            choose_b = Int((4 - 0 + 1) * Rnd() + 0)
            If choose_b = 0 Then
                block(3) = 368
            ElseIf choose_b = 1 Then
                block(3) = 323
            ElseIf choose_b = 2 Then
                block(3) = 322
            ElseIf choose_b = 3 Then
                block(3) = 321
            ElseIf choose_b = 4 Then
                block(3) = 29
            End If
            Me.BackColor = Color.SaddleBrown
            Me.BackgroundImage = ImageList9.Images(149)
            block(1) = 247
        ElseIf map_style = 9 Then
            Randomize()
            block(3) = Int((313 - 303 + 1) * Rnd() + 303)
            Me.BackgroundImage = ImageList9.Images(341)
            Me.BackColor = Color.White
            block(2) = 363
            block(1) = 364
        ElseIf map_style = 10 Then
            Randomize()
            block(3) = 17
            Me.BackgroundImage = ImageList9.Images(318)
            Me.BackColor = Color.Peru
            block(2) = 166
            block(1) = 167
        ElseIf map_style = 11 Then
            Randomize()
            block(3) = 319
            Me.BackgroundImage = ImageList9.Images(403)
            Me.BackColor = Color.LightSlateGray
            block(2) = 432
            Randomize()
            choose_b = Int((6 - 0 + 1) * Rnd() + 0)
            If choose_b = 0 Then
                block(1) = 138
            ElseIf choose_b = 1 Then
                block(1) = 39
            ElseIf choose_b = 2 Then
                block(1) = 222
            ElseIf choose_b = 3 Then
                block(1) = 100
            ElseIf choose_b = 4 Then
                block(1) = 256
            ElseIf choose_b = 5 Then
                block(1) = 263
            Else
                block(1) = 365
            End If
        ElseIf map_style = 12 Then
            Randomize()
            block(1) = Int((219 - 204 + 1) * Rnd() + 204)
            Me.BackgroundImage = ImageList9.Images(318)
            Me.BackColor = Color.Peru
            block(2) = Int((165 - 162 + 1) * Rnd() + 162)
            block(3) = Int((356 - 354 + 1) * Rnd() + 354)
        ElseIf map_style = 13 Then
            Randomize()
            block(2) = Int((420 - 417 + 1) * Rnd() + 417)
            Me.BackgroundImage = ImageList9.Images(336)
            Me.BackColor = Color.White
            block(3) = 441
            block(1) = Int((88 - 87 + 1) * Rnd() + 87)
        ElseIf map_style = 14 Then
            Randomize()
            block(2) = 259
            Me.BackgroundImage = ImageList9.Images(336)
            Me.BackColor = Color.White
            block(3) = 260
            block(1) = 82
        ElseIf map_style = 15 Then
            Randomize()
            block(2) = 80
            Me.BackgroundImage = ImageList9.Images(336)
            Me.BackColor = Color.White
            block(3) = 140
            block(1) = 167
        ElseIf map_style = 16 Then
            Randomize()
            block(2) = 261
            Me.BackgroundImage = ImageList9.Images(15)
            Me.BackColor = Color.Brown
            block(3) = 410
            block(1) = 426
        ElseIf map_style = 17 Then
            Randomize()
            block(2) = 367
            Me.BackgroundImage = ImageList9.Images(337)
            Me.BackColor = Color.White
            block(3) = 423
            block(1) = 7
        ElseIf map_style = 18 Then
            Randomize()
            block(1) = Int((133 - 124 + 1) * Rnd() + 124)
            Me.BackgroundImage = ImageList9.Images(101)
            Me.BackColor = Color.SaddleBrown
            block(3) = Int((456 - 442 + 1) * Rnd() + 442)
            block(2) = Int((161 - 157 + 1) * Rnd() + 157)
        ElseIf map_style = 19 Then
            Randomize()
            block(2) = 308
            Me.BackgroundImage = ImageList9.Images(142)
            Me.BackColor = Color.Bisque
            block(3) = Int((334 - 332 + 1) * Rnd() + 332)
            block(1) = 145
        End If
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
    '2P牆壁擋玩家執行(function back)
    Private Sub back2()
        If f12 = 0 Then
            If udlr2 = 0 Then
                PictureBox400.Top = PictureBox400.Top + un2 * PictureBox104.Height
                y2 = y2 + un2
            ElseIf udlr2 = 1 Then
                PictureBox400.Left = PictureBox400.Left + un2 * PictureBox104.Width
                x2 = x2 + un2
            End If
        End If
        If exer2 > 0 Then
            exer2 += 1
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
        If exer2 = 0 Then
            If xy2 = tnt21 Then
                Call back2()
            End If
            If xy2 = tnt22 Then
                Call back2()
            End If
        End If
        If exer2 = 0 Or exer2 = 1 Then
            If xy2 = tnt11 Then
                Call back2()
            End If
            If xy2 = tnt12 Then
                Call back2()
            End If
        End If
    End Sub

    '1P牆壁擋玩家偵測 + 呼叫 function back (function)
    Private Sub detection1()
        xy1 = x1 * 100 + y1
        If f12 = 0 Then
            If map = 0 Then
                If xy1 = 106 And PictureBox6.Visible = True Then
                    Call back1()
                End If
                If xy1 = 107 And PictureBox7.Visible = True Then
                    Call back1()
                End If
                If xy1 = 108 And PictureBox8.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 203 And PictureBox24.Visible = True Then
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
                If xy1 = 211 And PictureBox16.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
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
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1012 And PictureBox302.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
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
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
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
            ElseIf map = 1 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
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
                If xy1 = 112 And PictureBox12.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
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
                If xy1 = 210 And PictureBox17.Visible = True Then
                    Call back1()
                End If
                If xy1 = 212 And PictureBox15.Visible = True Then
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
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 307 And PictureBox53.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
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
                If xy1 = 407 And PictureBox86.Visible = True Then
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
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 602 And PictureBox180.Visible = True Then
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
                If xy1 = 703 And PictureBox212.Visible = True Then
                    Call back1()
                End If
                If xy1 = 704 And PictureBox211.Visible = True Then
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
                If xy1 = 710 And PictureBox205.Visible = True Then
                    Call back1()
                End If
                If xy1 = 711 And PictureBox204.Visible = True Then
                    Call back1()
                End If
                If xy1 = 712 And PictureBox203.Visible = True Then
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
                If xy1 = 806 And PictureBox242.Visible = True Then
                    Call back1()
                End If
                If xy1 = 807 And PictureBox241.Visible = True Then
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
                If xy1 = 902 And PictureBox279.Visible = True Then
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
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
                    Call back1()
                End If
                If xy1 = 912 And PictureBox269.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1002 And PictureBox312.Visible = True Then
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
                If xy1 = 1102 And PictureBox345.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1104 And PictureBox343.Visible = True Then
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
                If xy1 = 1110 And PictureBox337.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1111 And PictureBox336.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1112 And PictureBox335.Visible = True Then
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
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1207 And PictureBox373.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1208 And PictureBox372.Visible = True Then
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
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
                    Call back1()
                End If
                If xy1 = 204 And PictureBox23.Visible = True Then
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
                If xy1 = 211 And PictureBox16.Visible = True Then
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
                If xy1 = 306 And PictureBox54.Visible = True Then
                    Call back1()
                End If
                If xy1 = 308 And PictureBox52.Visible = True Then
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
                If xy1 = 604 And PictureBox178.Visible = True Then
                    Call back1()
                End If
                If xy1 = 605 And PictureBox177.Visible = True Then
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
                If xy1 = 705 And PictureBox210.Visible = True Then
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
                If xy1 = 907 And PictureBox274.Visible = True Then
                    Call back1()
                End If
                If xy1 = 908 And PictureBox273.Visible = True Then
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
                If xy1 = 1010 And PictureBox304.Visible = True Then
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
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 3 Then
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
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 112 And PictureBox12.Visible = True Then
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
                If xy1 = 205 And PictureBox22.Visible = True Then
                    Call back1()
                End If
                If xy1 = 207 And PictureBox20.Visible = True Then
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
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 312 And PictureBox48.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
                    Call back1()
                End If
                If xy1 = 402 And PictureBox91.Visible = True Then
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
                If xy1 = 503 And PictureBox146.Visible = True Then
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
                If xy1 = 609 And PictureBox173.Visible = True Then
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
                If xy1 = 712 And PictureBox203.Visible = True Then
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
                If xy1 = 906 And PictureBox275.Visible = True Then
                    Call back1()
                End If
                If xy1 = 907 And PictureBox274.Visible = True Then
                    Call back1()
                End If
                If xy1 = 909 And PictureBox272.Visible = True Then
                    Call back1()
                End If
                If xy1 = 910 And PictureBox271.Visible = True Then
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
                If xy1 = 1008 And PictureBox306.Visible = True Then
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
                If xy1 = 1103 And PictureBox344.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1105 And PictureBox342.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1107 And PictureBox340.Visible = True Then
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
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 4 Then
                If xy1 = 103 And PictureBox3.Visible = True Then
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
                If xy1 = 111 And PictureBox11.Visible = True Then
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
                If xy1 = 1203 And PictureBox377.Visible = True Then
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
                If xy1 = 1211 And PictureBox369.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 5 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
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
                If xy1 = 109 And PictureBox9.Visible = True Then
                    Call back1()
                End If
                If xy1 = 110 And PictureBox10.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
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
                If xy1 = 301 And PictureBox59.Visible = True Then
                    Call back1()
                End If
                If xy1 = 303 And PictureBox57.Visible = True Then
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
                If xy1 = 403 And PictureBox90.Visible = True Then
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
                If xy1 = 412 And PictureBox81.Visible = True Then
                    Call back1()
                End If
                If xy1 = 413 And PictureBox80.Visible = True Then
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
                If xy1 = 512 And PictureBox117.Visible = True Then
                    Call back1()
                End If
                If xy1 = 513 And PictureBox116.Visible = True Then
                    Call back1()
                End If
                If xy1 = 601 And PictureBox181.Visible = True Then
                    Call back1()
                End If
                If xy1 = 607 And PictureBox175.Visible = True Then
                    Call back1()
                End If
                If xy1 = 608 And PictureBox174.Visible = True Then
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
                If xy1 = 813 And PictureBox235.Visible = True Then
                    Call back1()
                End If
                If xy1 = 901 And PictureBox280.Visible = True Then
                    Call back1()
                End If
                If xy1 = 902 And PictureBox279.Visible = True Then
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
                If xy1 = 911 And PictureBox270.Visible = True Then
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
                If xy1 = 1011 And PictureBox303.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1013 And PictureBox301.Visible = True Then
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
                If xy1 = 1112 And PictureBox335.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1113 And PictureBox334.Visible = True Then
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
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 6 Then
                If xy1 = 101 And PictureBox104.Visible = True Then
                    Call back1()
                End If
                If xy1 = 102 And PictureBox2.Visible = True Then
                    Call back1()
                End If
                If xy1 = 201 And PictureBox26.Visible = True Then
                    Call back1()
                End If
                If xy1 = 202 And PictureBox25.Visible = True Then
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
                If xy1 = 309 And PictureBox51.Visible = True Then
                    Call back1()
                End If
                If xy1 = 310 And PictureBox50.Visible = True Then
                    Call back1()
                End If
                If xy1 = 313 And PictureBox47.Visible = True Then
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
                If xy1 = 413 And PictureBox80.Visible = True Then
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
                If xy1 = 607 And PictureBox175.Visible = True Then
                    Call back1()
                End If
                If xy1 = 608 And PictureBox174.Visible = True Then
                    Call back1()
                End If
                If xy1 = 609 And PictureBox173.Visible = True Then
                    Call back1()
                End If
                If xy1 = 611 And PictureBox171.Visible = True Then
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
                If xy1 = 901 And PictureBox280.Visible = True Then
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
                If xy1 = 1001 And PictureBox313.Visible = True Then
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
                If xy1 = 1104 And PictureBox343.Visible = True Then
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
                If xy1 = 1212 And PictureBox368.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1213 And PictureBox367.Visible = True Then
                    Call back1()
                End If
            ElseIf map = 7 Then
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
                If xy1 = 1204 And PictureBox376.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1205 And PictureBox375.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1206 And PictureBox374.Visible = True Then
                    Call back1()
                End If
                If xy1 = 1212 And PictureBox368.Visible = True Then
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
    '2P牆壁擋玩家偵測 + 呼叫 function back (function)
    Private Sub detection2()
        xy2 = x2 * 100 + y2
        If f12 = 0 Then
            If map = 0 Then
                If xy2 = 106 And PictureBox6.Visible = True Then
                    Call back2()
                End If
                If xy2 = 107 And PictureBox7.Visible = True Then
                    Call back2()
                End If
                If xy2 = 108 And PictureBox8.Visible = True Then
                    Call back2()
                End If
                If xy2 = 202 And PictureBox25.Visible = True Then
                    Call back2()
                End If
                If xy2 = 203 And PictureBox24.Visible = True Then
                    Call back2()
                End If
                If xy2 = 206 And PictureBox21.Visible = True Then
                    Call back2()
                End If
                If xy2 = 207 And PictureBox20.Visible = True Then
                    Call back2()
                End If
                If xy2 = 208 And PictureBox19.Visible = True Then
                    Call back2()
                End If
                If xy2 = 211 And PictureBox16.Visible = True Then
                    Call back2()
                End If
                If xy2 = 212 And PictureBox15.Visible = True Then
                    Call back2()
                End If
                If xy2 = 301 And PictureBox59.Visible = True Then
                    Call back2()
                End If
                If xy2 = 302 And PictureBox58.Visible = True Then
                    Call back2()
                End If
                If xy2 = 303 And PictureBox57.Visible = True Then
                    Call back2()
                End If
                If xy2 = 305 And PictureBox55.Visible = True Then
                    Call back2()
                End If
                If xy2 = 306 And PictureBox54.Visible = True Then
                    Call back2()
                End If
                If xy2 = 307 And PictureBox53.Visible = True Then
                    Call back2()
                End If
                If xy2 = 308 And PictureBox52.Visible = True Then
                    Call back2()
                End If
                If xy2 = 309 And PictureBox51.Visible = True Then
                    Call back2()
                End If
                If xy2 = 311 And PictureBox49.Visible = True Then
                    Call back2()
                End If
                If xy2 = 312 And PictureBox48.Visible = True Then
                    Call back2()
                End If
                If xy2 = 313 And PictureBox47.Visible = True Then
                    Call back2()
                End If
                If xy2 = 401 And PictureBox92.Visible = True Then
                    Call back2()
                End If
                If xy2 = 402 And PictureBox91.Visible = True Then
                    Call back2()
                End If
                If xy2 = 403 And PictureBox90.Visible = True Then
                    Call back2()
                End If
                If xy2 = 404 And PictureBox89.Visible = True Then
                    Call back2()
                End If
                If xy2 = 405 And PictureBox88.Visible = True Then
                    Call back2()
                End If
                If xy2 = 406 And PictureBox87.Visible = True Then
                    Call back2()
                End If
                If xy2 = 407 And PictureBox86.Visible = True Then
                    Call back2()
                End If
                If xy2 = 408 And PictureBox85.Visible = True Then
                    Call back2()
                End If
                If xy2 = 409 And PictureBox84.Visible = True Then
                    Call back2()
                End If
                If xy2 = 410 And PictureBox83.Visible = True Then
                    Call back2()
                End If
                If xy2 = 411 And PictureBox82.Visible = True Then
                    Call back2()
                End If
                If xy2 = 412 And PictureBox81.Visible = True Then
                    Call back2()
                End If
                If xy2 = 413 And PictureBox80.Visible = True Then
                    Call back2()
                End If
                If xy2 = 501 And PictureBox148.Visible = True Then
                    Call back2()
                End If
                If xy2 = 502 And PictureBox147.Visible = True Then
                    Call back2()
                End If
                If xy2 = 503 And PictureBox146.Visible = True Then
                    Call back2()
                End If
                If xy2 = 504 And PictureBox145.Visible = True Then
                    Call back2()
                End If
                If xy2 = 505 And PictureBox1.Visible = True Then
                    Call back2()
                End If
                If xy2 = 506 And PictureBox123.Visible = True Then
                    Call back2()
                End If
                If xy2 = 507 And PictureBox122.Visible = True Then
                    Call back2()
                End If
                If xy2 = 508 And PictureBox121.Visible = True Then
                    Call back2()
                End If
                If xy2 = 509 And PictureBox120.Visible = True Then
                    Call back2()
                End If
                If xy2 = 510 And PictureBox119.Visible = True Then
                    Call back2()
                End If
                If xy2 = 511 And PictureBox118.Visible = True Then
                    Call back2()
                End If
                If xy2 = 512 And PictureBox117.Visible = True Then
                    Call back2()
                End If
                If xy2 = 513 And PictureBox116.Visible = True Then
                    Call back2()
                End If
                If xy2 = 601 And PictureBox181.Visible = True Then
                    Call back2()
                End If
                If xy2 = 602 And PictureBox180.Visible = True Then
                    Call back2()
                End If
                If xy2 = 603 And PictureBox179.Visible = True Then
                    Call back2()
                End If
                If xy2 = 604 And PictureBox178.Visible = True Then
                    Call back2()
                End If
                If xy2 = 605 And PictureBox177.Visible = True Then
                    Call back2()
                End If
                If xy2 = 606 And PictureBox176.Visible = True Then
                    Call back2()
                End If
                If xy2 = 607 And PictureBox175.Visible = True Then
                    Call back2()
                End If
                If xy2 = 608 And PictureBox174.Visible = True Then
                    Call back2()
                End If
                If xy2 = 609 And PictureBox173.Visible = True Then
                    Call back2()
                End If
                If xy2 = 610 And PictureBox172.Visible = True Then
                    Call back2()
                End If
                If xy2 = 611 And PictureBox171.Visible = True Then
                    Call back2()
                End If
                If xy2 = 612 And PictureBox170.Visible = True Then
                    Call back2()
                End If
                If xy2 = 613 And PictureBox169.Visible = True Then
                    Call back2()
                End If
                If xy2 = 701 And PictureBox214.Visible = True Then
                    Call back2()
                End If
                If xy2 = 702 And PictureBox213.Visible = True Then
                    Call back2()
                End If
                If xy2 = 703 And PictureBox212.Visible = True Then
                    Call back2()
                End If
                If xy2 = 704 And PictureBox211.Visible = True Then
                    Call back2()
                End If
                If xy2 = 705 And PictureBox210.Visible = True Then
                    Call back2()
                End If
                If xy2 = 706 And PictureBox209.Visible = True Then
                    Call back2()
                End If
                If xy2 = 707 And PictureBox208.Visible = True Then
                    Call back2()
                End If
                If xy2 = 708 And PictureBox207.Visible = True Then
                    Call back2()
                End If
                If xy2 = 709 And PictureBox206.Visible = True Then
                    Call back2()
                End If
                If xy2 = 710 And PictureBox205.Visible = True Then
                    Call back2()
                End If
                If xy2 = 711 And PictureBox204.Visible = True Then
                    Call back2()
                End If
                If xy2 = 712 And PictureBox203.Visible = True Then
                    Call back2()
                End If
                If xy2 = 713 And PictureBox202.Visible = True Then
                    Call back2()
                End If
                If xy2 = 801 And PictureBox247.Visible = True Then
                    Call back2()
                End If
                If xy2 = 802 And PictureBox246.Visible = True Then
                    Call back2()
                End If
                If xy2 = 803 And PictureBox245.Visible = True Then
                    Call back2()
                End If
                If xy2 = 804 And PictureBox244.Visible = True Then
                    Call back2()
                End If
                If xy2 = 805 And PictureBox243.Visible = True Then
                    Call back2()
                End If
                If xy2 = 806 And PictureBox242.Visible = True Then
                    Call back2()
                End If
                If xy2 = 807 And PictureBox241.Visible = True Then
                    Call back2()
                End If
                If xy2 = 808 And PictureBox240.Visible = True Then
                    Call back2()
                End If
                If xy2 = 809 And PictureBox239.Visible = True Then
                    Call back2()
                End If
                If xy2 = 810 And PictureBox238.Visible = True Then
                    Call back2()
                End If
                If xy2 = 811 And PictureBox237.Visible = True Then
                    Call back2()
                End If
                If xy2 = 812 And PictureBox236.Visible = True Then
                    Call back2()
                End If
                If xy2 = 813 And PictureBox235.Visible = True Then
                    Call back2()
                End If
                If xy2 = 901 And PictureBox280.Visible = True Then
                    Call back2()
                End If
                If xy2 = 902 And PictureBox279.Visible = True Then
                    Call back2()
                End If
                If xy2 = 903 And PictureBox278.Visible = True Then
                    Call back2()
                End If
                If xy2 = 904 And PictureBox277.Visible = True Then
                    Call back2()
                End If
                If xy2 = 905 And PictureBox276.Visible = True Then
                    Call back2()
                End If
                If xy2 = 906 And PictureBox275.Visible = True Then
                    Call back2()
                End If
                If xy2 = 907 And PictureBox274.Visible = True Then
                    Call back2()
                End If
                If xy2 = 908 And PictureBox273.Visible = True Then
                    Call back2()
                End If
                If xy2 = 909 And PictureBox272.Visible = True Then
                    Call back2()
                End If
                If xy2 = 910 And PictureBox271.Visible = True Then
                    Call back2()
                End If
                If xy2 = 911 And PictureBox270.Visible = True Then
                    Call back2()
                End If
                If xy2 = 912 And PictureBox269.Visible = True Then
                    Call back2()
                End If
                If xy2 = 913 And PictureBox268.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1001 And PictureBox313.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1002 And PictureBox312.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1003 And PictureBox311.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1005 And PictureBox309.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1006 And PictureBox308.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1007 And PictureBox307.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1008 And PictureBox306.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1009 And PictureBox305.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1011 And PictureBox303.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1012 And PictureBox302.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1013 And PictureBox301.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1102 And PictureBox345.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1103 And PictureBox344.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1106 And PictureBox341.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1107 And PictureBox340.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1108 And PictureBox339.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1111 And PictureBox336.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1112 And PictureBox335.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1206 And PictureBox374.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1207 And PictureBox373.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1208 And PictureBox372.Visible = True Then
                    Call back2()
                End If
            ElseIf map = 1 Then
                If xy2 = 101 And PictureBox104.Visible = True Then
                    Call back2()
                End If
                If xy2 = 102 And PictureBox2.Visible = True Then
                    Call back2()
                End If
                If xy2 = 104 And PictureBox4.Visible = True Then
                    Call back2()
                End If
                If xy2 = 105 And PictureBox5.Visible = True Then
                    Call back2()
                End If
                If xy2 = 106 And PictureBox6.Visible = True Then
                    Call back2()
                End If
                If xy2 = 108 And PictureBox8.Visible = True Then
                    Call back2()
                End If
                If xy2 = 109 And PictureBox9.Visible = True Then
                    Call back2()
                End If
                If xy2 = 110 And PictureBox10.Visible = True Then
                    Call back2()
                End If
                If xy2 = 112 And PictureBox12.Visible = True Then
                    Call back2()
                End If
                If xy2 = 202 And PictureBox25.Visible = True Then
                    Call back2()
                End If
                If xy2 = 204 And PictureBox23.Visible = True Then
                    Call back2()
                End If
                If xy2 = 205 And PictureBox22.Visible = True Then
                    Call back2()
                End If
                If xy2 = 206 And PictureBox21.Visible = True Then
                    Call back2()
                End If
                If xy2 = 208 And PictureBox19.Visible = True Then
                    Call back2()
                End If
                If xy2 = 210 And PictureBox17.Visible = True Then
                    Call back2()
                End If
                If xy2 = 212 And PictureBox15.Visible = True Then
                    Call back2()
                End If
                If xy2 = 301 And PictureBox59.Visible = True Then
                    Call back2()
                End If
                If xy2 = 302 And PictureBox58.Visible = True Then
                    Call back2()
                End If
                If xy2 = 303 And PictureBox57.Visible = True Then
                    Call back2()
                End If
                If xy2 = 304 And PictureBox56.Visible = True Then
                    Call back2()
                End If
                If xy2 = 306 And PictureBox54.Visible = True Then
                    Call back2()
                End If
                If xy2 = 307 And PictureBox53.Visible = True Then
                    Call back2()
                End If
                If xy2 = 308 And PictureBox52.Visible = True Then
                    Call back2()
                End If
                If xy2 = 310 And PictureBox50.Visible = True Then
                    Call back2()
                End If
                If xy2 = 311 And PictureBox49.Visible = True Then
                    Call back2()
                End If
                If xy2 = 312 And PictureBox48.Visible = True Then
                    Call back2()
                End If
                If xy2 = 402 And PictureBox91.Visible = True Then
                    Call back2()
                End If
                If xy2 = 403 And PictureBox90.Visible = True Then
                    Call back2()
                End If
                If xy2 = 404 And PictureBox89.Visible = True Then
                    Call back2()
                End If
                If xy2 = 406 And PictureBox87.Visible = True Then
                    Call back2()
                End If
                If xy2 = 407 And PictureBox86.Visible = True Then
                    Call back2()
                End If
                If xy2 = 408 And PictureBox85.Visible = True Then
                    Call back2()
                End If
                If xy2 = 410 And PictureBox83.Visible = True Then
                    Call back2()
                End If
                If xy2 = 411 And PictureBox82.Visible = True Then
                    Call back2()
                End If
                If xy2 = 412 And PictureBox81.Visible = True Then
                    Call back2()
                End If
                If xy2 = 413 And PictureBox80.Visible = True Then
                    Call back2()
                End If
                If xy2 = 501 And PictureBox148.Visible = True Then
                    Call back2()
                End If
                If xy2 = 502 And PictureBox147.Visible = True Then
                    Call back2()
                End If
                If xy2 = 504 And PictureBox145.Visible = True Then
                    Call back2()
                End If
                If xy2 = 505 And PictureBox1.Visible = True Then
                    Call back2()
                End If
                If xy2 = 506 And PictureBox123.Visible = True Then
                    Call back2()
                End If
                If xy2 = 508 And PictureBox121.Visible = True Then
                    Call back2()
                End If
                If xy2 = 509 And PictureBox120.Visible = True Then
                    Call back2()
                End If
                If xy2 = 510 And PictureBox119.Visible = True Then
                    Call back2()
                End If
                If xy2 = 512 And PictureBox117.Visible = True Then
                    Call back2()
                End If
                If xy2 = 602 And PictureBox180.Visible = True Then
                    Call back2()
                End If
                If xy2 = 604 And PictureBox178.Visible = True Then
                    Call back2()
                End If
                If xy2 = 605 And PictureBox177.Visible = True Then
                    Call back2()
                End If
                If xy2 = 606 And PictureBox176.Visible = True Then
                    Call back2()
                End If
                If xy2 = 608 And PictureBox174.Visible = True Then
                    Call back2()
                End If
                If xy2 = 610 And PictureBox172.Visible = True Then
                    Call back2()
                End If
                If xy2 = 612 And PictureBox170.Visible = True Then
                    Call back2()
                End If
                If xy2 = 613 And PictureBox169.Visible = True Then
                    Call back2()
                End If
                If xy2 = 701 And PictureBox214.Visible = True Then
                    Call back2()
                End If
                If xy2 = 702 And PictureBox213.Visible = True Then
                    Call back2()
                End If
                If xy2 = 703 And PictureBox212.Visible = True Then
                    Call back2()
                End If
                If xy2 = 704 And PictureBox211.Visible = True Then
                    Call back2()
                End If
                If xy2 = 706 And PictureBox209.Visible = True Then
                    Call back2()
                End If
                If xy2 = 707 And PictureBox208.Visible = True Then
                    Call back2()
                End If
                If xy2 = 708 And PictureBox207.Visible = True Then
                    Call back2()
                End If
                If xy2 = 710 And PictureBox205.Visible = True Then
                    Call back2()
                End If
                If xy2 = 711 And PictureBox204.Visible = True Then
                    Call back2()
                End If
                If xy2 = 712 And PictureBox203.Visible = True Then
                    Call back2()
                End If
                If xy2 = 802 And PictureBox246.Visible = True Then
                    Call back2()
                End If
                If xy2 = 803 And PictureBox245.Visible = True Then
                    Call back2()
                End If
                If xy2 = 804 And PictureBox244.Visible = True Then
                    Call back2()
                End If
                If xy2 = 806 And PictureBox242.Visible = True Then
                    Call back2()
                End If
                If xy2 = 807 And PictureBox241.Visible = True Then
                    Call back2()
                End If
                If xy2 = 808 And PictureBox240.Visible = True Then
                    Call back2()
                End If
                If xy2 = 810 And PictureBox238.Visible = True Then
                    Call back2()
                End If
                If xy2 = 811 And PictureBox237.Visible = True Then
                    Call back2()
                End If
                If xy2 = 812 And PictureBox236.Visible = True Then
                    Call back2()
                End If
                If xy2 = 813 And PictureBox235.Visible = True Then
                    Call back2()
                End If
                If xy2 = 902 And PictureBox279.Visible = True Then
                    Call back2()
                End If
                If xy2 = 904 And PictureBox277.Visible = True Then
                    Call back2()
                End If
                If xy2 = 905 And PictureBox276.Visible = True Then
                    Call back2()
                End If
                If xy2 = 906 And PictureBox275.Visible = True Then
                    Call back2()
                End If
                If xy2 = 908 And PictureBox273.Visible = True Then
                    Call back2()
                End If
                If xy2 = 909 And PictureBox272.Visible = True Then
                    Call back2()
                End If
                If xy2 = 910 And PictureBox271.Visible = True Then
                    Call back2()
                End If
                If xy2 = 912 And PictureBox269.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1002 And PictureBox312.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1004 And PictureBox310.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1005 And PictureBox309.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1006 And PictureBox308.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1008 And PictureBox306.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1010 And PictureBox304.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1012 And PictureBox302.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1013 And PictureBox301.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1102 And PictureBox345.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1103 And PictureBox344.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1104 And PictureBox343.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1106 And PictureBox341.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1107 And PictureBox340.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1108 And PictureBox339.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1110 And PictureBox337.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1111 And PictureBox336.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1112 And PictureBox335.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1202 And PictureBox378.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1203 And PictureBox377.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1204 And PictureBox376.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1206 And PictureBox374.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1207 And PictureBox373.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1208 And PictureBox372.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1210 And PictureBox370.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1211 And PictureBox369.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1212 And PictureBox368.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1213 And PictureBox367.Visible = True Then
                    Call back2()
                End If
            ElseIf map = 2 Then
                If xy2 = 101 And PictureBox104.Visible = True Then
                    Call back2()
                End If
                If xy2 = 102 And PictureBox2.Visible = True Then
                    Call back2()
                End If
                If xy2 = 103 And PictureBox3.Visible = True Then
                    Call back2()
                End If
                If xy2 = 104 And PictureBox4.Visible = True Then
                    Call back2()
                End If
                If xy2 = 105 And PictureBox5.Visible = True Then
                    Call back2()
                End If
                If xy2 = 106 And PictureBox6.Visible = True Then
                    Call back2()
                End If
                If xy2 = 107 And PictureBox7.Visible = True Then
                    Call back2()
                End If
                If xy2 = 108 And PictureBox8.Visible = True Then
                    Call back2()
                End If
                If xy2 = 109 And PictureBox9.Visible = True Then
                    Call back2()
                End If
                If xy2 = 110 And PictureBox10.Visible = True Then
                    Call back2()
                End If
                If xy2 = 111 And PictureBox11.Visible = True Then
                    Call back2()
                End If
                If xy2 = 112 And PictureBox12.Visible = True Then
                    Call back2()
                End If
                If xy2 = 201 And PictureBox26.Visible = True Then
                    Call back2()
                End If
                If xy2 = 202 And PictureBox25.Visible = True Then
                    Call back2()
                End If
                If xy2 = 204 And PictureBox23.Visible = True Then
                    Call back2()
                End If
                If xy2 = 206 And PictureBox21.Visible = True Then
                    Call back2()
                End If
                If xy2 = 207 And PictureBox20.Visible = True Then
                    Call back2()
                End If
                If xy2 = 208 And PictureBox19.Visible = True Then
                    Call back2()
                End If
                If xy2 = 211 And PictureBox16.Visible = True Then
                    Call back2()
                End If
                If xy2 = 301 And PictureBox59.Visible = True Then
                    Call back2()
                End If
                If xy2 = 302 And PictureBox58.Visible = True Then
                    Call back2()
                End If
                If xy2 = 303 And PictureBox57.Visible = True Then
                    Call back2()
                End If
                If xy2 = 304 And PictureBox56.Visible = True Then
                    Call back2()
                End If
                If xy2 = 306 And PictureBox54.Visible = True Then
                    Call back2()
                End If
                If xy2 = 308 And PictureBox52.Visible = True Then
                    Call back2()
                End If
                If xy2 = 310 And PictureBox50.Visible = True Then
                    Call back2()
                End If
                If xy2 = 311 And PictureBox49.Visible = True Then
                    Call back2()
                End If
                If xy2 = 312 And PictureBox48.Visible = True Then
                    Call back2()
                End If
                If xy2 = 313 And PictureBox47.Visible = True Then
                    Call back2()
                End If
                If xy2 = 401 And PictureBox92.Visible = True Then
                    Call back2()
                End If
                If xy2 = 402 And PictureBox91.Visible = True Then
                    Call back2()
                End If
                If xy2 = 404 And PictureBox89.Visible = True Then
                    Call back2()
                End If
                If xy2 = 405 And PictureBox88.Visible = True Then
                    Call back2()
                End If
                If xy2 = 406 And PictureBox87.Visible = True Then
                    Call back2()
                End If
                If xy2 = 407 And PictureBox86.Visible = True Then
                    Call back2()
                End If
                If xy2 = 408 And PictureBox85.Visible = True Then
                    Call back2()
                End If
                If xy2 = 412 And PictureBox81.Visible = True Then
                    Call back2()
                End If
                If xy2 = 413 And PictureBox80.Visible = True Then
                    Call back2()
                End If
                If xy2 = 501 And PictureBox148.Visible = True Then
                    Call back2()
                End If
                If xy2 = 502 And PictureBox147.Visible = True Then
                    Call back2()
                End If
                If xy2 = 504 And PictureBox145.Visible = True Then
                    Call back2()
                End If
                If xy2 = 506 And PictureBox123.Visible = True Then
                    Call back2()
                End If
                If xy2 = 508 And PictureBox121.Visible = True Then
                    Call back2()
                End If
                If xy2 = 509 And PictureBox120.Visible = True Then
                    Call back2()
                End If
                If xy2 = 510 And PictureBox119.Visible = True Then
                    Call back2()
                End If
                If xy2 = 511 And PictureBox118.Visible = True Then
                    Call back2()
                End If
                If xy2 = 512 And PictureBox117.Visible = True Then
                    Call back2()
                End If
                If xy2 = 513 And PictureBox116.Visible = True Then
                    Call back2()
                End If
                If xy2 = 601 And PictureBox181.Visible = True Then
                    Call back2()
                End If
                If xy2 = 602 And PictureBox180.Visible = True Then
                    Call back2()
                End If
                If xy2 = 604 And PictureBox178.Visible = True Then
                    Call back2()
                End If
                If xy2 = 605 And PictureBox177.Visible = True Then
                    Call back2()
                End If
                If xy2 = 606 And PictureBox176.Visible = True Then
                    Call back2()
                End If
                If xy2 = 607 And PictureBox175.Visible = True Then
                    Call back2()
                End If
                If xy2 = 608 And PictureBox174.Visible = True Then
                    Call back2()
                End If
                If xy2 = 609 And PictureBox173.Visible = True Then
                    Call back2()
                End If
                If xy2 = 612 And PictureBox170.Visible = True Then
                    Call back2()
                End If
                If xy2 = 613 And PictureBox169.Visible = True Then
                    Call back2()
                End If
                If xy2 = 701 And PictureBox214.Visible = True Then
                    Call back2()
                End If
                If xy2 = 702 And PictureBox213.Visible = True Then
                    Call back2()
                End If
                If xy2 = 704 And PictureBox211.Visible = True Then
                    Call back2()
                End If
                If xy2 = 705 And PictureBox210.Visible = True Then
                    Call back2()
                End If
                If xy2 = 707 And PictureBox208.Visible = True Then
                    Call back2()
                End If
                If xy2 = 708 And PictureBox207.Visible = True Then
                    Call back2()
                End If
                If xy2 = 709 And PictureBox206.Visible = True Then
                    Call back2()
                End If
                If xy2 = 710 And PictureBox205.Visible = True Then
                    Call back2()
                End If
                If xy2 = 711 And PictureBox204.Visible = True Then
                    Call back2()
                End If
                If xy2 = 713 And PictureBox202.Visible = True Then
                    Call back2()
                End If
                If xy2 = 801 And PictureBox247.Visible = True Then
                    Call back2()
                End If
                If xy2 = 802 And PictureBox246.Visible = True Then
                    Call back2()
                End If
                If xy2 = 803 And PictureBox245.Visible = True Then
                    Call back2()
                End If
                If xy2 = 805 And PictureBox243.Visible = True Then
                    Call back2()
                End If
                If xy2 = 806 And PictureBox242.Visible = True Then
                    Call back2()
                End If
                If xy2 = 807 And PictureBox241.Visible = True Then
                    Call back2()
                End If
                If xy2 = 809 And PictureBox239.Visible = True Then
                    Call back2()
                End If
                If xy2 = 810 And PictureBox238.Visible = True Then
                    Call back2()
                End If
                If xy2 = 811 And PictureBox237.Visible = True Then
                    Call back2()
                End If
                If xy2 = 812 And PictureBox236.Visible = True Then
                    Call back2()
                End If
                If xy2 = 901 And PictureBox280.Visible = True Then
                    Call back2()
                End If
                If xy2 = 902 And PictureBox279.Visible = True Then
                    Call back2()
                End If
                If xy2 = 903 And PictureBox278.Visible = True Then
                    Call back2()
                End If
                If xy2 = 904 And PictureBox277.Visible = True Then
                    Call back2()
                End If
                If xy2 = 906 And PictureBox275.Visible = True Then
                    Call back2()
                End If
                If xy2 = 907 And PictureBox274.Visible = True Then
                    Call back2()
                End If
                If xy2 = 908 And PictureBox273.Visible = True Then
                    Call back2()
                End If
                If xy2 = 909 And PictureBox272.Visible = True Then
                    Call back2()
                End If
                If xy2 = 911 And PictureBox270.Visible = True Then
                    Call back2()
                End If
                If xy2 = 912 And PictureBox269.Visible = True Then
                    Call back2()
                End If
                If xy2 = 913 And PictureBox268.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1001 And PictureBox313.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1003 And PictureBox311.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1005 And PictureBox309.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1007 And PictureBox307.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1009 And PictureBox305.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1010 And PictureBox304.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1012 And PictureBox302.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1101 And PictureBox346.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1102 And PictureBox345.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1103 And PictureBox344.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1104 And PictureBox343.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1105 And PictureBox342.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1106 And PictureBox341.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1107 And PictureBox340.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1108 And PictureBox339.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1109 And PictureBox338.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1110 And PictureBox337.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1111 And PictureBox336.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1112 And PictureBox335.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1113 And PictureBox334.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1204 And PictureBox376.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1205 And PictureBox375.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1206 And PictureBox374.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1207 And PictureBox373.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1208 And PictureBox372.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1209 And PictureBox371.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1210 And PictureBox370.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1212 And PictureBox368.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1213 And PictureBox367.Visible = True Then
                    Call back2()
                End If
            ElseIf map = 3 Then
                If xy2 = 101 And PictureBox104.Visible = True Then
                    Call back2()
                End If
                If xy2 = 102 And PictureBox2.Visible = True Then
                    Call back2()
                End If
                If xy2 = 103 And PictureBox3.Visible = True Then
                    Call back2()
                End If
                If xy2 = 104 And PictureBox4.Visible = True Then
                    Call back2()
                End If
                If xy2 = 105 And PictureBox5.Visible = True Then
                    Call back2()
                End If
                If xy2 = 106 And PictureBox6.Visible = True Then
                    Call back2()
                End If
                If xy2 = 107 And PictureBox7.Visible = True Then
                    Call back2()
                End If
                If xy2 = 108 And PictureBox8.Visible = True Then
                    Call back2()
                End If
                If xy2 = 110 And PictureBox10.Visible = True Then
                    Call back2()
                End If
                If xy2 = 112 And PictureBox12.Visible = True Then
                    Call back2()
                End If
                If xy2 = 201 And PictureBox26.Visible = True Then
                    Call back2()
                End If
                If xy2 = 202 And PictureBox25.Visible = True Then
                    Call back2()
                End If
                If xy2 = 203 And PictureBox24.Visible = True Then
                    Call back2()
                End If
                If xy2 = 205 And PictureBox22.Visible = True Then
                    Call back2()
                End If
                If xy2 = 207 And PictureBox20.Visible = True Then
                    Call back2()
                End If
                If xy2 = 209 And PictureBox18.Visible = True Then
                    Call back2()
                End If
                If xy2 = 210 And PictureBox17.Visible = True Then
                    Call back2()
                End If
                If xy2 = 211 And PictureBox16.Visible = True Then
                    Call back2()
                End If
                If xy2 = 301 And PictureBox59.Visible = True Then
                    Call back2()
                End If
                If xy2 = 302 And PictureBox58.Visible = True Then
                    Call back2()
                End If
                If xy2 = 303 And PictureBox57.Visible = True Then
                    Call back2()
                End If
                If xy2 = 304 And PictureBox56.Visible = True Then
                    Call back2()
                End If
                If xy2 = 305 And PictureBox55.Visible = True Then
                    Call back2()
                End If
                If xy2 = 306 And PictureBox54.Visible = True Then
                    Call back2()
                End If
                If xy2 = 307 And PictureBox53.Visible = True Then
                    Call back2()
                End If
                If xy2 = 308 And PictureBox52.Visible = True Then
                    Call back2()
                End If
                If xy2 = 310 And PictureBox50.Visible = True Then
                    Call back2()
                End If
                If xy2 = 312 And PictureBox48.Visible = True Then
                    Call back2()
                End If
                If xy2 = 313 And PictureBox47.Visible = True Then
                    Call back2()
                End If
                If xy2 = 402 And PictureBox91.Visible = True Then
                    Call back2()
                End If
                If xy2 = 404 And PictureBox89.Visible = True Then
                    Call back2()
                End If
                If xy2 = 405 And PictureBox88.Visible = True Then
                    Call back2()
                End If
                If xy2 = 406 And PictureBox87.Visible = True Then
                    Call back2()
                End If
                If xy2 = 407 And PictureBox86.Visible = True Then
                    Call back2()
                End If
                If xy2 = 408 And PictureBox85.Visible = True Then
                    Call back2()
                End If
                If xy2 = 409 And PictureBox84.Visible = True Then
                    Call back2()
                End If
                If xy2 = 410 And PictureBox83.Visible = True Then
                    Call back2()
                End If
                If xy2 = 411 And PictureBox82.Visible = True Then
                    Call back2()
                End If
                If xy2 = 413 And PictureBox80.Visible = True Then
                    Call back2()
                End If
                If xy2 = 501 And PictureBox148.Visible = True Then
                    Call back2()
                End If
                If xy2 = 502 And PictureBox147.Visible = True Then
                    Call back2()
                End If
                If xy2 = 503 And PictureBox146.Visible = True Then
                    Call back2()
                End If
                If xy2 = 505 And PictureBox1.Visible = True Then
                    Call back2()
                End If
                If xy2 = 506 And PictureBox123.Visible = True Then
                    Call back2()
                End If
                If xy2 = 507 And PictureBox122.Visible = True Then
                    Call back2()
                End If
                If xy2 = 508 And PictureBox121.Visible = True Then
                    Call back2()
                End If
                If xy2 = 510 And PictureBox119.Visible = True Then
                    Call back2()
                End If
                If xy2 = 511 And PictureBox118.Visible = True Then
                    Call back2()
                End If
                If xy2 = 512 And PictureBox117.Visible = True Then
                    Call back2()
                End If
                If xy2 = 513 And PictureBox116.Visible = True Then
                    Call back2()
                End If
                If xy2 = 601 And PictureBox181.Visible = True Then
                    Call back2()
                End If
                If xy2 = 602 And PictureBox180.Visible = True Then
                    Call back2()
                End If
                If xy2 = 604 And PictureBox178.Visible = True Then
                    Call back2()
                End If
                If xy2 = 606 And PictureBox176.Visible = True Then
                    Call back2()
                End If
                If xy2 = 607 And PictureBox175.Visible = True Then
                    Call back2()
                End If
                If xy2 = 608 And PictureBox174.Visible = True Then
                    Call back2()
                End If
                If xy2 = 609 And PictureBox173.Visible = True Then
                    Call back2()
                End If
                If xy2 = 611 And PictureBox171.Visible = True Then
                    Call back2()
                End If
                If xy2 = 612 And PictureBox170.Visible = True Then
                    Call back2()
                End If
                If xy2 = 613 And PictureBox169.Visible = True Then
                    Call back2()
                End If
                If xy2 = 701 And PictureBox214.Visible = True Then
                    Call back2()
                End If
                If xy2 = 702 And PictureBox213.Visible = True Then
                    Call back2()
                End If
                If xy2 = 703 And PictureBox212.Visible = True Then
                    Call back2()
                End If
                If xy2 = 704 And PictureBox211.Visible = True Then
                    Call back2()
                End If
                If xy2 = 705 And PictureBox210.Visible = True Then
                    Call back2()
                End If
                If xy2 = 706 And PictureBox209.Visible = True Then
                    Call back2()
                End If
                If xy2 = 707 And PictureBox208.Visible = True Then
                    Call back2()
                End If
                If xy2 = 708 And PictureBox207.Visible = True Then
                    Call back2()
                End If
                If xy2 = 709 And PictureBox206.Visible = True Then
                    Call back2()
                End If
                If xy2 = 710 And PictureBox205.Visible = True Then
                    Call back2()
                End If
                If xy2 = 712 And PictureBox203.Visible = True Then
                    Call back2()
                End If
                If xy2 = 713 And PictureBox202.Visible = True Then
                    Call back2()
                End If
                If xy2 = 801 And PictureBox247.Visible = True Then
                    Call back2()
                End If
                If xy2 = 803 And PictureBox245.Visible = True Then
                    Call back2()
                End If
                If xy2 = 804 And PictureBox244.Visible = True Then
                    Call back2()
                End If
                If xy2 = 805 And PictureBox243.Visible = True Then
                    Call back2()
                End If
                If xy2 = 806 And PictureBox242.Visible = True Then
                    Call back2()
                End If
                If xy2 = 808 And PictureBox240.Visible = True Then
                    Call back2()
                End If
                If xy2 = 809 And PictureBox239.Visible = True Then
                    Call back2()
                End If
                If xy2 = 810 And PictureBox238.Visible = True Then
                    Call back2()
                End If
                If xy2 = 811 And PictureBox237.Visible = True Then
                    Call back2()
                End If
                If xy2 = 812 And PictureBox236.Visible = True Then
                    Call back2()
                End If
                If xy2 = 901 And PictureBox280.Visible = True Then
                    Call back2()
                End If
                If xy2 = 902 And PictureBox279.Visible = True Then
                    Call back2()
                End If
                If xy2 = 903 And PictureBox278.Visible = True Then
                    Call back2()
                End If
                If xy2 = 905 And PictureBox276.Visible = True Then
                    Call back2()
                End If
                If xy2 = 906 And PictureBox275.Visible = True Then
                    Call back2()
                End If
                If xy2 = 907 And PictureBox274.Visible = True Then
                    Call back2()
                End If
                If xy2 = 909 And PictureBox272.Visible = True Then
                    Call back2()
                End If
                If xy2 = 910 And PictureBox271.Visible = True Then
                    Call back2()
                End If
                If xy2 = 913 And PictureBox268.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1001 And PictureBox313.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1002 And PictureBox312.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1003 And PictureBox311.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1004 And PictureBox310.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1005 And PictureBox309.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1006 And PictureBox308.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1008 And PictureBox306.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1009 And PictureBox305.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1011 And PictureBox303.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1012 And PictureBox302.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1013 And PictureBox301.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1103 And PictureBox344.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1105 And PictureBox342.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1107 And PictureBox340.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1109 And PictureBox338.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1111 And PictureBox336.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1112 And PictureBox335.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1113 And PictureBox334.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1202 And PictureBox378.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1203 And PictureBox377.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1204 And PictureBox376.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1205 And PictureBox375.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1206 And PictureBox374.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1207 And PictureBox373.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1208 And PictureBox372.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1209 And PictureBox371.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1211 And PictureBox369.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1213 And PictureBox367.Visible = True Then
                    Call back2()
                End If
            ElseIf map = 4 Then
                If xy2 = 103 And PictureBox3.Visible = True Then
                    Call back2()
                End If
                If xy2 = 105 And PictureBox5.Visible = True Then
                    Call back2()
                End If
                If xy2 = 106 And PictureBox6.Visible = True Then
                    Call back2()
                End If
                If xy2 = 107 And PictureBox7.Visible = True Then
                    Call back2()
                End If
                If xy2 = 108 And PictureBox8.Visible = True Then
                    Call back2()
                End If
                If xy2 = 109 And PictureBox9.Visible = True Then
                    Call back2()
                End If
                If xy2 = 111 And PictureBox11.Visible = True Then
                    Call back2()
                End If
                If xy2 = 202 And PictureBox25.Visible = True Then
                    Call back2()
                End If
                If xy2 = 203 And PictureBox24.Visible = True Then
                    Call back2()
                End If
                If xy2 = 204 And PictureBox23.Visible = True Then
                    Call back2()
                End If
                If xy2 = 205 And PictureBox22.Visible = True Then
                    Call back2()
                End If
                If xy2 = 206 And PictureBox21.Visible = True Then
                    Call back2()
                End If
                If xy2 = 207 And PictureBox20.Visible = True Then
                    Call back2()
                End If
                If xy2 = 208 And PictureBox19.Visible = True Then
                    Call back2()
                End If
                If xy2 = 209 And PictureBox18.Visible = True Then
                    Call back2()
                End If
                If xy2 = 210 And PictureBox17.Visible = True Then
                    Call back2()
                End If
                If xy2 = 211 And PictureBox16.Visible = True Then
                    Call back2()
                End If
                If xy2 = 212 And PictureBox15.Visible = True Then
                    Call back2()
                End If
                If xy2 = 301 And PictureBox59.Visible = True Then
                    Call back2()
                End If
                If xy2 = 302 And PictureBox58.Visible = True Then
                    Call back2()
                End If
                If xy2 = 303 And PictureBox57.Visible = True Then
                    Call back2()
                End If
                If xy2 = 304 And PictureBox56.Visible = True Then
                    Call back2()
                End If
                If xy2 = 305 And PictureBox55.Visible = True Then
                    Call back2()
                End If
                If xy2 = 306 And PictureBox54.Visible = True Then
                    Call back2()
                End If
                If xy2 = 307 And PictureBox53.Visible = True Then
                    Call back2()
                End If
                If xy2 = 308 And PictureBox52.Visible = True Then
                    Call back2()
                End If
                If xy2 = 309 And PictureBox51.Visible = True Then
                    Call back2()
                End If
                If xy2 = 310 And PictureBox50.Visible = True Then
                    Call back2()
                End If
                If xy2 = 311 And PictureBox49.Visible = True Then
                    Call back2()
                End If
                If xy2 = 312 And PictureBox48.Visible = True Then
                    Call back2()
                End If
                If xy2 = 313 And PictureBox47.Visible = True Then
                    Call back2()
                End If
                If xy2 = 402 And PictureBox91.Visible = True Then
                    Call back2()
                End If
                If xy2 = 403 And PictureBox90.Visible = True Then
                    Call back2()
                End If
                If xy2 = 404 And PictureBox89.Visible = True Then
                    Call back2()
                End If
                If xy2 = 405 And PictureBox88.Visible = True Then
                    Call back2()
                End If
                If xy2 = 406 And PictureBox87.Visible = True Then
                    Call back2()
                End If
                If xy2 = 407 And PictureBox86.Visible = True Then
                    Call back2()
                End If
                If xy2 = 408 And PictureBox85.Visible = True Then
                    Call back2()
                End If
                If xy2 = 409 And PictureBox84.Visible = True Then
                    Call back2()
                End If
                If xy2 = 410 And PictureBox83.Visible = True Then
                    Call back2()
                End If
                If xy2 = 411 And PictureBox82.Visible = True Then
                    Call back2()
                End If
                If xy2 = 412 And PictureBox81.Visible = True Then
                    Call back2()
                End If
                If xy2 = 501 And PictureBox148.Visible = True Then
                    Call back2()
                End If
                If xy2 = 502 And PictureBox147.Visible = True Then
                    Call back2()
                End If
                If xy2 = 503 And PictureBox146.Visible = True Then
                    Call back2()
                End If
                If xy2 = 504 And PictureBox145.Visible = True Then
                    Call back2()
                End If
                If xy2 = 505 And PictureBox1.Visible = True Then
                    Call back2()
                End If
                If xy2 = 506 And PictureBox123.Visible = True Then
                    Call back2()
                End If
                If xy2 = 507 And PictureBox122.Visible = True Then
                    Call back2()
                End If
                If xy2 = 508 And PictureBox121.Visible = True Then
                    Call back2()
                End If
                If xy2 = 509 And PictureBox120.Visible = True Then
                    Call back2()
                End If
                If xy2 = 510 And PictureBox119.Visible = True Then
                    Call back2()
                End If
                If xy2 = 511 And PictureBox118.Visible = True Then
                    Call back2()
                End If
                If xy2 = 512 And PictureBox117.Visible = True Then
                    Call back2()
                End If
                If xy2 = 513 And PictureBox116.Visible = True Then
                    Call back2()
                End If
                If xy2 = 601 And PictureBox181.Visible = True Then
                    Call back2()
                End If
                If xy2 = 602 And PictureBox180.Visible = True Then
                    Call back2()
                End If
                If xy2 = 603 And PictureBox179.Visible = True Then
                    Call back2()
                End If
                If xy2 = 604 And PictureBox178.Visible = True Then
                    Call back2()
                End If
                If xy2 = 605 And PictureBox177.Visible = True Then
                    Call back2()
                End If
                If xy2 = 606 And PictureBox176.Visible = True Then
                    Call back2()
                End If
                If xy2 = 607 And PictureBox175.Visible = True Then
                    Call back2()
                End If
                If xy2 = 608 And PictureBox174.Visible = True Then
                    Call back2()
                End If
                If xy2 = 609 And PictureBox173.Visible = True Then
                    Call back2()
                End If
                If xy2 = 610 And PictureBox172.Visible = True Then
                    Call back2()
                End If
                If xy2 = 611 And PictureBox171.Visible = True Then
                    Call back2()
                End If
                If xy2 = 612 And PictureBox170.Visible = True Then
                    Call back2()
                End If
                If xy2 = 613 And PictureBox169.Visible = True Then
                    Call back2()
                End If
                If xy2 = 701 And PictureBox214.Visible = True Then
                    Call back2()
                End If
                If xy2 = 702 And PictureBox213.Visible = True Then
                    Call back2()
                End If
                If xy2 = 703 And PictureBox212.Visible = True Then
                    Call back2()
                End If
                If xy2 = 704 And PictureBox211.Visible = True Then
                    Call back2()
                End If
                If xy2 = 705 And PictureBox210.Visible = True Then
                    Call back2()
                End If
                If xy2 = 706 And PictureBox209.Visible = True Then
                    Call back2()
                End If
                If xy2 = 707 And PictureBox208.Visible = True Then
                    Call back2()
                End If
                If xy2 = 708 And PictureBox207.Visible = True Then
                    Call back2()
                End If
                If xy2 = 709 And PictureBox206.Visible = True Then
                    Call back2()
                End If
                If xy2 = 710 And PictureBox205.Visible = True Then
                    Call back2()
                End If
                If xy2 = 711 And PictureBox204.Visible = True Then
                    Call back2()
                End If
                If xy2 = 712 And PictureBox203.Visible = True Then
                    Call back2()
                End If
                If xy2 = 713 And PictureBox202.Visible = True Then
                    Call back2()
                End If
                If xy2 = 801 And PictureBox247.Visible = True Then
                    Call back2()
                End If
                If xy2 = 802 And PictureBox246.Visible = True Then
                    Call back2()
                End If
                If xy2 = 803 And PictureBox245.Visible = True Then
                    Call back2()
                End If
                If xy2 = 804 And PictureBox244.Visible = True Then
                    Call back2()
                End If
                If xy2 = 805 And PictureBox243.Visible = True Then
                    Call back2()
                End If
                If xy2 = 806 And PictureBox242.Visible = True Then
                    Call back2()
                End If
                If xy2 = 807 And PictureBox241.Visible = True Then
                    Call back2()
                End If
                If xy2 = 808 And PictureBox240.Visible = True Then
                    Call back2()
                End If
                If xy2 = 809 And PictureBox239.Visible = True Then
                    Call back2()
                End If
                If xy2 = 810 And PictureBox238.Visible = True Then
                    Call back2()
                End If
                If xy2 = 811 And PictureBox237.Visible = True Then
                    Call back2()
                End If
                If xy2 = 812 And PictureBox236.Visible = True Then
                    Call back2()
                End If
                If xy2 = 813 And PictureBox235.Visible = True Then
                    Call back2()
                End If
                If xy2 = 902 And PictureBox279.Visible = True Then
                    Call back2()
                End If
                If xy2 = 903 And PictureBox278.Visible = True Then
                    Call back2()
                End If
                If xy2 = 904 And PictureBox277.Visible = True Then
                    Call back2()
                End If
                If xy2 = 905 And PictureBox276.Visible = True Then
                    Call back2()
                End If
                If xy2 = 906 And PictureBox275.Visible = True Then
                    Call back2()
                End If
                If xy2 = 907 And PictureBox274.Visible = True Then
                    Call back2()
                End If
                If xy2 = 908 And PictureBox273.Visible = True Then
                    Call back2()
                End If
                If xy2 = 909 And PictureBox272.Visible = True Then
                    Call back2()
                End If
                If xy2 = 910 And PictureBox271.Visible = True Then
                    Call back2()
                End If
                If xy2 = 911 And PictureBox270.Visible = True Then
                    Call back2()
                End If
                If xy2 = 912 And PictureBox269.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1001 And PictureBox313.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1002 And PictureBox312.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1003 And PictureBox311.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1004 And PictureBox310.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1005 And PictureBox309.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1006 And PictureBox308.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1007 And PictureBox307.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1008 And PictureBox306.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1009 And PictureBox305.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1010 And PictureBox304.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1011 And PictureBox303.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1012 And PictureBox302.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1013 And PictureBox301.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1102 And PictureBox345.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1103 And PictureBox344.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1104 And PictureBox343.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1105 And PictureBox342.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1106 And PictureBox341.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1107 And PictureBox340.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1108 And PictureBox339.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1109 And PictureBox338.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1110 And PictureBox337.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1111 And PictureBox336.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1112 And PictureBox335.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1203 And PictureBox377.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1205 And PictureBox375.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1206 And PictureBox374.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1207 And PictureBox373.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1208 And PictureBox372.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1209 And PictureBox371.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1211 And PictureBox369.Visible = True Then
                    Call back2()
                End If
            ElseIf map = 5 Then
                If xy2 = 101 And PictureBox104.Visible = True Then
                    Call back2()
                End If
                If xy2 = 102 And PictureBox2.Visible = True Then
                    Call back2()
                End If
                If xy2 = 105 And PictureBox5.Visible = True Then
                    Call back2()
                End If
                If xy2 = 106 And PictureBox6.Visible = True Then
                    Call back2()
                End If
                If xy2 = 107 And PictureBox7.Visible = True Then
                    Call back2()
                End If
                If xy2 = 108 And PictureBox8.Visible = True Then
                    Call back2()
                End If
                If xy2 = 109 And PictureBox9.Visible = True Then
                    Call back2()
                End If
                If xy2 = 110 And PictureBox10.Visible = True Then
                    Call back2()
                End If
                If xy2 = 201 And PictureBox26.Visible = True Then
                    Call back2()
                End If
                If xy2 = 202 And PictureBox25.Visible = True Then
                    Call back2()
                End If
                If xy2 = 205 And PictureBox22.Visible = True Then
                    Call back2()
                End If
                If xy2 = 206 And PictureBox21.Visible = True Then
                    Call back2()
                End If
                If xy2 = 207 And PictureBox20.Visible = True Then
                    Call back2()
                End If
                If xy2 = 208 And PictureBox19.Visible = True Then
                    Call back2()
                End If
                If xy2 = 209 And PictureBox18.Visible = True Then
                    Call back2()
                End If
                If xy2 = 301 And PictureBox59.Visible = True Then
                    Call back2()
                End If
                If xy2 = 303 And PictureBox57.Visible = True Then
                    Call back2()
                End If
                If xy2 = 305 And PictureBox55.Visible = True Then
                    Call back2()
                End If
                If xy2 = 306 And PictureBox54.Visible = True Then
                    Call back2()
                End If
                If xy2 = 307 And PictureBox53.Visible = True Then
                    Call back2()
                End If
                If xy2 = 308 And PictureBox52.Visible = True Then
                    Call back2()
                End If
                If xy2 = 309 And PictureBox51.Visible = True Then
                    Call back2()
                End If
                If xy2 = 403 And PictureBox90.Visible = True Then
                    Call back2()
                End If
                If xy2 = 405 And PictureBox88.Visible = True Then
                    Call back2()
                End If
                If xy2 = 406 And PictureBox87.Visible = True Then
                    Call back2()
                End If
                If xy2 = 407 And PictureBox86.Visible = True Then
                    Call back2()
                End If
                If xy2 = 408 And PictureBox85.Visible = True Then
                    Call back2()
                End If
                If xy2 = 409 And PictureBox84.Visible = True Then
                    Call back2()
                End If
                If xy2 = 410 And PictureBox83.Visible = True Then
                    Call back2()
                End If
                If xy2 = 412 And PictureBox81.Visible = True Then
                    Call back2()
                End If
                If xy2 = 413 And PictureBox80.Visible = True Then
                    Call back2()
                End If
                If xy2 = 503 And PictureBox146.Visible = True Then
                    Call back2()
                End If
                If xy2 = 504 And PictureBox145.Visible = True Then
                    Call back2()
                End If
                If xy2 = 505 And PictureBox1.Visible = True Then
                    Call back2()
                End If
                If xy2 = 506 And PictureBox123.Visible = True Then
                    Call back2()
                End If
                If xy2 = 507 And PictureBox122.Visible = True Then
                    Call back2()
                End If
                If xy2 = 508 And PictureBox121.Visible = True Then
                    Call back2()
                End If
                If xy2 = 509 And PictureBox120.Visible = True Then
                    Call back2()
                End If
                If xy2 = 510 And PictureBox119.Visible = True Then
                    Call back2()
                End If
                If xy2 = 512 And PictureBox117.Visible = True Then
                    Call back2()
                End If
                If xy2 = 513 And PictureBox116.Visible = True Then
                    Call back2()
                End If
                If xy2 = 601 And PictureBox181.Visible = True Then
                    Call back2()
                End If
                If xy2 = 607 And PictureBox175.Visible = True Then
                    Call back2()
                End If
                If xy2 = 608 And PictureBox174.Visible = True Then
                    Call back2()
                End If
                If xy2 = 612 And PictureBox170.Visible = True Then
                    Call back2()
                End If
                If xy2 = 613 And PictureBox169.Visible = True Then
                    Call back2()
                End If
                If xy2 = 701 And PictureBox214.Visible = True Then
                    Call back2()
                End If
                If xy2 = 707 And PictureBox208.Visible = True Then
                    Call back2()
                End If
                If xy2 = 708 And PictureBox207.Visible = True Then
                    Call back2()
                End If
                If xy2 = 713 And PictureBox202.Visible = True Then
                    Call back2()
                End If
                If xy2 = 801 And PictureBox247.Visible = True Then
                    Call back2()
                End If
                If xy2 = 802 And PictureBox246.Visible = True Then
                    Call back2()
                End If
                If xy2 = 804 And PictureBox244.Visible = True Then
                    Call back2()
                End If
                If xy2 = 805 And PictureBox243.Visible = True Then
                    Call back2()
                End If
                If xy2 = 806 And PictureBox242.Visible = True Then
                    Call back2()
                End If
                If xy2 = 807 And PictureBox241.Visible = True Then
                    Call back2()
                End If
                If xy2 = 808 And PictureBox240.Visible = True Then
                    Call back2()
                End If
                If xy2 = 809 And PictureBox239.Visible = True Then
                    Call back2()
                End If
                If xy2 = 810 And PictureBox238.Visible = True Then
                    Call back2()
                End If
                If xy2 = 811 And PictureBox237.Visible = True Then
                    Call back2()
                End If
                If xy2 = 813 And PictureBox235.Visible = True Then
                    Call back2()
                End If
                If xy2 = 901 And PictureBox280.Visible = True Then
                    Call back2()
                End If
                If xy2 = 902 And PictureBox279.Visible = True Then
                    Call back2()
                End If
                If xy2 = 904 And PictureBox277.Visible = True Then
                    Call back2()
                End If
                If xy2 = 905 And PictureBox276.Visible = True Then
                    Call back2()
                End If
                If xy2 = 906 And PictureBox275.Visible = True Then
                    Call back2()
                End If
                If xy2 = 907 And PictureBox274.Visible = True Then
                    Call back2()
                End If
                If xy2 = 908 And PictureBox273.Visible = True Then
                    Call back2()
                End If
                If xy2 = 909 And PictureBox272.Visible = True Then
                    Call back2()
                End If
                If xy2 = 911 And PictureBox270.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1005 And PictureBox309.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1006 And PictureBox308.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1007 And PictureBox307.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1008 And PictureBox306.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1009 And PictureBox305.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1011 And PictureBox303.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1013 And PictureBox301.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1105 And PictureBox342.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1106 And PictureBox341.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1107 And PictureBox340.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1108 And PictureBox339.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1109 And PictureBox338.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1112 And PictureBox335.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1113 And PictureBox334.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1204 And PictureBox376.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1205 And PictureBox375.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1206 And PictureBox374.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1207 And PictureBox373.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1208 And PictureBox372.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1209 And PictureBox371.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1210 And PictureBox370.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1212 And PictureBox368.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1213 And PictureBox367.Visible = True Then
                    Call back2()
                End If
            ElseIf map = 6 Then
                If xy2 = 101 And PictureBox104.Visible = True Then
                    Call back2()
                End If
                If xy2 = 102 And PictureBox2.Visible = True Then
                    Call back2()
                End If
                If xy2 = 201 And PictureBox26.Visible = True Then
                    Call back2()
                End If
                If xy2 = 202 And PictureBox25.Visible = True Then
                    Call back2()
                End If
                If xy2 = 210 And PictureBox17.Visible = True Then
                    Call back2()
                End If
                If xy2 = 303 And PictureBox57.Visible = True Then
                    Call back2()
                End If
                If xy2 = 304 And PictureBox56.Visible = True Then
                    Call back2()
                End If
                If xy2 = 305 And PictureBox55.Visible = True Then
                    Call back2()
                End If
                If xy2 = 306 And PictureBox54.Visible = True Then
                    Call back2()
                End If
                If xy2 = 307 And PictureBox53.Visible = True Then
                    Call back2()
                End If
                If xy2 = 309 And PictureBox51.Visible = True Then
                    Call back2()
                End If
                If xy2 = 310 And PictureBox50.Visible = True Then
                    Call back2()
                End If
                If xy2 = 313 And PictureBox47.Visible = True Then
                    Call back2()
                End If
                If xy2 = 403 And PictureBox90.Visible = True Then
                    Call back2()
                End If
                If xy2 = 404 And PictureBox89.Visible = True Then
                    Call back2()
                End If
                If xy2 = 405 And PictureBox88.Visible = True Then
                    Call back2()
                End If
                If xy2 = 406 And PictureBox87.Visible = True Then
                    Call back2()
                End If
                If xy2 = 407 And PictureBox86.Visible = True Then
                    Call back2()
                End If
                If xy2 = 408 And PictureBox85.Visible = True Then
                    Call back2()
                End If
                If xy2 = 409 And PictureBox84.Visible = True Then
                    Call back2()
                End If
                If xy2 = 410 And PictureBox83.Visible = True Then
                    Call back2()
                End If
                If xy2 = 413 And PictureBox80.Visible = True Then
                    Call back2()
                End If
                If xy2 = 503 And PictureBox146.Visible = True Then
                    Call back2()
                End If
                If xy2 = 504 And PictureBox145.Visible = True Then
                    Call back2()
                End If
                If xy2 = 505 And PictureBox1.Visible = True Then
                    Call back2()
                End If
                If xy2 = 506 And PictureBox123.Visible = True Then
                    Call back2()
                End If
                If xy2 = 507 And PictureBox122.Visible = True Then
                    Call back2()
                End If
                If xy2 = 508 And PictureBox121.Visible = True Then
                    Call back2()
                End If
                If xy2 = 509 And PictureBox120.Visible = True Then
                    Call back2()
                End If
                If xy2 = 510 And PictureBox119.Visible = True Then
                    Call back2()
                End If
                If xy2 = 511 And PictureBox118.Visible = True Then
                    Call back2()
                End If
                If xy2 = 512 And PictureBox117.Visible = True Then
                    Call back2()
                End If
                If xy2 = 513 And PictureBox116.Visible = True Then
                    Call back2()
                End If
                If xy2 = 603 And PictureBox179.Visible = True Then
                    Call back2()
                End If
                If xy2 = 604 And PictureBox178.Visible = True Then
                    Call back2()
                End If
                If xy2 = 605 And PictureBox177.Visible = True Then
                    Call back2()
                End If
                If xy2 = 606 And PictureBox176.Visible = True Then
                    Call back2()
                End If
                If xy2 = 607 And PictureBox175.Visible = True Then
                    Call back2()
                End If
                If xy2 = 608 And PictureBox174.Visible = True Then
                    Call back2()
                End If
                If xy2 = 609 And PictureBox173.Visible = True Then
                    Call back2()
                End If
                If xy2 = 611 And PictureBox171.Visible = True Then
                    Call back2()
                End If
                If xy2 = 703 And PictureBox212.Visible = True Then
                    Call back2()
                End If
                If xy2 = 704 And PictureBox211.Visible = True Then
                    Call back2()
                End If
                If xy2 = 705 And PictureBox210.Visible = True Then
                    Call back2()
                End If
                If xy2 = 706 And PictureBox209.Visible = True Then
                    Call back2()
                End If
                If xy2 = 707 And PictureBox208.Visible = True Then
                    Call back2()
                End If
                If xy2 = 708 And PictureBox207.Visible = True Then
                    Call back2()
                End If
                If xy2 = 709 And PictureBox206.Visible = True Then
                    Call back2()
                End If
                If xy2 = 710 And PictureBox205.Visible = True Then
                    Call back2()
                End If
                If xy2 = 711 And PictureBox204.Visible = True Then
                    Call back2()
                End If
                If xy2 = 801 And PictureBox247.Visible = True Then
                    Call back2()
                End If
                If xy2 = 802 And PictureBox246.Visible = True Then
                    Call back2()
                End If
                If xy2 = 803 And PictureBox245.Visible = True Then
                    Call back2()
                End If
                If xy2 = 804 And PictureBox244.Visible = True Then
                    Call back2()
                End If
                If xy2 = 805 And PictureBox243.Visible = True Then
                    Call back2()
                End If
                If xy2 = 806 And PictureBox242.Visible = True Then
                    Call back2()
                End If
                If xy2 = 807 And PictureBox241.Visible = True Then
                    Call back2()
                End If
                If xy2 = 808 And PictureBox240.Visible = True Then
                    Call back2()
                End If
                If xy2 = 809 And PictureBox239.Visible = True Then
                    Call back2()
                End If
                If xy2 = 810 And PictureBox238.Visible = True Then
                    Call back2()
                End If
                If xy2 = 811 And PictureBox237.Visible = True Then
                    Call back2()
                End If
                If xy2 = 901 And PictureBox280.Visible = True Then
                    Call back2()
                End If
                If xy2 = 904 And PictureBox277.Visible = True Then
                    Call back2()
                End If
                If xy2 = 905 And PictureBox276.Visible = True Then
                    Call back2()
                End If
                If xy2 = 906 And PictureBox275.Visible = True Then
                    Call back2()
                End If
                If xy2 = 907 And PictureBox274.Visible = True Then
                    Call back2()
                End If
                If xy2 = 908 And PictureBox273.Visible = True Then
                    Call back2()
                End If
                If xy2 = 909 And PictureBox272.Visible = True Then
                    Call back2()
                End If
                If xy2 = 910 And PictureBox271.Visible = True Then
                    Call back2()
                End If
                If xy2 = 911 And PictureBox270.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1001 And PictureBox313.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1004 And PictureBox310.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1005 And PictureBox309.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1006 And PictureBox308.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1007 And PictureBox307.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1008 And PictureBox306.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1009 And PictureBox305.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1010 And PictureBox304.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1011 And PictureBox303.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1104 And PictureBox343.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1112 And PictureBox335.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1113 And PictureBox334.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1204 And PictureBox376.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1212 And PictureBox368.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1213 And PictureBox367.Visible = True Then
                    Call back2()
                End If
            ElseIf map = 7 Then
                If xy2 = 102 And PictureBox2.Visible = True Then
                    Call back2()
                End If
                If xy2 = 103 And PictureBox3.Visible = True Then
                    Call back2()
                End If
                If xy2 = 104 And PictureBox4.Visible = True Then
                    Call back2()
                End If
                If xy2 = 105 And PictureBox5.Visible = True Then
                    Call back2()
                End If
                If xy2 = 109 And PictureBox9.Visible = True Then
                    Call back2()
                End If
                If xy2 = 110 And PictureBox10.Visible = True Then
                    Call back2()
                End If
                If xy2 = 111 And PictureBox11.Visible = True Then
                    Call back2()
                End If
                If xy2 = 201 And PictureBox26.Visible = True Then
                    Call back2()
                End If
                If xy2 = 202 And PictureBox25.Visible = True Then
                    Call back2()
                End If
                If xy2 = 203 And PictureBox24.Visible = True Then
                    Call back2()
                End If
                If xy2 = 204 And PictureBox23.Visible = True Then
                    Call back2()
                End If
                If xy2 = 205 And PictureBox22.Visible = True Then
                    Call back2()
                End If
                If xy2 = 206 And PictureBox21.Visible = True Then
                    Call back2()
                End If
                If xy2 = 207 And PictureBox20.Visible = True Then
                    Call back2()
                End If
                If xy2 = 208 And PictureBox19.Visible = True Then
                    Call back2()
                End If
                If xy2 = 209 And PictureBox18.Visible = True Then
                    Call back2()
                End If
                If xy2 = 210 And PictureBox17.Visible = True Then
                    Call back2()
                End If
                If xy2 = 211 And PictureBox16.Visible = True Then
                    Call back2()
                End If
                If xy2 = 212 And PictureBox15.Visible = True Then
                    Call back2()
                End If
                If xy2 = 302 And PictureBox58.Visible = True Then
                    Call back2()
                End If
                If xy2 = 303 And PictureBox57.Visible = True Then
                    Call back2()
                End If
                If xy2 = 304 And PictureBox56.Visible = True Then
                    Call back2()
                End If
                If xy2 = 305 And PictureBox55.Visible = True Then
                    Call back2()
                End If
                If xy2 = 306 And PictureBox54.Visible = True Then
                    Call back2()
                End If
                If xy2 = 307 And PictureBox53.Visible = True Then
                    Call back2()
                End If
                If xy2 = 308 And PictureBox52.Visible = True Then
                    Call back2()
                End If
                If xy2 = 309 And PictureBox51.Visible = True Then
                    Call back2()
                End If
                If xy2 = 310 And PictureBox50.Visible = True Then
                    Call back2()
                End If
                If xy2 = 311 And PictureBox49.Visible = True Then
                    Call back2()
                End If
                If xy2 = 312 And PictureBox48.Visible = True Then
                    Call back2()
                End If
                If xy2 = 402 And PictureBox91.Visible = True Then
                    Call back2()
                End If
                If xy2 = 403 And PictureBox90.Visible = True Then
                    Call back2()
                End If
                If xy2 = 404 And PictureBox89.Visible = True Then
                    Call back2()
                End If
                If xy2 = 405 And PictureBox88.Visible = True Then
                    Call back2()
                End If
                If xy2 = 406 And PictureBox87.Visible = True Then
                    Call back2()
                End If
                If xy2 = 407 And PictureBox86.Visible = True Then
                    Call back2()
                End If
                If xy2 = 408 And PictureBox85.Visible = True Then
                    Call back2()
                End If
                If xy2 = 409 And PictureBox84.Visible = True Then
                    Call back2()
                End If
                If xy2 = 410 And PictureBox83.Visible = True Then
                    Call back2()
                End If
                If xy2 = 411 And PictureBox82.Visible = True Then
                    Call back2()
                End If
                If xy2 = 412 And PictureBox81.Visible = True Then
                    Call back2()
                End If
                If xy2 = 501 And PictureBox148.Visible = True Then
                    Call back2()
                End If
                If xy2 = 502 And PictureBox147.Visible = True Then
                    Call back2()
                End If
                If xy2 = 503 And PictureBox146.Visible = True Then
                    Call back2()
                End If
                If xy2 = 504 And PictureBox145.Visible = True Then
                    Call back2()
                End If
                If xy2 = 505 And PictureBox1.Visible = True Then
                    Call back2()
                End If
                If xy2 = 506 And PictureBox123.Visible = True Then
                    Call back2()
                End If
                If xy2 = 507 And PictureBox122.Visible = True Then
                    Call back2()
                End If
                If xy2 = 508 And PictureBox121.Visible = True Then
                    Call back2()
                End If
                If xy2 = 509 And PictureBox120.Visible = True Then
                    Call back2()
                End If
                If xy2 = 510 And PictureBox119.Visible = True Then
                    Call back2()
                End If
                If xy2 = 511 And PictureBox118.Visible = True Then
                    Call back2()
                End If
                If xy2 = 512 And PictureBox117.Visible = True Then
                    Call back2()
                End If
                If xy2 = 513 And PictureBox116.Visible = True Then
                    Call back2()
                End If
                If xy2 = 601 And PictureBox181.Visible = True Then
                    Call back2()
                End If
                If xy2 = 602 And PictureBox180.Visible = True Then
                    Call back2()
                End If
                If xy2 = 603 And PictureBox179.Visible = True Then
                    Call back2()
                End If
                If xy2 = 604 And PictureBox178.Visible = True Then
                    Call back2()
                End If
                If xy2 = 605 And PictureBox177.Visible = True Then
                    Call back2()
                End If
                If xy2 = 606 And PictureBox176.Visible = True Then
                    Call back2()
                End If
                If xy2 = 607 And PictureBox175.Visible = True Then
                    Call back2()
                End If
                If xy2 = 608 And PictureBox174.Visible = True Then
                    Call back2()
                End If
                If xy2 = 609 And PictureBox173.Visible = True Then
                    Call back2()
                End If
                If xy2 = 610 And PictureBox172.Visible = True Then
                    Call back2()
                End If
                If xy2 = 611 And PictureBox171.Visible = True Then
                    Call back2()
                End If
                If xy2 = 612 And PictureBox170.Visible = True Then
                    Call back2()
                End If
                If xy2 = 613 And PictureBox169.Visible = True Then
                    Call back2()
                End If
                If xy2 = 701 And PictureBox214.Visible = True Then
                    Call back2()
                End If
                If xy2 = 702 And PictureBox213.Visible = True Then
                    Call back2()
                End If
                If xy2 = 703 And PictureBox212.Visible = True Then
                    Call back2()
                End If
                If xy2 = 704 And PictureBox211.Visible = True Then
                    Call back2()
                End If
                If xy2 = 705 And PictureBox210.Visible = True Then
                    Call back2()
                End If
                If xy2 = 706 And PictureBox209.Visible = True Then
                    Call back2()
                End If
                If xy2 = 707 And PictureBox208.Visible = True Then
                    Call back2()
                End If
                If xy2 = 708 And PictureBox207.Visible = True Then
                    Call back2()
                End If
                If xy2 = 709 And PictureBox206.Visible = True Then
                    Call back2()
                End If
                If xy2 = 710 And PictureBox205.Visible = True Then
                    Call back2()
                End If
                If xy2 = 711 And PictureBox204.Visible = True Then
                    Call back2()
                End If
                If xy2 = 712 And PictureBox203.Visible = True Then
                    Call back2()
                End If
                If xy2 = 713 And PictureBox202.Visible = True Then
                    Call back2()
                End If
                If xy2 = 801 And PictureBox247.Visible = True Then
                    Call back2()
                End If
                If xy2 = 802 And PictureBox246.Visible = True Then
                    Call back2()
                End If
                If xy2 = 803 And PictureBox245.Visible = True Then
                    Call back2()
                End If
                If xy2 = 804 And PictureBox244.Visible = True Then
                    Call back2()
                End If
                If xy2 = 805 And PictureBox243.Visible = True Then
                    Call back2()
                End If
                If xy2 = 806 And PictureBox242.Visible = True Then
                    Call back2()
                End If
                If xy2 = 807 And PictureBox241.Visible = True Then
                    Call back2()
                End If
                If xy2 = 808 And PictureBox240.Visible = True Then
                    Call back2()
                End If
                If xy2 = 809 And PictureBox239.Visible = True Then
                    Call back2()
                End If
                If xy2 = 810 And PictureBox238.Visible = True Then
                    Call back2()
                End If
                If xy2 = 811 And PictureBox237.Visible = True Then
                    Call back2()
                End If
                If xy2 = 812 And PictureBox236.Visible = True Then
                    Call back2()
                End If
                If xy2 = 901 And PictureBox280.Visible = True Then
                    Call back2()
                End If
                If xy2 = 902 And PictureBox279.Visible = True Then
                    Call back2()
                End If
                If xy2 = 903 And PictureBox278.Visible = True Then
                    Call back2()
                End If
                If xy2 = 904 And PictureBox277.Visible = True Then
                    Call back2()
                End If
                If xy2 = 905 And PictureBox276.Visible = True Then
                    Call back2()
                End If
                If xy2 = 906 And PictureBox275.Visible = True Then
                    Call back2()
                End If
                If xy2 = 907 And PictureBox274.Visible = True Then
                    Call back2()
                End If
                If xy2 = 908 And PictureBox273.Visible = True Then
                    Call back2()
                End If
                If xy2 = 909 And PictureBox272.Visible = True Then
                    Call back2()
                End If
                If xy2 = 910 And PictureBox271.Visible = True Then
                    Call back2()
                End If
                If xy2 = 911 And PictureBox270.Visible = True Then
                    Call back2()
                End If
                If xy2 = 912 And PictureBox269.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1002 And PictureBox312.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1003 And PictureBox311.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1004 And PictureBox310.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1005 And PictureBox309.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1006 And PictureBox308.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1007 And PictureBox307.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1008 And PictureBox306.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1009 And PictureBox305.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1010 And PictureBox304.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1011 And PictureBox303.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1012 And PictureBox302.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1102 And PictureBox345.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1103 And PictureBox344.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1104 And PictureBox343.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1105 And PictureBox342.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1106 And PictureBox341.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1107 And PictureBox340.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1108 And PictureBox339.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1109 And PictureBox338.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1110 And PictureBox337.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1111 And PictureBox336.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1112 And PictureBox335.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1204 And PictureBox376.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1205 And PictureBox375.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1206 And PictureBox374.Visible = True Then
                    Call back2()
                End If
                If xy2 = 1212 And PictureBox368.Visible = True Then
                    Call back2()
                End If

            End If
            If y2 = 14 Then
                Call back2()
            End If
            If x2 = 13 Then
                Call back2()
            End If
        End If
    End Sub

    '1PTNT炸[可破壞牆壁]判斷&執行(function)
    Private Sub tnt_1()
        If map = 0 Then
            If tnt1xy = 106 Or tnt1xy = 206 Or tnt1xy = 6 Or tnt1xy = 107 Or tnt1xy = 105 Then
                PictureBox6.Visible = False
            End If
            If tnt1xy = 107 Or tnt1xy = 207 Or tnt1xy = 7 Or tnt1xy = 108 Or tnt1xy = 106 Then
                If PictureBox7.Visible = True Then
                    Call point1()
                End If
                PictureBox7.Visible = False
            End If
            If tnt1xy = 108 Or tnt1xy = 208 Or tnt1xy = 8 Or tnt1xy = 109 Or tnt1xy = 107 Then
                PictureBox8.Visible = False
            End If
            If tnt1xy = 301 Or tnt1xy = 401 Or tnt1xy = 201 Or tnt1xy = 302 Or tnt1xy = 300 Then
                PictureBox59.Visible = False
            End If
            If tnt1xy = 303 Or tnt1xy = 403 Or tnt1xy = 203 Or tnt1xy = 304 Or tnt1xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt1xy = 307 Or tnt1xy = 407 Or tnt1xy = 207 Or tnt1xy = 308 Or tnt1xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point1()
                End If
                PictureBox53.Visible = False
            End If
            If tnt1xy = 311 Or tnt1xy = 411 Or tnt1xy = 211 Or tnt1xy = 312 Or tnt1xy = 310 Then
                PictureBox49.Visible = False
            End If
            If tnt1xy = 313 Or tnt1xy = 413 Or tnt1xy = 213 Or tnt1xy = 314 Or tnt1xy = 312 Then
                PictureBox47.Visible = False
            End If
            If tnt1xy = 401 Or tnt1xy = 501 Or tnt1xy = 301 Or tnt1xy = 402 Or tnt1xy = 400 Then
                PictureBox92.Visible = False
            End If
            If tnt1xy = 403 Or tnt1xy = 503 Or tnt1xy = 303 Or tnt1xy = 404 Or tnt1xy = 402 Then
                PictureBox90.Visible = False
            End If
            If tnt1xy = 405 Or tnt1xy = 505 Or tnt1xy = 305 Or tnt1xy = 406 Or tnt1xy = 404 Then
                If PictureBox88.Visible = True Then
                    Call point1()
                End If
                PictureBox88.Visible = False
            End If
            If tnt1xy = 406 Or tnt1xy = 506 Or tnt1xy = 306 Or tnt1xy = 407 Or tnt1xy = 405 Then
                PictureBox87.Visible = False
            End If
            If tnt1xy = 407 Or tnt1xy = 507 Or tnt1xy = 307 Or tnt1xy = 408 Or tnt1xy = 406 Then
                PictureBox86.Visible = False
            End If
            If tnt1xy = 408 Or tnt1xy = 508 Or tnt1xy = 308 Or tnt1xy = 409 Or tnt1xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt1xy = 409 Or tnt1xy = 509 Or tnt1xy = 309 Or tnt1xy = 410 Or tnt1xy = 408 Then
                If PictureBox84.Visible = True Then
                    Call point1()
                End If
                PictureBox84.Visible = False
            End If
            If tnt1xy = 411 Or tnt1xy = 511 Or tnt1xy = 311 Or tnt1xy = 412 Or tnt1xy = 410 Then
                PictureBox82.Visible = False
            End If
            If tnt1xy = 413 Or tnt1xy = 513 Or tnt1xy = 313 Or tnt1xy = 414 Or tnt1xy = 412 Then
                PictureBox80.Visible = False
            End If
            If tnt1xy = 501 Or tnt1xy = 601 Or tnt1xy = 401 Or tnt1xy = 502 Or tnt1xy = 500 Then
                PictureBox148.Visible = False
            End If
            If tnt1xy = 503 Or tnt1xy = 603 Or tnt1xy = 403 Or tnt1xy = 504 Or tnt1xy = 502 Then
                PictureBox146.Visible = False
            End If
            If tnt1xy = 505 Or tnt1xy = 605 Or tnt1xy = 405 Or tnt1xy = 506 Or tnt1xy = 504 Then
                PictureBox1.Visible = False
            End If
            If tnt1xy = 509 Or tnt1xy = 609 Or tnt1xy = 409 Or tnt1xy = 510 Or tnt1xy = 508 Then
                PictureBox120.Visible = False
            End If
            If tnt1xy = 511 Or tnt1xy = 611 Or tnt1xy = 411 Or tnt1xy = 512 Or tnt1xy = 510 Then
                PictureBox118.Visible = False
            End If
            If tnt1xy = 513 Or tnt1xy = 613 Or tnt1xy = 413 Or tnt1xy = 514 Or tnt1xy = 512 Then
                PictureBox116.Visible = False
            End If
            If tnt1xy = 601 Or tnt1xy = 701 Or tnt1xy = 501 Or tnt1xy = 602 Or tnt1xy = 600 Then
                If PictureBox181.Visible = True Then
                    Call point1()
                End If
                PictureBox181.Visible = False
            End If
            If tnt1xy = 603 Or tnt1xy = 703 Or tnt1xy = 503 Or tnt1xy = 604 Or tnt1xy = 602 Then
                PictureBox179.Visible = False
            End If
            If tnt1xy = 604 Or tnt1xy = 704 Or tnt1xy = 504 Or tnt1xy = 605 Or tnt1xy = 603 Then
                If PictureBox178.Visible = True Then
                    Call point1()
                End If
                PictureBox178.Visible = False
            End If
            If tnt1xy = 605 Or tnt1xy = 705 Or tnt1xy = 505 Or tnt1xy = 606 Or tnt1xy = 604 Then
                PictureBox177.Visible = False
            End If
            If tnt1xy = 606 Or tnt1xy = 706 Or tnt1xy = 506 Or tnt1xy = 607 Or tnt1xy = 605 Then
                PictureBox176.Visible = False
            End If
            If tnt1xy = 607 Or tnt1xy = 707 Or tnt1xy = 507 Or tnt1xy = 608 Or tnt1xy = 606 Then
                If PictureBox175.Visible = True Then
                    Call point1()
                End If
                PictureBox175.Visible = False
            End If
            If tnt1xy = 608 Or tnt1xy = 708 Or tnt1xy = 508 Or tnt1xy = 609 Or tnt1xy = 607 Then
                PictureBox174.Visible = False
            End If
            If tnt1xy = 609 Or tnt1xy = 709 Or tnt1xy = 509 Or tnt1xy = 610 Or tnt1xy = 608 Then
                PictureBox173.Visible = False
            End If
            If tnt1xy = 610 Or tnt1xy = 710 Or tnt1xy = 510 Or tnt1xy = 611 Or tnt1xy = 609 Then
                If PictureBox172.Visible = True Then
                    Call point1()
                End If
                PictureBox172.Visible = False
            End If
            If tnt1xy = 611 Or tnt1xy = 711 Or tnt1xy = 511 Or tnt1xy = 612 Or tnt1xy = 610 Then
                PictureBox171.Visible = False
            End If
            If tnt1xy = 613 Or tnt1xy = 713 Or tnt1xy = 513 Or tnt1xy = 614 Or tnt1xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point1()
                End If
                PictureBox169.Visible = False
            End If
            If tnt1xy = 701 Or tnt1xy = 801 Or tnt1xy = 601 Or tnt1xy = 702 Or tnt1xy = 700 Then
                If PictureBox214.Visible = True Then
                    Call point1()
                End If
                PictureBox214.Visible = False
            End If
            If tnt1xy = 703 Or tnt1xy = 803 Or tnt1xy = 603 Or tnt1xy = 704 Or tnt1xy = 702 Then
                PictureBox212.Visible = False
            End If
            If tnt1xy = 704 Or tnt1xy = 804 Or tnt1xy = 604 Or tnt1xy = 705 Or tnt1xy = 703 Then
                If PictureBox211.Visible = True Then
                    Call point1()
                End If
                PictureBox211.Visible = False
            End If
            If tnt1xy = 705 Or tnt1xy = 805 Or tnt1xy = 605 Or tnt1xy = 706 Or tnt1xy = 704 Then
                PictureBox210.Visible = False
            End If
            If tnt1xy = 706 Or tnt1xy = 806 Or tnt1xy = 606 Or tnt1xy = 707 Or tnt1xy = 705 Then
                PictureBox209.Visible = False
            End If
            If tnt1xy = 707 Or tnt1xy = 807 Or tnt1xy = 607 Or tnt1xy = 708 Or tnt1xy = 706 Then
                If PictureBox208.Visible = True Then
                    Call point1()
                End If
                PictureBox208.Visible = False
            End If
            If tnt1xy = 708 Or tnt1xy = 808 Or tnt1xy = 608 Or tnt1xy = 709 Or tnt1xy = 707 Then
                PictureBox207.Visible = False
            End If
            If tnt1xy = 709 Or tnt1xy = 809 Or tnt1xy = 609 Or tnt1xy = 710 Or tnt1xy = 708 Then
                PictureBox206.Visible = False
            End If
            If tnt1xy = 710 Or tnt1xy = 810 Or tnt1xy = 610 Or tnt1xy = 711 Or tnt1xy = 709 Then
                If PictureBox205.Visible = True Then
                    Call point1()
                End If
                PictureBox205.Visible = False
            End If
            If tnt1xy = 711 Or tnt1xy = 811 Or tnt1xy = 611 Or tnt1xy = 712 Or tnt1xy = 710 Then
                PictureBox204.Visible = False
            End If
            If tnt1xy = 713 Or tnt1xy = 813 Or tnt1xy = 613 Or tnt1xy = 714 Or tnt1xy = 712 Then
                If PictureBox202.Visible = True Then
                    Call point1()
                End If
                PictureBox202.Visible = False
            End If
            If tnt1xy = 801 Or tnt1xy = 901 Or tnt1xy = 701 Or tnt1xy = 802 Or tnt1xy = 800 Then
                PictureBox247.Visible = False
            End If
            If tnt1xy = 803 Or tnt1xy = 903 Or tnt1xy = 703 Or tnt1xy = 804 Or tnt1xy = 802 Then
                PictureBox245.Visible = False
            End If
            If tnt1xy = 805 Or tnt1xy = 905 Or tnt1xy = 705 Or tnt1xy = 806 Or tnt1xy = 804 Then
                PictureBox243.Visible = False
            End If
            If tnt1xy = 809 Or tnt1xy = 909 Or tnt1xy = 709 Or tnt1xy = 810 Or tnt1xy = 808 Then
                PictureBox239.Visible = False
            End If
            If tnt1xy = 811 Or tnt1xy = 911 Or tnt1xy = 711 Or tnt1xy = 812 Or tnt1xy = 810 Then
                PictureBox237.Visible = False
            End If
            If tnt1xy = 813 Or tnt1xy = 913 Or tnt1xy = 713 Or tnt1xy = 814 Or tnt1xy = 812 Then
                PictureBox235.Visible = False
            End If
            If tnt1xy = 901 Or tnt1xy = 1001 Or tnt1xy = 801 Or tnt1xy = 902 Or tnt1xy = 900 Then
                PictureBox280.Visible = False
            End If
            If tnt1xy = 903 Or tnt1xy = 1003 Or tnt1xy = 803 Or tnt1xy = 904 Or tnt1xy = 902 Then
                PictureBox278.Visible = False
            End If
            If tnt1xy = 905 Or tnt1xy = 1005 Or tnt1xy = 805 Or tnt1xy = 906 Or tnt1xy = 904 Then
                If PictureBox276.Visible = True Then
                    Call point1()
                End If
                PictureBox276.Visible = False
            End If
            If tnt1xy = 906 Or tnt1xy = 1006 Or tnt1xy = 806 Or tnt1xy = 907 Or tnt1xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt1xy = 907 Or tnt1xy = 1007 Or tnt1xy = 807 Or tnt1xy = 908 Or tnt1xy = 906 Then
                PictureBox274.Visible = False
            End If
            If tnt1xy = 908 Or tnt1xy = 1008 Or tnt1xy = 808 Or tnt1xy = 909 Or tnt1xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt1xy = 909 Or tnt1xy = 1009 Or tnt1xy = 809 Or tnt1xy = 910 Or tnt1xy = 908 Then
                If PictureBox272.Visible = True Then
                    Call point1()
                End If
                PictureBox272.Visible = False
            End If
            If tnt1xy = 911 Or tnt1xy = 1011 Or tnt1xy = 811 Or tnt1xy = 912 Or tnt1xy = 910 Then
                PictureBox270.Visible = False
            End If
            If tnt1xy = 913 Or tnt1xy = 1013 Or tnt1xy = 813 Or tnt1xy = 914 Or tnt1xy = 912 Then
                PictureBox268.Visible = False
            End If
            If tnt1xy = 1001 Or tnt1xy = 1101 Or tnt1xy = 901 Or tnt1xy = 1002 Or tnt1xy = 1000 Then
                PictureBox313.Visible = False
            End If
            If tnt1xy = 1003 Or tnt1xy = 1103 Or tnt1xy = 903 Or tnt1xy = 1004 Or tnt1xy = 1002 Then
                PictureBox311.Visible = False
            End If
            If tnt1xy = 1007 Or tnt1xy = 1107 Or tnt1xy = 907 Or tnt1xy = 1008 Or tnt1xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point1()
                End If
                PictureBox307.Visible = False
            End If
            If tnt1xy = 1011 Or tnt1xy = 1111 Or tnt1xy = 911 Or tnt1xy = 1012 Or tnt1xy = 1010 Then
                PictureBox303.Visible = False
            End If
            If tnt1xy = 1013 Or tnt1xy = 1113 Or tnt1xy = 913 Or tnt1xy = 1014 Or tnt1xy = 1012 Then
                PictureBox301.Visible = False
            End If
            If tnt1xy = 1206 Or tnt1xy = 1306 Or tnt1xy = 1106 Or tnt1xy = 1207 Or tnt1xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt1xy = 1207 Or tnt1xy = 1307 Or tnt1xy = 1107 Or tnt1xy = 1208 Or tnt1xy = 1206 Then
                If PictureBox373.Visible = True Then
                    Call point1()
                End If
                PictureBox373.Visible = False
            End If
            If tnt1xy = 1208 Or tnt1xy = 1308 Or tnt1xy = 1108 Or tnt1xy = 1209 Or tnt1xy = 1207 Then
                PictureBox372.Visible = False
            End If
        ElseIf map = 1 Then
            If tnt1xy = 101 Or tnt1xy = 201 Or tnt1xy = 1 Or tnt1xy = 102 Or tnt1xy = 100 Then
                If PictureBox104.Visible = True Then
                    Call point1()
                End If
                PictureBox104.Visible = False
            End If
            If tnt1xy = 104 Or tnt1xy = 204 Or tnt1xy = 4 Or tnt1xy = 105 Or tnt1xy = 103 Then
                PictureBox4.Visible = False
            End If
            If tnt1xy = 105 Or tnt1xy = 205 Or tnt1xy = 5 Or tnt1xy = 106 Or tnt1xy = 104 Then
                If PictureBox5.Visible = True Then
                    Call point1()
                End If
                PictureBox5.Visible = False
            End If
            If tnt1xy = 108 Or tnt1xy = 208 Or tnt1xy = 8 Or tnt1xy = 109 Or tnt1xy = 107 Then
                PictureBox8.Visible = False
            End If
            If tnt1xy = 109 Or tnt1xy = 209 Or tnt1xy = 9 Or tnt1xy = 110 Or tnt1xy = 108 Then
                If PictureBox9.Visible = True Then
                    Call point1()
                End If
                PictureBox9.Visible = False
            End If
            If tnt1xy = 112 Or tnt1xy = 212 Or tnt1xy = 12 Or tnt1xy = 113 Or tnt1xy = 111 Then
                PictureBox12.Visible = False
            End If
            If tnt1xy = 202 Or tnt1xy = 302 Or tnt1xy = 102 Or tnt1xy = 203 Or tnt1xy = 201 Then
                PictureBox25.Visible = False
            End If
            If tnt1xy = 206 Or tnt1xy = 306 Or tnt1xy = 106 Or tnt1xy = 207 Or tnt1xy = 205 Then
                PictureBox21.Visible = False
            End If
            If tnt1xy = 210 Or tnt1xy = 310 Or tnt1xy = 110 Or tnt1xy = 211 Or tnt1xy = 209 Then
                PictureBox17.Visible = False
            End If
            If tnt1xy = 301 Or tnt1xy = 401 Or tnt1xy = 201 Or tnt1xy = 302 Or tnt1xy = 300 Then
                PictureBox59.Visible = False
            End If
            If tnt1xy = 304 Or tnt1xy = 404 Or tnt1xy = 204 Or tnt1xy = 305 Or tnt1xy = 303 Then
                PictureBox56.Visible = False
            End If
            If tnt1xy = 308 Or tnt1xy = 408 Or tnt1xy = 208 Or tnt1xy = 309 Or tnt1xy = 307 Then
                PictureBox52.Visible = False
            End If
            If tnt1xy = 312 Or tnt1xy = 412 Or tnt1xy = 212 Or tnt1xy = 313 Or tnt1xy = 311 Then
                PictureBox48.Visible = False
            End If
            If tnt1xy = 402 Or tnt1xy = 502 Or tnt1xy = 302 Or tnt1xy = 403 Or tnt1xy = 401 Then
                PictureBox91.Visible = False
            End If
            If tnt1xy = 403 Or tnt1xy = 503 Or tnt1xy = 303 Or tnt1xy = 404 Or tnt1xy = 402 Then
                If PictureBox90.Visible = True Then
                    Call point1()
                End If
                PictureBox90.Visible = False
            End If
            If tnt1xy = 406 Or tnt1xy = 506 Or tnt1xy = 306 Or tnt1xy = 407 Or tnt1xy = 405 Then
                PictureBox87.Visible = False
            End If
            If tnt1xy = 407 Or tnt1xy = 507 Or tnt1xy = 307 Or tnt1xy = 408 Or tnt1xy = 406 Then
                If PictureBox86.Visible = True Then
                    Call point1()
                End If
                PictureBox86.Visible = False
            End If
            If tnt1xy = 410 Or tnt1xy = 510 Or tnt1xy = 310 Or tnt1xy = 411 Or tnt1xy = 409 Then
                PictureBox83.Visible = False
            End If
            If tnt1xy = 411 Or tnt1xy = 511 Or tnt1xy = 311 Or tnt1xy = 412 Or tnt1xy = 410 Then
                If PictureBox82.Visible = True Then
                    Call point1()
                End If
                PictureBox82.Visible = False
            End If
            If tnt1xy = 413 Or tnt1xy = 513 Or tnt1xy = 313 Or tnt1xy = 414 Or tnt1xy = 412 Then
                PictureBox80.Visible = False
            End If
            If tnt1xy = 501 Or tnt1xy = 601 Or tnt1xy = 401 Or tnt1xy = 502 Or tnt1xy = 500 Then
                If PictureBox148.Visible = True Then
                    Call point1()
                End If
                PictureBox148.Visible = False
            End If
            If tnt1xy = 504 Or tnt1xy = 604 Or tnt1xy = 404 Or tnt1xy = 505 Or tnt1xy = 503 Then
                PictureBox145.Visible = False
            End If
            If tnt1xy = 505 Or tnt1xy = 605 Or tnt1xy = 405 Or tnt1xy = 506 Or tnt1xy = 504 Then
                If PictureBox1.Visible = True Then
                    Call point1()
                End If
                PictureBox1.Visible = False
            End If
            If tnt1xy = 508 Or tnt1xy = 608 Or tnt1xy = 408 Or tnt1xy = 509 Or tnt1xy = 507 Then
                PictureBox121.Visible = False
            End If
            If tnt1xy = 509 Or tnt1xy = 609 Or tnt1xy = 409 Or tnt1xy = 510 Or tnt1xy = 508 Then
                If PictureBox120.Visible = True Then
                    Call point1()
                End If
                PictureBox120.Visible = False
            End If
            If tnt1xy = 512 Or tnt1xy = 612 Or tnt1xy = 412 Or tnt1xy = 513 Or tnt1xy = 511 Then
                PictureBox117.Visible = False
            End If
            If tnt1xy = 602 Or tnt1xy = 702 Or tnt1xy = 502 Or tnt1xy = 603 Or tnt1xy = 601 Then
                PictureBox180.Visible = False
            End If
            If tnt1xy = 606 Or tnt1xy = 706 Or tnt1xy = 506 Or tnt1xy = 607 Or tnt1xy = 605 Then
                PictureBox176.Visible = False
            End If
            If tnt1xy = 610 Or tnt1xy = 710 Or tnt1xy = 510 Or tnt1xy = 611 Or tnt1xy = 609 Then
                PictureBox172.Visible = False
            End If
            If tnt1xy = 613 Or tnt1xy = 713 Or tnt1xy = 513 Or tnt1xy = 614 Or tnt1xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point1()
                End If
                PictureBox169.Visible = False
            End If
            If tnt1xy = 701 Or tnt1xy = 801 Or tnt1xy = 601 Or tnt1xy = 702 Or tnt1xy = 700 Then
                PictureBox214.Visible = False
            End If
            If tnt1xy = 704 Or tnt1xy = 804 Or tnt1xy = 604 Or tnt1xy = 705 Or tnt1xy = 703 Then
                PictureBox211.Visible = False
            End If
            If tnt1xy = 708 Or tnt1xy = 808 Or tnt1xy = 608 Or tnt1xy = 709 Or tnt1xy = 707 Then
                PictureBox207.Visible = False
            End If
            If tnt1xy = 712 Or tnt1xy = 812 Or tnt1xy = 612 Or tnt1xy = 713 Or tnt1xy = 711 Then
                PictureBox203.Visible = False
            End If
            If tnt1xy = 802 Or tnt1xy = 902 Or tnt1xy = 702 Or tnt1xy = 803 Or tnt1xy = 801 Then
                PictureBox246.Visible = False
            End If
            If tnt1xy = 803 Or tnt1xy = 903 Or tnt1xy = 703 Or tnt1xy = 804 Or tnt1xy = 802 Then
                If PictureBox245.Visible = True Then
                    Call point1()
                End If
                PictureBox245.Visible = False
            End If
            If tnt1xy = 806 Or tnt1xy = 906 Or tnt1xy = 706 Or tnt1xy = 807 Or tnt1xy = 805 Then
                PictureBox242.Visible = False
            End If
            If tnt1xy = 807 Or tnt1xy = 907 Or tnt1xy = 707 Or tnt1xy = 808 Or tnt1xy = 806 Then
                If PictureBox241.Visible = True Then
                    Call point1()
                End If
                PictureBox241.Visible = False
            End If
            If tnt1xy = 810 Or tnt1xy = 910 Or tnt1xy = 710 Or tnt1xy = 811 Or tnt1xy = 809 Then
                PictureBox238.Visible = False
            End If
            If tnt1xy = 811 Or tnt1xy = 911 Or tnt1xy = 711 Or tnt1xy = 812 Or tnt1xy = 810 Then
                If PictureBox237.Visible = True Then
                    Call point1()
                End If
                PictureBox237.Visible = False
            End If
            If tnt1xy = 813 Or tnt1xy = 913 Or tnt1xy = 713 Or tnt1xy = 814 Or tnt1xy = 812 Then
                PictureBox235.Visible = False
            End If
            If tnt1xy = 904 Or tnt1xy = 1004 Or tnt1xy = 804 Or tnt1xy = 905 Or tnt1xy = 903 Then
                PictureBox277.Visible = False
            End If
            If tnt1xy = 905 Or tnt1xy = 1005 Or tnt1xy = 805 Or tnt1xy = 906 Or tnt1xy = 904 Then
                If PictureBox276.Visible = True Then
                    Call point1()
                End If
                PictureBox276.Visible = False
            End If
            If tnt1xy = 908 Or tnt1xy = 1008 Or tnt1xy = 808 Or tnt1xy = 909 Or tnt1xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt1xy = 909 Or tnt1xy = 1009 Or tnt1xy = 809 Or tnt1xy = 910 Or tnt1xy = 908 Then
                If PictureBox272.Visible = True Then
                    Call point1()
                End If
                PictureBox272.Visible = False
            End If
            If tnt1xy = 912 Or tnt1xy = 1012 Or tnt1xy = 812 Or tnt1xy = 913 Or tnt1xy = 911 Then
                PictureBox269.Visible = False
            End If
            If tnt1xy = 1002 Or tnt1xy = 1102 Or tnt1xy = 902 Or tnt1xy = 1003 Or tnt1xy = 1001 Then
                PictureBox312.Visible = False
            End If
            If tnt1xy = 1006 Or tnt1xy = 1106 Or tnt1xy = 906 Or tnt1xy = 1007 Or tnt1xy = 1005 Then
                PictureBox308.Visible = False
            End If
            If tnt1xy = 1010 Or tnt1xy = 1110 Or tnt1xy = 910 Or tnt1xy = 1011 Or tnt1xy = 1009 Then
                PictureBox304.Visible = False
            End If
            If tnt1xy = 1013 Or tnt1xy = 1113 Or tnt1xy = 913 Or tnt1xy = 1014 Or tnt1xy = 1012 Then
                If PictureBox301.Visible = True Then
                    Call point1()
                End If
                PictureBox301.Visible = False
            End If
            If tnt1xy = 1104 Or tnt1xy = 1204 Or tnt1xy = 1004 Or tnt1xy = 1105 Or tnt1xy = 1103 Then
                PictureBox343.Visible = False
            End If
            If tnt1xy = 1108 Or tnt1xy = 1208 Or tnt1xy = 1008 Or tnt1xy = 1109 Or tnt1xy = 1107 Then
                PictureBox339.Visible = False
            End If
            If tnt1xy = 1112 Or tnt1xy = 1212 Or tnt1xy = 1012 Or tnt1xy = 1113 Or tnt1xy = 1111 Then
                PictureBox335.Visible = False
            End If
            If tnt1xy = 1202 Or tnt1xy = 1302 Or tnt1xy = 1102 Or tnt1xy = 1203 Or tnt1xy = 1201 Then
                PictureBox378.Visible = False
            End If
            If tnt1xy = 1203 Or tnt1xy = 1303 Or tnt1xy = 1103 Or tnt1xy = 1204 Or tnt1xy = 1202 Then
                If PictureBox377.Visible = True Then
                    Call point1()
                End If
                PictureBox377.Visible = False
            End If
            If tnt1xy = 1206 Or tnt1xy = 1306 Or tnt1xy = 1106 Or tnt1xy = 1207 Or tnt1xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt1xy = 1207 Or tnt1xy = 1307 Or tnt1xy = 1107 Or tnt1xy = 1208 Or tnt1xy = 1206 Then
                If PictureBox373.Visible = True Then
                    Call point1()
                End If
                PictureBox373.Visible = False
            End If
            If tnt1xy = 1210 Or tnt1xy = 1310 Or tnt1xy = 1110 Or tnt1xy = 1211 Or tnt1xy = 1209 Then
                PictureBox370.Visible = False
            End If
            If tnt1xy = 1211 Or tnt1xy = 1311 Or tnt1xy = 1111 Or tnt1xy = 1212 Or tnt1xy = 1210 Then
                If PictureBox369.Visible = True Then
                    Call point1()
                End If
                PictureBox369.Visible = False
            End If
            If tnt1xy = 1213 Or tnt1xy = 1313 Or tnt1xy = 1113 Or tnt1xy = 1214 Or tnt1xy = 1212 Then
                PictureBox367.Visible = False
            End If
        ElseIf map = 2 Then
            If tnt1xy = 103 Or tnt1xy = 203 Or tnt1xy = 3 Or tnt1xy = 104 Or tnt1xy = 102 Then
                If PictureBox3.Visible = True Then
                    Call point1()
                End If
                PictureBox3.Visible = False
            End If
            If tnt1xy = 107 Or tnt1xy = 207 Or tnt1xy = 7 Or tnt1xy = 108 Or tnt1xy = 106 Then
                If PictureBox7.Visible = True Then
                    Call point1()
                End If
                PictureBox7.Visible = False
            End If
            If tnt1xy = 111 Or tnt1xy = 211 Or tnt1xy = 11 Or tnt1xy = 112 Or tnt1xy = 110 Then
                If PictureBox11.Visible = True Then
                    Call point1()
                End If
                PictureBox11.Visible = False
            End If
            If tnt1xy = 201 Or tnt1xy = 301 Or tnt1xy = 101 Or tnt1xy = 202 Or tnt1xy = 200 Then
                If PictureBox26.Visible = True Then
                    Call point1()
                End If
                PictureBox26.Visible = False
            End If
            If tnt1xy = 202 Or tnt1xy = 302 Or tnt1xy = 102 Or tnt1xy = 203 Or tnt1xy = 201 Then
                PictureBox25.Visible = False
            End If
            If tnt1xy = 207 Or tnt1xy = 307 Or tnt1xy = 107 Or tnt1xy = 208 Or tnt1xy = 206 Then
                PictureBox20.Visible = False
            End If
            If tnt1xy = 211 Or tnt1xy = 311 Or tnt1xy = 111 Or tnt1xy = 212 Or tnt1xy = 210 Then
                PictureBox16.Visible = False
            End If
            If tnt1xy = 303 Or tnt1xy = 403 Or tnt1xy = 203 Or tnt1xy = 304 Or tnt1xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt1xy = 306 Or tnt1xy = 406 Or tnt1xy = 206 Or tnt1xy = 307 Or tnt1xy = 305 Then
                PictureBox54.Visible = False
            End If
            If tnt1xy = 407 Or tnt1xy = 507 Or tnt1xy = 307 Or tnt1xy = 408 Or tnt1xy = 406 Then
                PictureBox86.Visible = False
            End If
            If tnt1xy = 408 Or tnt1xy = 508 Or tnt1xy = 308 Or tnt1xy = 409 Or tnt1xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt1xy = 412 Or tnt1xy = 512 Or tnt1xy = 312 Or tnt1xy = 413 Or tnt1xy = 411 Then
                PictureBox81.Visible = False
            End If
            If tnt1xy = 413 Or tnt1xy = 513 Or tnt1xy = 313 Or tnt1xy = 414 Or tnt1xy = 412 Then
                If PictureBox80.Visible = True Then
                    Call point1()
                End If
                PictureBox80.Visible = False
            End If
            If tnt1xy = 501 Or tnt1xy = 601 Or tnt1xy = 401 Or tnt1xy = 502 Or tnt1xy = 500 Then
                If PictureBox148.Visible = True Then
                    Call point1()
                End If
                PictureBox148.Visible = False
            End If
            If tnt1xy = 502 Or tnt1xy = 602 Or tnt1xy = 402 Or tnt1xy = 503 Or tnt1xy = 501 Then
                PictureBox147.Visible = False
            End If
            If tnt1xy = 504 Or tnt1xy = 604 Or tnt1xy = 404 Or tnt1xy = 505 Or tnt1xy = 503 Then
                PictureBox145.Visible = False
            End If
            If tnt1xy = 506 Or tnt1xy = 606 Or tnt1xy = 406 Or tnt1xy = 507 Or tnt1xy = 505 Then
                PictureBox123.Visible = False
            End If
            If tnt1xy = 509 Or tnt1xy = 609 Or tnt1xy = 409 Or tnt1xy = 510 Or tnt1xy = 508 Then
                PictureBox120.Visible = False
            End If
            If tnt1xy = 609 Or tnt1xy = 709 Or tnt1xy = 509 Or tnt1xy = 610 Or tnt1xy = 608 Then
                PictureBox173.Visible = False
            End If
            If tnt1xy = 612 Or tnt1xy = 712 Or tnt1xy = 512 Or tnt1xy = 613 Or tnt1xy = 611 Then
                PictureBox170.Visible = False
            End If
            If tnt1xy = 701 Or tnt1xy = 801 Or tnt1xy = 601 Or tnt1xy = 702 Or tnt1xy = 700 Then
                If PictureBox214.Visible = True Then
                    Call point1()
                End If
                PictureBox214.Visible = False
            End If
            If tnt1xy = 702 Or tnt1xy = 802 Or tnt1xy = 602 Or tnt1xy = 703 Or tnt1xy = 701 Then
                PictureBox213.Visible = False
            End If
            If tnt1xy = 704 Or tnt1xy = 804 Or tnt1xy = 604 Or tnt1xy = 705 Or tnt1xy = 703 Then
                PictureBox211.Visible = False
            End If
            If tnt1xy = 705 Or tnt1xy = 805 Or tnt1xy = 605 Or tnt1xy = 706 Or tnt1xy = 704 Then
                PictureBox210.Visible = False
            End If
            If tnt1xy = 707 Or tnt1xy = 807 Or tnt1xy = 607 Or tnt1xy = 708 Or tnt1xy = 706 Then
                PictureBox208.Visible = False
            End If
            If tnt1xy = 713 Or tnt1xy = 813 Or tnt1xy = 613 Or tnt1xy = 714 Or tnt1xy = 712 Then
                PictureBox202.Visible = False
            End If
            If tnt1xy = 807 Or tnt1xy = 907 Or tnt1xy = 707 Or tnt1xy = 808 Or tnt1xy = 806 Then
                PictureBox241.Visible = False
            End If
            If tnt1xy = 809 Or tnt1xy = 909 Or tnt1xy = 709 Or tnt1xy = 810 Or tnt1xy = 808 Then
                PictureBox239.Visible = False
            End If
            If tnt1xy = 810 Or tnt1xy = 910 Or tnt1xy = 710 Or tnt1xy = 811 Or tnt1xy = 809 Then
                PictureBox238.Visible = False
            End If
            If tnt1xy = 901 Or tnt1xy = 1001 Or tnt1xy = 801 Or tnt1xy = 902 Or tnt1xy = 900 Then
                If PictureBox280.Visible = True Then
                    Call point1()
                End If
                PictureBox280.Visible = False
            End If
            If tnt1xy = 904 Or tnt1xy = 1004 Or tnt1xy = 804 Or tnt1xy = 905 Or tnt1xy = 903 Then
                PictureBox277.Visible = False
            End If
            If tnt1xy = 906 Or tnt1xy = 1006 Or tnt1xy = 806 Or tnt1xy = 907 Or tnt1xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt1xy = 908 Or tnt1xy = 1008 Or tnt1xy = 808 Or tnt1xy = 909 Or tnt1xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt1xy = 913 Or tnt1xy = 1013 Or tnt1xy = 813 Or tnt1xy = 914 Or tnt1xy = 912 Then
                PictureBox268.Visible = False
            End If
            If tnt1xy = 1001 Or tnt1xy = 1101 Or tnt1xy = 901 Or tnt1xy = 1002 Or tnt1xy = 1000 Then
                PictureBox313.Visible = False
            End If
            If tnt1xy = 1007 Or tnt1xy = 1107 Or tnt1xy = 907 Or tnt1xy = 1008 Or tnt1xy = 1006 Then
                PictureBox307.Visible = False
            End If
            If tnt1xy = 1010 Or tnt1xy = 1110 Or tnt1xy = 910 Or tnt1xy = 1011 Or tnt1xy = 1009 Then
                PictureBox304.Visible = False
            End If
            If tnt1xy = 1012 Or tnt1xy = 1112 Or tnt1xy = 912 Or tnt1xy = 1013 Or tnt1xy = 1011 Then
                PictureBox302.Visible = False
            End If
            If tnt1xy = 1102 Or tnt1xy = 1202 Or tnt1xy = 1002 Or tnt1xy = 1103 Or tnt1xy = 1101 Then
                PictureBox345.Visible = False
            End If
            If tnt1xy = 1104 Or tnt1xy = 1204 Or tnt1xy = 1004 Or tnt1xy = 1105 Or tnt1xy = 1103 Then
                PictureBox343.Visible = False
            End If
            If tnt1xy = 1106 Or tnt1xy = 1206 Or tnt1xy = 1006 Or tnt1xy = 1107 Or tnt1xy = 1105 Then
                PictureBox341.Visible = False
            End If
            If tnt1xy = 1108 Or tnt1xy = 1208 Or tnt1xy = 1008 Or tnt1xy = 1109 Or tnt1xy = 1107 Then
                If PictureBox339.Visible = True Then
                    Call point1()
                End If
                PictureBox339.Visible = False
            End If
            If tnt1xy = 1111 Or tnt1xy = 1211 Or tnt1xy = 1011 Or tnt1xy = 1112 Or tnt1xy = 1110 Then
                PictureBox336.Visible = False
            End If
            If tnt1xy = 1113 Or tnt1xy = 1213 Or tnt1xy = 1013 Or tnt1xy = 1114 Or tnt1xy = 1112 Then
                PictureBox334.Visible = False
            End If
            If tnt1xy = 1204 Or tnt1xy = 1304 Or tnt1xy = 1104 Or tnt1xy = 1205 Or tnt1xy = 1203 Then
                PictureBox376.Visible = False
            End If
            If tnt1xy = 1206 Or tnt1xy = 1306 Or tnt1xy = 1106 Or tnt1xy = 1207 Or tnt1xy = 1205 Then
                If PictureBox374.Visible = True Then
                    Call point1()
                End If
                PictureBox374.Visible = False
            End If
            If tnt1xy = 1209 Or tnt1xy = 1309 Or tnt1xy = 1109 Or tnt1xy = 1210 Or tnt1xy = 1208 Then
                If PictureBox371.Visible = True Then
                    Call point1()
                End If
                PictureBox371.Visible = False
            End If
            If tnt1xy = 1210 Or tnt1xy = 1310 Or tnt1xy = 1110 Or tnt1xy = 1211 Or tnt1xy = 1209 Then
                PictureBox370.Visible = False
            End If
            If tnt1xy = 1213 Or tnt1xy = 1313 Or tnt1xy = 1113 Or tnt1xy = 1214 Or tnt1xy = 1212 Then
                If PictureBox367.Visible = True Then
                    Call point1()
                End If
                PictureBox367.Visible = False
            End If
        ElseIf map = 3 Then
            If tnt1xy = 102 Or tnt1xy = 202 Or tnt1xy = 2 Or tnt1xy = 103 Or tnt1xy = 101 Then
                If PictureBox2.Visible = True Then
                    Call point1()
                End If
                PictureBox2.Visible = False
            End If
            If tnt1xy = 103 Or tnt1xy = 203 Or tnt1xy = 3 Or tnt1xy = 104 Or tnt1xy = 102 Then
                PictureBox3.Visible = False
            End If
            If tnt1xy = 104 Or tnt1xy = 204 Or tnt1xy = 4 Or tnt1xy = 105 Or tnt1xy = 103 Then
                If PictureBox4.Visible = True Then
                    Call point1()
                End If
                PictureBox4.Visible = False
            End If
            If tnt1xy = 108 Or tnt1xy = 208 Or tnt1xy = 8 Or tnt1xy = 109 Or tnt1xy = 107 Then
                PictureBox8.Visible = False
            End If
            If tnt1xy = 110 Or tnt1xy = 210 Or tnt1xy = 10 Or tnt1xy = 111 Or tnt1xy = 109 Then
                If PictureBox10.Visible = True Then
                    Call point1()
                End If
                PictureBox10.Visible = False
            End If
            If tnt1xy = 201 Or tnt1xy = 301 Or tnt1xy = 101 Or tnt1xy = 202 Or tnt1xy = 200 Then
                If PictureBox26.Visible = True Then
                    Call point1()
                End If
                PictureBox26.Visible = False
            End If
            If tnt1xy = 203 Or tnt1xy = 303 Or tnt1xy = 103 Or tnt1xy = 204 Or tnt1xy = 202 Then
                PictureBox24.Visible = False
            End If
            If tnt1xy = 205 Or tnt1xy = 305 Or tnt1xy = 105 Or tnt1xy = 206 Or tnt1xy = 204 Then
                If PictureBox22.Visible = True Then
                    Call point1()
                End If
                PictureBox22.Visible = False
            End If
            If tnt1xy = 207 Or tnt1xy = 307 Or tnt1xy = 107 Or tnt1xy = 208 Or tnt1xy = 206 Then
                If PictureBox20.Visible = True Then
                    Call point1()
                End If
                PictureBox20.Visible = False
            End If
            If tnt1xy = 211 Or tnt1xy = 311 Or tnt1xy = 111 Or tnt1xy = 212 Or tnt1xy = 210 Then
                PictureBox16.Visible = False
            End If
            If tnt1xy = 301 Or tnt1xy = 401 Or tnt1xy = 201 Or tnt1xy = 302 Or tnt1xy = 300 Then
                PictureBox59.Visible = False
            End If
            If tnt1xy = 303 Or tnt1xy = 403 Or tnt1xy = 203 Or tnt1xy = 304 Or tnt1xy = 302 Then
                If PictureBox57.Visible = True Then
                    Call point1()
                End If
                PictureBox57.Visible = False
            End If
            If tnt1xy = 306 Or tnt1xy = 406 Or tnt1xy = 206 Or tnt1xy = 307 Or tnt1xy = 305 Then
                PictureBox54.Visible = False
            End If
            If tnt1xy = 308 Or tnt1xy = 408 Or tnt1xy = 208 Or tnt1xy = 309 Or tnt1xy = 307 Then
                PictureBox52.Visible = False
            End If
            If tnt1xy = 310 Or tnt1xy = 410 Or tnt1xy = 210 Or tnt1xy = 311 Or tnt1xy = 309 Then
                PictureBox50.Visible = False
            End If
            If tnt1xy = 312 Or tnt1xy = 412 Or tnt1xy = 212 Or tnt1xy = 313 Or tnt1xy = 311 Then
                If PictureBox48.Visible = True Then
                    Call point1()
                End If
                PictureBox48.Visible = False
            End If
            If tnt1xy = 402 Or tnt1xy = 502 Or tnt1xy = 302 Or tnt1xy = 403 Or tnt1xy = 401 Then
                PictureBox91.Visible = False
            End If
            If tnt1xy = 407 Or tnt1xy = 507 Or tnt1xy = 307 Or tnt1xy = 408 Or tnt1xy = 406 Then
                If PictureBox86.Visible = True Then
                    Call point1()
                End If
                PictureBox86.Visible = False
            End If
            If tnt1xy = 409 Or tnt1xy = 509 Or tnt1xy = 309 Or tnt1xy = 410 Or tnt1xy = 408 Then
                If PictureBox84.Visible = True Then
                    Call point1()
                End If
                PictureBox84.Visible = False
            End If
            If tnt1xy = 410 Or tnt1xy = 510 Or tnt1xy = 310 Or tnt1xy = 411 Or tnt1xy = 409 Then
                PictureBox83.Visible = False
            End If
            If tnt1xy = 413 Or tnt1xy = 513 Or tnt1xy = 313 Or tnt1xy = 414 Or tnt1xy = 412 Then
                PictureBox80.Visible = False
            End If
            If tnt1xy = 503 Or tnt1xy = 603 Or tnt1xy = 403 Or tnt1xy = 504 Or tnt1xy = 502 Then
                PictureBox146.Visible = False
            End If
            If tnt1xy = 505 Or tnt1xy = 605 Or tnt1xy = 405 Or tnt1xy = 506 Or tnt1xy = 504 Then
                If PictureBox1.Visible = True Then
                    Call point1()
                End If
                PictureBox1.Visible = False
            End If
            If tnt1xy = 507 Or tnt1xy = 607 Or tnt1xy = 407 Or tnt1xy = 508 Or tnt1xy = 506 Then
                PictureBox122.Visible = False
            End If
            If tnt1xy = 508 Or tnt1xy = 608 Or tnt1xy = 408 Or tnt1xy = 509 Or tnt1xy = 507 Then
                PictureBox121.Visible = False
            End If
            If tnt1xy = 510 Or tnt1xy = 610 Or tnt1xy = 410 Or tnt1xy = 511 Or tnt1xy = 509 Then
                If PictureBox119.Visible = True Then
                    Call point1()
                End If
                PictureBox119.Visible = False
            End If
            If tnt1xy = 513 Or tnt1xy = 613 Or tnt1xy = 413 Or tnt1xy = 514 Or tnt1xy = 512 Then
                PictureBox116.Visible = False
            End If
            If tnt1xy = 602 Or tnt1xy = 702 Or tnt1xy = 502 Or tnt1xy = 603 Or tnt1xy = 601 Then
                PictureBox180.Visible = False
            End If
            If tnt1xy = 606 Or tnt1xy = 706 Or tnt1xy = 506 Or tnt1xy = 607 Or tnt1xy = 605 Then
                PictureBox176.Visible = False
            End If
            If tnt1xy = 609 Or tnt1xy = 709 Or tnt1xy = 509 Or tnt1xy = 610 Or tnt1xy = 608 Then
                PictureBox173.Visible = False
            End If
            If tnt1xy = 611 Or tnt1xy = 711 Or tnt1xy = 511 Or tnt1xy = 612 Or tnt1xy = 610 Then
                PictureBox171.Visible = False
            End If
            If tnt1xy = 613 Or tnt1xy = 713 Or tnt1xy = 513 Or tnt1xy = 614 Or tnt1xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point1()
                End If
                PictureBox169.Visible = False
            End If
            If tnt1xy = 702 Or tnt1xy = 802 Or tnt1xy = 602 Or tnt1xy = 703 Or tnt1xy = 701 Then
                If PictureBox213.Visible = True Then
                    Call point1()
                End If
                PictureBox213.Visible = False
            End If
            If tnt1xy = 703 Or tnt1xy = 803 Or tnt1xy = 603 Or tnt1xy = 704 Or tnt1xy = 702 Then
                PictureBox212.Visible = False
            End If
            If tnt1xy = 705 Or tnt1xy = 805 Or tnt1xy = 605 Or tnt1xy = 706 Or tnt1xy = 704 Then
                If PictureBox210.Visible = True Then
                    Call point1()
                End If
                PictureBox210.Visible = False
            End If
            If tnt1xy = 707 Or tnt1xy = 807 Or tnt1xy = 607 Or tnt1xy = 708 Or tnt1xy = 706 Then
                PictureBox208.Visible = False
            End If
            If tnt1xy = 709 Or tnt1xy = 809 Or tnt1xy = 609 Or tnt1xy = 710 Or tnt1xy = 708 Then
                If PictureBox206.Visible = True Then
                    Call point1()
                End If
                PictureBox206.Visible = False
            End If
            If tnt1xy = 712 Or tnt1xy = 812 Or tnt1xy = 612 Or tnt1xy = 713 Or tnt1xy = 711 Then
                If PictureBox203.Visible = True Then
                    Call point1()
                End If
                PictureBox203.Visible = False
            End If
            If tnt1xy = 801 Or tnt1xy = 901 Or tnt1xy = 701 Or tnt1xy = 802 Or tnt1xy = 800 Then
                If PictureBox247.Visible = True Then
                    Call point1()
                End If
                PictureBox247.Visible = False
            End If
            If tnt1xy = 804 Or tnt1xy = 904 Or tnt1xy = 704 Or tnt1xy = 805 Or tnt1xy = 803 Then
                PictureBox244.Visible = False
            End If
            If tnt1xy = 808 Or tnt1xy = 908 Or tnt1xy = 708 Or tnt1xy = 809 Or tnt1xy = 807 Then
                If PictureBox240.Visible = True Then
                    Call point1()
                End If
                PictureBox240.Visible = False
            End If
            If tnt1xy = 811 Or tnt1xy = 911 Or tnt1xy = 711 Or tnt1xy = 812 Or tnt1xy = 810 Then
                PictureBox237.Visible = False
            End If
            If tnt1xy = 902 Or tnt1xy = 1002 Or tnt1xy = 802 Or tnt1xy = 903 Or tnt1xy = 901 Then
                PictureBox279.Visible = False
            End If
            If tnt1xy = 903 Or tnt1xy = 1003 Or tnt1xy = 803 Or tnt1xy = 904 Or tnt1xy = 902 Then
                If PictureBox278.Visible = True Then
                    Call point1()
                End If
                PictureBox278.Visible = False
            End If
            If tnt1xy = 906 Or tnt1xy = 1006 Or tnt1xy = 806 Or tnt1xy = 907 Or tnt1xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt1xy = 907 Or tnt1xy = 1007 Or tnt1xy = 807 Or tnt1xy = 908 Or tnt1xy = 906 Then
                If PictureBox274.Visible = True Then
                    Call point1()
                End If
                PictureBox274.Visible = False
            End If
            If tnt1xy = 909 Or tnt1xy = 1009 Or tnt1xy = 809 Or tnt1xy = 910 Or tnt1xy = 908 Then
                If PictureBox272.Visible = True Then
                    Call point1()
                End If
                PictureBox272.Visible = False
            End If
            If tnt1xy = 910 Or tnt1xy = 1010 Or tnt1xy = 810 Or tnt1xy = 911 Or tnt1xy = 909 Then
                PictureBox271.Visible = False
            End If
            If tnt1xy = 913 Or tnt1xy = 1013 Or tnt1xy = 813 Or tnt1xy = 914 Or tnt1xy = 912 Then
                PictureBox268.Visible = False
            End If
            If tnt1xy = 1001 Or tnt1xy = 1101 Or tnt1xy = 901 Or tnt1xy = 1002 Or tnt1xy = 1000 Then
                If PictureBox313.Visible = True Then
                    Call point1()
                End If
                PictureBox313.Visible = False
            End If
            If tnt1xy = 1004 Or tnt1xy = 1104 Or tnt1xy = 904 Or tnt1xy = 1005 Or tnt1xy = 1003 Then
                If PictureBox310.Visible = True Then
                    Call point1()
                End If
                PictureBox310.Visible = False
            End If
            If tnt1xy = 1009 Or tnt1xy = 1109 Or tnt1xy = 909 Or tnt1xy = 1010 Or tnt1xy = 1008 Then
                PictureBox305.Visible = False
            End If
            If tnt1xy = 1012 Or tnt1xy = 1112 Or tnt1xy = 912 Or tnt1xy = 1013 Or tnt1xy = 1011 Then
                If PictureBox302.Visible = True Then
                    Call point1()
                End If
                PictureBox302.Visible = False
            End If
            If tnt1xy = 1103 Or tnt1xy = 1203 Or tnt1xy = 1003 Or tnt1xy = 1104 Or tnt1xy = 1102 Then
                PictureBox344.Visible = False
            End If
            If tnt1xy = 1105 Or tnt1xy = 1205 Or tnt1xy = 1005 Or tnt1xy = 1106 Or tnt1xy = 1104 Then
                PictureBox342.Visible = False
            End If
            If tnt1xy = 1107 Or tnt1xy = 1207 Or tnt1xy = 1007 Or tnt1xy = 1108 Or tnt1xy = 1106 Then
                If PictureBox340.Visible = True Then
                    Call point1()
                End If
                PictureBox340.Visible = False
            End If
            If tnt1xy = 1113 Or tnt1xy = 1213 Or tnt1xy = 1013 Or tnt1xy = 1114 Or tnt1xy = 1112 Then
                If PictureBox334.Visible = True Then
                    Call point1()
                End If
                PictureBox334.Visible = False
            End If
            If tnt1xy = 1204 Or tnt1xy = 1304 Or tnt1xy = 1104 Or tnt1xy = 1205 Or tnt1xy = 1203 Then
                If PictureBox376.Visible = True Then
                    Call point1()
                End If
                PictureBox376.Visible = False
            End If
            If tnt1xy = 1206 Or tnt1xy = 1306 Or tnt1xy = 1106 Or tnt1xy = 1207 Or tnt1xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt1xy = 1211 Or tnt1xy = 1311 Or tnt1xy = 1111 Or tnt1xy = 1212 Or tnt1xy = 1210 Then
                If PictureBox369.Visible = True Then
                    Call point1()
                End If
                PictureBox369.Visible = False
            End If
            If tnt1xy = 1213 Or tnt1xy = 1313 Or tnt1xy = 1113 Or tnt1xy = 1214 Or tnt1xy = 1212 Then
                PictureBox367.Visible = False
            End If
        ElseIf map = 4 Then
            If tnt1xy = 103 Or tnt1xy = 203 Or tnt1xy = 3 Or tnt1xy = 104 Or tnt1xy = 102 Then
                PictureBox3.Visible = False
            End If
            If tnt1xy = 105 Or tnt1xy = 205 Or tnt1xy = 5 Or tnt1xy = 106 Or tnt1xy = 104 Then
                PictureBox5.Visible = False
            End If
            If tnt1xy = 106 Or tnt1xy = 206 Or tnt1xy = 6 Or tnt1xy = 107 Or tnt1xy = 105 Then
                If PictureBox6.Visible = True Then
                    Call point1()
                End If
                PictureBox6.Visible = False
            End If
            If tnt1xy = 107 Or tnt1xy = 207 Or tnt1xy = 7 Or tnt1xy = 108 Or tnt1xy = 106 Then
                If PictureBox7.Visible = True Then
                    Call point1()
                End If
                PictureBox7.Visible = False
            End If
            If tnt1xy = 108 Or tnt1xy = 208 Or tnt1xy = 8 Or tnt1xy = 109 Or tnt1xy = 107 Then
                If PictureBox8.Visible = True Then
                    Call point1()
                End If
                PictureBox8.Visible = False
            End If
            If tnt1xy = 109 Or tnt1xy = 209 Or tnt1xy = 9 Or tnt1xy = 110 Or tnt1xy = 108 Then
                PictureBox9.Visible = False
            End If
            If tnt1xy = 111 Or tnt1xy = 211 Or tnt1xy = 11 Or tnt1xy = 112 Or tnt1xy = 110 Then
                PictureBox11.Visible = False
            End If
            If tnt1xy = 204 Or tnt1xy = 304 Or tnt1xy = 104 Or tnt1xy = 205 Or tnt1xy = 203 Then
                If PictureBox23.Visible = True Then
                    Call point1()
                End If
                PictureBox23.Visible = False
            End If
            If tnt1xy = 205 Or tnt1xy = 305 Or tnt1xy = 105 Or tnt1xy = 206 Or tnt1xy = 204 Then
                If PictureBox22.Visible = True Then
                    Call point1()
                End If
                PictureBox22.Visible = False
            End If
            If tnt1xy = 206 Or tnt1xy = 306 Or tnt1xy = 106 Or tnt1xy = 207 Or tnt1xy = 205 Then
                If PictureBox21.Visible = True Then
                    Call point1()
                End If
                PictureBox21.Visible = False
            End If
            If tnt1xy = 207 Or tnt1xy = 307 Or tnt1xy = 107 Or tnt1xy = 208 Or tnt1xy = 206 Then
                If PictureBox20.Visible = True Then
                    Call point1()
                End If
                PictureBox20.Visible = False
            End If
            If tnt1xy = 208 Or tnt1xy = 308 Or tnt1xy = 108 Or tnt1xy = 209 Or tnt1xy = 207 Then
                If PictureBox19.Visible = True Then
                    Call point1()
                End If
                PictureBox19.Visible = False
            End If
            If tnt1xy = 209 Or tnt1xy = 309 Or tnt1xy = 109 Or tnt1xy = 210 Or tnt1xy = 208 Then
                If PictureBox18.Visible = True Then
                    Call point1()
                End If
                PictureBox18.Visible = False
            End If
            If tnt1xy = 210 Or tnt1xy = 310 Or tnt1xy = 110 Or tnt1xy = 211 Or tnt1xy = 209 Then
                If PictureBox17.Visible = True Then
                    Call point1()
                End If
                PictureBox17.Visible = False
            End If
            If tnt1xy = 301 Or tnt1xy = 401 Or tnt1xy = 201 Or tnt1xy = 302 Or tnt1xy = 300 Then
                PictureBox59.Visible = False
            End If
            If tnt1xy = 303 Or tnt1xy = 403 Or tnt1xy = 203 Or tnt1xy = 304 Or tnt1xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt1xy = 304 Or tnt1xy = 404 Or tnt1xy = 204 Or tnt1xy = 305 Or tnt1xy = 303 Then
                If PictureBox56.Visible = True Then
                    Call point1()
                End If
                PictureBox56.Visible = False
            End If
            If tnt1xy = 305 Or tnt1xy = 405 Or tnt1xy = 205 Or tnt1xy = 306 Or tnt1xy = 304 Then
                If PictureBox55.Visible = True Then
                    Call point1()
                End If
                PictureBox55.Visible = False
            End If
            If tnt1xy = 306 Or tnt1xy = 406 Or tnt1xy = 206 Or tnt1xy = 307 Or tnt1xy = 305 Then
                If PictureBox54.Visible = True Then
                    Call point1()
                End If
                PictureBox54.Visible = False
            End If
            If tnt1xy = 307 Or tnt1xy = 407 Or tnt1xy = 207 Or tnt1xy = 308 Or tnt1xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point1()
                End If
                PictureBox53.Visible = False
            End If
            If tnt1xy = 308 Or tnt1xy = 408 Or tnt1xy = 208 Or tnt1xy = 309 Or tnt1xy = 307 Then
                If PictureBox52.Visible = True Then
                    Call point1()
                End If
                PictureBox52.Visible = False
            End If
            If tnt1xy = 309 Or tnt1xy = 409 Or tnt1xy = 209 Or tnt1xy = 310 Or tnt1xy = 308 Then
                If PictureBox51.Visible = True Then
                    Call point1()
                End If
                PictureBox51.Visible = False
            End If
            If tnt1xy = 310 Or tnt1xy = 410 Or tnt1xy = 210 Or tnt1xy = 311 Or tnt1xy = 309 Then
                If PictureBox50.Visible = True Then
                    Call point1()
                End If
                PictureBox50.Visible = False
            End If
            If tnt1xy = 311 Or tnt1xy = 411 Or tnt1xy = 211 Or tnt1xy = 312 Or tnt1xy = 310 Then
                PictureBox49.Visible = False
            End If
            If tnt1xy = 313 Or tnt1xy = 413 Or tnt1xy = 213 Or tnt1xy = 314 Or tnt1xy = 312 Then
                PictureBox47.Visible = False
            End If
            If tnt1xy = 402 Or tnt1xy = 502 Or tnt1xy = 302 Or tnt1xy = 403 Or tnt1xy = 401 Then
                If PictureBox91.Visible = True Then
                    Call point1()
                End If
                PictureBox91.Visible = False
            End If
            If tnt1xy = 403 Or tnt1xy = 503 Or tnt1xy = 303 Or tnt1xy = 404 Or tnt1xy = 402 Then
                If PictureBox90.Visible = True Then
                    Call point1()
                End If
                PictureBox90.Visible = False
            End If
            If tnt1xy = 405 Or tnt1xy = 505 Or tnt1xy = 305 Or tnt1xy = 406 Or tnt1xy = 404 Then
                If PictureBox88.Visible = True Then
                    Call point1()
                End If
                PictureBox88.Visible = False
            End If
            If tnt1xy = 406 Or tnt1xy = 506 Or tnt1xy = 306 Or tnt1xy = 407 Or tnt1xy = 405 Then
                If PictureBox87.Visible = True Then
                    Call point1()
                End If
                PictureBox87.Visible = False
            End If
            If tnt1xy = 407 Or tnt1xy = 507 Or tnt1xy = 307 Or tnt1xy = 408 Or tnt1xy = 406 Then
                If PictureBox86.Visible = True Then
                    Call point1()
                End If
                PictureBox86.Visible = False
            End If
            If tnt1xy = 408 Or tnt1xy = 508 Or tnt1xy = 308 Or tnt1xy = 409 Or tnt1xy = 407 Then
                If PictureBox85.Visible = True Then
                    Call point1()
                End If
                PictureBox85.Visible = False
            End If
            If tnt1xy = 409 Or tnt1xy = 509 Or tnt1xy = 309 Or tnt1xy = 410 Or tnt1xy = 408 Then
                If PictureBox84.Visible = True Then
                    Call point1()
                End If
                PictureBox84.Visible = False
            End If
            If tnt1xy = 411 Or tnt1xy = 511 Or tnt1xy = 311 Or tnt1xy = 412 Or tnt1xy = 410 Then
                If PictureBox82.Visible = True Then
                    Call point1()
                End If
                PictureBox82.Visible = False
            End If
            If tnt1xy = 412 Or tnt1xy = 512 Or tnt1xy = 312 Or tnt1xy = 413 Or tnt1xy = 411 Then
                If PictureBox81.Visible = True Then
                    Call point1()
                End If
                PictureBox81.Visible = False
            End If
            If tnt1xy = 501 Or tnt1xy = 601 Or tnt1xy = 401 Or tnt1xy = 502 Or tnt1xy = 500 Then
                PictureBox148.Visible = False
            End If
            If tnt1xy = 502 Or tnt1xy = 602 Or tnt1xy = 402 Or tnt1xy = 503 Or tnt1xy = 501 Then
                If PictureBox147.Visible = True Then
                    Call point1()
                End If
                PictureBox147.Visible = False
            End If
            If tnt1xy = 503 Or tnt1xy = 603 Or tnt1xy = 403 Or tnt1xy = 504 Or tnt1xy = 502 Then
                If PictureBox146.Visible = True Then
                    Call point1()
                End If
                PictureBox146.Visible = False
            End If
            If tnt1xy = 504 Or tnt1xy = 604 Or tnt1xy = 404 Or tnt1xy = 505 Or tnt1xy = 503 Then
                If PictureBox145.Visible = True Then
                    Call point1()
                End If
                PictureBox145.Visible = False
            End If
            If tnt1xy = 510 Or tnt1xy = 610 Or tnt1xy = 410 Or tnt1xy = 511 Or tnt1xy = 509 Then
                If PictureBox119.Visible = True Then
                    Call point1()
                End If
                PictureBox119.Visible = False
            End If
            If tnt1xy = 511 Or tnt1xy = 611 Or tnt1xy = 411 Or tnt1xy = 512 Or tnt1xy = 510 Then
                If PictureBox118.Visible = True Then
                    Call point1()
                End If
                PictureBox118.Visible = False
            End If
            If tnt1xy = 512 Or tnt1xy = 612 Or tnt1xy = 412 Or tnt1xy = 513 Or tnt1xy = 511 Then
                If PictureBox117.Visible = True Then
                    Call point1()
                End If
                PictureBox117.Visible = False
            End If
            If tnt1xy = 513 Or tnt1xy = 613 Or tnt1xy = 413 Or tnt1xy = 514 Or tnt1xy = 512 Then
                PictureBox116.Visible = False
            End If
            If tnt1xy = 601 Or tnt1xy = 701 Or tnt1xy = 501 Or tnt1xy = 602 Or tnt1xy = 600 Then
                If PictureBox181.Visible = True Then
                    Call point1()
                End If
                PictureBox181.Visible = False
            End If
            If tnt1xy = 602 Or tnt1xy = 702 Or tnt1xy = 502 Or tnt1xy = 603 Or tnt1xy = 601 Then
                If PictureBox180.Visible = True Then
                    Call point1()
                End If
                PictureBox180.Visible = False
            End If
            If tnt1xy = 603 Or tnt1xy = 703 Or tnt1xy = 503 Or tnt1xy = 604 Or tnt1xy = 602 Then
                If PictureBox179.Visible = True Then
                    Call point1()
                End If
                PictureBox179.Visible = False
            End If
            If tnt1xy = 604 Or tnt1xy = 704 Or tnt1xy = 504 Or tnt1xy = 605 Or tnt1xy = 603 Then
                If PictureBox178.Visible = True Then
                    Call point1()
                End If
                PictureBox178.Visible = False
            End If
            If tnt1xy = 606 Or tnt1xy = 706 Or tnt1xy = 506 Or tnt1xy = 607 Or tnt1xy = 605 Then
                PictureBox176.Visible = False
            End If
            If tnt1xy = 607 Or tnt1xy = 707 Or tnt1xy = 507 Or tnt1xy = 608 Or tnt1xy = 606 Then
                PictureBox175.Visible = False
            End If
            If tnt1xy = 608 Or tnt1xy = 708 Or tnt1xy = 508 Or tnt1xy = 609 Or tnt1xy = 607 Then
                PictureBox174.Visible = False
            End If
            If tnt1xy = 610 Or tnt1xy = 710 Or tnt1xy = 510 Or tnt1xy = 611 Or tnt1xy = 609 Then
                If PictureBox172.Visible = True Then
                    Call point1()
                End If
                PictureBox172.Visible = False
            End If
            If tnt1xy = 611 Or tnt1xy = 711 Or tnt1xy = 511 Or tnt1xy = 612 Or tnt1xy = 610 Then
                If PictureBox171.Visible = True Then
                    Call point1()
                End If
                PictureBox171.Visible = False
            End If
            If tnt1xy = 612 Or tnt1xy = 712 Or tnt1xy = 512 Or tnt1xy = 613 Or tnt1xy = 611 Then
                If PictureBox170.Visible = True Then
                    Call point1()
                End If
                PictureBox170.Visible = False
            End If
            If tnt1xy = 613 Or tnt1xy = 713 Or tnt1xy = 513 Or tnt1xy = 614 Or tnt1xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point1()
                End If
                PictureBox169.Visible = False
            End If
            If tnt1xy = 701 Or tnt1xy = 801 Or tnt1xy = 601 Or tnt1xy = 702 Or tnt1xy = 700 Then
                If PictureBox214.Visible = True Then
                    Call point1()
                End If
                PictureBox214.Visible = False
            End If
            If tnt1xy = 702 Or tnt1xy = 802 Or tnt1xy = 602 Or tnt1xy = 703 Or tnt1xy = 701 Then
                If PictureBox213.Visible = True Then
                    Call point1()
                End If
                PictureBox213.Visible = False
            End If
            If tnt1xy = 703 Or tnt1xy = 803 Or tnt1xy = 603 Or tnt1xy = 704 Or tnt1xy = 702 Then
                If PictureBox212.Visible = True Then
                    Call point1()
                End If
                PictureBox212.Visible = False
            End If
            If tnt1xy = 704 Or tnt1xy = 804 Or tnt1xy = 604 Or tnt1xy = 705 Or tnt1xy = 703 Then
                If PictureBox211.Visible = True Then
                    Call point1()
                End If
                PictureBox211.Visible = False
            End If
            If tnt1xy = 706 Or tnt1xy = 806 Or tnt1xy = 606 Or tnt1xy = 707 Or tnt1xy = 705 Then
                PictureBox209.Visible = False
            End If
            If tnt1xy = 707 Or tnt1xy = 807 Or tnt1xy = 607 Or tnt1xy = 708 Or tnt1xy = 706 Then
                PictureBox208.Visible = False
            End If
            If tnt1xy = 708 Or tnt1xy = 808 Or tnt1xy = 608 Or tnt1xy = 709 Or tnt1xy = 707 Then
                PictureBox207.Visible = False
            End If
            If tnt1xy = 710 Or tnt1xy = 810 Or tnt1xy = 610 Or tnt1xy = 711 Or tnt1xy = 709 Then
                If PictureBox205.Visible = True Then
                    Call point1()
                End If
                PictureBox205.Visible = False
            End If
            If tnt1xy = 711 Or tnt1xy = 811 Or tnt1xy = 611 Or tnt1xy = 712 Or tnt1xy = 710 Then
                If PictureBox204.Visible = True Then
                    Call point1()
                End If
                PictureBox204.Visible = False
            End If
            If tnt1xy = 712 Or tnt1xy = 812 Or tnt1xy = 612 Or tnt1xy = 713 Or tnt1xy = 711 Then
                If PictureBox203.Visible = True Then
                    Call point1()
                End If
                PictureBox203.Visible = False
            End If
            If tnt1xy = 713 Or tnt1xy = 813 Or tnt1xy = 613 Or tnt1xy = 714 Or tnt1xy = 712 Then
                If PictureBox202.Visible = True Then
                    Call point1()
                End If
                PictureBox202.Visible = False
            End If
            If tnt1xy = 801 Or tnt1xy = 901 Or tnt1xy = 701 Or tnt1xy = 802 Or tnt1xy = 800 Then
                PictureBox247.Visible = False
            End If
            If tnt1xy = 802 Or tnt1xy = 902 Or tnt1xy = 702 Or tnt1xy = 803 Or tnt1xy = 801 Then
                If PictureBox246.Visible = True Then
                    Call point1()
                End If
                PictureBox246.Visible = False
            End If
            If tnt1xy = 803 Or tnt1xy = 903 Or tnt1xy = 703 Or tnt1xy = 804 Or tnt1xy = 802 Then
                If PictureBox245.Visible = True Then
                    Call point1()
                End If
                PictureBox245.Visible = False
            End If
            If tnt1xy = 804 Or tnt1xy = 904 Or tnt1xy = 704 Or tnt1xy = 805 Or tnt1xy = 803 Then
                If PictureBox244.Visible = True Then
                    Call point1()
                End If
                PictureBox244.Visible = False
            End If
            If tnt1xy = 810 Or tnt1xy = 910 Or tnt1xy = 710 Or tnt1xy = 811 Or tnt1xy = 809 Then
                If PictureBox238.Visible = True Then
                    Call point1()
                End If
                PictureBox238.Visible = False
            End If
            If tnt1xy = 811 Or tnt1xy = 911 Or tnt1xy = 711 Or tnt1xy = 812 Or tnt1xy = 810 Then
                If PictureBox237.Visible = True Then
                    Call point1()
                End If
                PictureBox237.Visible = False
            End If
            If tnt1xy = 812 Or tnt1xy = 912 Or tnt1xy = 712 Or tnt1xy = 813 Or tnt1xy = 811 Then
                If PictureBox236.Visible = True Then
                    Call point1()
                End If
                PictureBox236.Visible = False
            End If
            If tnt1xy = 813 Or tnt1xy = 913 Or tnt1xy = 713 Or tnt1xy = 814 Or tnt1xy = 812 Then
                PictureBox235.Visible = False
            End If
            If tnt1xy = 902 Or tnt1xy = 1002 Or tnt1xy = 802 Or tnt1xy = 903 Or tnt1xy = 901 Then
                If PictureBox279.Visible = True Then
                    Call point1()
                End If
                PictureBox279.Visible = False
            End If
            If tnt1xy = 903 Or tnt1xy = 1003 Or tnt1xy = 803 Or tnt1xy = 904 Or tnt1xy = 902 Then
                If PictureBox278.Visible = True Then
                    Call point1()
                End If
                PictureBox278.Visible = False
            End If
            If tnt1xy = 905 Or tnt1xy = 1005 Or tnt1xy = 805 Or tnt1xy = 906 Or tnt1xy = 904 Then
                If PictureBox276.Visible = True Then
                    Call point1()
                End If
                PictureBox276.Visible = False
            End If
            If tnt1xy = 906 Or tnt1xy = 1006 Or tnt1xy = 806 Or tnt1xy = 907 Or tnt1xy = 905 Then
                If PictureBox275.Visible = True Then
                    Call point1()
                End If
                PictureBox275.Visible = False
            End If
            If tnt1xy = 907 Or tnt1xy = 1007 Or tnt1xy = 807 Or tnt1xy = 908 Or tnt1xy = 906 Then
                If PictureBox274.Visible = True Then
                    Call point1()
                End If
                PictureBox274.Visible = False
            End If
            If tnt1xy = 908 Or tnt1xy = 1008 Or tnt1xy = 808 Or tnt1xy = 909 Or tnt1xy = 907 Then
                If PictureBox273.Visible = True Then
                    Call point1()
                End If
                PictureBox273.Visible = False
            End If
            If tnt1xy = 909 Or tnt1xy = 1009 Or tnt1xy = 809 Or tnt1xy = 910 Or tnt1xy = 908 Then
                If PictureBox272.Visible = True Then
                    Call point1()
                End If
                PictureBox272.Visible = False
            End If
            If tnt1xy = 911 Or tnt1xy = 1011 Or tnt1xy = 811 Or tnt1xy = 912 Or tnt1xy = 910 Then
                If PictureBox270.Visible = True Then
                    Call point1()
                End If
                PictureBox270.Visible = False
            End If
            If tnt1xy = 912 Or tnt1xy = 1012 Or tnt1xy = 812 Or tnt1xy = 913 Or tnt1xy = 911 Then
                If PictureBox269.Visible = True Then
                    Call point1()
                End If
                PictureBox269.Visible = False
            End If
            If tnt1xy = 1001 Or tnt1xy = 1101 Or tnt1xy = 901 Or tnt1xy = 1002 Or tnt1xy = 1000 Then
                PictureBox313.Visible = False
            End If
            If tnt1xy = 1003 Or tnt1xy = 1103 Or tnt1xy = 903 Or tnt1xy = 1004 Or tnt1xy = 1002 Then
                PictureBox311.Visible = False
            End If
            If tnt1xy = 1004 Or tnt1xy = 1104 Or tnt1xy = 904 Or tnt1xy = 1005 Or tnt1xy = 1003 Then
                If PictureBox310.Visible = True Then
                    Call point1()
                End If
                PictureBox310.Visible = False
            End If
            If tnt1xy = 1005 Or tnt1xy = 1105 Or tnt1xy = 905 Or tnt1xy = 1006 Or tnt1xy = 1004 Then
                If PictureBox309.Visible = True Then
                    Call point1()
                End If
                PictureBox309.Visible = False
            End If
            If tnt1xy = 1006 Or tnt1xy = 1106 Or tnt1xy = 906 Or tnt1xy = 1007 Or tnt1xy = 1005 Then
                If PictureBox308.Visible = True Then
                    Call point1()
                End If
                PictureBox308.Visible = False
            End If
            If tnt1xy = 1007 Or tnt1xy = 1107 Or tnt1xy = 907 Or tnt1xy = 1008 Or tnt1xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point1()
                End If
                PictureBox307.Visible = False
            End If
            If tnt1xy = 1008 Or tnt1xy = 1108 Or tnt1xy = 908 Or tnt1xy = 1009 Or tnt1xy = 1007 Then
                If PictureBox306.Visible = True Then
                    Call point1()
                End If
                PictureBox306.Visible = False
            End If
            If tnt1xy = 1009 Or tnt1xy = 1109 Or tnt1xy = 909 Or tnt1xy = 1010 Or tnt1xy = 1008 Then
                If PictureBox305.Visible = True Then
                    Call point1()
                End If
                PictureBox305.Visible = False
            End If
            If tnt1xy = 1010 Or tnt1xy = 1110 Or tnt1xy = 910 Or tnt1xy = 1011 Or tnt1xy = 1009 Then
                If PictureBox304.Visible = True Then
                    Call point1()
                End If
                PictureBox304.Visible = False
            End If
            If tnt1xy = 1011 Or tnt1xy = 1111 Or tnt1xy = 911 Or tnt1xy = 1012 Or tnt1xy = 1010 Then
                PictureBox303.Visible = False
            End If
            If tnt1xy = 1013 Or tnt1xy = 1113 Or tnt1xy = 913 Or tnt1xy = 1014 Or tnt1xy = 1012 Then
                PictureBox301.Visible = False
            End If
            If tnt1xy = 1104 Or tnt1xy = 1204 Or tnt1xy = 1004 Or tnt1xy = 1105 Or tnt1xy = 1103 Then
                If PictureBox343.Visible = True Then
                    Call point1()
                End If
                PictureBox343.Visible = False
            End If
            If tnt1xy = 1105 Or tnt1xy = 1205 Or tnt1xy = 1005 Or tnt1xy = 1106 Or tnt1xy = 1104 Then
                If PictureBox342.Visible = True Then
                    Call point1()
                End If
                PictureBox342.Visible = False
            End If
            If tnt1xy = 1106 Or tnt1xy = 1206 Or tnt1xy = 1006 Or tnt1xy = 1107 Or tnt1xy = 1105 Then
                If PictureBox341.Visible = True Then
                    Call point1()
                End If
                PictureBox341.Visible = False
            End If
            If tnt1xy = 1107 Or tnt1xy = 1207 Or tnt1xy = 1007 Or tnt1xy = 1108 Or tnt1xy = 1106 Then
                If PictureBox340.Visible = True Then
                    Call point1()
                End If
                PictureBox340.Visible = False
            End If
            If tnt1xy = 1108 Or tnt1xy = 1208 Or tnt1xy = 1008 Or tnt1xy = 1109 Or tnt1xy = 1107 Then
                If PictureBox339.Visible = True Then
                    Call point1()
                End If
                PictureBox339.Visible = False
            End If
            If tnt1xy = 1109 Or tnt1xy = 1209 Or tnt1xy = 1009 Or tnt1xy = 1110 Or tnt1xy = 1108 Then
                If PictureBox338.Visible = True Then
                    Call point1()
                End If
                PictureBox338.Visible = False
            End If
            If tnt1xy = 1110 Or tnt1xy = 1210 Or tnt1xy = 1010 Or tnt1xy = 1111 Or tnt1xy = 1109 Then
                If PictureBox337.Visible = True Then
                    Call point1()
                End If
                PictureBox337.Visible = False
            End If
            If tnt1xy = 1203 Or tnt1xy = 1303 Or tnt1xy = 1103 Or tnt1xy = 1204 Or tnt1xy = 1202 Then
                PictureBox377.Visible = False
            End If
            If tnt1xy = 1205 Or tnt1xy = 1305 Or tnt1xy = 1105 Or tnt1xy = 1206 Or tnt1xy = 1204 Then
                PictureBox375.Visible = False
            End If
            If tnt1xy = 1206 Or tnt1xy = 1306 Or tnt1xy = 1106 Or tnt1xy = 1207 Or tnt1xy = 1205 Then
                If PictureBox374.Visible = True Then
                    Call point1()
                End If
                PictureBox374.Visible = False
            End If
            If tnt1xy = 1207 Or tnt1xy = 1307 Or tnt1xy = 1107 Or tnt1xy = 1208 Or tnt1xy = 1206 Then
                If PictureBox373.Visible = True Then
                    Call point1()
                End If
                PictureBox373.Visible = False
            End If
            If tnt1xy = 1208 Or tnt1xy = 1308 Or tnt1xy = 1108 Or tnt1xy = 1209 Or tnt1xy = 1207 Then
                If PictureBox372.Visible = True Then
                    Call point1()
                End If
                PictureBox372.Visible = False
            End If
            If tnt1xy = 1209 Or tnt1xy = 1309 Or tnt1xy = 1109 Or tnt1xy = 1210 Or tnt1xy = 1208 Then
                PictureBox371.Visible = False
            End If
            If tnt1xy = 1211 Or tnt1xy = 1311 Or tnt1xy = 1111 Or tnt1xy = 1212 Or tnt1xy = 1210 Then
                PictureBox369.Visible = False
            End If
        ElseIf map = 5 Then
            If tnt1xy = 101 Or tnt1xy = 201 Or tnt1xy = 1 Or tnt1xy = 102 Or tnt1xy = 100 Then
                PictureBox104.Visible = False
            End If
            If tnt1xy = 102 Or tnt1xy = 202 Or tnt1xy = 2 Or tnt1xy = 103 Or tnt1xy = 101 Then
                PictureBox2.Visible = False
            End If
            If tnt1xy = 105 Or tnt1xy = 205 Or tnt1xy = 5 Or tnt1xy = 106 Or tnt1xy = 104 Then
                PictureBox5.Visible = False
            End If
            If tnt1xy = 106 Or tnt1xy = 206 Or tnt1xy = 6 Or tnt1xy = 107 Or tnt1xy = 105 Then
                PictureBox6.Visible = False
            End If
            If tnt1xy = 107 Or tnt1xy = 207 Or tnt1xy = 7 Or tnt1xy = 108 Or tnt1xy = 106 Then
                PictureBox7.Visible = False
            End If
            If tnt1xy = 108 Or tnt1xy = 208 Or tnt1xy = 8 Or tnt1xy = 109 Or tnt1xy = 107 Then
                PictureBox8.Visible = False
            End If
            If tnt1xy = 109 Or tnt1xy = 209 Or tnt1xy = 9 Or tnt1xy = 110 Or tnt1xy = 108 Then
                PictureBox9.Visible = False
            End If
            If tnt1xy = 201 Or tnt1xy = 301 Or tnt1xy = 101 Or tnt1xy = 202 Or tnt1xy = 200 Then
                PictureBox26.Visible = False
            End If
            If tnt1xy = 202 Or tnt1xy = 302 Or tnt1xy = 102 Or tnt1xy = 203 Or tnt1xy = 201 Then
                PictureBox25.Visible = False
            End If
            If tnt1xy = 205 Or tnt1xy = 305 Or tnt1xy = 105 Or tnt1xy = 206 Or tnt1xy = 204 Then
                PictureBox22.Visible = False
            End If
            If tnt1xy = 206 Or tnt1xy = 306 Or tnt1xy = 106 Or tnt1xy = 207 Or tnt1xy = 205 Then
                If PictureBox21.Visible = True Then
                    Call point1()
                End If
                PictureBox21.Visible = False
            End If
            If tnt1xy = 207 Or tnt1xy = 307 Or tnt1xy = 107 Or tnt1xy = 208 Or tnt1xy = 206 Then
                If PictureBox20.Visible = True Then
                    Call point1()
                End If
                PictureBox20.Visible = False
            End If
            If tnt1xy = 208 Or tnt1xy = 308 Or tnt1xy = 108 Or tnt1xy = 209 Or tnt1xy = 207 Then
                If PictureBox19.Visible = True Then
                    Call point1()
                End If
                PictureBox19.Visible = False
            End If
            If tnt1xy = 209 Or tnt1xy = 309 Or tnt1xy = 109 Or tnt1xy = 210 Or tnt1xy = 208 Then
                PictureBox18.Visible = False
            End If
            If tnt1xy = 301 Or tnt1xy = 401 Or tnt1xy = 201 Or tnt1xy = 302 Or tnt1xy = 300 Then
                If PictureBox59.Visible = True Then
                    Call point1()
                End If
                PictureBox59.Visible = False
            End If
            If tnt1xy = 305 Or tnt1xy = 405 Or tnt1xy = 205 Or tnt1xy = 306 Or tnt1xy = 304 Then
                PictureBox55.Visible = False
            End If
            If tnt1xy = 306 Or tnt1xy = 406 Or tnt1xy = 206 Or tnt1xy = 307 Or tnt1xy = 305 Then
                If PictureBox54.Visible = True Then
                    Call point1()
                End If
                PictureBox54.Visible = False
            End If
            If tnt1xy = 307 Or tnt1xy = 407 Or tnt1xy = 207 Or tnt1xy = 308 Or tnt1xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point1()
                End If
                PictureBox53.Visible = False
            End If
            If tnt1xy = 308 Or tnt1xy = 408 Or tnt1xy = 208 Or tnt1xy = 309 Or tnt1xy = 307 Then
                If PictureBox52.Visible = True Then
                    Call point1()
                End If
                PictureBox52.Visible = False
            End If
            If tnt1xy = 309 Or tnt1xy = 409 Or tnt1xy = 209 Or tnt1xy = 310 Or tnt1xy = 308 Then
                PictureBox51.Visible = False
            End If
            If tnt1xy = 405 Or tnt1xy = 505 Or tnt1xy = 305 Or tnt1xy = 406 Or tnt1xy = 404 Then
                PictureBox88.Visible = False
            End If
            If tnt1xy = 406 Or tnt1xy = 506 Or tnt1xy = 306 Or tnt1xy = 407 Or tnt1xy = 405 Then
                PictureBox87.Visible = False
            End If
            If tnt1xy = 407 Or tnt1xy = 507 Or tnt1xy = 307 Or tnt1xy = 408 Or tnt1xy = 406 Then
                PictureBox86.Visible = False
            End If
            If tnt1xy = 408 Or tnt1xy = 508 Or tnt1xy = 308 Or tnt1xy = 409 Or tnt1xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt1xy = 409 Or tnt1xy = 509 Or tnt1xy = 309 Or tnt1xy = 410 Or tnt1xy = 408 Then
                PictureBox84.Visible = False
            End If
            If tnt1xy = 601 Or tnt1xy = 701 Or tnt1xy = 501 Or tnt1xy = 602 Or tnt1xy = 600 Then
                If PictureBox181.Visible = True Then
                    Call point1()
                End If
                PictureBox181.Visible = False
            End If
            If tnt1xy = 607 Or tnt1xy = 707 Or tnt1xy = 507 Or tnt1xy = 608 Or tnt1xy = 606 Then
                PictureBox175.Visible = False
            End If
            If tnt1xy = 608 Or tnt1xy = 708 Or tnt1xy = 508 Or tnt1xy = 609 Or tnt1xy = 607 Then
                PictureBox174.Visible = False
            End If
            If tnt1xy = 612 Or tnt1xy = 712 Or tnt1xy = 512 Or tnt1xy = 613 Or tnt1xy = 611 Then
                If PictureBox170.Visible = True Then
                    Call point1()
                End If
                PictureBox170.Visible = False
            End If
            If tnt1xy = 707 Or tnt1xy = 807 Or tnt1xy = 607 Or tnt1xy = 708 Or tnt1xy = 706 Then
                PictureBox208.Visible = False
            End If
            If tnt1xy = 708 Or tnt1xy = 808 Or tnt1xy = 608 Or tnt1xy = 709 Or tnt1xy = 707 Then
                PictureBox207.Visible = False
            End If
            If tnt1xy = 905 Or tnt1xy = 1005 Or tnt1xy = 805 Or tnt1xy = 906 Or tnt1xy = 904 Then
                PictureBox276.Visible = False
            End If
            If tnt1xy = 906 Or tnt1xy = 1006 Or tnt1xy = 806 Or tnt1xy = 907 Or tnt1xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt1xy = 907 Or tnt1xy = 1007 Or tnt1xy = 807 Or tnt1xy = 908 Or tnt1xy = 906 Then
                PictureBox274.Visible = False
            End If
            If tnt1xy = 908 Or tnt1xy = 1008 Or tnt1xy = 808 Or tnt1xy = 909 Or tnt1xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt1xy = 909 Or tnt1xy = 1009 Or tnt1xy = 809 Or tnt1xy = 910 Or tnt1xy = 908 Then
                PictureBox272.Visible = False
            End If
            If tnt1xy = 1005 Or tnt1xy = 1105 Or tnt1xy = 905 Or tnt1xy = 1006 Or tnt1xy = 1004 Then
                PictureBox309.Visible = False
            End If
            If tnt1xy = 1006 Or tnt1xy = 1106 Or tnt1xy = 906 Or tnt1xy = 1007 Or tnt1xy = 1005 Then
                If PictureBox308.Visible = True Then
                    Call point1()
                End If
                PictureBox308.Visible = False
            End If
            If tnt1xy = 1007 Or tnt1xy = 1107 Or tnt1xy = 907 Or tnt1xy = 1008 Or tnt1xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point1()
                End If
                PictureBox307.Visible = False
            End If
            If tnt1xy = 1008 Or tnt1xy = 1108 Or tnt1xy = 908 Or tnt1xy = 1009 Or tnt1xy = 1007 Then
                If PictureBox306.Visible = True Then
                    Call point1()
                End If
                PictureBox306.Visible = False
            End If
            If tnt1xy = 1009 Or tnt1xy = 1109 Or tnt1xy = 909 Or tnt1xy = 1010 Or tnt1xy = 1008 Then
                PictureBox305.Visible = False
            End If
            If tnt1xy = 1013 Or tnt1xy = 1113 Or tnt1xy = 913 Or tnt1xy = 1014 Or tnt1xy = 1012 Then
                If PictureBox301.Visible = True Then
                    Call point1()
                End If
                PictureBox301.Visible = False
            End If
            If tnt1xy = 1105 Or tnt1xy = 1205 Or tnt1xy = 1005 Or tnt1xy = 1106 Or tnt1xy = 1104 Then
                PictureBox342.Visible = False
            End If
            If tnt1xy = 1106 Or tnt1xy = 1206 Or tnt1xy = 1006 Or tnt1xy = 1107 Or tnt1xy = 1105 Then
                If PictureBox341.Visible = True Then
                    Call point1()
                End If
                PictureBox341.Visible = False
            End If
            If tnt1xy = 1107 Or tnt1xy = 1207 Or tnt1xy = 1007 Or tnt1xy = 1108 Or tnt1xy = 1106 Then
                If PictureBox340.Visible = True Then
                    Call point1()
                End If
                PictureBox340.Visible = False
            End If
            If tnt1xy = 1108 Or tnt1xy = 1208 Or tnt1xy = 1008 Or tnt1xy = 1109 Or tnt1xy = 1107 Then
                If PictureBox339.Visible = True Then
                    Call point1()
                End If
                PictureBox339.Visible = False
            End If
            If tnt1xy = 1109 Or tnt1xy = 1209 Or tnt1xy = 1009 Or tnt1xy = 1110 Or tnt1xy = 1108 Then
                PictureBox338.Visible = False
            End If
            If tnt1xy = 1112 Or tnt1xy = 1212 Or tnt1xy = 1012 Or tnt1xy = 1113 Or tnt1xy = 1111 Then
                PictureBox335.Visible = False
            End If
            If tnt1xy = 1113 Or tnt1xy = 1213 Or tnt1xy = 1013 Or tnt1xy = 1114 Or tnt1xy = 1112 Then
                PictureBox334.Visible = False
            End If
            If tnt1xy = 1205 Or tnt1xy = 1305 Or tnt1xy = 1105 Or tnt1xy = 1206 Or tnt1xy = 1204 Then
                PictureBox375.Visible = False
            End If
            If tnt1xy = 1206 Or tnt1xy = 1306 Or tnt1xy = 1106 Or tnt1xy = 1207 Or tnt1xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt1xy = 1207 Or tnt1xy = 1307 Or tnt1xy = 1107 Or tnt1xy = 1208 Or tnt1xy = 1206 Then
                PictureBox373.Visible = False
            End If
            If tnt1xy = 1208 Or tnt1xy = 1308 Or tnt1xy = 1108 Or tnt1xy = 1209 Or tnt1xy = 1207 Then
                PictureBox372.Visible = False
            End If
            If tnt1xy = 1209 Or tnt1xy = 1309 Or tnt1xy = 1109 Or tnt1xy = 1210 Or tnt1xy = 1208 Then
                PictureBox371.Visible = False
            End If
            If tnt1xy = 1212 Or tnt1xy = 1312 Or tnt1xy = 1112 Or tnt1xy = 1213 Or tnt1xy = 1211 Then
                PictureBox368.Visible = False
            End If
            If tnt1xy = 1213 Or tnt1xy = 1313 Or tnt1xy = 1113 Or tnt1xy = 1214 Or tnt1xy = 1212 Then
                PictureBox367.Visible = False
            End If
        ElseIf map = 6 Then
            If tnt1xy = 101 Or tnt1xy = 201 Or tnt1xy = 1 Or tnt1xy = 102 Or tnt1xy = 100 Then
                If PictureBox104.Visible = True Then
                    Call point1()
                End If
                PictureBox104.Visible = False
            End If
            If tnt1xy = 102 Or tnt1xy = 202 Or tnt1xy = 2 Or tnt1xy = 103 Or tnt1xy = 101 Then
                If PictureBox2.Visible = True Then
                    Call point1()
                End If
                PictureBox2.Visible = False
            End If
            If tnt1xy = 201 Or tnt1xy = 301 Or tnt1xy = 101 Or tnt1xy = 202 Or tnt1xy = 200 Then
                If PictureBox26.Visible = True Then
                    Call point1()
                End If
                PictureBox26.Visible = False
            End If
            If tnt1xy = 202 Or tnt1xy = 302 Or tnt1xy = 102 Or tnt1xy = 203 Or tnt1xy = 201 Then
                If PictureBox25.Visible = True Then
                    Call point1()
                End If
                PictureBox25.Visible = False
            End If
            If tnt1xy = 303 Or tnt1xy = 403 Or tnt1xy = 203 Or tnt1xy = 304 Or tnt1xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt1xy = 306 Or tnt1xy = 406 Or tnt1xy = 206 Or tnt1xy = 307 Or tnt1xy = 305 Then
                PictureBox54.Visible = False
            End If
            If tnt1xy = 307 Or tnt1xy = 407 Or tnt1xy = 207 Or tnt1xy = 308 Or tnt1xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point1()
                End If
                PictureBox53.Visible = False
            End If
            If tnt1xy = 404 Or tnt1xy = 504 Or tnt1xy = 304 Or tnt1xy = 405 Or tnt1xy = 403 Then
                PictureBox89.Visible = False
            End If
            If tnt1xy = 405 Or tnt1xy = 505 Or tnt1xy = 305 Or tnt1xy = 406 Or tnt1xy = 404 Then
                PictureBox88.Visible = False
            End If
            If tnt1xy = 406 Or tnt1xy = 506 Or tnt1xy = 306 Or tnt1xy = 407 Or tnt1xy = 405 Then
                PictureBox87.Visible = False
            End If
            If tnt1xy = 407 Or tnt1xy = 507 Or tnt1xy = 307 Or tnt1xy = 408 Or tnt1xy = 406 Then
                PictureBox86.Visible = False
            End If
            If tnt1xy = 408 Or tnt1xy = 508 Or tnt1xy = 308 Or tnt1xy = 409 Or tnt1xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt1xy = 409 Or tnt1xy = 509 Or tnt1xy = 309 Or tnt1xy = 410 Or tnt1xy = 408 Then
                PictureBox84.Visible = False
            End If
            If tnt1xy = 410 Or tnt1xy = 510 Or tnt1xy = 310 Or tnt1xy = 411 Or tnt1xy = 409 Then
                PictureBox83.Visible = False
            End If
            If tnt1xy = 504 Or tnt1xy = 604 Or tnt1xy = 404 Or tnt1xy = 505 Or tnt1xy = 503 Then
                PictureBox145.Visible = False
            End If
            If tnt1xy = 505 Or tnt1xy = 605 Or tnt1xy = 405 Or tnt1xy = 506 Or tnt1xy = 504 Then
                If PictureBox1.Visible = True Then
                    Call point1()
                End If
                PictureBox1.Visible = False
            End If
            If tnt1xy = 506 Or tnt1xy = 606 Or tnt1xy = 406 Or tnt1xy = 507 Or tnt1xy = 505 Then
                If PictureBox123.Visible = True Then
                    Call point1()
                End If
                PictureBox123.Visible = False
            End If
            If tnt1xy = 507 Or tnt1xy = 607 Or tnt1xy = 407 Or tnt1xy = 508 Or tnt1xy = 506 Then
                If PictureBox122.Visible = True Then
                    Call point1()
                End If
                PictureBox122.Visible = False
            End If
            If tnt1xy = 508 Or tnt1xy = 608 Or tnt1xy = 408 Or tnt1xy = 509 Or tnt1xy = 507 Then
                If PictureBox121.Visible = True Then
                    Call point1()
                End If
                PictureBox121.Visible = False
            End If
            If tnt1xy = 509 Or tnt1xy = 609 Or tnt1xy = 409 Or tnt1xy = 510 Or tnt1xy = 508 Then
                If PictureBox120.Visible = True Then
                    Call point1()
                End If
                PictureBox120.Visible = False
            End If
            If tnt1xy = 510 Or tnt1xy = 610 Or tnt1xy = 410 Or tnt1xy = 511 Or tnt1xy = 509 Then
                PictureBox119.Visible = False
            End If
            If tnt1xy = 604 Or tnt1xy = 704 Or tnt1xy = 504 Or tnt1xy = 605 Or tnt1xy = 603 Then
                If PictureBox178.Visible = True Then
                    Call point1()
                End If
                PictureBox178.Visible = False
            End If
            If tnt1xy = 605 Or tnt1xy = 705 Or tnt1xy = 505 Or tnt1xy = 606 Or tnt1xy = 604 Then
                If PictureBox177.Visible = True Then
                    Call point1()
                End If
                PictureBox177.Visible = False
            End If
            If tnt1xy = 606 Or tnt1xy = 706 Or tnt1xy = 506 Or tnt1xy = 607 Or tnt1xy = 605 Then
                If PictureBox176.Visible = True Then
                    Call point1()
                End If
                PictureBox176.Visible = False
            End If
            If tnt1xy = 607 Or tnt1xy = 707 Or tnt1xy = 507 Or tnt1xy = 608 Or tnt1xy = 606 Then
                If PictureBox175.Visible = True Then
                    Call point1()
                End If
                PictureBox175.Visible = False
            End If
            If tnt1xy = 608 Or tnt1xy = 708 Or tnt1xy = 508 Or tnt1xy = 609 Or tnt1xy = 607 Then
                If PictureBox174.Visible = True Then
                    Call point1()
                End If
                PictureBox174.Visible = False
            End If
            If tnt1xy = 609 Or tnt1xy = 709 Or tnt1xy = 509 Or tnt1xy = 610 Or tnt1xy = 608 Then
                If PictureBox173.Visible = True Then
                    Call point1()
                End If
                PictureBox173.Visible = False
            End If
            If tnt1xy = 704 Or tnt1xy = 804 Or tnt1xy = 604 Or tnt1xy = 705 Or tnt1xy = 703 Then
                If PictureBox211.Visible = True Then
                    Call point1()
                End If
                PictureBox211.Visible = False
            End If
            If tnt1xy = 705 Or tnt1xy = 805 Or tnt1xy = 605 Or tnt1xy = 706 Or tnt1xy = 704 Then
                If PictureBox210.Visible = True Then
                    Call point1()
                End If
                PictureBox210.Visible = False
            End If
            If tnt1xy = 706 Or tnt1xy = 806 Or tnt1xy = 606 Or tnt1xy = 707 Or tnt1xy = 705 Then
                If PictureBox209.Visible = True Then
                    Call point1()
                End If
                PictureBox209.Visible = False
            End If
            If tnt1xy = 707 Or tnt1xy = 807 Or tnt1xy = 607 Or tnt1xy = 708 Or tnt1xy = 706 Then
                If PictureBox208.Visible = True Then
                    Call point1()
                End If
                PictureBox208.Visible = False
            End If
            If tnt1xy = 708 Or tnt1xy = 808 Or tnt1xy = 608 Or tnt1xy = 709 Or tnt1xy = 707 Then
                If PictureBox207.Visible = True Then
                    Call point1()
                End If
                PictureBox207.Visible = False
            End If
            If tnt1xy = 709 Or tnt1xy = 809 Or tnt1xy = 609 Or tnt1xy = 710 Or tnt1xy = 708 Then
                If PictureBox206.Visible = True Then
                    Call point1()
                End If
                PictureBox206.Visible = False
            End If
            If tnt1xy = 710 Or tnt1xy = 810 Or tnt1xy = 610 Or tnt1xy = 711 Or tnt1xy = 709 Then
                If PictureBox205.Visible = True Then
                    Call point1()
                End If
                PictureBox205.Visible = False
            End If
            If tnt1xy = 804 Or tnt1xy = 904 Or tnt1xy = 704 Or tnt1xy = 805 Or tnt1xy = 803 Then
                PictureBox244.Visible = False
            End If
            If tnt1xy = 805 Or tnt1xy = 905 Or tnt1xy = 705 Or tnt1xy = 806 Or tnt1xy = 804 Then
                If PictureBox243.Visible = True Then
                    Call point1()
                End If
                PictureBox243.Visible = False
            End If
            If tnt1xy = 806 Or tnt1xy = 906 Or tnt1xy = 706 Or tnt1xy = 807 Or tnt1xy = 805 Then
                If PictureBox242.Visible = True Then
                    Call point1()
                End If
                PictureBox242.Visible = False
            End If
            If tnt1xy = 807 Or tnt1xy = 907 Or tnt1xy = 707 Or tnt1xy = 808 Or tnt1xy = 806 Then
                If PictureBox241.Visible = True Then
                    Call point1()
                End If
                PictureBox241.Visible = False
            End If
            If tnt1xy = 808 Or tnt1xy = 908 Or tnt1xy = 708 Or tnt1xy = 809 Or tnt1xy = 807 Then
                If PictureBox240.Visible = True Then
                    Call point1()
                End If
                PictureBox240.Visible = False
            End If
            If tnt1xy = 809 Or tnt1xy = 909 Or tnt1xy = 709 Or tnt1xy = 810 Or tnt1xy = 808 Then
                If PictureBox239.Visible = True Then
                    Call point1()
                End If
                PictureBox239.Visible = False
            End If
            If tnt1xy = 810 Or tnt1xy = 910 Or tnt1xy = 710 Or tnt1xy = 811 Or tnt1xy = 809 Then
                PictureBox238.Visible = False
            End If
            If tnt1xy = 904 Or tnt1xy = 1004 Or tnt1xy = 804 Or tnt1xy = 905 Or tnt1xy = 903 Then
                PictureBox277.Visible = False
            End If
            If tnt1xy = 905 Or tnt1xy = 1005 Or tnt1xy = 805 Or tnt1xy = 906 Or tnt1xy = 904 Then
                PictureBox276.Visible = False
            End If
            If tnt1xy = 906 Or tnt1xy = 1006 Or tnt1xy = 806 Or tnt1xy = 907 Or tnt1xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt1xy = 907 Or tnt1xy = 1007 Or tnt1xy = 807 Or tnt1xy = 908 Or tnt1xy = 906 Then
                PictureBox274.Visible = False
            End If
            If tnt1xy = 908 Or tnt1xy = 1008 Or tnt1xy = 808 Or tnt1xy = 909 Or tnt1xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt1xy = 909 Or tnt1xy = 1009 Or tnt1xy = 809 Or tnt1xy = 910 Or tnt1xy = 908 Then
                PictureBox272.Visible = False
            End If
            If tnt1xy = 910 Or tnt1xy = 1010 Or tnt1xy = 810 Or tnt1xy = 911 Or tnt1xy = 909 Then
                PictureBox271.Visible = False
            End If
            If tnt1xy = 1006 Or tnt1xy = 1106 Or tnt1xy = 906 Or tnt1xy = 1007 Or tnt1xy = 1005 Then
                PictureBox308.Visible = False
            End If
            If tnt1xy = 1007 Or tnt1xy = 1107 Or tnt1xy = 907 Or tnt1xy = 1008 Or tnt1xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point1()
                End If
                PictureBox307.Visible = False
            End If
            If tnt1xy = 1008 Or tnt1xy = 1108 Or tnt1xy = 908 Or tnt1xy = 1009 Or tnt1xy = 1007 Then
                PictureBox306.Visible = False
            End If
            If tnt1xy = 1011 Or tnt1xy = 1111 Or tnt1xy = 911 Or tnt1xy = 1012 Or tnt1xy = 1010 Then
                PictureBox303.Visible = False
            End If
            If tnt1xy = 1112 Or tnt1xy = 1212 Or tnt1xy = 1012 Or tnt1xy = 1113 Or tnt1xy = 1111 Then
                If PictureBox335.Visible = True Then
                    Call point1()
                End If
                PictureBox335.Visible = False
            End If
            If tnt1xy = 1113 Or tnt1xy = 1213 Or tnt1xy = 1013 Or tnt1xy = 1114 Or tnt1xy = 1112 Then
                If PictureBox334.Visible = True Then
                    Call point1()
                End If
                PictureBox334.Visible = False
            End If
            If tnt1xy = 1204 Or tnt1xy = 1304 Or tnt1xy = 1104 Or tnt1xy = 1205 Or tnt1xy = 1203 Then
                PictureBox376.Visible = False
            End If
            If tnt1xy = 1212 Or tnt1xy = 1312 Or tnt1xy = 1112 Or tnt1xy = 1213 Or tnt1xy = 1211 Then
                If PictureBox368.Visible = True Then
                    Call point1()
                End If
                PictureBox368.Visible = False
            End If
            If tnt1xy = 1213 Or tnt1xy = 1313 Or tnt1xy = 1113 Or tnt1xy = 1214 Or tnt1xy = 1212 Then
                If PictureBox367.Visible = True Then
                    Call point1()
                End If
                PictureBox367.Visible = False
            End If
        ElseIf map = 7 Then
            If tnt1xy = 102 Or tnt1xy = 202 Or tnt1xy = 2 Or tnt1xy = 103 Or tnt1xy = 101 Then
                PictureBox2.Visible = False
            End If
            If tnt1xy = 103 Or tnt1xy = 203 Or tnt1xy = 3 Or tnt1xy = 104 Or tnt1xy = 102 Then
                PictureBox3.Visible = False
            End If
            If tnt1xy = 104 Or tnt1xy = 204 Or tnt1xy = 4 Or tnt1xy = 105 Or tnt1xy = 103 Then
                If PictureBox4.Visible = True Then
                    Call point1()
                End If
                PictureBox4.Visible = False
            End If
            If tnt1xy = 105 Or tnt1xy = 205 Or tnt1xy = 5 Or tnt1xy = 106 Or tnt1xy = 104 Then
                PictureBox5.Visible = False
            End If
            If tnt1xy = 109 Or tnt1xy = 209 Or tnt1xy = 9 Or tnt1xy = 110 Or tnt1xy = 108 Then
                PictureBox9.Visible = False
            End If
            If tnt1xy = 110 Or tnt1xy = 210 Or tnt1xy = 10 Or tnt1xy = 111 Or tnt1xy = 109 Then
                If PictureBox10.Visible = True Then
                    Call point1()
                End If
                PictureBox10.Visible = False
            End If
            If tnt1xy = 111 Or tnt1xy = 211 Or tnt1xy = 11 Or tnt1xy = 112 Or tnt1xy = 110 Then
                PictureBox11.Visible = False
            End If
            If tnt1xy = 201 Or tnt1xy = 301 Or tnt1xy = 101 Or tnt1xy = 202 Or tnt1xy = 200 Then
                PictureBox26.Visible = False
            End If
            If tnt1xy = 204 Or tnt1xy = 304 Or tnt1xy = 104 Or tnt1xy = 205 Or tnt1xy = 203 Then
                PictureBox23.Visible = False
            End If
            If tnt1xy = 210 Or tnt1xy = 310 Or tnt1xy = 110 Or tnt1xy = 211 Or tnt1xy = 209 Then
                PictureBox17.Visible = False
            End If
            If tnt1xy = 303 Or tnt1xy = 403 Or tnt1xy = 203 Or tnt1xy = 304 Or tnt1xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt1xy = 304 Or tnt1xy = 404 Or tnt1xy = 204 Or tnt1xy = 305 Or tnt1xy = 303 Then
                If PictureBox56.Visible = True Then
                    Call point1()
                End If
                PictureBox56.Visible = False
            End If
            If tnt1xy = 305 Or tnt1xy = 405 Or tnt1xy = 205 Or tnt1xy = 306 Or tnt1xy = 304 Then
                PictureBox55.Visible = False
            End If
            If tnt1xy = 306 Or tnt1xy = 406 Or tnt1xy = 206 Or tnt1xy = 307 Or tnt1xy = 305 Then
                If PictureBox54.Visible = True Then
                    Call point1()
                End If
                PictureBox54.Visible = False
            End If
            If tnt1xy = 307 Or tnt1xy = 407 Or tnt1xy = 207 Or tnt1xy = 308 Or tnt1xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point1()
                End If
                PictureBox53.Visible = False
            End If
            If tnt1xy = 308 Or tnt1xy = 408 Or tnt1xy = 208 Or tnt1xy = 309 Or tnt1xy = 307 Then
                If PictureBox52.Visible = True Then
                    Call point1()
                End If
                PictureBox52.Visible = False
            End If
            If tnt1xy = 309 Or tnt1xy = 409 Or tnt1xy = 209 Or tnt1xy = 310 Or tnt1xy = 308 Then
                PictureBox51.Visible = False
            End If
            If tnt1xy = 310 Or tnt1xy = 410 Or tnt1xy = 210 Or tnt1xy = 311 Or tnt1xy = 309 Then
                If PictureBox50.Visible = True Then
                    Call point1()
                End If
                PictureBox50.Visible = False
            End If
            If tnt1xy = 311 Or tnt1xy = 411 Or tnt1xy = 211 Or tnt1xy = 312 Or tnt1xy = 310 Then
                PictureBox49.Visible = False
            End If
            If tnt1xy = 403 Or tnt1xy = 503 Or tnt1xy = 303 Or tnt1xy = 404 Or tnt1xy = 402 Then
                If PictureBox90.Visible = True Then
                    Call point1()
                End If
                PictureBox90.Visible = False
            End If
            If tnt1xy = 408 Or tnt1xy = 508 Or tnt1xy = 308 Or tnt1xy = 409 Or tnt1xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt1xy = 411 Or tnt1xy = 511 Or tnt1xy = 311 Or tnt1xy = 412 Or tnt1xy = 410 Then
                If PictureBox82.Visible = True Then
                    Call point1()
                End If
                PictureBox82.Visible = False
            End If
            If tnt1xy = 501 Or tnt1xy = 601 Or tnt1xy = 401 Or tnt1xy = 502 Or tnt1xy = 500 Then
                PictureBox148.Visible = False
            End If
            If tnt1xy = 503 Or tnt1xy = 603 Or tnt1xy = 403 Or tnt1xy = 504 Or tnt1xy = 502 Then
                PictureBox146.Visible = False
            End If
            If tnt1xy = 505 Or tnt1xy = 605 Or tnt1xy = 405 Or tnt1xy = 506 Or tnt1xy = 504 Then
                If PictureBox1.Visible = True Then
                    Call point1()
                End If
                PictureBox1.Visible = False
            End If
            If tnt1xy = 506 Or tnt1xy = 606 Or tnt1xy = 406 Or tnt1xy = 507 Or tnt1xy = 505 Then
                PictureBox123.Visible = False
            End If
            If tnt1xy = 507 Or tnt1xy = 607 Or tnt1xy = 407 Or tnt1xy = 508 Or tnt1xy = 506 Then
                If PictureBox122.Visible = True Then
                    Call point1()
                End If
                PictureBox122.Visible = False
            End If
            If tnt1xy = 508 Or tnt1xy = 608 Or tnt1xy = 408 Or tnt1xy = 509 Or tnt1xy = 507 Then
                If PictureBox121.Visible = True Then
                    Call point1()
                End If
                PictureBox121.Visible = False
            End If
            If tnt1xy = 509 Or tnt1xy = 609 Or tnt1xy = 409 Or tnt1xy = 510 Or tnt1xy = 508 Then
                If PictureBox120.Visible = True Then
                    Call point1()
                End If
                PictureBox120.Visible = False
            End If
            If tnt1xy = 511 Or tnt1xy = 611 Or tnt1xy = 411 Or tnt1xy = 512 Or tnt1xy = 510 Then
                PictureBox118.Visible = False
            End If
            If tnt1xy = 513 Or tnt1xy = 613 Or tnt1xy = 413 Or tnt1xy = 514 Or tnt1xy = 512 Then
                PictureBox116.Visible = False
            End If
            If tnt1xy = 601 Or tnt1xy = 701 Or tnt1xy = 501 Or tnt1xy = 602 Or tnt1xy = 600 Then
                If PictureBox181.Visible = True Then
                    Call point1()
                End If
                PictureBox181.Visible = False
            End If
            If tnt1xy = 602 Or tnt1xy = 702 Or tnt1xy = 502 Or tnt1xy = 603 Or tnt1xy = 601 Then
                PictureBox180.Visible = False
            End If
            If tnt1xy = 603 Or tnt1xy = 703 Or tnt1xy = 503 Or tnt1xy = 604 Or tnt1xy = 602 Then
                If PictureBox179.Visible = True Then
                    Call point1()
                End If
                PictureBox179.Visible = False
            End If
            If tnt1xy = 604 Or tnt1xy = 704 Or tnt1xy = 504 Or tnt1xy = 605 Or tnt1xy = 603 Then
                PictureBox178.Visible = False
            End If
            If tnt1xy = 605 Or tnt1xy = 705 Or tnt1xy = 505 Or tnt1xy = 606 Or tnt1xy = 604 Then
                If PictureBox177.Visible = True Then
                    Call point1()
                End If
                PictureBox177.Visible = False
            End If
            If tnt1xy = 607 Or tnt1xy = 707 Or tnt1xy = 507 Or tnt1xy = 608 Or tnt1xy = 606 Then
                If PictureBox175.Visible = True Then
                    Call point1()
                End If
                PictureBox175.Visible = False
            End If
            If tnt1xy = 608 Or tnt1xy = 708 Or tnt1xy = 508 Or tnt1xy = 609 Or tnt1xy = 607 Then
                If PictureBox174.Visible = True Then
                    Call point1()
                End If
                PictureBox174.Visible = False
            End If
            If tnt1xy = 609 Or tnt1xy = 709 Or tnt1xy = 509 Or tnt1xy = 610 Or tnt1xy = 608 Then
                If PictureBox173.Visible = True Then
                    Call point1()
                End If
                PictureBox173.Visible = False
            End If
            If tnt1xy = 611 Or tnt1xy = 711 Or tnt1xy = 511 Or tnt1xy = 612 Or tnt1xy = 610 Then
                If PictureBox171.Visible = True Then
                    Call point1()
                End If
                PictureBox171.Visible = False
            End If
            If tnt1xy = 612 Or tnt1xy = 712 Or tnt1xy = 512 Or tnt1xy = 613 Or tnt1xy = 611 Then
                PictureBox170.Visible = False
            End If
            If tnt1xy = 613 Or tnt1xy = 713 Or tnt1xy = 513 Or tnt1xy = 614 Or tnt1xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point1()
                End If
                PictureBox169.Visible = False
            End If
            If tnt1xy = 701 Or tnt1xy = 801 Or tnt1xy = 601 Or tnt1xy = 702 Or tnt1xy = 700 Then
                PictureBox214.Visible = False
            End If
            If tnt1xy = 705 Or tnt1xy = 805 Or tnt1xy = 605 Or tnt1xy = 706 Or tnt1xy = 704 Then
                PictureBox210.Visible = False
            End If
            If tnt1xy = 709 Or tnt1xy = 809 Or tnt1xy = 609 Or tnt1xy = 710 Or tnt1xy = 708 Then
                PictureBox206.Visible = False
            End If
            If tnt1xy = 711 Or tnt1xy = 811 Or tnt1xy = 611 Or tnt1xy = 712 Or tnt1xy = 710 Then
                PictureBox204.Visible = False
            End If
            If tnt1xy = 713 Or tnt1xy = 813 Or tnt1xy = 613 Or tnt1xy = 714 Or tnt1xy = 712 Then
                PictureBox202.Visible = False
            End If
            If tnt1xy = 801 Or tnt1xy = 901 Or tnt1xy = 701 Or tnt1xy = 802 Or tnt1xy = 800 Then
                If PictureBox247.Visible = True Then
                    Call point1()
                End If
                PictureBox247.Visible = False
            End If
            If tnt1xy = 802 Or tnt1xy = 902 Or tnt1xy = 702 Or tnt1xy = 803 Or tnt1xy = 801 Then
                PictureBox246.Visible = False
            End If
            If tnt1xy = 803 Or tnt1xy = 903 Or tnt1xy = 703 Or tnt1xy = 804 Or tnt1xy = 802 Then
                If PictureBox245.Visible = True Then
                    Call point1()
                End If
                PictureBox245.Visible = False
            End If
            If tnt1xy = 805 Or tnt1xy = 905 Or tnt1xy = 705 Or tnt1xy = 806 Or tnt1xy = 804 Then
                If PictureBox243.Visible = True Then
                    Call point1()
                End If
                PictureBox243.Visible = False
            End If
            If tnt1xy = 806 Or tnt1xy = 906 Or tnt1xy = 706 Or tnt1xy = 807 Or tnt1xy = 805 Then
                PictureBox242.Visible = False
            End If
            If tnt1xy = 807 Or tnt1xy = 907 Or tnt1xy = 707 Or tnt1xy = 808 Or tnt1xy = 806 Then
                If PictureBox241.Visible = True Then
                    Call point1()
                End If
                PictureBox241.Visible = False
            End If
            If tnt1xy = 808 Or tnt1xy = 908 Or tnt1xy = 708 Or tnt1xy = 809 Or tnt1xy = 807 Then
                If PictureBox240.Visible = True Then
                    Call point1()
                End If
                PictureBox240.Visible = False
            End If
            If tnt1xy = 809 Or tnt1xy = 909 Or tnt1xy = 709 Or tnt1xy = 810 Or tnt1xy = 808 Then
                If PictureBox239.Visible = True Then
                    Call point1()
                End If
                PictureBox239.Visible = False
            End If
            If tnt1xy = 810 Or tnt1xy = 910 Or tnt1xy = 710 Or tnt1xy = 811 Or tnt1xy = 809 Then
                PictureBox238.Visible = False
            End If
            If tnt1xy = 811 Or tnt1xy = 911 Or tnt1xy = 711 Or tnt1xy = 812 Or tnt1xy = 810 Then
                If PictureBox237.Visible = True Then
                    Call point1()
                End If
                PictureBox237.Visible = False
            End If
            If tnt1xy = 901 Or tnt1xy = 1001 Or tnt1xy = 801 Or tnt1xy = 902 Or tnt1xy = 900 Then
                PictureBox280.Visible = False
            End If
            If tnt1xy = 903 Or tnt1xy = 1003 Or tnt1xy = 803 Or tnt1xy = 904 Or tnt1xy = 902 Then
                If PictureBox278.Visible = True Then
                    Call point1()
                End If
                PictureBox278.Visible = False
            End If
            If tnt1xy = 905 Or tnt1xy = 1005 Or tnt1xy = 805 Or tnt1xy = 906 Or tnt1xy = 904 Then
                PictureBox276.Visible = False
            End If
            If tnt1xy = 911 Or tnt1xy = 1011 Or tnt1xy = 811 Or tnt1xy = 912 Or tnt1xy = 910 Then
                PictureBox270.Visible = False
            End If
            If tnt1xy = 1003 Or tnt1xy = 1103 Or tnt1xy = 903 Or tnt1xy = 1004 Or tnt1xy = 1002 Then
                If PictureBox311.Visible = True Then
                    Call point1()
                End If
                PictureBox311.Visible = False
            End If
            If tnt1xy = 1004 Or tnt1xy = 1104 Or tnt1xy = 904 Or tnt1xy = 1005 Or tnt1xy = 1003 Then
                PictureBox310.Visible = False
            End If
            If tnt1xy = 1005 Or tnt1xy = 1105 Or tnt1xy = 905 Or tnt1xy = 1006 Or tnt1xy = 1004 Then
                If PictureBox309.Visible = True Then
                    Call point1()
                End If
                PictureBox309.Visible = False
            End If
            If tnt1xy = 1006 Or tnt1xy = 1106 Or tnt1xy = 906 Or tnt1xy = 1007 Or tnt1xy = 1005 Then
                PictureBox308.Visible = False
            End If
            If tnt1xy = 1007 Or tnt1xy = 1107 Or tnt1xy = 907 Or tnt1xy = 1008 Or tnt1xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point1()
                End If
                PictureBox307.Visible = False
            End If
            If tnt1xy = 1008 Or tnt1xy = 1108 Or tnt1xy = 908 Or tnt1xy = 1009 Or tnt1xy = 1007 Then
                If PictureBox306.Visible = True Then
                    Call point1()
                End If
                PictureBox306.Visible = False
            End If
            If tnt1xy = 1009 Or tnt1xy = 1109 Or tnt1xy = 909 Or tnt1xy = 1010 Or tnt1xy = 1008 Then
                If PictureBox305.Visible = True Then
                    Call point1()
                End If
                PictureBox305.Visible = False
            End If
            If tnt1xy = 1010 Or tnt1xy = 1110 Or tnt1xy = 910 Or tnt1xy = 1011 Or tnt1xy = 1009 Then
                If PictureBox304.Visible = True Then
                    Call point1()
                End If
                PictureBox304.Visible = False
            End If
            If tnt1xy = 1011 Or tnt1xy = 1111 Or tnt1xy = 911 Or tnt1xy = 1012 Or tnt1xy = 1010 Then
                If PictureBox303.Visible = True Then
                    Call point1()
                End If
                PictureBox303.Visible = False
            End If
            If tnt1xy = 1105 Or tnt1xy = 1205 Or tnt1xy = 1005 Or tnt1xy = 1106 Or tnt1xy = 1104 Then
                PictureBox342.Visible = False
            End If
            If tnt1xy = 1111 Or tnt1xy = 1211 Or tnt1xy = 1011 Or tnt1xy = 1112 Or tnt1xy = 1110 Then
                PictureBox336.Visible = False
            End If
            If tnt1xy = 1204 Or tnt1xy = 1304 Or tnt1xy = 1104 Or tnt1xy = 1205 Or tnt1xy = 1203 Then
                PictureBox376.Visible = False
            End If
            If tnt1xy = 1205 Or tnt1xy = 1305 Or tnt1xy = 1105 Or tnt1xy = 1206 Or tnt1xy = 1204 Then
                If PictureBox375.Visible = True Then
                    Call point1()
                End If
                PictureBox375.Visible = False
            End If
            If tnt1xy = 1206 Or tnt1xy = 1306 Or tnt1xy = 1106 Or tnt1xy = 1207 Or tnt1xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt1xy = 1212 Or tnt1xy = 1312 Or tnt1xy = 1112 Or tnt1xy = 1213 Or tnt1xy = 1211 Then
                PictureBox368.Visible = False
            End If
        End If
    End Sub
    '2PTNT炸[可破壞牆壁]判斷&執行(function)
    Private Sub tnt_2()
        If map = 0 Then
            If tnt2xy = 106 Or tnt2xy = 206 Or tnt2xy = 6 Or tnt2xy = 107 Or tnt2xy = 105 Then
                PictureBox6.Visible = False
            End If
            If tnt2xy = 107 Or tnt2xy = 207 Or tnt2xy = 7 Or tnt2xy = 108 Or tnt2xy = 106 Then
                If PictureBox7.Visible = True Then
                    Call point2()
                End If
                PictureBox7.Visible = False
            End If
            If tnt2xy = 108 Or tnt2xy = 208 Or tnt2xy = 8 Or tnt2xy = 109 Or tnt2xy = 107 Then
                PictureBox8.Visible = False
            End If
            If tnt2xy = 301 Or tnt2xy = 401 Or tnt2xy = 201 Or tnt2xy = 302 Or tnt2xy = 300 Then
                PictureBox59.Visible = False
            End If
            If tnt2xy = 303 Or tnt2xy = 403 Or tnt2xy = 203 Or tnt2xy = 304 Or tnt2xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt2xy = 307 Or tnt2xy = 407 Or tnt2xy = 207 Or tnt2xy = 308 Or tnt2xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point2()
                End If
                PictureBox53.Visible = False
            End If
            If tnt2xy = 311 Or tnt2xy = 411 Or tnt2xy = 211 Or tnt2xy = 312 Or tnt2xy = 310 Then
                PictureBox49.Visible = False
            End If
            If tnt2xy = 313 Or tnt2xy = 413 Or tnt2xy = 213 Or tnt2xy = 314 Or tnt2xy = 312 Then
                PictureBox47.Visible = False
            End If
            If tnt2xy = 401 Or tnt2xy = 501 Or tnt2xy = 301 Or tnt2xy = 402 Or tnt2xy = 400 Then
                PictureBox92.Visible = False
            End If
            If tnt2xy = 403 Or tnt2xy = 503 Or tnt2xy = 303 Or tnt2xy = 404 Or tnt2xy = 402 Then
                PictureBox90.Visible = False
            End If
            If tnt2xy = 405 Or tnt2xy = 505 Or tnt2xy = 305 Or tnt2xy = 406 Or tnt2xy = 404 Then
                If PictureBox88.Visible = True Then
                    Call point2()
                End If
                PictureBox88.Visible = False
            End If
            If tnt2xy = 406 Or tnt2xy = 506 Or tnt2xy = 306 Or tnt2xy = 407 Or tnt2xy = 405 Then
                PictureBox87.Visible = False
            End If
            If tnt2xy = 407 Or tnt2xy = 507 Or tnt2xy = 307 Or tnt2xy = 408 Or tnt2xy = 406 Then
                PictureBox86.Visible = False
            End If
            If tnt2xy = 408 Or tnt2xy = 508 Or tnt2xy = 308 Or tnt2xy = 409 Or tnt2xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt2xy = 409 Or tnt2xy = 509 Or tnt2xy = 309 Or tnt2xy = 410 Or tnt2xy = 408 Then
                If PictureBox84.Visible = True Then
                    Call point2()
                End If
                PictureBox84.Visible = False
            End If
            If tnt2xy = 411 Or tnt2xy = 511 Or tnt2xy = 311 Or tnt2xy = 412 Or tnt2xy = 410 Then
                PictureBox82.Visible = False
            End If
            If tnt2xy = 413 Or tnt2xy = 513 Or tnt2xy = 313 Or tnt2xy = 414 Or tnt2xy = 412 Then
                PictureBox80.Visible = False
            End If
            If tnt2xy = 501 Or tnt2xy = 601 Or tnt2xy = 401 Or tnt2xy = 502 Or tnt2xy = 500 Then
                PictureBox148.Visible = False
            End If
            If tnt2xy = 503 Or tnt2xy = 603 Or tnt2xy = 403 Or tnt2xy = 504 Or tnt2xy = 502 Then
                PictureBox146.Visible = False
            End If
            If tnt2xy = 505 Or tnt2xy = 605 Or tnt2xy = 405 Or tnt2xy = 506 Or tnt2xy = 504 Then
                PictureBox1.Visible = False
            End If
            If tnt2xy = 509 Or tnt2xy = 609 Or tnt2xy = 409 Or tnt2xy = 510 Or tnt2xy = 508 Then
                PictureBox120.Visible = False
            End If
            If tnt2xy = 511 Or tnt2xy = 611 Or tnt2xy = 411 Or tnt2xy = 512 Or tnt2xy = 510 Then
                PictureBox118.Visible = False
            End If
            If tnt2xy = 513 Or tnt2xy = 613 Or tnt2xy = 413 Or tnt2xy = 514 Or tnt2xy = 512 Then
                PictureBox116.Visible = False
            End If
            If tnt2xy = 601 Or tnt2xy = 701 Or tnt2xy = 501 Or tnt2xy = 602 Or tnt2xy = 600 Then
                If PictureBox181.Visible = True Then
                    Call point2()
                End If
                PictureBox181.Visible = False
            End If
            If tnt2xy = 603 Or tnt2xy = 703 Or tnt2xy = 503 Or tnt2xy = 604 Or tnt2xy = 602 Then
                PictureBox179.Visible = False
            End If
            If tnt2xy = 604 Or tnt2xy = 704 Or tnt2xy = 504 Or tnt2xy = 605 Or tnt2xy = 603 Then
                If PictureBox178.Visible = True Then
                    Call point2()
                End If
                PictureBox178.Visible = False
            End If
            If tnt2xy = 605 Or tnt2xy = 705 Or tnt2xy = 505 Or tnt2xy = 606 Or tnt2xy = 604 Then
                PictureBox177.Visible = False
            End If
            If tnt2xy = 606 Or tnt2xy = 706 Or tnt2xy = 506 Or tnt2xy = 607 Or tnt2xy = 605 Then
                PictureBox176.Visible = False
            End If
            If tnt2xy = 607 Or tnt2xy = 707 Or tnt2xy = 507 Or tnt2xy = 608 Or tnt2xy = 606 Then
                If PictureBox175.Visible = True Then
                    Call point2()
                End If
                PictureBox175.Visible = False
            End If
            If tnt2xy = 608 Or tnt2xy = 708 Or tnt2xy = 508 Or tnt2xy = 609 Or tnt2xy = 607 Then
                PictureBox174.Visible = False
            End If
            If tnt2xy = 609 Or tnt2xy = 709 Or tnt2xy = 509 Or tnt2xy = 610 Or tnt2xy = 608 Then
                PictureBox173.Visible = False
            End If
            If tnt2xy = 610 Or tnt2xy = 710 Or tnt2xy = 510 Or tnt2xy = 611 Or tnt2xy = 609 Then
                If PictureBox172.Visible = True Then
                    Call point2()
                End If
                PictureBox172.Visible = False
            End If
            If tnt2xy = 611 Or tnt2xy = 711 Or tnt2xy = 511 Or tnt2xy = 612 Or tnt2xy = 610 Then
                PictureBox171.Visible = False
            End If
            If tnt2xy = 613 Or tnt2xy = 713 Or tnt2xy = 513 Or tnt2xy = 614 Or tnt2xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point2()
                End If
                PictureBox169.Visible = False
            End If
            If tnt2xy = 701 Or tnt2xy = 801 Or tnt2xy = 601 Or tnt2xy = 702 Or tnt2xy = 700 Then
                If PictureBox214.Visible = True Then
                    Call point2()
                End If
                PictureBox214.Visible = False
            End If
            If tnt2xy = 703 Or tnt2xy = 803 Or tnt2xy = 603 Or tnt2xy = 704 Or tnt2xy = 702 Then
                PictureBox212.Visible = False
            End If
            If tnt2xy = 704 Or tnt2xy = 804 Or tnt2xy = 604 Or tnt2xy = 705 Or tnt2xy = 703 Then
                If PictureBox211.Visible = True Then
                    Call point2()
                End If
                PictureBox211.Visible = False
            End If
            If tnt2xy = 705 Or tnt2xy = 805 Or tnt2xy = 605 Or tnt2xy = 706 Or tnt2xy = 704 Then
                PictureBox210.Visible = False
            End If
            If tnt2xy = 706 Or tnt2xy = 806 Or tnt2xy = 606 Or tnt2xy = 707 Or tnt2xy = 705 Then
                PictureBox209.Visible = False
            End If
            If tnt2xy = 707 Or tnt2xy = 807 Or tnt2xy = 607 Or tnt2xy = 708 Or tnt2xy = 706 Then
                If PictureBox208.Visible = True Then
                    Call point2()
                End If
                PictureBox208.Visible = False
            End If
            If tnt2xy = 708 Or tnt2xy = 808 Or tnt2xy = 608 Or tnt2xy = 709 Or tnt2xy = 707 Then
                PictureBox207.Visible = False
            End If
            If tnt2xy = 709 Or tnt2xy = 809 Or tnt2xy = 609 Or tnt2xy = 710 Or tnt2xy = 708 Then
                PictureBox206.Visible = False
            End If
            If tnt2xy = 710 Or tnt2xy = 810 Or tnt2xy = 610 Or tnt2xy = 711 Or tnt2xy = 709 Then
                If PictureBox205.Visible = True Then
                    Call point2()
                End If
                PictureBox205.Visible = False
            End If
            If tnt2xy = 711 Or tnt2xy = 811 Or tnt2xy = 611 Or tnt2xy = 712 Or tnt2xy = 710 Then
                PictureBox204.Visible = False
            End If
            If tnt2xy = 713 Or tnt2xy = 813 Or tnt2xy = 613 Or tnt2xy = 714 Or tnt2xy = 712 Then
                If PictureBox202.Visible = True Then
                    Call point2()
                End If
                PictureBox202.Visible = False
            End If
            If tnt2xy = 801 Or tnt2xy = 901 Or tnt2xy = 701 Or tnt2xy = 802 Or tnt2xy = 800 Then
                PictureBox247.Visible = False
            End If
            If tnt2xy = 803 Or tnt2xy = 903 Or tnt2xy = 703 Or tnt2xy = 804 Or tnt2xy = 802 Then
                PictureBox245.Visible = False
            End If
            If tnt2xy = 805 Or tnt2xy = 905 Or tnt2xy = 705 Or tnt2xy = 806 Or tnt2xy = 804 Then
                PictureBox243.Visible = False
            End If
            If tnt2xy = 809 Or tnt2xy = 909 Or tnt2xy = 709 Or tnt2xy = 810 Or tnt2xy = 808 Then
                PictureBox239.Visible = False
            End If
            If tnt2xy = 811 Or tnt2xy = 911 Or tnt2xy = 711 Or tnt2xy = 812 Or tnt2xy = 810 Then
                PictureBox237.Visible = False
            End If
            If tnt2xy = 813 Or tnt2xy = 913 Or tnt2xy = 713 Or tnt2xy = 814 Or tnt2xy = 812 Then
                PictureBox235.Visible = False
            End If
            If tnt2xy = 901 Or tnt2xy = 1001 Or tnt2xy = 801 Or tnt2xy = 902 Or tnt2xy = 900 Then
                PictureBox280.Visible = False
            End If
            If tnt2xy = 903 Or tnt2xy = 1003 Or tnt2xy = 803 Or tnt2xy = 904 Or tnt2xy = 902 Then
                PictureBox278.Visible = False
            End If
            If tnt2xy = 905 Or tnt2xy = 1005 Or tnt2xy = 805 Or tnt2xy = 906 Or tnt2xy = 904 Then
                If PictureBox276.Visible = True Then
                    Call point2()
                End If
                PictureBox276.Visible = False
            End If
            If tnt2xy = 906 Or tnt2xy = 1006 Or tnt2xy = 806 Or tnt2xy = 907 Or tnt2xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt2xy = 907 Or tnt2xy = 1007 Or tnt2xy = 807 Or tnt2xy = 908 Or tnt2xy = 906 Then
                PictureBox274.Visible = False
            End If
            If tnt2xy = 908 Or tnt2xy = 1008 Or tnt2xy = 808 Or tnt2xy = 909 Or tnt2xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt2xy = 909 Or tnt2xy = 1009 Or tnt2xy = 809 Or tnt2xy = 910 Or tnt2xy = 908 Then
                If PictureBox272.Visible = True Then
                    Call point2()
                End If
                PictureBox272.Visible = False
            End If
            If tnt2xy = 911 Or tnt2xy = 1011 Or tnt2xy = 811 Or tnt2xy = 912 Or tnt2xy = 910 Then
                PictureBox270.Visible = False
            End If
            If tnt2xy = 913 Or tnt2xy = 1013 Or tnt2xy = 813 Or tnt2xy = 914 Or tnt2xy = 912 Then
                PictureBox268.Visible = False
            End If
            If tnt2xy = 1001 Or tnt2xy = 1101 Or tnt2xy = 901 Or tnt2xy = 1002 Or tnt2xy = 1000 Then
                PictureBox313.Visible = False
            End If
            If tnt2xy = 1003 Or tnt2xy = 1103 Or tnt2xy = 903 Or tnt2xy = 1004 Or tnt2xy = 1002 Then
                PictureBox311.Visible = False
            End If
            If tnt2xy = 1007 Or tnt2xy = 1107 Or tnt2xy = 907 Or tnt2xy = 1008 Or tnt2xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point2()
                End If
                PictureBox307.Visible = False
            End If
            If tnt2xy = 1011 Or tnt2xy = 1111 Or tnt2xy = 911 Or tnt2xy = 1012 Or tnt2xy = 1010 Then
                PictureBox303.Visible = False
            End If
            If tnt2xy = 1013 Or tnt2xy = 1113 Or tnt2xy = 913 Or tnt2xy = 1014 Or tnt2xy = 1012 Then
                PictureBox301.Visible = False
            End If
            If tnt2xy = 1206 Or tnt2xy = 1306 Or tnt2xy = 1106 Or tnt2xy = 1207 Or tnt2xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt2xy = 1207 Or tnt2xy = 1307 Or tnt2xy = 1107 Or tnt2xy = 1208 Or tnt2xy = 1206 Then
                If PictureBox373.Visible = True Then
                    Call point2()
                End If
                PictureBox373.Visible = False
            End If
            If tnt2xy = 1208 Or tnt2xy = 1308 Or tnt2xy = 1108 Or tnt2xy = 1209 Or tnt2xy = 1207 Then
                PictureBox372.Visible = False
            End If
        ElseIf map = 1 Then
            If tnt2xy = 101 Or tnt2xy = 201 Or tnt2xy = 1 Or tnt2xy = 102 Or tnt2xy = 100 Then
                If PictureBox104.Visible = True Then
                    Call point2()
                End If
                PictureBox104.Visible = False
            End If
            If tnt2xy = 104 Or tnt2xy = 204 Or tnt2xy = 4 Or tnt2xy = 105 Or tnt2xy = 103 Then
                PictureBox4.Visible = False
            End If
            If tnt2xy = 105 Or tnt2xy = 205 Or tnt2xy = 5 Or tnt2xy = 106 Or tnt2xy = 104 Then
                If PictureBox5.Visible = True Then
                    Call point2()
                End If
                PictureBox5.Visible = False
            End If
            If tnt2xy = 108 Or tnt2xy = 208 Or tnt2xy = 8 Or tnt2xy = 109 Or tnt2xy = 107 Then
                PictureBox8.Visible = False
            End If
            If tnt2xy = 109 Or tnt2xy = 209 Or tnt2xy = 9 Or tnt2xy = 110 Or tnt2xy = 108 Then
                If PictureBox9.Visible = True Then
                    Call point2()
                End If
                PictureBox9.Visible = False
            End If
            If tnt2xy = 112 Or tnt2xy = 212 Or tnt2xy = 12 Or tnt2xy = 113 Or tnt2xy = 111 Then
                PictureBox12.Visible = False
            End If
            If tnt2xy = 202 Or tnt2xy = 302 Or tnt2xy = 102 Or tnt2xy = 203 Or tnt2xy = 201 Then
                PictureBox25.Visible = False
            End If
            If tnt2xy = 206 Or tnt2xy = 306 Or tnt2xy = 106 Or tnt2xy = 207 Or tnt2xy = 205 Then
                PictureBox21.Visible = False
            End If
            If tnt2xy = 210 Or tnt2xy = 310 Or tnt2xy = 110 Or tnt2xy = 211 Or tnt2xy = 209 Then
                PictureBox17.Visible = False
            End If
            If tnt2xy = 301 Or tnt2xy = 401 Or tnt2xy = 201 Or tnt2xy = 302 Or tnt2xy = 300 Then
                PictureBox59.Visible = False
            End If
            If tnt2xy = 304 Or tnt2xy = 404 Or tnt2xy = 204 Or tnt2xy = 305 Or tnt2xy = 303 Then
                PictureBox56.Visible = False
            End If
            If tnt2xy = 308 Or tnt2xy = 408 Or tnt2xy = 208 Or tnt2xy = 309 Or tnt2xy = 307 Then
                PictureBox52.Visible = False
            End If
            If tnt2xy = 312 Or tnt2xy = 412 Or tnt2xy = 212 Or tnt2xy = 313 Or tnt2xy = 311 Then
                PictureBox48.Visible = False
            End If
            If tnt2xy = 402 Or tnt2xy = 502 Or tnt2xy = 302 Or tnt2xy = 403 Or tnt2xy = 401 Then
                PictureBox91.Visible = False
            End If
            If tnt2xy = 403 Or tnt2xy = 503 Or tnt2xy = 303 Or tnt2xy = 404 Or tnt2xy = 402 Then
                If PictureBox90.Visible = True Then
                    Call point2()
                End If
                PictureBox90.Visible = False
            End If
            If tnt2xy = 406 Or tnt2xy = 506 Or tnt2xy = 306 Or tnt2xy = 407 Or tnt2xy = 405 Then
                PictureBox87.Visible = False
            End If
            If tnt2xy = 407 Or tnt2xy = 507 Or tnt2xy = 307 Or tnt2xy = 408 Or tnt2xy = 406 Then
                If PictureBox86.Visible = True Then
                    Call point2()
                End If
                PictureBox86.Visible = False
            End If
            If tnt2xy = 410 Or tnt2xy = 510 Or tnt2xy = 310 Or tnt2xy = 411 Or tnt2xy = 409 Then
                PictureBox83.Visible = False
            End If
            If tnt2xy = 411 Or tnt2xy = 511 Or tnt2xy = 311 Or tnt2xy = 412 Or tnt2xy = 410 Then
                If PictureBox82.Visible = True Then
                    Call point2()
                End If
                PictureBox82.Visible = False
            End If
            If tnt2xy = 413 Or tnt2xy = 513 Or tnt2xy = 313 Or tnt2xy = 414 Or tnt2xy = 412 Then
                PictureBox80.Visible = False
            End If
            If tnt2xy = 501 Or tnt2xy = 601 Or tnt2xy = 401 Or tnt2xy = 502 Or tnt2xy = 500 Then
                If PictureBox148.Visible = True Then
                    Call point2()
                End If
                PictureBox148.Visible = False
            End If
            If tnt2xy = 504 Or tnt2xy = 604 Or tnt2xy = 404 Or tnt2xy = 505 Or tnt2xy = 503 Then
                PictureBox145.Visible = False
            End If
            If tnt2xy = 505 Or tnt2xy = 605 Or tnt2xy = 405 Or tnt2xy = 506 Or tnt2xy = 504 Then
                If PictureBox1.Visible = True Then
                    Call point2()
                End If
                PictureBox1.Visible = False
            End If
            If tnt2xy = 508 Or tnt2xy = 608 Or tnt2xy = 408 Or tnt2xy = 509 Or tnt2xy = 507 Then
                PictureBox121.Visible = False
            End If
            If tnt2xy = 509 Or tnt2xy = 609 Or tnt2xy = 409 Or tnt2xy = 510 Or tnt2xy = 508 Then
                If PictureBox120.Visible = True Then
                    Call point2()
                End If
                PictureBox120.Visible = False
            End If
            If tnt2xy = 512 Or tnt2xy = 612 Or tnt2xy = 412 Or tnt2xy = 513 Or tnt2xy = 511 Then
                PictureBox117.Visible = False
            End If
            If tnt2xy = 602 Or tnt2xy = 702 Or tnt2xy = 502 Or tnt2xy = 603 Or tnt2xy = 601 Then
                PictureBox180.Visible = False
            End If
            If tnt2xy = 606 Or tnt2xy = 706 Or tnt2xy = 506 Or tnt2xy = 607 Or tnt2xy = 605 Then
                PictureBox176.Visible = False
            End If
            If tnt2xy = 610 Or tnt2xy = 710 Or tnt2xy = 510 Or tnt2xy = 611 Or tnt2xy = 609 Then
                PictureBox172.Visible = False
            End If
            If tnt2xy = 613 Or tnt2xy = 713 Or tnt2xy = 513 Or tnt2xy = 614 Or tnt2xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point2()
                End If
                PictureBox169.Visible = False
            End If
            If tnt2xy = 701 Or tnt2xy = 801 Or tnt2xy = 601 Or tnt2xy = 702 Or tnt2xy = 700 Then
                PictureBox214.Visible = False
            End If
            If tnt2xy = 704 Or tnt2xy = 804 Or tnt2xy = 604 Or tnt2xy = 705 Or tnt2xy = 703 Then
                PictureBox211.Visible = False
            End If
            If tnt2xy = 708 Or tnt2xy = 808 Or tnt2xy = 608 Or tnt2xy = 709 Or tnt2xy = 707 Then
                PictureBox207.Visible = False
            End If
            If tnt2xy = 712 Or tnt2xy = 812 Or tnt2xy = 612 Or tnt2xy = 713 Or tnt2xy = 711 Then
                PictureBox203.Visible = False
            End If
            If tnt2xy = 802 Or tnt2xy = 902 Or tnt2xy = 702 Or tnt2xy = 803 Or tnt2xy = 801 Then
                PictureBox246.Visible = False
            End If
            If tnt2xy = 803 Or tnt2xy = 903 Or tnt2xy = 703 Or tnt2xy = 804 Or tnt2xy = 802 Then
                If PictureBox245.Visible = True Then
                    Call point2()
                End If
                PictureBox245.Visible = False
            End If
            If tnt2xy = 806 Or tnt2xy = 906 Or tnt2xy = 706 Or tnt2xy = 807 Or tnt2xy = 805 Then
                PictureBox242.Visible = False
            End If
            If tnt2xy = 807 Or tnt2xy = 907 Or tnt2xy = 707 Or tnt2xy = 808 Or tnt2xy = 806 Then
                If PictureBox241.Visible = True Then
                    Call point2()
                End If
                PictureBox241.Visible = False
            End If
            If tnt2xy = 810 Or tnt2xy = 910 Or tnt2xy = 710 Or tnt2xy = 811 Or tnt2xy = 809 Then
                PictureBox238.Visible = False
            End If
            If tnt2xy = 811 Or tnt2xy = 911 Or tnt2xy = 711 Or tnt2xy = 812 Or tnt2xy = 810 Then
                If PictureBox237.Visible = True Then
                    Call point2()
                End If
                PictureBox237.Visible = False
            End If
            If tnt2xy = 813 Or tnt2xy = 913 Or tnt2xy = 713 Or tnt2xy = 814 Or tnt2xy = 812 Then
                PictureBox235.Visible = False
            End If
            If tnt2xy = 904 Or tnt2xy = 1004 Or tnt2xy = 804 Or tnt2xy = 905 Or tnt2xy = 903 Then
                PictureBox277.Visible = False
            End If
            If tnt2xy = 905 Or tnt2xy = 1005 Or tnt2xy = 805 Or tnt2xy = 906 Or tnt2xy = 904 Then
                If PictureBox276.Visible = True Then
                    Call point2()
                End If
                PictureBox276.Visible = False
            End If
            If tnt2xy = 908 Or tnt2xy = 1008 Or tnt2xy = 808 Or tnt2xy = 909 Or tnt2xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt2xy = 909 Or tnt2xy = 1009 Or tnt2xy = 809 Or tnt2xy = 910 Or tnt2xy = 908 Then
                If PictureBox272.Visible = True Then
                    Call point2()
                End If
                PictureBox272.Visible = False
            End If
            If tnt2xy = 912 Or tnt2xy = 1012 Or tnt2xy = 812 Or tnt2xy = 913 Or tnt2xy = 911 Then
                PictureBox269.Visible = False
            End If
            If tnt2xy = 1002 Or tnt2xy = 1102 Or tnt2xy = 902 Or tnt2xy = 1003 Or tnt2xy = 1001 Then
                PictureBox312.Visible = False
            End If
            If tnt2xy = 1006 Or tnt2xy = 1106 Or tnt2xy = 906 Or tnt2xy = 1007 Or tnt2xy = 1005 Then
                PictureBox308.Visible = False
            End If
            If tnt2xy = 1010 Or tnt2xy = 1110 Or tnt2xy = 910 Or tnt2xy = 1011 Or tnt2xy = 1009 Then
                PictureBox304.Visible = False
            End If
            If tnt2xy = 1013 Or tnt2xy = 1113 Or tnt2xy = 913 Or tnt2xy = 1014 Or tnt2xy = 1012 Then
                If PictureBox301.Visible = True Then
                    Call point2()
                End If
                PictureBox301.Visible = False
            End If
            If tnt2xy = 1104 Or tnt2xy = 1204 Or tnt2xy = 1004 Or tnt2xy = 1105 Or tnt2xy = 1103 Then
                PictureBox343.Visible = False
            End If
            If tnt2xy = 1108 Or tnt2xy = 1208 Or tnt2xy = 1008 Or tnt2xy = 1109 Or tnt2xy = 1107 Then
                PictureBox339.Visible = False
            End If
            If tnt2xy = 1112 Or tnt2xy = 1212 Or tnt2xy = 1012 Or tnt2xy = 1113 Or tnt2xy = 1111 Then
                PictureBox335.Visible = False
            End If
            If tnt2xy = 1202 Or tnt2xy = 1302 Or tnt2xy = 1102 Or tnt2xy = 1203 Or tnt2xy = 1201 Then
                PictureBox378.Visible = False
            End If
            If tnt2xy = 1203 Or tnt2xy = 1303 Or tnt2xy = 1103 Or tnt2xy = 1204 Or tnt2xy = 1202 Then
                If PictureBox377.Visible = True Then
                    Call point2()
                End If
                PictureBox377.Visible = False
            End If
            If tnt2xy = 1206 Or tnt2xy = 1306 Or tnt2xy = 1106 Or tnt2xy = 1207 Or tnt2xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt2xy = 1207 Or tnt2xy = 1307 Or tnt2xy = 1107 Or tnt2xy = 1208 Or tnt2xy = 1206 Then
                If PictureBox373.Visible = True Then
                    Call point2()
                End If
                PictureBox373.Visible = False
            End If
            If tnt2xy = 1210 Or tnt2xy = 1310 Or tnt2xy = 1110 Or tnt2xy = 1211 Or tnt2xy = 1209 Then
                PictureBox370.Visible = False
            End If
            If tnt2xy = 1211 Or tnt2xy = 1311 Or tnt2xy = 1111 Or tnt2xy = 1212 Or tnt2xy = 1210 Then
                If PictureBox369.Visible = True Then
                    Call point2()
                End If
                PictureBox369.Visible = False
            End If
            If tnt2xy = 1213 Or tnt2xy = 1313 Or tnt2xy = 1113 Or tnt2xy = 1214 Or tnt2xy = 1212 Then
                PictureBox367.Visible = False
            End If
        ElseIf map = 2 Then
            If tnt2xy = 103 Or tnt2xy = 203 Or tnt2xy = 3 Or tnt2xy = 104 Or tnt2xy = 102 Then
                If PictureBox3.Visible = True Then
                    Call point2()
                End If
                PictureBox3.Visible = False
            End If
            If tnt2xy = 107 Or tnt2xy = 207 Or tnt2xy = 7 Or tnt2xy = 108 Or tnt2xy = 106 Then
                If PictureBox7.Visible = True Then
                    Call point2()
                End If
                PictureBox7.Visible = False
            End If
            If tnt2xy = 111 Or tnt2xy = 211 Or tnt2xy = 11 Or tnt2xy = 112 Or tnt2xy = 110 Then
                If PictureBox11.Visible = True Then
                    Call point2()
                End If
                PictureBox11.Visible = False
            End If
            If tnt2xy = 201 Or tnt2xy = 301 Or tnt2xy = 101 Or tnt2xy = 202 Or tnt2xy = 200 Then
                If PictureBox26.Visible = True Then
                    Call point2()
                End If
                PictureBox26.Visible = False
            End If
            If tnt2xy = 202 Or tnt2xy = 302 Or tnt2xy = 102 Or tnt2xy = 203 Or tnt2xy = 201 Then
                PictureBox25.Visible = False
            End If
            If tnt2xy = 207 Or tnt2xy = 307 Or tnt2xy = 107 Or tnt2xy = 208 Or tnt2xy = 206 Then
                PictureBox20.Visible = False
            End If
            If tnt2xy = 211 Or tnt2xy = 311 Or tnt2xy = 111 Or tnt2xy = 212 Or tnt2xy = 210 Then
                PictureBox16.Visible = False
            End If
            If tnt2xy = 303 Or tnt2xy = 403 Or tnt2xy = 203 Or tnt2xy = 304 Or tnt2xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt2xy = 306 Or tnt2xy = 406 Or tnt2xy = 206 Or tnt2xy = 307 Or tnt2xy = 305 Then
                PictureBox54.Visible = False
            End If
            If tnt2xy = 407 Or tnt2xy = 507 Or tnt2xy = 307 Or tnt2xy = 408 Or tnt2xy = 406 Then
                PictureBox86.Visible = False
            End If
            If tnt2xy = 408 Or tnt2xy = 508 Or tnt2xy = 308 Or tnt2xy = 409 Or tnt2xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt2xy = 412 Or tnt2xy = 512 Or tnt2xy = 312 Or tnt2xy = 413 Or tnt2xy = 411 Then
                PictureBox81.Visible = False
            End If
            If tnt2xy = 413 Or tnt2xy = 513 Or tnt2xy = 313 Or tnt2xy = 414 Or tnt2xy = 412 Then
                If PictureBox80.Visible = True Then
                    Call point2()
                End If
                PictureBox80.Visible = False
            End If
            If tnt2xy = 501 Or tnt2xy = 601 Or tnt2xy = 401 Or tnt2xy = 502 Or tnt2xy = 500 Then
                If PictureBox148.Visible = True Then
                    Call point2()
                End If
                PictureBox148.Visible = False
            End If
            If tnt2xy = 502 Or tnt2xy = 602 Or tnt2xy = 402 Or tnt2xy = 503 Or tnt2xy = 501 Then
                PictureBox147.Visible = False
            End If
            If tnt2xy = 504 Or tnt2xy = 604 Or tnt2xy = 404 Or tnt2xy = 505 Or tnt2xy = 503 Then
                PictureBox145.Visible = False
            End If
            If tnt2xy = 506 Or tnt2xy = 606 Or tnt2xy = 406 Or tnt2xy = 507 Or tnt2xy = 505 Then
                PictureBox123.Visible = False
            End If
            If tnt2xy = 509 Or tnt2xy = 609 Or tnt2xy = 409 Or tnt2xy = 510 Or tnt2xy = 508 Then
                PictureBox120.Visible = False
            End If
            If tnt2xy = 609 Or tnt2xy = 709 Or tnt2xy = 509 Or tnt2xy = 610 Or tnt2xy = 608 Then
                PictureBox173.Visible = False
            End If
            If tnt2xy = 612 Or tnt2xy = 712 Or tnt2xy = 512 Or tnt2xy = 613 Or tnt2xy = 611 Then
                PictureBox170.Visible = False
            End If
            If tnt2xy = 701 Or tnt2xy = 801 Or tnt2xy = 601 Or tnt2xy = 702 Or tnt2xy = 700 Then
                If PictureBox214.Visible = True Then
                    Call point2()
                End If
                PictureBox214.Visible = False
            End If
            If tnt2xy = 702 Or tnt2xy = 802 Or tnt2xy = 602 Or tnt2xy = 703 Or tnt2xy = 701 Then
                PictureBox213.Visible = False
            End If
            If tnt2xy = 704 Or tnt2xy = 804 Or tnt2xy = 604 Or tnt2xy = 705 Or tnt2xy = 703 Then
                PictureBox211.Visible = False
            End If
            If tnt2xy = 705 Or tnt2xy = 805 Or tnt2xy = 605 Or tnt2xy = 706 Or tnt2xy = 704 Then
                PictureBox210.Visible = False
            End If
            If tnt2xy = 707 Or tnt2xy = 807 Or tnt2xy = 607 Or tnt2xy = 708 Or tnt2xy = 706 Then
                PictureBox208.Visible = False
            End If
            If tnt2xy = 713 Or tnt2xy = 813 Or tnt2xy = 613 Or tnt2xy = 714 Or tnt2xy = 712 Then
                PictureBox202.Visible = False
            End If
            If tnt2xy = 807 Or tnt2xy = 907 Or tnt2xy = 707 Or tnt2xy = 808 Or tnt2xy = 806 Then
                PictureBox241.Visible = False
            End If
            If tnt2xy = 809 Or tnt2xy = 909 Or tnt2xy = 709 Or tnt2xy = 810 Or tnt2xy = 808 Then
                PictureBox239.Visible = False
            End If
            If tnt2xy = 810 Or tnt2xy = 910 Or tnt2xy = 710 Or tnt2xy = 811 Or tnt2xy = 809 Then
                PictureBox238.Visible = False
            End If
            If tnt2xy = 901 Or tnt2xy = 1001 Or tnt2xy = 801 Or tnt2xy = 902 Or tnt2xy = 900 Then
                If PictureBox280.Visible = True Then
                    Call point2()
                End If
                PictureBox280.Visible = False
            End If
            If tnt2xy = 904 Or tnt2xy = 1004 Or tnt2xy = 804 Or tnt2xy = 905 Or tnt2xy = 903 Then
                PictureBox277.Visible = False
            End If
            If tnt2xy = 906 Or tnt2xy = 1006 Or tnt2xy = 806 Or tnt2xy = 907 Or tnt2xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt2xy = 908 Or tnt2xy = 1008 Or tnt2xy = 808 Or tnt2xy = 909 Or tnt2xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt2xy = 913 Or tnt2xy = 1013 Or tnt2xy = 813 Or tnt2xy = 914 Or tnt2xy = 912 Then
                PictureBox268.Visible = False
            End If
            If tnt2xy = 1001 Or tnt2xy = 1101 Or tnt2xy = 901 Or tnt2xy = 1002 Or tnt2xy = 1000 Then
                PictureBox313.Visible = False
            End If
            If tnt2xy = 1007 Or tnt2xy = 1107 Or tnt2xy = 907 Or tnt2xy = 1008 Or tnt2xy = 1006 Then
                PictureBox307.Visible = False
            End If
            If tnt2xy = 1010 Or tnt2xy = 1110 Or tnt2xy = 910 Or tnt2xy = 1011 Or tnt2xy = 1009 Then
                PictureBox304.Visible = False
            End If
            If tnt2xy = 1012 Or tnt2xy = 1112 Or tnt2xy = 912 Or tnt2xy = 1013 Or tnt2xy = 1011 Then
                PictureBox302.Visible = False
            End If
            If tnt2xy = 1102 Or tnt2xy = 1202 Or tnt2xy = 1002 Or tnt2xy = 1103 Or tnt2xy = 1101 Then
                PictureBox345.Visible = False
            End If
            If tnt2xy = 1104 Or tnt2xy = 1204 Or tnt2xy = 1004 Or tnt2xy = 1105 Or tnt2xy = 1103 Then
                PictureBox343.Visible = False
            End If
            If tnt2xy = 1106 Or tnt2xy = 1206 Or tnt2xy = 1006 Or tnt2xy = 1107 Or tnt2xy = 1105 Then
                PictureBox341.Visible = False
            End If
            If tnt2xy = 1108 Or tnt2xy = 1208 Or tnt2xy = 1008 Or tnt2xy = 1109 Or tnt2xy = 1107 Then
                If PictureBox339.Visible = True Then
                    Call point2()
                End If
                PictureBox339.Visible = False
            End If
            If tnt2xy = 1111 Or tnt2xy = 1211 Or tnt2xy = 1011 Or tnt2xy = 1112 Or tnt2xy = 1110 Then
                PictureBox336.Visible = False
            End If
            If tnt2xy = 1113 Or tnt2xy = 1213 Or tnt2xy = 1013 Or tnt2xy = 1114 Or tnt2xy = 1112 Then
                PictureBox334.Visible = False
            End If
            If tnt2xy = 1204 Or tnt2xy = 1304 Or tnt2xy = 1104 Or tnt2xy = 1205 Or tnt2xy = 1203 Then
                PictureBox376.Visible = False
            End If
            If tnt2xy = 1206 Or tnt2xy = 1306 Or tnt2xy = 1106 Or tnt2xy = 1207 Or tnt2xy = 1205 Then
                If PictureBox374.Visible = True Then
                    Call point2()
                End If
                PictureBox374.Visible = False
            End If
            If tnt2xy = 1209 Or tnt2xy = 1309 Or tnt2xy = 1109 Or tnt2xy = 1210 Or tnt2xy = 1208 Then
                If PictureBox371.Visible = True Then
                    Call point2()
                End If
                PictureBox371.Visible = False
            End If
            If tnt2xy = 1210 Or tnt2xy = 1310 Or tnt2xy = 1110 Or tnt2xy = 1211 Or tnt2xy = 1209 Then
                PictureBox370.Visible = False
            End If
            If tnt2xy = 1213 Or tnt2xy = 1313 Or tnt2xy = 1113 Or tnt2xy = 1214 Or tnt2xy = 1212 Then
                If PictureBox367.Visible = True Then
                    Call point2()
                End If
                PictureBox367.Visible = False
            End If
        ElseIf map = 3 Then
            If tnt2xy = 102 Or tnt2xy = 202 Or tnt2xy = 2 Or tnt2xy = 103 Or tnt2xy = 101 Then
                If PictureBox2.Visible = True Then
                    Call point2()
                End If
                PictureBox2.Visible = False
            End If
            If tnt2xy = 103 Or tnt2xy = 203 Or tnt2xy = 3 Or tnt2xy = 104 Or tnt2xy = 102 Then
                PictureBox3.Visible = False
            End If
            If tnt2xy = 104 Or tnt2xy = 204 Or tnt2xy = 4 Or tnt2xy = 105 Or tnt2xy = 103 Then
                If PictureBox4.Visible = True Then
                    Call point2()
                End If
                PictureBox4.Visible = False
            End If
            If tnt2xy = 108 Or tnt2xy = 208 Or tnt2xy = 8 Or tnt2xy = 109 Or tnt2xy = 107 Then
                PictureBox8.Visible = False
            End If
            If tnt2xy = 110 Or tnt2xy = 210 Or tnt2xy = 10 Or tnt2xy = 111 Or tnt2xy = 109 Then
                If PictureBox10.Visible = True Then
                    Call point2()
                End If
                PictureBox10.Visible = False
            End If
            If tnt2xy = 201 Or tnt2xy = 301 Or tnt2xy = 101 Or tnt2xy = 202 Or tnt2xy = 200 Then
                If PictureBox26.Visible = True Then
                    Call point2()
                End If
                PictureBox26.Visible = False
            End If
            If tnt2xy = 203 Or tnt2xy = 303 Or tnt2xy = 103 Or tnt2xy = 204 Or tnt2xy = 202 Then
                PictureBox24.Visible = False
            End If
            If tnt2xy = 205 Or tnt2xy = 305 Or tnt2xy = 105 Or tnt2xy = 206 Or tnt2xy = 204 Then
                If PictureBox22.Visible = True Then
                    Call point2()
                End If
                PictureBox22.Visible = False
            End If
            If tnt2xy = 207 Or tnt2xy = 307 Or tnt2xy = 107 Or tnt2xy = 208 Or tnt2xy = 206 Then
                If PictureBox20.Visible = True Then
                    Call point2()
                End If
                PictureBox20.Visible = False
            End If
            If tnt2xy = 211 Or tnt2xy = 311 Or tnt2xy = 111 Or tnt2xy = 212 Or tnt2xy = 210 Then
                PictureBox16.Visible = False
            End If
            If tnt2xy = 301 Or tnt2xy = 401 Or tnt2xy = 201 Or tnt2xy = 302 Or tnt2xy = 300 Then
                PictureBox59.Visible = False
            End If
            If tnt2xy = 303 Or tnt2xy = 403 Or tnt2xy = 203 Or tnt2xy = 304 Or tnt2xy = 302 Then
                If PictureBox57.Visible = True Then
                    Call point2()
                End If
                PictureBox57.Visible = False
            End If
            If tnt2xy = 306 Or tnt2xy = 406 Or tnt2xy = 206 Or tnt2xy = 307 Or tnt2xy = 305 Then
                PictureBox54.Visible = False
            End If
            If tnt2xy = 308 Or tnt2xy = 408 Or tnt2xy = 208 Or tnt2xy = 309 Or tnt2xy = 307 Then
                PictureBox52.Visible = False
            End If
            If tnt2xy = 310 Or tnt2xy = 410 Or tnt2xy = 210 Or tnt2xy = 311 Or tnt2xy = 309 Then
                PictureBox50.Visible = False
            End If
            If tnt2xy = 312 Or tnt2xy = 412 Or tnt2xy = 212 Or tnt2xy = 313 Or tnt2xy = 311 Then
                If PictureBox48.Visible = True Then
                    Call point2()
                End If
                PictureBox48.Visible = False
            End If
            If tnt2xy = 402 Or tnt2xy = 502 Or tnt2xy = 302 Or tnt2xy = 403 Or tnt2xy = 401 Then
                PictureBox91.Visible = False
            End If
            If tnt2xy = 407 Or tnt2xy = 507 Or tnt2xy = 307 Or tnt2xy = 408 Or tnt2xy = 406 Then
                If PictureBox86.Visible = True Then
                    Call point2()
                End If
                PictureBox86.Visible = False
            End If
            If tnt2xy = 409 Or tnt2xy = 509 Or tnt2xy = 309 Or tnt2xy = 410 Or tnt2xy = 408 Then
                If PictureBox84.Visible = True Then
                    Call point2()
                End If
                PictureBox84.Visible = False
            End If
            If tnt2xy = 410 Or tnt2xy = 510 Or tnt2xy = 310 Or tnt2xy = 411 Or tnt2xy = 409 Then
                PictureBox83.Visible = False
            End If
            If tnt2xy = 413 Or tnt2xy = 513 Or tnt2xy = 313 Or tnt2xy = 414 Or tnt2xy = 412 Then
                PictureBox80.Visible = False
            End If
            If tnt2xy = 503 Or tnt2xy = 603 Or tnt2xy = 403 Or tnt2xy = 504 Or tnt2xy = 502 Then
                PictureBox146.Visible = False
            End If
            If tnt2xy = 505 Or tnt2xy = 605 Or tnt2xy = 405 Or tnt2xy = 506 Or tnt2xy = 504 Then
                If PictureBox1.Visible = True Then
                    Call point2()
                End If
                PictureBox1.Visible = False
            End If
            If tnt2xy = 507 Or tnt2xy = 607 Or tnt2xy = 407 Or tnt2xy = 508 Or tnt2xy = 506 Then
                PictureBox122.Visible = False
            End If
            If tnt2xy = 508 Or tnt2xy = 608 Or tnt2xy = 408 Or tnt2xy = 509 Or tnt2xy = 507 Then
                PictureBox121.Visible = False
            End If
            If tnt2xy = 510 Or tnt2xy = 610 Or tnt2xy = 410 Or tnt2xy = 511 Or tnt2xy = 509 Then
                If PictureBox119.Visible = True Then
                    Call point2()
                End If
                PictureBox119.Visible = False
            End If
            If tnt2xy = 513 Or tnt2xy = 613 Or tnt2xy = 413 Or tnt2xy = 514 Or tnt2xy = 512 Then
                PictureBox116.Visible = False
            End If
            If tnt2xy = 602 Or tnt2xy = 702 Or tnt2xy = 502 Or tnt2xy = 603 Or tnt2xy = 601 Then
                PictureBox180.Visible = False
            End If
            If tnt2xy = 606 Or tnt2xy = 706 Or tnt2xy = 506 Or tnt2xy = 607 Or tnt2xy = 605 Then
                PictureBox176.Visible = False
            End If
            If tnt2xy = 609 Or tnt2xy = 709 Or tnt2xy = 509 Or tnt2xy = 610 Or tnt2xy = 608 Then
                PictureBox173.Visible = False
            End If
            If tnt2xy = 611 Or tnt2xy = 711 Or tnt2xy = 511 Or tnt2xy = 612 Or tnt2xy = 610 Then
                PictureBox171.Visible = False
            End If
            If tnt2xy = 613 Or tnt2xy = 713 Or tnt2xy = 513 Or tnt2xy = 614 Or tnt2xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point2()
                End If
                PictureBox169.Visible = False
            End If
            If tnt2xy = 702 Or tnt2xy = 802 Or tnt2xy = 602 Or tnt2xy = 703 Or tnt2xy = 701 Then
                If PictureBox213.Visible = True Then
                    Call point2()
                End If
                PictureBox213.Visible = False
            End If
            If tnt2xy = 703 Or tnt2xy = 803 Or tnt2xy = 603 Or tnt2xy = 704 Or tnt2xy = 702 Then
                PictureBox212.Visible = False
            End If
            If tnt2xy = 705 Or tnt2xy = 805 Or tnt2xy = 605 Or tnt2xy = 706 Or tnt2xy = 704 Then
                If PictureBox210.Visible = True Then
                    Call point2()
                End If
                PictureBox210.Visible = False
            End If
            If tnt2xy = 707 Or tnt2xy = 807 Or tnt2xy = 607 Or tnt2xy = 708 Or tnt2xy = 706 Then
                PictureBox208.Visible = False
            End If
            If tnt2xy = 709 Or tnt2xy = 809 Or tnt2xy = 609 Or tnt2xy = 710 Or tnt2xy = 708 Then
                If PictureBox206.Visible = True Then
                    Call point2()
                End If
                PictureBox206.Visible = False
            End If
            If tnt2xy = 712 Or tnt2xy = 812 Or tnt2xy = 612 Or tnt2xy = 713 Or tnt2xy = 711 Then
                If PictureBox203.Visible = True Then
                    Call point2()
                End If
                PictureBox203.Visible = False
            End If
            If tnt2xy = 801 Or tnt2xy = 901 Or tnt2xy = 701 Or tnt2xy = 802 Or tnt2xy = 800 Then
                If PictureBox247.Visible = True Then
                    Call point2()
                End If
                PictureBox247.Visible = False
            End If
            If tnt2xy = 804 Or tnt2xy = 904 Or tnt2xy = 704 Or tnt2xy = 805 Or tnt2xy = 803 Then
                PictureBox244.Visible = False
            End If
            If tnt2xy = 808 Or tnt2xy = 908 Or tnt2xy = 708 Or tnt2xy = 809 Or tnt2xy = 807 Then
                If PictureBox240.Visible = True Then
                    Call point2()
                End If
                PictureBox240.Visible = False
            End If
            If tnt2xy = 811 Or tnt2xy = 911 Or tnt2xy = 711 Or tnt2xy = 812 Or tnt2xy = 810 Then
                PictureBox237.Visible = False
            End If
            If tnt2xy = 902 Or tnt2xy = 1002 Or tnt2xy = 802 Or tnt2xy = 903 Or tnt2xy = 901 Then
                PictureBox279.Visible = False
            End If
            If tnt2xy = 903 Or tnt2xy = 1003 Or tnt2xy = 803 Or tnt2xy = 904 Or tnt2xy = 902 Then
                If PictureBox278.Visible = True Then
                    Call point2()
                End If
                PictureBox278.Visible = False
            End If
            If tnt2xy = 906 Or tnt2xy = 1006 Or tnt2xy = 806 Or tnt2xy = 907 Or tnt2xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt2xy = 907 Or tnt2xy = 1007 Or tnt2xy = 807 Or tnt2xy = 908 Or tnt2xy = 906 Then
                If PictureBox274.Visible = True Then
                    Call point2()
                End If
                PictureBox274.Visible = False
            End If
            If tnt2xy = 909 Or tnt2xy = 1009 Or tnt2xy = 809 Or tnt2xy = 910 Or tnt2xy = 908 Then
                If PictureBox272.Visible = True Then
                    Call point2()
                End If
                PictureBox272.Visible = False
            End If
            If tnt2xy = 910 Or tnt2xy = 1010 Or tnt2xy = 810 Or tnt2xy = 911 Or tnt2xy = 909 Then
                PictureBox271.Visible = False
            End If
            If tnt2xy = 913 Or tnt2xy = 1013 Or tnt2xy = 813 Or tnt2xy = 914 Or tnt2xy = 912 Then
                PictureBox268.Visible = False
            End If
            If tnt2xy = 1001 Or tnt2xy = 1101 Or tnt2xy = 901 Or tnt2xy = 1002 Or tnt2xy = 1000 Then
                If PictureBox313.Visible = True Then
                    Call point2()
                End If
                PictureBox313.Visible = False
            End If
            If tnt2xy = 1004 Or tnt2xy = 1104 Or tnt2xy = 904 Or tnt2xy = 1005 Or tnt2xy = 1003 Then
                If PictureBox310.Visible = True Then
                    Call point2()
                End If
                PictureBox310.Visible = False
            End If
            If tnt2xy = 1009 Or tnt2xy = 1109 Or tnt2xy = 909 Or tnt2xy = 1010 Or tnt2xy = 1008 Then
                PictureBox305.Visible = False
            End If
            If tnt2xy = 1012 Or tnt2xy = 1112 Or tnt2xy = 912 Or tnt2xy = 1013 Or tnt2xy = 1011 Then
                If PictureBox302.Visible = True Then
                    Call point2()
                End If
                PictureBox302.Visible = False
            End If
            If tnt2xy = 1103 Or tnt2xy = 1203 Or tnt2xy = 1003 Or tnt2xy = 1104 Or tnt2xy = 1102 Then
                PictureBox344.Visible = False
            End If
            If tnt2xy = 1105 Or tnt2xy = 1205 Or tnt2xy = 1005 Or tnt2xy = 1106 Or tnt2xy = 1104 Then
                PictureBox342.Visible = False
            End If
            If tnt2xy = 1107 Or tnt2xy = 1207 Or tnt2xy = 1007 Or tnt2xy = 1108 Or tnt2xy = 1106 Then
                If PictureBox340.Visible = True Then
                    Call point2()
                End If
                PictureBox340.Visible = False
            End If
            If tnt2xy = 1113 Or tnt2xy = 1213 Or tnt2xy = 1013 Or tnt2xy = 1114 Or tnt2xy = 1112 Then
                If PictureBox334.Visible = True Then
                    Call point2()
                End If
                PictureBox334.Visible = False
            End If
            If tnt2xy = 1204 Or tnt2xy = 1304 Or tnt2xy = 1104 Or tnt2xy = 1205 Or tnt2xy = 1203 Then
                If PictureBox376.Visible = True Then
                    Call point2()
                End If
                PictureBox376.Visible = False
            End If
            If tnt2xy = 1206 Or tnt2xy = 1306 Or tnt2xy = 1106 Or tnt2xy = 1207 Or tnt2xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt2xy = 1211 Or tnt2xy = 1311 Or tnt2xy = 1111 Or tnt2xy = 1212 Or tnt2xy = 1210 Then
                If PictureBox369.Visible = True Then
                    Call point2()
                End If
                PictureBox369.Visible = False
            End If
            If tnt2xy = 1213 Or tnt2xy = 1313 Or tnt2xy = 1113 Or tnt2xy = 1214 Or tnt2xy = 1212 Then
                PictureBox367.Visible = False
            End If
        ElseIf map = 4 Then
            If tnt2xy = 103 Or tnt2xy = 203 Or tnt2xy = 3 Or tnt2xy = 104 Or tnt2xy = 102 Then
                PictureBox3.Visible = False
            End If
            If tnt2xy = 105 Or tnt2xy = 205 Or tnt2xy = 5 Or tnt2xy = 106 Or tnt2xy = 104 Then
                PictureBox5.Visible = False
            End If
            If tnt2xy = 106 Or tnt2xy = 206 Or tnt2xy = 6 Or tnt2xy = 107 Or tnt2xy = 105 Then
                If PictureBox6.Visible = True Then
                    Call point2()
                End If
                PictureBox6.Visible = False
            End If
            If tnt2xy = 107 Or tnt2xy = 207 Or tnt2xy = 7 Or tnt2xy = 108 Or tnt2xy = 106 Then
                If PictureBox7.Visible = True Then
                    Call point2()
                End If
                PictureBox7.Visible = False
            End If
            If tnt2xy = 108 Or tnt2xy = 208 Or tnt2xy = 8 Or tnt2xy = 109 Or tnt2xy = 107 Then
                If PictureBox8.Visible = True Then
                    Call point2()
                End If
                PictureBox8.Visible = False
            End If
            If tnt2xy = 109 Or tnt2xy = 209 Or tnt2xy = 9 Or tnt2xy = 110 Or tnt2xy = 108 Then
                PictureBox9.Visible = False
            End If
            If tnt2xy = 111 Or tnt2xy = 211 Or tnt2xy = 11 Or tnt2xy = 112 Or tnt2xy = 110 Then
                PictureBox11.Visible = False
            End If
            If tnt2xy = 204 Or tnt2xy = 304 Or tnt2xy = 104 Or tnt2xy = 205 Or tnt2xy = 203 Then
                If PictureBox23.Visible = True Then
                    Call point2()
                End If
                PictureBox23.Visible = False
            End If
            If tnt2xy = 205 Or tnt2xy = 305 Or tnt2xy = 105 Or tnt2xy = 206 Or tnt2xy = 204 Then
                If PictureBox22.Visible = True Then
                    Call point2()
                End If
                PictureBox22.Visible = False
            End If
            If tnt2xy = 206 Or tnt2xy = 306 Or tnt2xy = 106 Or tnt2xy = 207 Or tnt2xy = 205 Then
                If PictureBox21.Visible = True Then
                    Call point2()
                End If
                PictureBox21.Visible = False
            End If
            If tnt2xy = 207 Or tnt2xy = 307 Or tnt2xy = 107 Or tnt2xy = 208 Or tnt2xy = 206 Then
                If PictureBox20.Visible = True Then
                    Call point2()
                End If
                PictureBox20.Visible = False
            End If
            If tnt2xy = 208 Or tnt2xy = 308 Or tnt2xy = 108 Or tnt2xy = 209 Or tnt2xy = 207 Then
                If PictureBox19.Visible = True Then
                    Call point2()
                End If
                PictureBox19.Visible = False
            End If
            If tnt2xy = 209 Or tnt2xy = 309 Or tnt2xy = 109 Or tnt2xy = 210 Or tnt2xy = 208 Then
                If PictureBox18.Visible = True Then
                    Call point2()
                End If
                PictureBox18.Visible = False
            End If
            If tnt2xy = 210 Or tnt2xy = 310 Or tnt2xy = 110 Or tnt2xy = 211 Or tnt2xy = 209 Then
                If PictureBox17.Visible = True Then
                    Call point2()
                End If
                PictureBox17.Visible = False
            End If
            If tnt2xy = 301 Or tnt2xy = 401 Or tnt2xy = 201 Or tnt2xy = 302 Or tnt2xy = 300 Then
                PictureBox59.Visible = False
            End If
            If tnt2xy = 303 Or tnt2xy = 403 Or tnt2xy = 203 Or tnt2xy = 304 Or tnt2xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt2xy = 304 Or tnt2xy = 404 Or tnt2xy = 204 Or tnt2xy = 305 Or tnt2xy = 303 Then
                If PictureBox56.Visible = True Then
                    Call point2()
                End If
                PictureBox56.Visible = False
            End If
            If tnt2xy = 305 Or tnt2xy = 405 Or tnt2xy = 205 Or tnt2xy = 306 Or tnt2xy = 304 Then
                If PictureBox55.Visible = True Then
                    Call point2()
                End If
                PictureBox55.Visible = False
            End If
            If tnt2xy = 306 Or tnt2xy = 406 Or tnt2xy = 206 Or tnt2xy = 307 Or tnt2xy = 305 Then
                If PictureBox54.Visible = True Then
                    Call point2()
                End If
                PictureBox54.Visible = False
            End If
            If tnt2xy = 307 Or tnt2xy = 407 Or tnt2xy = 207 Or tnt2xy = 308 Or tnt2xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point2()
                End If
                PictureBox53.Visible = False
            End If
            If tnt2xy = 308 Or tnt2xy = 408 Or tnt2xy = 208 Or tnt2xy = 309 Or tnt2xy = 307 Then
                If PictureBox52.Visible = True Then
                    Call point2()
                End If
                PictureBox52.Visible = False
            End If
            If tnt2xy = 309 Or tnt2xy = 409 Or tnt2xy = 209 Or tnt2xy = 310 Or tnt2xy = 308 Then
                If PictureBox51.Visible = True Then
                    Call point2()
                End If
                PictureBox51.Visible = False
            End If
            If tnt2xy = 310 Or tnt2xy = 410 Or tnt2xy = 210 Or tnt2xy = 311 Or tnt2xy = 309 Then
                If PictureBox50.Visible = True Then
                    Call point2()
                End If
                PictureBox50.Visible = False
            End If
            If tnt2xy = 311 Or tnt2xy = 411 Or tnt2xy = 211 Or tnt2xy = 312 Or tnt2xy = 310 Then
                PictureBox49.Visible = False
            End If
            If tnt2xy = 313 Or tnt2xy = 413 Or tnt2xy = 213 Or tnt2xy = 314 Or tnt2xy = 312 Then
                PictureBox47.Visible = False
            End If
            If tnt2xy = 402 Or tnt2xy = 502 Or tnt2xy = 302 Or tnt2xy = 403 Or tnt2xy = 401 Then
                If PictureBox91.Visible = True Then
                    Call point2()
                End If
                PictureBox91.Visible = False
            End If
            If tnt2xy = 403 Or tnt2xy = 503 Or tnt2xy = 303 Or tnt2xy = 404 Or tnt2xy = 402 Then
                If PictureBox90.Visible = True Then
                    Call point2()
                End If
                PictureBox90.Visible = False
            End If
            If tnt2xy = 405 Or tnt2xy = 505 Or tnt2xy = 305 Or tnt2xy = 406 Or tnt2xy = 404 Then
                If PictureBox88.Visible = True Then
                    Call point2()
                End If
                PictureBox88.Visible = False
            End If
            If tnt2xy = 406 Or tnt2xy = 506 Or tnt2xy = 306 Or tnt2xy = 407 Or tnt2xy = 405 Then
                If PictureBox87.Visible = True Then
                    Call point2()
                End If
                PictureBox87.Visible = False
            End If
            If tnt2xy = 407 Or tnt2xy = 507 Or tnt2xy = 307 Or tnt2xy = 408 Or tnt2xy = 406 Then
                If PictureBox86.Visible = True Then
                    Call point2()
                End If
                PictureBox86.Visible = False
            End If
            If tnt2xy = 408 Or tnt2xy = 508 Or tnt2xy = 308 Or tnt2xy = 409 Or tnt2xy = 407 Then
                If PictureBox85.Visible = True Then
                    Call point2()
                End If
                PictureBox85.Visible = False
            End If
            If tnt2xy = 409 Or tnt2xy = 509 Or tnt2xy = 309 Or tnt2xy = 410 Or tnt2xy = 408 Then
                If PictureBox84.Visible = True Then
                    Call point2()
                End If
                PictureBox84.Visible = False
            End If
            If tnt2xy = 411 Or tnt2xy = 511 Or tnt2xy = 311 Or tnt2xy = 412 Or tnt2xy = 410 Then
                If PictureBox82.Visible = True Then
                    Call point2()
                End If
                PictureBox82.Visible = False
            End If
            If tnt2xy = 412 Or tnt2xy = 512 Or tnt2xy = 312 Or tnt2xy = 413 Or tnt2xy = 411 Then
                If PictureBox81.Visible = True Then
                    Call point2()
                End If
                PictureBox81.Visible = False
            End If
            If tnt2xy = 501 Or tnt2xy = 601 Or tnt2xy = 401 Or tnt2xy = 502 Or tnt2xy = 500 Then
                PictureBox148.Visible = False
            End If
            If tnt2xy = 502 Or tnt2xy = 602 Or tnt2xy = 402 Or tnt2xy = 503 Or tnt2xy = 501 Then
                If PictureBox147.Visible = True Then
                    Call point2()
                End If
                PictureBox147.Visible = False
            End If
            If tnt2xy = 503 Or tnt2xy = 603 Or tnt2xy = 403 Or tnt2xy = 504 Or tnt2xy = 502 Then
                If PictureBox146.Visible = True Then
                    Call point2()
                End If
                PictureBox146.Visible = False
            End If
            If tnt2xy = 504 Or tnt2xy = 604 Or tnt2xy = 404 Or tnt2xy = 505 Or tnt2xy = 503 Then
                If PictureBox145.Visible = True Then
                    Call point2()
                End If
                PictureBox145.Visible = False
            End If
            If tnt2xy = 510 Or tnt2xy = 610 Or tnt2xy = 410 Or tnt2xy = 511 Or tnt2xy = 509 Then
                If PictureBox119.Visible = True Then
                    Call point2()
                End If
                PictureBox119.Visible = False
            End If
            If tnt2xy = 511 Or tnt2xy = 611 Or tnt2xy = 411 Or tnt2xy = 512 Or tnt2xy = 510 Then
                If PictureBox118.Visible = True Then
                    Call point2()
                End If
                PictureBox118.Visible = False
            End If
            If tnt2xy = 512 Or tnt2xy = 612 Or tnt2xy = 412 Or tnt2xy = 513 Or tnt2xy = 511 Then
                If PictureBox117.Visible = True Then
                    Call point2()
                End If
                PictureBox117.Visible = False
            End If
            If tnt2xy = 513 Or tnt2xy = 613 Or tnt2xy = 413 Or tnt2xy = 514 Or tnt2xy = 512 Then
                PictureBox116.Visible = False
            End If
            If tnt2xy = 601 Or tnt2xy = 701 Or tnt2xy = 501 Or tnt2xy = 602 Or tnt2xy = 600 Then
                If PictureBox181.Visible = True Then
                    Call point2()
                End If
                PictureBox181.Visible = False
            End If
            If tnt2xy = 602 Or tnt2xy = 702 Or tnt2xy = 502 Or tnt2xy = 603 Or tnt2xy = 601 Then
                If PictureBox180.Visible = True Then
                    Call point2()
                End If
                PictureBox180.Visible = False
            End If
            If tnt2xy = 603 Or tnt2xy = 703 Or tnt2xy = 503 Or tnt2xy = 604 Or tnt2xy = 602 Then
                If PictureBox179.Visible = True Then
                    Call point2()
                End If
                PictureBox179.Visible = False
            End If
            If tnt2xy = 604 Or tnt2xy = 704 Or tnt2xy = 504 Or tnt2xy = 605 Or tnt2xy = 603 Then
                If PictureBox178.Visible = True Then
                    Call point2()
                End If
                PictureBox178.Visible = False
            End If
            If tnt2xy = 606 Or tnt2xy = 706 Or tnt2xy = 506 Or tnt2xy = 607 Or tnt2xy = 605 Then
                PictureBox176.Visible = False
            End If
            If tnt2xy = 607 Or tnt2xy = 707 Or tnt2xy = 507 Or tnt2xy = 608 Or tnt2xy = 606 Then
                PictureBox175.Visible = False
            End If
            If tnt2xy = 608 Or tnt2xy = 708 Or tnt2xy = 508 Or tnt2xy = 609 Or tnt2xy = 607 Then
                PictureBox174.Visible = False
            End If
            If tnt2xy = 610 Or tnt2xy = 710 Or tnt2xy = 510 Or tnt2xy = 611 Or tnt2xy = 609 Then
                If PictureBox172.Visible = True Then
                    Call point2()
                End If
                PictureBox172.Visible = False
            End If
            If tnt2xy = 611 Or tnt2xy = 711 Or tnt2xy = 511 Or tnt2xy = 612 Or tnt2xy = 610 Then
                If PictureBox171.Visible = True Then
                    Call point2()
                End If
                PictureBox171.Visible = False
            End If
            If tnt2xy = 612 Or tnt2xy = 712 Or tnt2xy = 512 Or tnt2xy = 613 Or tnt2xy = 611 Then
                If PictureBox170.Visible = True Then
                    Call point2()
                End If
                PictureBox170.Visible = False
            End If
            If tnt2xy = 613 Or tnt2xy = 713 Or tnt2xy = 513 Or tnt2xy = 614 Or tnt2xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point2()
                End If
                PictureBox169.Visible = False
            End If
            If tnt2xy = 701 Or tnt2xy = 801 Or tnt2xy = 601 Or tnt2xy = 702 Or tnt2xy = 700 Then
                If PictureBox214.Visible = True Then
                    Call point2()
                End If
                PictureBox214.Visible = False
            End If
            If tnt2xy = 702 Or tnt2xy = 802 Or tnt2xy = 602 Or tnt2xy = 703 Or tnt2xy = 701 Then
                If PictureBox213.Visible = True Then
                    Call point2()
                End If
                PictureBox213.Visible = False
            End If
            If tnt2xy = 703 Or tnt2xy = 803 Or tnt2xy = 603 Or tnt2xy = 704 Or tnt2xy = 702 Then
                If PictureBox212.Visible = True Then
                    Call point2()
                End If
                PictureBox212.Visible = False
            End If
            If tnt2xy = 704 Or tnt2xy = 804 Or tnt2xy = 604 Or tnt2xy = 705 Or tnt2xy = 703 Then
                If PictureBox211.Visible = True Then
                    Call point2()
                End If
                PictureBox211.Visible = False
            End If
            If tnt2xy = 706 Or tnt2xy = 806 Or tnt2xy = 606 Or tnt2xy = 707 Or tnt2xy = 705 Then
                PictureBox209.Visible = False
            End If
            If tnt2xy = 707 Or tnt2xy = 807 Or tnt2xy = 607 Or tnt2xy = 708 Or tnt2xy = 706 Then
                PictureBox208.Visible = False
            End If
            If tnt2xy = 708 Or tnt2xy = 808 Or tnt2xy = 608 Or tnt2xy = 709 Or tnt2xy = 707 Then
                PictureBox207.Visible = False
            End If
            If tnt2xy = 710 Or tnt2xy = 810 Or tnt2xy = 610 Or tnt2xy = 711 Or tnt2xy = 709 Then
                If PictureBox205.Visible = True Then
                    Call point2()
                End If
                PictureBox205.Visible = False
            End If
            If tnt2xy = 711 Or tnt2xy = 811 Or tnt2xy = 611 Or tnt2xy = 712 Or tnt2xy = 710 Then
                If PictureBox204.Visible = True Then
                    Call point2()
                End If
                PictureBox204.Visible = False
            End If
            If tnt2xy = 712 Or tnt2xy = 812 Or tnt2xy = 612 Or tnt2xy = 713 Or tnt2xy = 711 Then
                If PictureBox203.Visible = True Then
                    Call point2()
                End If
                PictureBox203.Visible = False
            End If
            If tnt2xy = 713 Or tnt2xy = 813 Or tnt2xy = 613 Or tnt2xy = 714 Or tnt2xy = 712 Then
                If PictureBox202.Visible = True Then
                    Call point2()
                End If
                PictureBox202.Visible = False
            End If
            If tnt2xy = 801 Or tnt2xy = 901 Or tnt2xy = 701 Or tnt2xy = 802 Or tnt2xy = 800 Then
                PictureBox247.Visible = False
            End If
            If tnt2xy = 802 Or tnt2xy = 902 Or tnt2xy = 702 Or tnt2xy = 803 Or tnt2xy = 801 Then
                If PictureBox246.Visible = True Then
                    Call point2()
                End If
                PictureBox246.Visible = False
            End If
            If tnt2xy = 803 Or tnt2xy = 903 Or tnt2xy = 703 Or tnt2xy = 804 Or tnt2xy = 802 Then
                If PictureBox245.Visible = True Then
                    Call point2()
                End If
                PictureBox245.Visible = False
            End If
            If tnt2xy = 804 Or tnt2xy = 904 Or tnt2xy = 704 Or tnt2xy = 805 Or tnt2xy = 803 Then
                If PictureBox244.Visible = True Then
                    Call point2()
                End If
                PictureBox244.Visible = False
            End If
            If tnt2xy = 810 Or tnt2xy = 910 Or tnt2xy = 710 Or tnt2xy = 811 Or tnt2xy = 809 Then
                If PictureBox238.Visible = True Then
                    Call point2()
                End If
                PictureBox238.Visible = False
            End If
            If tnt2xy = 811 Or tnt2xy = 911 Or tnt2xy = 711 Or tnt2xy = 812 Or tnt2xy = 810 Then
                If PictureBox237.Visible = True Then
                    Call point2()
                End If
                PictureBox237.Visible = False
            End If
            If tnt2xy = 812 Or tnt2xy = 912 Or tnt2xy = 712 Or tnt2xy = 813 Or tnt2xy = 811 Then
                If PictureBox236.Visible = True Then
                    Call point2()
                End If
                PictureBox236.Visible = False
            End If
            If tnt2xy = 813 Or tnt2xy = 913 Or tnt2xy = 713 Or tnt2xy = 814 Or tnt2xy = 812 Then
                PictureBox235.Visible = False
            End If
            If tnt2xy = 902 Or tnt2xy = 1002 Or tnt2xy = 802 Or tnt2xy = 903 Or tnt2xy = 901 Then
                If PictureBox279.Visible = True Then
                    Call point2()
                End If
                PictureBox279.Visible = False
            End If
            If tnt2xy = 903 Or tnt2xy = 1003 Or tnt2xy = 803 Or tnt2xy = 904 Or tnt2xy = 902 Then
                If PictureBox278.Visible = True Then
                    Call point2()
                End If
                PictureBox278.Visible = False
            End If
            If tnt2xy = 905 Or tnt2xy = 1005 Or tnt2xy = 805 Or tnt2xy = 906 Or tnt2xy = 904 Then
                If PictureBox276.Visible = True Then
                    Call point2()
                End If
                PictureBox276.Visible = False
            End If
            If tnt2xy = 906 Or tnt2xy = 1006 Or tnt2xy = 806 Or tnt2xy = 907 Or tnt2xy = 905 Then
                If PictureBox275.Visible = True Then
                    Call point2()
                End If
                PictureBox275.Visible = False
            End If
            If tnt2xy = 907 Or tnt2xy = 1007 Or tnt2xy = 807 Or tnt2xy = 908 Or tnt2xy = 906 Then
                If PictureBox274.Visible = True Then
                    Call point2()
                End If
                PictureBox274.Visible = False
            End If
            If tnt2xy = 908 Or tnt2xy = 1008 Or tnt2xy = 808 Or tnt2xy = 909 Or tnt2xy = 907 Then
                If PictureBox273.Visible = True Then
                    Call point2()
                End If
                PictureBox273.Visible = False
            End If
            If tnt2xy = 909 Or tnt2xy = 1009 Or tnt2xy = 809 Or tnt2xy = 910 Or tnt2xy = 908 Then
                If PictureBox272.Visible = True Then
                    Call point2()
                End If
                PictureBox272.Visible = False
            End If
            If tnt2xy = 911 Or tnt2xy = 1011 Or tnt2xy = 811 Or tnt2xy = 912 Or tnt2xy = 910 Then
                If PictureBox270.Visible = True Then
                    Call point2()
                End If
                PictureBox270.Visible = False
            End If
            If tnt2xy = 912 Or tnt2xy = 1012 Or tnt2xy = 812 Or tnt2xy = 913 Or tnt2xy = 911 Then
                If PictureBox269.Visible = True Then
                    Call point2()
                End If
                PictureBox269.Visible = False
            End If
            If tnt2xy = 1001 Or tnt2xy = 1101 Or tnt2xy = 901 Or tnt2xy = 1002 Or tnt2xy = 1000 Then
                PictureBox313.Visible = False
            End If
            If tnt2xy = 1003 Or tnt2xy = 1103 Or tnt2xy = 903 Or tnt2xy = 1004 Or tnt2xy = 1002 Then
                PictureBox311.Visible = False
            End If
            If tnt2xy = 1004 Or tnt2xy = 1104 Or tnt2xy = 904 Or tnt2xy = 1005 Or tnt2xy = 1003 Then
                If PictureBox310.Visible = True Then
                    Call point2()
                End If
                PictureBox310.Visible = False
            End If
            If tnt2xy = 1005 Or tnt2xy = 1105 Or tnt2xy = 905 Or tnt2xy = 1006 Or tnt2xy = 1004 Then
                If PictureBox309.Visible = True Then
                    Call point2()
                End If
                PictureBox309.Visible = False
            End If
            If tnt2xy = 1006 Or tnt2xy = 1106 Or tnt2xy = 906 Or tnt2xy = 1007 Or tnt2xy = 1005 Then
                If PictureBox308.Visible = True Then
                    Call point2()
                End If
                PictureBox308.Visible = False
            End If
            If tnt2xy = 1007 Or tnt2xy = 1107 Or tnt2xy = 907 Or tnt2xy = 1008 Or tnt2xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point2()
                End If
                PictureBox307.Visible = False
            End If
            If tnt2xy = 1008 Or tnt2xy = 1108 Or tnt2xy = 908 Or tnt2xy = 1009 Or tnt2xy = 1007 Then
                If PictureBox306.Visible = True Then
                    Call point2()
                End If
                PictureBox306.Visible = False
            End If
            If tnt2xy = 1009 Or tnt2xy = 1109 Or tnt2xy = 909 Or tnt2xy = 1010 Or tnt2xy = 1008 Then
                If PictureBox305.Visible = True Then
                    Call point2()
                End If
                PictureBox305.Visible = False
            End If
            If tnt2xy = 1010 Or tnt2xy = 1110 Or tnt2xy = 910 Or tnt2xy = 1011 Or tnt2xy = 1009 Then
                If PictureBox304.Visible = True Then
                    Call point2()
                End If
                PictureBox304.Visible = False
            End If
            If tnt2xy = 1011 Or tnt2xy = 1111 Or tnt2xy = 911 Or tnt2xy = 1012 Or tnt2xy = 1010 Then
                PictureBox303.Visible = False
            End If
            If tnt2xy = 1013 Or tnt2xy = 1113 Or tnt2xy = 913 Or tnt2xy = 1014 Or tnt2xy = 1012 Then
                PictureBox301.Visible = False
            End If
            If tnt2xy = 1104 Or tnt2xy = 1204 Or tnt2xy = 1004 Or tnt2xy = 1105 Or tnt2xy = 1103 Then
                If PictureBox343.Visible = True Then
                    Call point2()
                End If
                PictureBox343.Visible = False
            End If
            If tnt2xy = 1105 Or tnt2xy = 1205 Or tnt2xy = 1005 Or tnt2xy = 1106 Or tnt2xy = 1104 Then
                If PictureBox342.Visible = True Then
                    Call point2()
                End If
                PictureBox342.Visible = False
            End If
            If tnt2xy = 1106 Or tnt2xy = 1206 Or tnt2xy = 1006 Or tnt2xy = 1107 Or tnt2xy = 1105 Then
                If PictureBox341.Visible = True Then
                    Call point2()
                End If
                PictureBox341.Visible = False
            End If
            If tnt2xy = 1107 Or tnt2xy = 1207 Or tnt2xy = 1007 Or tnt2xy = 1108 Or tnt2xy = 1106 Then
                If PictureBox340.Visible = True Then
                    Call point2()
                End If
                PictureBox340.Visible = False
            End If
            If tnt2xy = 1108 Or tnt2xy = 1208 Or tnt2xy = 1008 Or tnt2xy = 1109 Or tnt2xy = 1107 Then
                If PictureBox339.Visible = True Then
                    Call point2()
                End If
                PictureBox339.Visible = False
            End If
            If tnt2xy = 1109 Or tnt2xy = 1209 Or tnt2xy = 1009 Or tnt2xy = 1110 Or tnt2xy = 1108 Then
                If PictureBox338.Visible = True Then
                    Call point2()
                End If
                PictureBox338.Visible = False
            End If
            If tnt2xy = 1110 Or tnt2xy = 1210 Or tnt2xy = 1010 Or tnt2xy = 1111 Or tnt2xy = 1109 Then
                If PictureBox337.Visible = True Then
                    Call point2()
                End If
                PictureBox337.Visible = False
            End If
            If tnt2xy = 1203 Or tnt2xy = 1303 Or tnt2xy = 1103 Or tnt2xy = 1204 Or tnt2xy = 1202 Then
                PictureBox377.Visible = False
            End If
            If tnt2xy = 1205 Or tnt2xy = 1305 Or tnt2xy = 1105 Or tnt2xy = 1206 Or tnt2xy = 1204 Then
                PictureBox375.Visible = False
            End If
            If tnt2xy = 1206 Or tnt2xy = 1306 Or tnt2xy = 1106 Or tnt2xy = 1207 Or tnt2xy = 1205 Then
                If PictureBox374.Visible = True Then
                    Call point2()
                End If
                PictureBox374.Visible = False
            End If
            If tnt2xy = 1207 Or tnt2xy = 1307 Or tnt2xy = 1107 Or tnt2xy = 1208 Or tnt2xy = 1206 Then
                If PictureBox373.Visible = True Then
                    Call point2()
                End If
                PictureBox373.Visible = False
            End If
            If tnt2xy = 1208 Or tnt2xy = 1308 Or tnt2xy = 1108 Or tnt2xy = 1209 Or tnt2xy = 1207 Then
                If PictureBox372.Visible = True Then
                    Call point2()
                End If
                PictureBox372.Visible = False
            End If
            If tnt2xy = 1209 Or tnt2xy = 1309 Or tnt2xy = 1109 Or tnt2xy = 1210 Or tnt2xy = 1208 Then
                PictureBox371.Visible = False
            End If
            If tnt2xy = 1211 Or tnt2xy = 1311 Or tnt2xy = 1111 Or tnt2xy = 1212 Or tnt2xy = 1210 Then
                PictureBox369.Visible = False
            End If
        ElseIf map = 5 Then
            If tnt2xy = 101 Or tnt2xy = 201 Or tnt2xy = 1 Or tnt2xy = 102 Or tnt2xy = 100 Then
                PictureBox104.Visible = False
            End If
            If tnt2xy = 102 Or tnt2xy = 202 Or tnt2xy = 2 Or tnt2xy = 103 Or tnt2xy = 101 Then
                PictureBox2.Visible = False
            End If
            If tnt2xy = 105 Or tnt2xy = 205 Or tnt2xy = 5 Or tnt2xy = 106 Or tnt2xy = 104 Then
                PictureBox5.Visible = False
            End If
            If tnt2xy = 106 Or tnt2xy = 206 Or tnt2xy = 6 Or tnt2xy = 107 Or tnt2xy = 105 Then
                PictureBox6.Visible = False
            End If
            If tnt2xy = 107 Or tnt2xy = 207 Or tnt2xy = 7 Or tnt2xy = 108 Or tnt2xy = 106 Then
                PictureBox7.Visible = False
            End If
            If tnt2xy = 108 Or tnt2xy = 208 Or tnt2xy = 8 Or tnt2xy = 109 Or tnt2xy = 107 Then
                PictureBox8.Visible = False
            End If
            If tnt2xy = 109 Or tnt2xy = 209 Or tnt2xy = 9 Or tnt2xy = 110 Or tnt2xy = 108 Then
                PictureBox9.Visible = False
            End If
            If tnt2xy = 201 Or tnt2xy = 301 Or tnt2xy = 101 Or tnt2xy = 202 Or tnt2xy = 200 Then
                PictureBox26.Visible = False
            End If
            If tnt2xy = 202 Or tnt2xy = 302 Or tnt2xy = 102 Or tnt2xy = 203 Or tnt2xy = 201 Then
                PictureBox25.Visible = False
            End If
            If tnt2xy = 205 Or tnt2xy = 305 Or tnt2xy = 105 Or tnt2xy = 206 Or tnt2xy = 204 Then
                PictureBox22.Visible = False
            End If
            If tnt2xy = 206 Or tnt2xy = 306 Or tnt2xy = 106 Or tnt2xy = 207 Or tnt2xy = 205 Then
                If PictureBox21.Visible = True Then
                    Call point2()
                End If
                PictureBox21.Visible = False
            End If
            If tnt2xy = 207 Or tnt2xy = 307 Or tnt2xy = 107 Or tnt2xy = 208 Or tnt2xy = 206 Then
                If PictureBox20.Visible = True Then
                    Call point2()
                End If
                PictureBox20.Visible = False
            End If
            If tnt2xy = 208 Or tnt2xy = 308 Or tnt2xy = 108 Or tnt2xy = 209 Or tnt2xy = 207 Then
                If PictureBox19.Visible = True Then
                    Call point2()
                End If
                PictureBox19.Visible = False
            End If
            If tnt2xy = 209 Or tnt2xy = 309 Or tnt2xy = 109 Or tnt2xy = 210 Or tnt2xy = 208 Then
                PictureBox18.Visible = False
            End If
            If tnt2xy = 301 Or tnt2xy = 401 Or tnt2xy = 201 Or tnt2xy = 302 Or tnt2xy = 300 Then
                If PictureBox59.Visible = True Then
                    Call point2()
                End If
                PictureBox59.Visible = False
            End If
            If tnt2xy = 305 Or tnt2xy = 405 Or tnt2xy = 205 Or tnt2xy = 306 Or tnt2xy = 304 Then
                PictureBox55.Visible = False
            End If
            If tnt2xy = 306 Or tnt2xy = 406 Or tnt2xy = 206 Or tnt2xy = 307 Or tnt2xy = 305 Then
                If PictureBox54.Visible = True Then
                    Call point2()
                End If
                PictureBox54.Visible = False
            End If
            If tnt2xy = 307 Or tnt2xy = 407 Or tnt2xy = 207 Or tnt2xy = 308 Or tnt2xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point2()
                End If
                PictureBox53.Visible = False
            End If
            If tnt2xy = 308 Or tnt2xy = 408 Or tnt2xy = 208 Or tnt2xy = 309 Or tnt2xy = 307 Then
                If PictureBox52.Visible = True Then
                    Call point2()
                End If
                PictureBox52.Visible = False
            End If
            If tnt2xy = 309 Or tnt2xy = 409 Or tnt2xy = 209 Or tnt2xy = 310 Or tnt2xy = 308 Then
                PictureBox51.Visible = False
            End If
            If tnt2xy = 405 Or tnt2xy = 505 Or tnt2xy = 305 Or tnt2xy = 406 Or tnt2xy = 404 Then
                PictureBox88.Visible = False
            End If
            If tnt2xy = 406 Or tnt2xy = 506 Or tnt2xy = 306 Or tnt2xy = 407 Or tnt2xy = 405 Then
                PictureBox87.Visible = False
            End If
            If tnt2xy = 407 Or tnt2xy = 507 Or tnt2xy = 307 Or tnt2xy = 408 Or tnt2xy = 406 Then
                PictureBox86.Visible = False
            End If
            If tnt2xy = 408 Or tnt2xy = 508 Or tnt2xy = 308 Or tnt2xy = 409 Or tnt2xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt2xy = 409 Or tnt2xy = 509 Or tnt2xy = 309 Or tnt2xy = 410 Or tnt2xy = 408 Then
                PictureBox84.Visible = False
            End If
            If tnt2xy = 601 Or tnt2xy = 701 Or tnt2xy = 501 Or tnt2xy = 602 Or tnt2xy = 600 Then
                If PictureBox181.Visible = True Then
                    Call point2()
                End If
                PictureBox181.Visible = False
            End If
            If tnt2xy = 607 Or tnt2xy = 707 Or tnt2xy = 507 Or tnt2xy = 608 Or tnt2xy = 606 Then
                PictureBox175.Visible = False
            End If
            If tnt2xy = 608 Or tnt2xy = 708 Or tnt2xy = 508 Or tnt2xy = 609 Or tnt2xy = 607 Then
                PictureBox174.Visible = False
            End If
            If tnt2xy = 612 Or tnt2xy = 712 Or tnt2xy = 512 Or tnt2xy = 613 Or tnt2xy = 611 Then
                If PictureBox170.Visible = True Then
                    Call point2()
                End If
                PictureBox170.Visible = False
            End If
            If tnt2xy = 707 Or tnt2xy = 807 Or tnt2xy = 607 Or tnt2xy = 708 Or tnt2xy = 706 Then
                PictureBox208.Visible = False
            End If
            If tnt2xy = 708 Or tnt2xy = 808 Or tnt2xy = 608 Or tnt2xy = 709 Or tnt2xy = 707 Then
                PictureBox207.Visible = False
            End If
            If tnt2xy = 905 Or tnt2xy = 1005 Or tnt2xy = 805 Or tnt2xy = 906 Or tnt2xy = 904 Then
                PictureBox276.Visible = False
            End If
            If tnt2xy = 906 Or tnt2xy = 1006 Or tnt2xy = 806 Or tnt2xy = 907 Or tnt2xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt2xy = 907 Or tnt2xy = 1007 Or tnt2xy = 807 Or tnt2xy = 908 Or tnt2xy = 906 Then
                PictureBox274.Visible = False
            End If
            If tnt2xy = 908 Or tnt2xy = 1008 Or tnt2xy = 808 Or tnt2xy = 909 Or tnt2xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt2xy = 909 Or tnt2xy = 1009 Or tnt2xy = 809 Or tnt2xy = 910 Or tnt2xy = 908 Then
                PictureBox272.Visible = False
            End If
            If tnt2xy = 1005 Or tnt2xy = 1105 Or tnt2xy = 905 Or tnt2xy = 1006 Or tnt2xy = 1004 Then
                PictureBox309.Visible = False
            End If
            If tnt2xy = 1006 Or tnt2xy = 1106 Or tnt2xy = 906 Or tnt2xy = 1007 Or tnt2xy = 1005 Then
                If PictureBox308.Visible = True Then
                    Call point2()
                End If
                PictureBox308.Visible = False
            End If
            If tnt2xy = 1007 Or tnt2xy = 1107 Or tnt2xy = 907 Or tnt2xy = 1008 Or tnt2xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point2()
                End If
                PictureBox307.Visible = False
            End If
            If tnt2xy = 1008 Or tnt2xy = 1108 Or tnt2xy = 908 Or tnt2xy = 1009 Or tnt2xy = 1007 Then
                If PictureBox306.Visible = True Then
                    Call point2()
                End If
                PictureBox306.Visible = False
            End If
            If tnt2xy = 1009 Or tnt2xy = 1109 Or tnt2xy = 909 Or tnt2xy = 1010 Or tnt2xy = 1008 Then
                PictureBox305.Visible = False
            End If
            If tnt2xy = 1013 Or tnt2xy = 1113 Or tnt2xy = 913 Or tnt2xy = 1014 Or tnt2xy = 1012 Then
                If PictureBox301.Visible = True Then
                    Call point2()
                End If
                PictureBox301.Visible = False
            End If
            If tnt2xy = 1105 Or tnt2xy = 1205 Or tnt2xy = 1005 Or tnt2xy = 1106 Or tnt2xy = 1104 Then
                PictureBox342.Visible = False
            End If
            If tnt2xy = 1106 Or tnt2xy = 1206 Or tnt2xy = 1006 Or tnt2xy = 1107 Or tnt2xy = 1105 Then
                If PictureBox341.Visible = True Then
                    Call point2()
                End If
                PictureBox341.Visible = False
            End If
            If tnt2xy = 1107 Or tnt2xy = 1207 Or tnt2xy = 1007 Or tnt2xy = 1108 Or tnt2xy = 1106 Then
                If PictureBox340.Visible = True Then
                    Call point2()
                End If
                PictureBox340.Visible = False
            End If
            If tnt2xy = 1108 Or tnt2xy = 1208 Or tnt2xy = 1008 Or tnt2xy = 1109 Or tnt2xy = 1107 Then
                If PictureBox339.Visible = True Then
                    Call point2()
                End If
                PictureBox339.Visible = False
            End If
            If tnt2xy = 1109 Or tnt2xy = 1209 Or tnt2xy = 1009 Or tnt2xy = 1110 Or tnt2xy = 1108 Then
                PictureBox338.Visible = False
            End If
            If tnt2xy = 1112 Or tnt2xy = 1212 Or tnt2xy = 1012 Or tnt2xy = 1113 Or tnt2xy = 1111 Then
                PictureBox335.Visible = False
            End If
            If tnt2xy = 1113 Or tnt2xy = 1213 Or tnt2xy = 1013 Or tnt2xy = 1114 Or tnt2xy = 1112 Then
                PictureBox334.Visible = False
            End If
            If tnt2xy = 1205 Or tnt2xy = 1305 Or tnt2xy = 1105 Or tnt2xy = 1206 Or tnt2xy = 1204 Then
                PictureBox375.Visible = False
            End If
            If tnt2xy = 1206 Or tnt2xy = 1306 Or tnt2xy = 1106 Or tnt2xy = 1207 Or tnt2xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt2xy = 1207 Or tnt2xy = 1307 Or tnt2xy = 1107 Or tnt2xy = 1208 Or tnt2xy = 1206 Then
                PictureBox373.Visible = False
            End If
            If tnt2xy = 1208 Or tnt2xy = 1308 Or tnt2xy = 1108 Or tnt2xy = 1209 Or tnt2xy = 1207 Then
                PictureBox372.Visible = False
            End If
            If tnt2xy = 1209 Or tnt2xy = 1309 Or tnt2xy = 1109 Or tnt2xy = 1210 Or tnt2xy = 1208 Then
                PictureBox371.Visible = False
            End If
            If tnt2xy = 1212 Or tnt2xy = 1312 Or tnt2xy = 1112 Or tnt2xy = 1213 Or tnt2xy = 1211 Then
                PictureBox368.Visible = False
            End If
            If tnt2xy = 1213 Or tnt2xy = 1313 Or tnt2xy = 1113 Or tnt2xy = 1214 Or tnt2xy = 1212 Then
                PictureBox367.Visible = False
            End If
        ElseIf map = 6 Then
            If tnt2xy = 101 Or tnt2xy = 201 Or tnt2xy = 1 Or tnt2xy = 102 Or tnt2xy = 100 Then
                If PictureBox104.Visible = True Then
                    Call point2()
                End If
                PictureBox104.Visible = False
            End If
            If tnt2xy = 102 Or tnt2xy = 202 Or tnt2xy = 2 Or tnt2xy = 103 Or tnt2xy = 101 Then
                If PictureBox2.Visible = True Then
                    Call point2()
                End If
                PictureBox2.Visible = False
            End If
            If tnt2xy = 201 Or tnt2xy = 301 Or tnt2xy = 101 Or tnt2xy = 202 Or tnt2xy = 200 Then
                If PictureBox26.Visible = True Then
                    Call point2()
                End If
                PictureBox26.Visible = False
            End If
            If tnt2xy = 202 Or tnt2xy = 302 Or tnt2xy = 102 Or tnt2xy = 203 Or tnt2xy = 201 Then
                If PictureBox25.Visible = True Then
                    Call point2()
                End If
                PictureBox25.Visible = False
            End If
            If tnt2xy = 303 Or tnt2xy = 403 Or tnt2xy = 203 Or tnt2xy = 304 Or tnt2xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt2xy = 306 Or tnt2xy = 406 Or tnt2xy = 206 Or tnt2xy = 307 Or tnt2xy = 305 Then
                PictureBox54.Visible = False
            End If
            If tnt2xy = 307 Or tnt2xy = 407 Or tnt2xy = 207 Or tnt2xy = 308 Or tnt2xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point2()
                End If
                PictureBox53.Visible = False
            End If
            If tnt2xy = 404 Or tnt2xy = 504 Or tnt2xy = 304 Or tnt2xy = 405 Or tnt2xy = 403 Then
                PictureBox89.Visible = False
            End If
            If tnt2xy = 405 Or tnt2xy = 505 Or tnt2xy = 305 Or tnt2xy = 406 Or tnt2xy = 404 Then
                PictureBox88.Visible = False
            End If
            If tnt2xy = 406 Or tnt2xy = 506 Or tnt2xy = 306 Or tnt2xy = 407 Or tnt2xy = 405 Then
                PictureBox87.Visible = False
            End If
            If tnt2xy = 407 Or tnt2xy = 507 Or tnt2xy = 307 Or tnt2xy = 408 Or tnt2xy = 406 Then
                PictureBox86.Visible = False
            End If
            If tnt2xy = 408 Or tnt2xy = 508 Or tnt2xy = 308 Or tnt2xy = 409 Or tnt2xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt2xy = 409 Or tnt2xy = 509 Or tnt2xy = 309 Or tnt2xy = 410 Or tnt2xy = 408 Then
                PictureBox84.Visible = False
            End If
            If tnt2xy = 410 Or tnt2xy = 510 Or tnt2xy = 310 Or tnt2xy = 411 Or tnt2xy = 409 Then
                PictureBox83.Visible = False
            End If
            If tnt2xy = 504 Or tnt2xy = 604 Or tnt2xy = 404 Or tnt2xy = 505 Or tnt2xy = 503 Then
                PictureBox145.Visible = False
            End If
            If tnt2xy = 505 Or tnt2xy = 605 Or tnt2xy = 405 Or tnt2xy = 506 Or tnt2xy = 504 Then
                If PictureBox1.Visible = True Then
                    Call point2()
                End If
                PictureBox1.Visible = False
            End If
            If tnt2xy = 506 Or tnt2xy = 606 Or tnt2xy = 406 Or tnt2xy = 507 Or tnt2xy = 505 Then
                If PictureBox123.Visible = True Then
                    Call point2()
                End If
                PictureBox123.Visible = False
            End If
            If tnt2xy = 507 Or tnt2xy = 607 Or tnt2xy = 407 Or tnt2xy = 508 Or tnt2xy = 506 Then
                If PictureBox122.Visible = True Then
                    Call point2()
                End If
                PictureBox122.Visible = False
            End If
            If tnt2xy = 508 Or tnt2xy = 608 Or tnt2xy = 408 Or tnt2xy = 509 Or tnt2xy = 507 Then
                If PictureBox121.Visible = True Then
                    Call point2()
                End If
                PictureBox121.Visible = False
            End If
            If tnt2xy = 509 Or tnt2xy = 609 Or tnt2xy = 409 Or tnt2xy = 510 Or tnt2xy = 508 Then
                If PictureBox120.Visible = True Then
                    Call point2()
                End If
                PictureBox120.Visible = False
            End If
            If tnt2xy = 510 Or tnt2xy = 610 Or tnt2xy = 410 Or tnt2xy = 511 Or tnt2xy = 509 Then
                PictureBox119.Visible = False
            End If
            If tnt2xy = 604 Or tnt2xy = 704 Or tnt2xy = 504 Or tnt2xy = 605 Or tnt2xy = 603 Then
                If PictureBox178.Visible = True Then
                    Call point2()
                End If
                PictureBox178.Visible = False
            End If
            If tnt2xy = 605 Or tnt2xy = 705 Or tnt2xy = 505 Or tnt2xy = 606 Or tnt2xy = 604 Then
                If PictureBox177.Visible = True Then
                    Call point2()
                End If
                PictureBox177.Visible = False
            End If
            If tnt2xy = 606 Or tnt2xy = 706 Or tnt2xy = 506 Or tnt2xy = 607 Or tnt2xy = 605 Then
                If PictureBox176.Visible = True Then
                    Call point2()
                End If
                PictureBox176.Visible = False
            End If
            If tnt2xy = 607 Or tnt2xy = 707 Or tnt2xy = 507 Or tnt2xy = 608 Or tnt2xy = 606 Then
                If PictureBox175.Visible = True Then
                    Call point2()
                End If
                PictureBox175.Visible = False
            End If
            If tnt2xy = 608 Or tnt2xy = 708 Or tnt2xy = 508 Or tnt2xy = 609 Or tnt2xy = 607 Then
                If PictureBox174.Visible = True Then
                    Call point2()
                End If
                PictureBox174.Visible = False
            End If
            If tnt2xy = 609 Or tnt2xy = 709 Or tnt2xy = 509 Or tnt2xy = 610 Or tnt2xy = 608 Then
                If PictureBox173.Visible = True Then
                    Call point2()
                End If
                PictureBox173.Visible = False
            End If
            If tnt2xy = 704 Or tnt2xy = 804 Or tnt2xy = 604 Or tnt2xy = 705 Or tnt2xy = 703 Then
                If PictureBox211.Visible = True Then
                    Call point2()
                End If
                PictureBox211.Visible = False
            End If
            If tnt2xy = 705 Or tnt2xy = 805 Or tnt2xy = 605 Or tnt2xy = 706 Or tnt2xy = 704 Then
                If PictureBox210.Visible = True Then
                    Call point2()
                End If
                PictureBox210.Visible = False
            End If
            If tnt2xy = 706 Or tnt2xy = 806 Or tnt2xy = 606 Or tnt2xy = 707 Or tnt2xy = 705 Then
                If PictureBox209.Visible = True Then
                    Call point2()
                End If
                PictureBox209.Visible = False
            End If
            If tnt2xy = 707 Or tnt2xy = 807 Or tnt2xy = 607 Or tnt2xy = 708 Or tnt2xy = 706 Then
                If PictureBox208.Visible = True Then
                    Call point2()
                End If
                PictureBox208.Visible = False
            End If
            If tnt2xy = 708 Or tnt2xy = 808 Or tnt2xy = 608 Or tnt2xy = 709 Or tnt2xy = 707 Then
                If PictureBox207.Visible = True Then
                    Call point2()
                End If
                PictureBox207.Visible = False
            End If
            If tnt2xy = 709 Or tnt2xy = 809 Or tnt2xy = 609 Or tnt2xy = 710 Or tnt2xy = 708 Then
                If PictureBox206.Visible = True Then
                    Call point2()
                End If
                PictureBox206.Visible = False
            End If
            If tnt2xy = 710 Or tnt2xy = 810 Or tnt2xy = 610 Or tnt2xy = 711 Or tnt2xy = 709 Then
                If PictureBox205.Visible = True Then
                    Call point2()
                End If
                PictureBox205.Visible = False
            End If
            If tnt2xy = 804 Or tnt2xy = 904 Or tnt2xy = 704 Or tnt2xy = 805 Or tnt2xy = 803 Then
                PictureBox244.Visible = False
            End If
            If tnt2xy = 805 Or tnt2xy = 905 Or tnt2xy = 705 Or tnt2xy = 806 Or tnt2xy = 804 Then
                If PictureBox243.Visible = True Then
                    Call point2()
                End If
                PictureBox243.Visible = False
            End If
            If tnt2xy = 806 Or tnt2xy = 906 Or tnt2xy = 706 Or tnt2xy = 807 Or tnt2xy = 805 Then
                If PictureBox242.Visible = True Then
                    Call point2()
                End If
                PictureBox242.Visible = False
            End If
            If tnt2xy = 807 Or tnt2xy = 907 Or tnt2xy = 707 Or tnt2xy = 808 Or tnt2xy = 806 Then
                If PictureBox241.Visible = True Then
                    Call point2()
                End If
                PictureBox241.Visible = False
            End If
            If tnt2xy = 808 Or tnt2xy = 908 Or tnt2xy = 708 Or tnt2xy = 809 Or tnt2xy = 807 Then
                If PictureBox240.Visible = True Then
                    Call point2()
                End If
                PictureBox240.Visible = False
            End If
            If tnt2xy = 809 Or tnt2xy = 909 Or tnt2xy = 709 Or tnt2xy = 810 Or tnt2xy = 808 Then
                If PictureBox239.Visible = True Then
                    Call point2()
                End If
                PictureBox239.Visible = False
            End If
            If tnt2xy = 810 Or tnt2xy = 910 Or tnt2xy = 710 Or tnt2xy = 811 Or tnt2xy = 809 Then
                PictureBox238.Visible = False
            End If
            If tnt2xy = 904 Or tnt2xy = 1004 Or tnt2xy = 804 Or tnt2xy = 905 Or tnt2xy = 903 Then
                PictureBox277.Visible = False
            End If
            If tnt2xy = 905 Or tnt2xy = 1005 Or tnt2xy = 805 Or tnt2xy = 906 Or tnt2xy = 904 Then
                PictureBox276.Visible = False
            End If
            If tnt2xy = 906 Or tnt2xy = 1006 Or tnt2xy = 806 Or tnt2xy = 907 Or tnt2xy = 905 Then
                PictureBox275.Visible = False
            End If
            If tnt2xy = 907 Or tnt2xy = 1007 Or tnt2xy = 807 Or tnt2xy = 908 Or tnt2xy = 906 Then
                PictureBox274.Visible = False
            End If
            If tnt2xy = 908 Or tnt2xy = 1008 Or tnt2xy = 808 Or tnt2xy = 909 Or tnt2xy = 907 Then
                PictureBox273.Visible = False
            End If
            If tnt2xy = 909 Or tnt2xy = 1009 Or tnt2xy = 809 Or tnt2xy = 910 Or tnt2xy = 908 Then
                PictureBox272.Visible = False
            End If
            If tnt2xy = 910 Or tnt2xy = 1010 Or tnt2xy = 810 Or tnt2xy = 911 Or tnt2xy = 909 Then
                PictureBox271.Visible = False
            End If
            If tnt2xy = 1006 Or tnt2xy = 1106 Or tnt2xy = 906 Or tnt2xy = 1007 Or tnt2xy = 1005 Then
                PictureBox308.Visible = False
            End If
            If tnt2xy = 1007 Or tnt2xy = 1107 Or tnt2xy = 907 Or tnt2xy = 1008 Or tnt2xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point2()
                End If
                PictureBox307.Visible = False
            End If
            If tnt2xy = 1008 Or tnt2xy = 1108 Or tnt2xy = 908 Or tnt2xy = 1009 Or tnt2xy = 1007 Then
                PictureBox306.Visible = False
            End If
            If tnt2xy = 1011 Or tnt2xy = 1111 Or tnt2xy = 911 Or tnt2xy = 1012 Or tnt2xy = 1010 Then
                PictureBox303.Visible = False
            End If
            If tnt2xy = 1112 Or tnt2xy = 1212 Or tnt2xy = 1012 Or tnt2xy = 1113 Or tnt2xy = 1111 Then
                If PictureBox335.Visible = True Then
                    Call point2()
                End If
                PictureBox335.Visible = False
            End If
            If tnt2xy = 1113 Or tnt2xy = 1213 Or tnt2xy = 1013 Or tnt2xy = 1114 Or tnt2xy = 1112 Then
                If PictureBox334.Visible = True Then
                    Call point2()
                End If
                PictureBox334.Visible = False
            End If
            If tnt2xy = 1204 Or tnt2xy = 1304 Or tnt2xy = 1104 Or tnt2xy = 1205 Or tnt2xy = 1203 Then
                PictureBox376.Visible = False
            End If
            If tnt2xy = 1212 Or tnt2xy = 1312 Or tnt2xy = 1112 Or tnt2xy = 1213 Or tnt2xy = 1211 Then
                If PictureBox368.Visible = True Then
                    Call point2()
                End If
                PictureBox368.Visible = False
            End If
            If tnt2xy = 1213 Or tnt2xy = 1313 Or tnt2xy = 1113 Or tnt2xy = 1214 Or tnt2xy = 1212 Then
                If PictureBox367.Visible = True Then
                    Call point2()
                End If
                PictureBox367.Visible = False
            End If
        ElseIf map = 7 Then
            If tnt2xy = 102 Or tnt2xy = 202 Or tnt2xy = 2 Or tnt2xy = 103 Or tnt2xy = 101 Then
                PictureBox2.Visible = False
            End If
            If tnt2xy = 103 Or tnt2xy = 203 Or tnt2xy = 3 Or tnt2xy = 104 Or tnt2xy = 102 Then
                PictureBox3.Visible = False
            End If
            If tnt2xy = 104 Or tnt2xy = 204 Or tnt2xy = 4 Or tnt2xy = 105 Or tnt2xy = 103 Then
                If PictureBox4.Visible = True Then
                    Call point2()
                End If
                PictureBox4.Visible = False
            End If
            If tnt2xy = 105 Or tnt2xy = 205 Or tnt2xy = 5 Or tnt2xy = 106 Or tnt2xy = 104 Then
                PictureBox5.Visible = False
            End If
            If tnt2xy = 109 Or tnt2xy = 209 Or tnt2xy = 9 Or tnt2xy = 110 Or tnt2xy = 108 Then
                PictureBox9.Visible = False
            End If
            If tnt2xy = 110 Or tnt2xy = 210 Or tnt2xy = 10 Or tnt2xy = 111 Or tnt2xy = 109 Then
                If PictureBox10.Visible = True Then
                    Call point2()
                End If
                PictureBox10.Visible = False
            End If
            If tnt2xy = 111 Or tnt2xy = 211 Or tnt2xy = 11 Or tnt2xy = 112 Or tnt2xy = 110 Then
                PictureBox11.Visible = False
            End If
            If tnt2xy = 201 Or tnt2xy = 301 Or tnt2xy = 101 Or tnt2xy = 202 Or tnt2xy = 200 Then
                PictureBox26.Visible = False
            End If
            If tnt2xy = 204 Or tnt2xy = 304 Or tnt2xy = 104 Or tnt2xy = 205 Or tnt2xy = 203 Then
                PictureBox23.Visible = False
            End If
            If tnt2xy = 210 Or tnt2xy = 310 Or tnt2xy = 110 Or tnt2xy = 211 Or tnt2xy = 209 Then
                PictureBox17.Visible = False
            End If
            If tnt2xy = 303 Or tnt2xy = 403 Or tnt2xy = 203 Or tnt2xy = 304 Or tnt2xy = 302 Then
                PictureBox57.Visible = False
            End If
            If tnt2xy = 304 Or tnt2xy = 404 Or tnt2xy = 204 Or tnt2xy = 305 Or tnt2xy = 303 Then
                If PictureBox56.Visible = True Then
                    Call point2()
                End If
                PictureBox56.Visible = False
            End If
            If tnt2xy = 305 Or tnt2xy = 405 Or tnt2xy = 205 Or tnt2xy = 306 Or tnt2xy = 304 Then
                PictureBox55.Visible = False
            End If
            If tnt2xy = 306 Or tnt2xy = 406 Or tnt2xy = 206 Or tnt2xy = 307 Or tnt2xy = 305 Then
                If PictureBox54.Visible = True Then
                    Call point2()
                End If
                PictureBox54.Visible = False
            End If
            If tnt2xy = 307 Or tnt2xy = 407 Or tnt2xy = 207 Or tnt2xy = 308 Or tnt2xy = 306 Then
                If PictureBox53.Visible = True Then
                    Call point2()
                End If
                PictureBox53.Visible = False
            End If
            If tnt2xy = 308 Or tnt2xy = 408 Or tnt2xy = 208 Or tnt2xy = 309 Or tnt2xy = 307 Then
                If PictureBox52.Visible = True Then
                    Call point2()
                End If
                PictureBox52.Visible = False
            End If
            If tnt2xy = 309 Or tnt2xy = 409 Or tnt2xy = 209 Or tnt2xy = 310 Or tnt2xy = 308 Then
                PictureBox51.Visible = False
            End If
            If tnt2xy = 310 Or tnt2xy = 410 Or tnt2xy = 210 Or tnt2xy = 311 Or tnt2xy = 309 Then
                If PictureBox50.Visible = True Then
                    Call point2()
                End If
                PictureBox50.Visible = False
            End If
            If tnt2xy = 311 Or tnt2xy = 411 Or tnt2xy = 211 Or tnt2xy = 312 Or tnt2xy = 310 Then
                PictureBox49.Visible = False
            End If
            If tnt2xy = 403 Or tnt2xy = 503 Or tnt2xy = 303 Or tnt2xy = 404 Or tnt2xy = 402 Then
                If PictureBox90.Visible = True Then
                    Call point2()
                End If
                PictureBox90.Visible = False
            End If
            If tnt2xy = 408 Or tnt2xy = 508 Or tnt2xy = 308 Or tnt2xy = 409 Or tnt2xy = 407 Then
                PictureBox85.Visible = False
            End If
            If tnt2xy = 411 Or tnt2xy = 511 Or tnt2xy = 311 Or tnt2xy = 412 Or tnt2xy = 410 Then
                If PictureBox82.Visible = True Then
                    Call point2()
                End If
                PictureBox82.Visible = False
            End If
            If tnt2xy = 501 Or tnt2xy = 601 Or tnt2xy = 401 Or tnt2xy = 502 Or tnt2xy = 500 Then
                PictureBox148.Visible = False
            End If
            If tnt2xy = 503 Or tnt2xy = 603 Or tnt2xy = 403 Or tnt2xy = 504 Or tnt2xy = 502 Then
                PictureBox146.Visible = False
            End If
            If tnt2xy = 505 Or tnt2xy = 605 Or tnt2xy = 405 Or tnt2xy = 506 Or tnt2xy = 504 Then
                If PictureBox1.Visible = True Then
                    Call point2()
                End If
                PictureBox1.Visible = False
            End If
            If tnt2xy = 506 Or tnt2xy = 606 Or tnt2xy = 406 Or tnt2xy = 507 Or tnt2xy = 505 Then
                PictureBox123.Visible = False
            End If
            If tnt2xy = 507 Or tnt2xy = 607 Or tnt2xy = 407 Or tnt2xy = 508 Or tnt2xy = 506 Then
                If PictureBox122.Visible = True Then
                    Call point2()
                End If
                PictureBox122.Visible = False
            End If
            If tnt2xy = 508 Or tnt2xy = 608 Or tnt2xy = 408 Or tnt2xy = 509 Or tnt2xy = 507 Then
                If PictureBox121.Visible = True Then
                    Call point2()
                End If
                PictureBox121.Visible = False
            End If
            If tnt2xy = 509 Or tnt2xy = 609 Or tnt2xy = 409 Or tnt2xy = 510 Or tnt2xy = 508 Then
                If PictureBox120.Visible = True Then
                    Call point2()
                End If
                PictureBox120.Visible = False
            End If
            If tnt2xy = 511 Or tnt2xy = 611 Or tnt2xy = 411 Or tnt2xy = 512 Or tnt2xy = 510 Then
                PictureBox118.Visible = False
            End If
            If tnt2xy = 513 Or tnt2xy = 613 Or tnt2xy = 413 Or tnt2xy = 514 Or tnt2xy = 512 Then
                PictureBox116.Visible = False
            End If
            If tnt2xy = 601 Or tnt2xy = 701 Or tnt2xy = 501 Or tnt2xy = 602 Or tnt2xy = 600 Then
                If PictureBox181.Visible = True Then
                    Call point2()
                End If
                PictureBox181.Visible = False
            End If
            If tnt2xy = 602 Or tnt2xy = 702 Or tnt2xy = 502 Or tnt2xy = 603 Or tnt2xy = 601 Then
                PictureBox180.Visible = False
            End If
            If tnt2xy = 603 Or tnt2xy = 703 Or tnt2xy = 503 Or tnt2xy = 604 Or tnt2xy = 602 Then
                If PictureBox179.Visible = True Then
                    Call point2()
                End If
                PictureBox179.Visible = False
            End If
            If tnt2xy = 604 Or tnt2xy = 704 Or tnt2xy = 504 Or tnt2xy = 605 Or tnt2xy = 603 Then
                PictureBox178.Visible = False
            End If
            If tnt2xy = 605 Or tnt2xy = 705 Or tnt2xy = 505 Or tnt2xy = 606 Or tnt2xy = 604 Then
                If PictureBox177.Visible = True Then
                    Call point2()
                End If
                PictureBox177.Visible = False
            End If
            If tnt2xy = 607 Or tnt2xy = 707 Or tnt2xy = 507 Or tnt2xy = 608 Or tnt2xy = 606 Then
                If PictureBox175.Visible = True Then
                    Call point2()
                End If
                PictureBox175.Visible = False
            End If
            If tnt2xy = 608 Or tnt2xy = 708 Or tnt2xy = 508 Or tnt2xy = 609 Or tnt2xy = 607 Then
                If PictureBox174.Visible = True Then
                    Call point2()
                End If
                PictureBox174.Visible = False
            End If
            If tnt2xy = 609 Or tnt2xy = 709 Or tnt2xy = 509 Or tnt2xy = 610 Or tnt2xy = 608 Then
                If PictureBox173.Visible = True Then
                    Call point2()
                End If
                PictureBox173.Visible = False
            End If
            If tnt2xy = 611 Or tnt2xy = 711 Or tnt2xy = 511 Or tnt2xy = 612 Or tnt2xy = 610 Then
                If PictureBox171.Visible = True Then
                    Call point2()
                End If
                PictureBox171.Visible = False
            End If
            If tnt2xy = 612 Or tnt2xy = 712 Or tnt2xy = 512 Or tnt2xy = 613 Or tnt2xy = 611 Then
                PictureBox170.Visible = False
            End If
            If tnt2xy = 613 Or tnt2xy = 713 Or tnt2xy = 513 Or tnt2xy = 614 Or tnt2xy = 612 Then
                If PictureBox169.Visible = True Then
                    Call point2()
                End If
                PictureBox169.Visible = False
            End If
            If tnt2xy = 701 Or tnt2xy = 801 Or tnt2xy = 601 Or tnt2xy = 702 Or tnt2xy = 700 Then
                PictureBox214.Visible = False
            End If
            If tnt2xy = 705 Or tnt2xy = 805 Or tnt2xy = 605 Or tnt2xy = 706 Or tnt2xy = 704 Then
                PictureBox210.Visible = False
            End If
            If tnt2xy = 709 Or tnt2xy = 809 Or tnt2xy = 609 Or tnt2xy = 710 Or tnt2xy = 708 Then
                PictureBox206.Visible = False
            End If
            If tnt2xy = 711 Or tnt2xy = 811 Or tnt2xy = 611 Or tnt2xy = 712 Or tnt2xy = 710 Then
                PictureBox204.Visible = False
            End If
            If tnt2xy = 713 Or tnt2xy = 813 Or tnt2xy = 613 Or tnt2xy = 714 Or tnt2xy = 712 Then
                PictureBox202.Visible = False
            End If
            If tnt2xy = 801 Or tnt2xy = 901 Or tnt2xy = 701 Or tnt2xy = 802 Or tnt2xy = 800 Then
                If PictureBox247.Visible = True Then
                    Call point2()
                End If
                PictureBox247.Visible = False
            End If
            If tnt2xy = 802 Or tnt2xy = 902 Or tnt2xy = 702 Or tnt2xy = 803 Or tnt2xy = 801 Then
                PictureBox246.Visible = False
            End If
            If tnt2xy = 803 Or tnt2xy = 903 Or tnt2xy = 703 Or tnt2xy = 804 Or tnt2xy = 802 Then
                If PictureBox245.Visible = True Then
                    Call point2()
                End If
                PictureBox245.Visible = False
            End If
            If tnt2xy = 805 Or tnt2xy = 905 Or tnt2xy = 705 Or tnt2xy = 806 Or tnt2xy = 804 Then
                If PictureBox243.Visible = True Then
                    Call point2()
                End If
                PictureBox243.Visible = False
            End If
            If tnt2xy = 806 Or tnt2xy = 906 Or tnt2xy = 706 Or tnt2xy = 807 Or tnt2xy = 805 Then
                PictureBox242.Visible = False
            End If
            If tnt2xy = 807 Or tnt2xy = 907 Or tnt2xy = 707 Or tnt2xy = 808 Or tnt2xy = 806 Then
                If PictureBox241.Visible = True Then
                    Call point2()
                End If
                PictureBox241.Visible = False
            End If
            If tnt2xy = 808 Or tnt2xy = 908 Or tnt2xy = 708 Or tnt2xy = 809 Or tnt2xy = 807 Then
                If PictureBox240.Visible = True Then
                    Call point2()
                End If
                PictureBox240.Visible = False
            End If
            If tnt2xy = 809 Or tnt2xy = 909 Or tnt2xy = 709 Or tnt2xy = 810 Or tnt2xy = 808 Then
                If PictureBox239.Visible = True Then
                    Call point2()
                End If
                PictureBox239.Visible = False
            End If
            If tnt2xy = 810 Or tnt2xy = 910 Or tnt2xy = 710 Or tnt2xy = 811 Or tnt2xy = 809 Then
                PictureBox238.Visible = False
            End If
            If tnt2xy = 811 Or tnt2xy = 911 Or tnt2xy = 711 Or tnt2xy = 812 Or tnt2xy = 810 Then
                If PictureBox237.Visible = True Then
                    Call point2()
                End If
                PictureBox237.Visible = False
            End If
            If tnt2xy = 901 Or tnt2xy = 1001 Or tnt2xy = 801 Or tnt2xy = 902 Or tnt2xy = 900 Then
                PictureBox280.Visible = False
            End If
            If tnt2xy = 903 Or tnt2xy = 1003 Or tnt2xy = 803 Or tnt2xy = 904 Or tnt2xy = 902 Then
                If PictureBox278.Visible = True Then
                    Call point2()
                End If
                PictureBox278.Visible = False
            End If
            If tnt2xy = 905 Or tnt2xy = 1005 Or tnt2xy = 805 Or tnt2xy = 906 Or tnt2xy = 904 Then
                PictureBox276.Visible = False
            End If
            If tnt2xy = 911 Or tnt2xy = 1011 Or tnt2xy = 811 Or tnt2xy = 912 Or tnt2xy = 910 Then
                PictureBox270.Visible = False
            End If
            If tnt2xy = 1003 Or tnt2xy = 1103 Or tnt2xy = 903 Or tnt2xy = 1004 Or tnt2xy = 1002 Then
                If PictureBox311.Visible = True Then
                    Call point2()
                End If
                PictureBox311.Visible = False
            End If
            If tnt2xy = 1004 Or tnt2xy = 1104 Or tnt2xy = 904 Or tnt2xy = 1005 Or tnt2xy = 1003 Then
                PictureBox310.Visible = False
            End If
            If tnt2xy = 1005 Or tnt2xy = 1105 Or tnt2xy = 905 Or tnt2xy = 1006 Or tnt2xy = 1004 Then
                If PictureBox309.Visible = True Then
                    Call point2()
                End If
                PictureBox309.Visible = False
            End If
            If tnt2xy = 1006 Or tnt2xy = 1106 Or tnt2xy = 906 Or tnt2xy = 1007 Or tnt2xy = 1005 Then
                PictureBox308.Visible = False
            End If
            If tnt2xy = 1007 Or tnt2xy = 1107 Or tnt2xy = 907 Or tnt2xy = 1008 Or tnt2xy = 1006 Then
                If PictureBox307.Visible = True Then
                    Call point2()
                End If
                PictureBox307.Visible = False
            End If
            If tnt2xy = 1008 Or tnt2xy = 1108 Or tnt2xy = 908 Or tnt2xy = 1009 Or tnt2xy = 1007 Then
                If PictureBox306.Visible = True Then
                    Call point2()
                End If
                PictureBox306.Visible = False
            End If
            If tnt2xy = 1009 Or tnt2xy = 1109 Or tnt2xy = 909 Or tnt2xy = 1010 Or tnt2xy = 1008 Then
                If PictureBox305.Visible = True Then
                    Call point2()
                End If
                PictureBox305.Visible = False
            End If
            If tnt2xy = 1010 Or tnt2xy = 1110 Or tnt2xy = 910 Or tnt2xy = 1011 Or tnt2xy = 1009 Then
                If PictureBox304.Visible = True Then
                    Call point2()
                End If
                PictureBox304.Visible = False
            End If
            If tnt2xy = 1011 Or tnt2xy = 1111 Or tnt2xy = 911 Or tnt2xy = 1012 Or tnt2xy = 1010 Then
                If PictureBox303.Visible = True Then
                    Call point2()
                End If
                PictureBox303.Visible = False
            End If
            If tnt2xy = 1105 Or tnt2xy = 1205 Or tnt2xy = 1005 Or tnt2xy = 1106 Or tnt2xy = 1104 Then
                PictureBox342.Visible = False
            End If
            If tnt2xy = 1111 Or tnt2xy = 1211 Or tnt2xy = 1011 Or tnt2xy = 1112 Or tnt2xy = 1110 Then
                PictureBox336.Visible = False
            End If
            If tnt2xy = 1204 Or tnt2xy = 1304 Or tnt2xy = 1104 Or tnt2xy = 1205 Or tnt2xy = 1203 Then
                PictureBox376.Visible = False
            End If
            If tnt2xy = 1205 Or tnt2xy = 1305 Or tnt2xy = 1105 Or tnt2xy = 1206 Or tnt2xy = 1204 Then
                If PictureBox375.Visible = True Then
                    Call point2()
                End If
                PictureBox375.Visible = False
            End If
            If tnt2xy = 1206 Or tnt2xy = 1306 Or tnt2xy = 1106 Or tnt2xy = 1207 Or tnt2xy = 1205 Then
                PictureBox374.Visible = False
            End If
            If tnt2xy = 1212 Or tnt2xy = 1312 Or tnt2xy = 1112 Or tnt2xy = 1213 Or tnt2xy = 1211 Then
                PictureBox368.Visible = False
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
            ElseIf e.KeyCode = Keys.W Then
                Character2_up()
            ElseIf e.KeyCode = Keys.A Then
                Character2_left()
            ElseIf e.KeyCode = Keys.S Then
                Character2_down()
            ElseIf e.KeyCode = Keys.D Then
                Character2_right()
            ElseIf e.KeyCode = Keys.G Then
                If tnt2_tick1 < 1 Or tnt2_tick1 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\fuse.wav", AudioPlayMode.Background)
                    tnt21 = x2 * 100 + y2
                    PictureBox398.Image = ImageList2.Images(0)
                    PictureBox398.Left = (x2 - 1) * PictureBox104.Width
                    PictureBox398.Top = (y2 - 1) * PictureBox104.Height
                    tnt2_tick1 = 0
                    exer2 = 1
                    Timer3.Enabled = True
                ElseIf tnt2_tick2 < 1 Or tnt2_tick2 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\fuse.wav", AudioPlayMode.Background)
                    tnt22 = x2 * 100 + y2
                    PictureBox399.Image = ImageList2.Images(0)
                    PictureBox399.Left = (x2 - 1) * PictureBox104.Width
                    PictureBox399.Top = (y2 - 1) * PictureBox104.Height
                    tnt2_tick2 = 0
                    exer2 = 1
                    Timer4.Enabled = True
                End If
            ElseIf e.KeyCode = 191 Then
                If tnt1_tick1 < 1 Or tnt1_tick1 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\fuse.wav", AudioPlayMode.Background)
                    tnt11 = x1 * 100 + y1
                    PictureBox392.Image = ImageList2.Images(0)
                    PictureBox392.Left = (x1 - 1) * PictureBox104.Width
                    PictureBox392.Top = (y1 - 1) * PictureBox104.Height
                    tnt1_tick1 = 0
                    exer1 = 1
                    Timer1.Enabled = True
                ElseIf tnt1_tick2 < 1 Or tnt1_tick2 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\fuse.wav", AudioPlayMode.Background)
                    tnt12 = x1 * 100 + y1
                    PictureBox393.Image = ImageList2.Images(0)
                    PictureBox393.Left = (x1 - 1) * PictureBox104.Width
                    PictureBox393.Top = (y1 - 1) * PictureBox104.Height
                    tnt1_tick2 = 0
                    exer1 = 1
                    Timer2.Enabled = True
                End If
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
            Call Engineering_mode()
            Call tntwall()
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
    End Sub

    '載入遊戲時初始化(onload)
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        If mc = 1 Then
            minecraft()
        Else
            block(1) = 0
            block(2) = 1
            block(3) = 2
        End If
        Dim gm, gs As Integer
        If random = True Then
            Randomize()
            map = Int((7 - 0 + 1) * Rnd() + 0)
        End If
        PictureBox136.Visible = False
        Button1.Visible = False
        Button2.Visible = False
        Button3.Visible = False
        gm = (gaintime + 30) \ 60
        gs = gaintime Mod 60 - 30
        m = 4 + gm
        s = 30 + gs
        x1 = 12
        y1 = 1
        x2 = 1
        y2 = 13
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
        Label10.BringToFront()
        Label11.BringToFront()
        Label7.BringToFront()
        Label6.BringToFront()

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

        player.settings.setMode("loop", True)
        If music = 0 Then
            Dim i As Integer
            Randomize()
            i = Int((6 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm1.mp3"
            ElseIf i = 1 Then
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm2.mp3"
            ElseIf i = 2 Then
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm3.mp3"
            ElseIf i = 3 Then
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm4.mp3"
            ElseIf i = 4 Then
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm5.mp3"
            ElseIf i = 5 Then
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm6.mp3"
            ElseIf i = 6 Then
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm7.mp3"
            End If
        ElseIf music = 1 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\Dies Irae.mp3"
        ElseIf music = 2 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\+9.mp3"
        ElseIf music = 3 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\apple.mp3"
        End If
        maptype()
        Timer6.Enabled = True
    End Sub

    '1P TNT[1]引爆timer(Timer_1)
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        tnt1_tick1 += 1
        Label4.Text = "tnt1_tick:" & tnt1_tick1 & " (tick/s)"
        PictureBox392.Visible = True
        If tnt1_tick1 Mod 2 = 0 Then
            PictureBox392.Image = ImageList2.Images(0)
        Else
            PictureBox392.Image = ImageList2.Images(1)
        End If
        If tnt1_tick1 = 5 Then
            PictureBox392.Image = ImageList2.Images(2)
            tnt1xy = tnt11
            Label3.Text = "tnt1xy: " & tnt1xy
            Call tnt_1()
            Call ex1()
            Call die1()
            Call die2()
            Call ending2()
        End If
        If tnt1_tick1 > 5 Then
            tnt11 = 0
            Timer1.Enabled = False
            PictureBox392.Visible = False
            PictureBox380.Visible = False
            PictureBox381.Visible = False
            PictureBox382.Visible = False
            PictureBox383.Visible = False
            PictureBox384.Visible = False
            PictureBox385.Visible = False
            PictureBox386.Visible = False
            PictureBox387.Visible = False
        End If

    End Sub
    '1P TNT[2]引爆timer(Timer_2)
    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        tnt1_tick2 += 1
        Label5.Text = "tnt2_tick:" & tnt1_tick2 & " (tick/s)"
        PictureBox393.Visible = True
        If tnt1_tick2 Mod 2 = 0 Then
            PictureBox393.Image = ImageList2.Images(0)
        Else
            PictureBox393.Image = ImageList2.Images(1)
        End If
        If tnt1_tick2 = 5 Then
            Label3.Text = "tnt1xy: " & tnt1xy
            PictureBox393.Image = ImageList2.Images(2)
            tnt1xy = tnt12
            Label3.Text = "tnt1xy: " & tnt1xy
            Call tnt_1()
            Call ex1()
            Call die1()
            Call die2()
            Call ending2()
        End If
        If tnt1_tick2 > 5 Then
            tnt12 = 0
            Timer2.Enabled = False
            PictureBox393.Visible = False
            PictureBox380.Visible = False
            PictureBox381.Visible = False
            PictureBox382.Visible = False
            PictureBox383.Visible = False
            PictureBox384.Visible = False
            PictureBox385.Visible = False
            PictureBox386.Visible = False
            PictureBox387.Visible = False
        End If
    End Sub

    '2P TNT[1]引爆timer(Timer_1)
    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
        tnt2_tick1 += 1
        PictureBox398.Visible = True
        If tnt2_tick1 Mod 2 = 0 Then
            PictureBox398.Image = ImageList2.Images(0)
        Else
            PictureBox398.Image = ImageList2.Images(1)
        End If
        If tnt2_tick1 = 5 Then
            Label15.Text = "tnt2xy: " & tnt2xy
            PictureBox398.Image = ImageList2.Images(2)
            tnt2xy = tnt21
            Label15.Text = "tnt2xy: " & tnt2xy
            Call tnt_2()
            Call ex2()
            Call die1()
            Call die2()
            Call ending2()
        End If
        If tnt2_tick1 > 5 Then
            tnt21 = 0
            Timer3.Enabled = False
            PictureBox398.Visible = False
            PictureBox388.Visible = False
            PictureBox389.Visible = False
            PictureBox390.Visible = False
            PictureBox391.Visible = False
            PictureBox394.Visible = False
            PictureBox395.Visible = False
            PictureBox396.Visible = False
            PictureBox397.Visible = False
        End If

    End Sub
    '2P TNT[2]引爆timer(Timer_2)
    Private Sub Timer4_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer4.Tick
        tnt2_tick2 += 1
        PictureBox399.Visible = True
        If tnt2_tick2 Mod 2 = 0 Then
            PictureBox399.Image = ImageList2.Images(0)
        Else
            PictureBox399.Image = ImageList2.Images(1)
        End If
        If tnt2_tick2 = 5 Then
            Label15.Text = "tnt2xy: " & tnt2xy
            PictureBox399.Image = ImageList2.Images(2)
            tnt2xy = tnt22
            Label15.Text = "tnt2xy: " & tnt2xy
            Call tnt_2()
            Call ex2()
            Call die1()
            Call die2()
            Call ending2()
        End If
        If tnt2_tick2 > 5 Then
            tnt22 = 0
            Timer4.Enabled = False
            PictureBox399.Visible = False
            PictureBox388.Visible = False
            PictureBox389.Visible = False
            PictureBox390.Visible = False
            PictureBox391.Visible = False
            PictureBox394.Visible = False
            PictureBox395.Visible = False
            PictureBox396.Visible = False
            PictureBox397.Visible = False
        End If
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
        replay1 = 5
        Me.Close()
    End Sub
    'back
    Private Sub Button3_Click(sender As System.Object, e As System.EventArgs) Handles Button3.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        Form2.Show()
        Me.Close()

    End Sub

    '時間倒數
    Private Sub Timer6_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer6.Tick
        s -= 1
        If s < 0 And m >= 0 Then
            m -= 1
            s = 59
        End If
        If s <= 0 And m <= 0 Then
            ending3()
            Timer6.Enabled = False
        End If
        Label18.Text = m & "分"
        Label19.Text = s & "秒"

    End Sub
    '地圖
    Private Sub maptype()
        If map = 0 Then
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox16.Visible = True
            PictureBox15.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
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
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox176.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox210.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True
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
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True




            PictureBox7.Image = ImageList9.Images(block(1))
            PictureBox53.Image = ImageList9.Images(block(1))
            PictureBox88.Image = ImageList9.Images(block(1))
            PictureBox84.Image = ImageList9.Images(block(1))
            PictureBox181.Image = ImageList9.Images(block(1))
            PictureBox178.Image = ImageList9.Images(block(1))
            PictureBox175.Image = ImageList9.Images(block(1))
            PictureBox172.Image = ImageList9.Images(block(1))
            PictureBox169.Image = ImageList9.Images(block(1))
            PictureBox214.Image = ImageList9.Images(block(1))
            PictureBox211.Image = ImageList9.Images(block(1))
            PictureBox208.Image = ImageList9.Images(block(1))
            PictureBox205.Image = ImageList9.Images(block(1))
            PictureBox202.Image = ImageList9.Images(block(1))
            PictureBox276.Image = ImageList9.Images(block(1))
            PictureBox272.Image = ImageList9.Images(block(1))
            PictureBox307.Image = ImageList9.Images(block(1))
            PictureBox373.Image = ImageList9.Images(block(1))
            PictureBox6.Image = ImageList9.Images(block(2))
            PictureBox8.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox47.Image = ImageList9.Images(block(2))
            PictureBox92.Image = ImageList9.Images(block(2))
            PictureBox90.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox86.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox82.Image = ImageList9.Images(block(2))
            PictureBox80.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox1.Image = ImageList9.Images(block(2))
            PictureBox120.Image = ImageList9.Images(block(2))
            PictureBox118.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox179.Image = ImageList9.Images(block(2))
            PictureBox177.Image = ImageList9.Images(block(2))
            PictureBox176.Image = ImageList9.Images(block(2))
            PictureBox174.Image = ImageList9.Images(block(2))
            PictureBox173.Image = ImageList9.Images(block(2))
            PictureBox171.Image = ImageList9.Images(block(2))
            PictureBox212.Image = ImageList9.Images(block(2))
            PictureBox210.Image = ImageList9.Images(block(2))
            PictureBox209.Image = ImageList9.Images(block(2))
            PictureBox207.Image = ImageList9.Images(block(2))
            PictureBox206.Image = ImageList9.Images(block(2))
            PictureBox204.Image = ImageList9.Images(block(2))
            PictureBox247.Image = ImageList9.Images(block(2))
            PictureBox245.Image = ImageList9.Images(block(2))
            PictureBox243.Image = ImageList9.Images(block(2))
            PictureBox239.Image = ImageList9.Images(block(2))
            PictureBox237.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(2))
            PictureBox278.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox274.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox301.Image = ImageList9.Images(block(2))
            PictureBox374.Image = ImageList9.Images(block(2))
            PictureBox372.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(3))
            PictureBox24.Image = ImageList9.Images(block(3))
            PictureBox21.Image = ImageList9.Images(block(3))
            PictureBox20.Image = ImageList9.Images(block(3))
            PictureBox19.Image = ImageList9.Images(block(3))
            PictureBox16.Image = ImageList9.Images(block(3))
            PictureBox15.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox55.Image = ImageList9.Images(block(3))
            PictureBox54.Image = ImageList9.Images(block(3))
            PictureBox52.Image = ImageList9.Images(block(3))
            PictureBox51.Image = ImageList9.Images(block(3))
            PictureBox48.Image = ImageList9.Images(block(3))
            PictureBox91.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox83.Image = ImageList9.Images(block(3))
            PictureBox81.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(block(3))
            PictureBox145.Image = ImageList9.Images(block(3))
            PictureBox123.Image = ImageList9.Images(block(3))
            PictureBox122.Image = ImageList9.Images(block(3))
            PictureBox121.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox180.Image = ImageList9.Images(block(3))
            PictureBox170.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox203.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(block(3))
            PictureBox244.Image = ImageList9.Images(block(3))
            PictureBox242.Image = ImageList9.Images(block(3))
            PictureBox241.Image = ImageList9.Images(block(3))
            PictureBox240.Image = ImageList9.Images(block(3))
            PictureBox238.Image = ImageList9.Images(block(3))
            PictureBox236.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox271.Image = ImageList9.Images(block(3))
            PictureBox269.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox308.Image = ImageList9.Images(block(3))
            PictureBox306.Image = ImageList9.Images(block(3))
            PictureBox305.Image = ImageList9.Images(block(3))
            PictureBox302.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox341.Image = ImageList9.Images(block(3))
            PictureBox340.Image = ImageList9.Images(block(3))
            PictureBox339.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
            PictureBox336.Image = ImageList9.Images(block(3))
        ElseIf map = 1 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox12.Visible = True
            PictureBox25.Visible = True
            PictureBox23.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox19.Visible = True
            PictureBox17.Visible = True
            PictureBox15.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox91.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox87.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox83.Visible = True
            PictureBox82.Visible = True
            PictureBox81.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox145.Visible = True
            PictureBox1.Visible = True
            PictureBox123.Visible = True
            PictureBox121.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox117.Visible = True
            PictureBox180.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox176.Visible = True
            PictureBox174.Visible = True
            PictureBox172.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
            PictureBox203.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox242.Visible = True
            PictureBox241.Visible = True
            PictureBox240.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
            PictureBox279.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox273.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox269.Visible = True
            PictureBox312.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox306.Visible = True
            PictureBox304.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox345.Visible = True
            PictureBox344.Visible = True
            PictureBox343.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox337.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox370.Visible = True
            PictureBox369.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox104.Image = ImageList9.Images(block(1))
            PictureBox5.Image = ImageList9.Images(block(1))
            PictureBox9.Image = ImageList9.Images(block(1))
            PictureBox90.Image = ImageList9.Images(block(1))
            PictureBox86.Image = ImageList9.Images(block(1))
            PictureBox82.Image = ImageList9.Images(block(1))
            PictureBox148.Image = ImageList9.Images(block(1))
            PictureBox1.Image = ImageList9.Images(block(1))
            PictureBox120.Image = ImageList9.Images(block(1))
            PictureBox169.Image = ImageList9.Images(block(1))
            PictureBox245.Image = ImageList9.Images(block(1))
            PictureBox241.Image = ImageList9.Images(block(1))
            PictureBox237.Image = ImageList9.Images(block(1))
            PictureBox276.Image = ImageList9.Images(block(1))
            PictureBox272.Image = ImageList9.Images(block(1))
            PictureBox301.Image = ImageList9.Images(block(1))
            PictureBox377.Image = ImageList9.Images(block(1))
            PictureBox373.Image = ImageList9.Images(block(1))
            PictureBox369.Image = ImageList9.Images(block(1))
            PictureBox4.Image = ImageList9.Images(block(2))
            PictureBox8.Image = ImageList9.Images(block(2))
            PictureBox12.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox21.Image = ImageList9.Images(block(2))
            PictureBox17.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox56.Image = ImageList9.Images(block(2))
            PictureBox52.Image = ImageList9.Images(block(2))
            PictureBox48.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox121.Image = ImageList9.Images(block(2))
            PictureBox117.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox176.Image = ImageList9.Images(block(2))
            PictureBox172.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox211.Image = ImageList9.Images(block(2))
            PictureBox207.Image = ImageList9.Images(block(2))
            PictureBox203.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox242.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox269.Image = ImageList9.Images(block(2))
            PictureBox312.Image = ImageList9.Images(block(2))
            PictureBox308.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox343.Image = ImageList9.Images(block(2))
            PictureBox339.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox378.Image = ImageList9.Images(block(2))
            PictureBox374.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox23.Image = ImageList9.Images(block(3))
            PictureBox22.Image = ImageList9.Images(block(3))
            PictureBox19.Image = ImageList9.Images(block(3))
            PictureBox15.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox57.Image = ImageList9.Images(block(3))
            PictureBox54.Image = ImageList9.Images(block(3))
            PictureBox53.Image = ImageList9.Images(block(3))
            PictureBox50.Image = ImageList9.Images(block(3))
            PictureBox49.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox85.Image = ImageList9.Images(block(3))
            PictureBox81.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(block(3))
            PictureBox123.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox178.Image = ImageList9.Images(block(3))
            PictureBox177.Image = ImageList9.Images(block(3))
            PictureBox174.Image = ImageList9.Images(block(3))
            PictureBox170.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox212.Image = ImageList9.Images(block(3))
            PictureBox209.Image = ImageList9.Images(block(3))
            PictureBox208.Image = ImageList9.Images(block(3))
            PictureBox205.Image = ImageList9.Images(block(3))
            PictureBox204.Image = ImageList9.Images(block(3))
            PictureBox244.Image = ImageList9.Images(block(3))
            PictureBox240.Image = ImageList9.Images(block(3))
            PictureBox236.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox275.Image = ImageList9.Images(block(3))
            PictureBox271.Image = ImageList9.Images(block(3))
            PictureBox310.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox306.Image = ImageList9.Images(block(3))
            PictureBox302.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox341.Image = ImageList9.Images(block(3))
            PictureBox340.Image = ImageList9.Images(block(3))
            PictureBox337.Image = ImageList9.Images(block(3))
            PictureBox336.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
        ElseIf map = 2 Then
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
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox23.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox16.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox54.Visible = True
            PictureBox52.Visible = True
            PictureBox50.Visible = True
            PictureBox49.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox92.Visible = True
            PictureBox91.Visible = True
            PictureBox89.Visible = True
            PictureBox88.Visible = True
            PictureBox87.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox148.Visible = True
            PictureBox147.Visible = True
            PictureBox145.Visible = True
            PictureBox123.Visible = True
            PictureBox121.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox176.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox211.Visible = True
            PictureBox210.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox245.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox241.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox277.Visible = True
            PictureBox275.Visible = True
            PictureBox274.Visible = True
            PictureBox273.Visible = True
            PictureBox272.Visible = True
            PictureBox270.Visible = True
            PictureBox269.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox311.Visible = True
            PictureBox309.Visible = True
            PictureBox307.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox302.Visible = True
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
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox3.Image = ImageList9.Images(block(1))
            PictureBox7.Image = ImageList9.Images(block(1))
            PictureBox11.Image = ImageList9.Images(block(1))
            PictureBox26.Image = ImageList9.Images(block(1))
            PictureBox80.Image = ImageList9.Images(block(1))
            PictureBox148.Image = ImageList9.Images(block(1))
            PictureBox214.Image = ImageList9.Images(block(1))
            PictureBox280.Image = ImageList9.Images(block(1))
            PictureBox339.Image = ImageList9.Images(block(1))
            PictureBox374.Image = ImageList9.Images(block(1))
            PictureBox371.Image = ImageList9.Images(block(1))
            PictureBox367.Image = ImageList9.Images(block(1))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox20.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox54.Image = ImageList9.Images(block(2))
            PictureBox86.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox81.Image = ImageList9.Images(block(2))
            PictureBox147.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox123.Image = ImageList9.Images(block(2))
            PictureBox120.Image = ImageList9.Images(block(2))
            PictureBox173.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox213.Image = ImageList9.Images(block(2))
            PictureBox211.Image = ImageList9.Images(block(2))
            PictureBox210.Image = ImageList9.Images(block(2))
            PictureBox208.Image = ImageList9.Images(block(2))
            PictureBox202.Image = ImageList9.Images(block(2))
            PictureBox241.Image = ImageList9.Images(block(2))
            PictureBox239.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox307.Image = ImageList9.Images(block(2))
            PictureBox304.Image = ImageList9.Images(block(2))
            PictureBox302.Image = ImageList9.Images(block(2))
            PictureBox345.Image = ImageList9.Images(block(2))
            PictureBox343.Image = ImageList9.Images(block(2))
            PictureBox341.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox334.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox2.Image = ImageList9.Images(block(3))
            PictureBox4.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox8.Image = ImageList9.Images(block(3))
            PictureBox9.Image = ImageList9.Images(block(3))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox23.Image = ImageList9.Images(block(3))
            PictureBox21.Image = ImageList9.Images(block(3))
            PictureBox19.Image = ImageList9.Images(block(3))
            PictureBox59.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox56.Image = ImageList9.Images(block(3))
            PictureBox52.Image = ImageList9.Images(block(3))
            PictureBox50.Image = ImageList9.Images(block(3))
            PictureBox49.Image = ImageList9.Images(block(3))
            PictureBox48.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox92.Image = ImageList9.Images(block(3))
            PictureBox91.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox88.Image = ImageList9.Images(block(3))
            PictureBox87.Image = ImageList9.Images(block(3))
            PictureBox121.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox118.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox180.Image = ImageList9.Images(block(3))
            PictureBox178.Image = ImageList9.Images(block(3))
            PictureBox177.Image = ImageList9.Images(block(3))
            PictureBox176.Image = ImageList9.Images(block(3))
            PictureBox175.Image = ImageList9.Images(block(3))
            PictureBox174.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox207.Image = ImageList9.Images(block(3))
            PictureBox206.Image = ImageList9.Images(block(3))
            PictureBox205.Image = ImageList9.Images(block(3))
            PictureBox204.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(block(3))
            PictureBox245.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox242.Image = ImageList9.Images(block(3))
            PictureBox237.Image = ImageList9.Images(block(3))
            PictureBox236.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox278.Image = ImageList9.Images(block(3))
            PictureBox274.Image = ImageList9.Images(block(3))
            PictureBox272.Image = ImageList9.Images(block(3))
            PictureBox270.Image = ImageList9.Images(block(3))
            PictureBox269.Image = ImageList9.Images(block(3))
            PictureBox311.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox305.Image = ImageList9.Images(block(3))
            PictureBox346.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox342.Image = ImageList9.Images(block(3))
            PictureBox340.Image = ImageList9.Images(block(3))
            PictureBox338.Image = ImageList9.Images(block(3))
            PictureBox337.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
            PictureBox368.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
        ElseIf map = 3 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox10.Visible = True
            PictureBox12.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox24.Visible = True
            PictureBox22.Visible = True
            PictureBox20.Visible = True
            PictureBox18.Visible = True
            PictureBox17.Visible = True
            PictureBox16.Visible = True
            PictureBox59.Visible = True
            PictureBox58.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox50.Visible = True
            PictureBox48.Visible = True
            PictureBox47.Visible = True
            PictureBox91.Visible = True
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
            PictureBox146.Visible = True
            PictureBox1.Visible = True
            PictureBox123.Visible = True
            PictureBox122.Visible = True
            PictureBox121.Visible = True
            PictureBox119.Visible = True
            PictureBox118.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox178.Visible = True
            PictureBox176.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox210.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True
            PictureBox205.Visible = True
            PictureBox203.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox245.Visible = True
            PictureBox244.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox278.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox274.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox268.Visible = True
            PictureBox313.Visible = True
            PictureBox312.Visible = True
            PictureBox311.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox303.Visible = True
            PictureBox302.Visible = True
            PictureBox301.Visible = True
            PictureBox344.Visible = True
            PictureBox342.Visible = True
            PictureBox340.Visible = True
            PictureBox338.Visible = True
            PictureBox336.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox378.Visible = True
            PictureBox377.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox369.Visible = True
            PictureBox367.Visible = True




            PictureBox2.Image = ImageList9.Images(block(1))
            PictureBox4.Image = ImageList9.Images(block(1))
            PictureBox10.Image = ImageList9.Images(block(1))
            PictureBox26.Image = ImageList9.Images(block(1))
            PictureBox22.Image = ImageList9.Images(block(1))
            PictureBox20.Image = ImageList9.Images(block(1))
            PictureBox57.Image = ImageList9.Images(block(1))
            PictureBox48.Image = ImageList9.Images(block(1))
            PictureBox86.Image = ImageList9.Images(block(1))
            PictureBox84.Image = ImageList9.Images(block(1))
            PictureBox1.Image = ImageList9.Images(block(1))
            PictureBox119.Image = ImageList9.Images(block(1))
            PictureBox169.Image = ImageList9.Images(block(1))
            PictureBox213.Image = ImageList9.Images(block(1))
            PictureBox210.Image = ImageList9.Images(block(1))
            PictureBox206.Image = ImageList9.Images(block(1))
            PictureBox203.Image = ImageList9.Images(block(1))
            PictureBox247.Image = ImageList9.Images(block(1))
            PictureBox240.Image = ImageList9.Images(block(1))
            PictureBox278.Image = ImageList9.Images(block(1))
            PictureBox274.Image = ImageList9.Images(block(1))
            PictureBox272.Image = ImageList9.Images(block(1))
            PictureBox313.Image = ImageList9.Images(block(1))
            PictureBox310.Image = ImageList9.Images(block(1))
            PictureBox302.Image = ImageList9.Images(block(1))
            PictureBox340.Image = ImageList9.Images(block(1))
            PictureBox334.Image = ImageList9.Images(block(1))
            PictureBox376.Image = ImageList9.Images(block(1))
            PictureBox369.Image = ImageList9.Images(block(1))
            PictureBox3.Image = ImageList9.Images(block(2))
            PictureBox8.Image = ImageList9.Images(block(2))
            PictureBox24.Image = ImageList9.Images(block(2))
            PictureBox16.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox54.Image = ImageList9.Images(block(2))
            PictureBox52.Image = ImageList9.Images(block(2))
            PictureBox50.Image = ImageList9.Images(block(2))
            PictureBox91.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox80.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox122.Image = ImageList9.Images(block(2))
            PictureBox121.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox176.Image = ImageList9.Images(block(2))
            PictureBox173.Image = ImageList9.Images(block(2))
            PictureBox171.Image = ImageList9.Images(block(2))
            PictureBox212.Image = ImageList9.Images(block(2))
            PictureBox208.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox237.Image = ImageList9.Images(block(2))
            PictureBox279.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox268.Image = ImageList9.Images(block(2))
            PictureBox305.Image = ImageList9.Images(block(2))
            PictureBox344.Image = ImageList9.Images(block(2))
            PictureBox342.Image = ImageList9.Images(block(2))
            PictureBox374.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox104.Image = ImageList9.Images(block(3))
            PictureBox5.Image = ImageList9.Images(block(3))
            PictureBox6.Image = ImageList9.Images(block(3))
            PictureBox7.Image = ImageList9.Images(block(3))
            PictureBox12.Image = ImageList9.Images(block(3))
            PictureBox25.Image = ImageList9.Images(block(3))
            PictureBox18.Image = ImageList9.Images(block(3))
            PictureBox17.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox56.Image = ImageList9.Images(block(3))
            PictureBox55.Image = ImageList9.Images(block(3))
            PictureBox53.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox88.Image = ImageList9.Images(block(3))
            PictureBox87.Image = ImageList9.Images(block(3))
            PictureBox85.Image = ImageList9.Images(block(3))
            PictureBox82.Image = ImageList9.Images(block(3))
            PictureBox148.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(block(3))
            PictureBox123.Image = ImageList9.Images(block(3))
            PictureBox118.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox181.Image = ImageList9.Images(block(3))
            PictureBox178.Image = ImageList9.Images(block(3))
            PictureBox175.Image = ImageList9.Images(block(3))
            PictureBox174.Image = ImageList9.Images(block(3))
            PictureBox170.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox211.Image = ImageList9.Images(block(3))
            PictureBox209.Image = ImageList9.Images(block(3))
            PictureBox207.Image = ImageList9.Images(block(3))
            PictureBox205.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox245.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox242.Image = ImageList9.Images(block(3))
            PictureBox239.Image = ImageList9.Images(block(3))
            PictureBox238.Image = ImageList9.Images(block(3))
            PictureBox236.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox276.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(block(3))
            PictureBox311.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox308.Image = ImageList9.Images(block(3))
            PictureBox306.Image = ImageList9.Images(block(3))
            PictureBox303.Image = ImageList9.Images(block(3))
            PictureBox301.Image = ImageList9.Images(block(3))
            PictureBox338.Image = ImageList9.Images(block(3))
            PictureBox336.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
            PictureBox378.Image = ImageList9.Images(block(3))
            PictureBox377.Image = ImageList9.Images(block(3))
            PictureBox375.Image = ImageList9.Images(block(3))
            PictureBox373.Image = ImageList9.Images(block(3))
            PictureBox372.Image = ImageList9.Images(block(3))
            PictureBox371.Image = ImageList9.Images(block(3))
        ElseIf map = 4 Then
            PictureBox3.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox11.Visible = True
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
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox176.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox210.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True
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
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
            PictureBox235.Visible = True
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
            PictureBox377.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox369.Visible = True




            PictureBox6.Image = ImageList9.Images(block(1))
            PictureBox7.Image = ImageList9.Images(block(1))
            PictureBox8.Image = ImageList9.Images(block(1))
            PictureBox23.Image = ImageList9.Images(block(1))
            PictureBox22.Image = ImageList9.Images(block(1))
            PictureBox21.Image = ImageList9.Images(block(1))
            PictureBox20.Image = ImageList9.Images(block(1))
            PictureBox19.Image = ImageList9.Images(block(1))
            PictureBox18.Image = ImageList9.Images(block(1))
            PictureBox17.Image = ImageList9.Images(block(1))
            PictureBox56.Image = ImageList9.Images(block(1))
            PictureBox55.Image = ImageList9.Images(block(1))
            PictureBox54.Image = ImageList9.Images(block(1))
            PictureBox53.Image = ImageList9.Images(block(1))
            PictureBox52.Image = ImageList9.Images(block(1))
            PictureBox51.Image = ImageList9.Images(block(1))
            PictureBox50.Image = ImageList9.Images(block(1))
            PictureBox91.Image = ImageList9.Images(block(1))
            PictureBox90.Image = ImageList9.Images(block(1))
            PictureBox88.Image = ImageList9.Images(block(1))
            PictureBox87.Image = ImageList9.Images(block(1))
            PictureBox86.Image = ImageList9.Images(block(1))
            PictureBox85.Image = ImageList9.Images(block(1))
            PictureBox84.Image = ImageList9.Images(block(1))
            PictureBox82.Image = ImageList9.Images(block(1))
            PictureBox81.Image = ImageList9.Images(block(1))
            PictureBox147.Image = ImageList9.Images(block(1))
            PictureBox146.Image = ImageList9.Images(block(1))
            PictureBox145.Image = ImageList9.Images(block(1))
            PictureBox119.Image = ImageList9.Images(block(1))
            PictureBox118.Image = ImageList9.Images(block(1))
            PictureBox117.Image = ImageList9.Images(block(1))
            PictureBox181.Image = ImageList9.Images(block(1))
            PictureBox180.Image = ImageList9.Images(block(1))
            PictureBox179.Image = ImageList9.Images(block(1))
            PictureBox178.Image = ImageList9.Images(block(1))
            PictureBox172.Image = ImageList9.Images(block(1))
            PictureBox171.Image = ImageList9.Images(block(1))
            PictureBox170.Image = ImageList9.Images(block(1))
            PictureBox169.Image = ImageList9.Images(block(1))
            PictureBox214.Image = ImageList9.Images(block(1))
            PictureBox213.Image = ImageList9.Images(block(1))
            PictureBox212.Image = ImageList9.Images(block(1))
            PictureBox211.Image = ImageList9.Images(block(1))
            PictureBox205.Image = ImageList9.Images(block(1))
            PictureBox204.Image = ImageList9.Images(block(1))
            PictureBox203.Image = ImageList9.Images(block(1))
            PictureBox202.Image = ImageList9.Images(block(1))
            PictureBox246.Image = ImageList9.Images(block(1))
            PictureBox245.Image = ImageList9.Images(block(1))
            PictureBox244.Image = ImageList9.Images(block(1))
            PictureBox238.Image = ImageList9.Images(block(1))
            PictureBox237.Image = ImageList9.Images(block(1))
            PictureBox236.Image = ImageList9.Images(block(1))
            PictureBox279.Image = ImageList9.Images(block(1))
            PictureBox278.Image = ImageList9.Images(block(1))
            PictureBox276.Image = ImageList9.Images(block(1))
            PictureBox275.Image = ImageList9.Images(block(1))
            PictureBox274.Image = ImageList9.Images(block(1))
            PictureBox273.Image = ImageList9.Images(block(1))
            PictureBox272.Image = ImageList9.Images(block(1))
            PictureBox270.Image = ImageList9.Images(block(1))
            PictureBox269.Image = ImageList9.Images(block(1))
            PictureBox310.Image = ImageList9.Images(block(1))
            PictureBox309.Image = ImageList9.Images(block(1))
            PictureBox308.Image = ImageList9.Images(block(1))
            PictureBox307.Image = ImageList9.Images(block(1))
            PictureBox306.Image = ImageList9.Images(block(1))
            PictureBox305.Image = ImageList9.Images(block(1))
            PictureBox304.Image = ImageList9.Images(block(1))
            PictureBox343.Image = ImageList9.Images(block(1))
            PictureBox342.Image = ImageList9.Images(block(1))
            PictureBox341.Image = ImageList9.Images(block(1))
            PictureBox340.Image = ImageList9.Images(block(1))
            PictureBox339.Image = ImageList9.Images(block(1))
            PictureBox338.Image = ImageList9.Images(block(1))
            PictureBox337.Image = ImageList9.Images(block(1))
            PictureBox374.Image = ImageList9.Images(block(1))
            PictureBox373.Image = ImageList9.Images(block(1))
            PictureBox372.Image = ImageList9.Images(block(1))
            PictureBox3.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(2))
            PictureBox9.Image = ImageList9.Images(block(2))
            PictureBox11.Image = ImageList9.Images(block(2))
            PictureBox59.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox47.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox176.Image = ImageList9.Images(block(2))
            PictureBox175.Image = ImageList9.Images(block(2))
            PictureBox174.Image = ImageList9.Images(block(2))
            PictureBox209.Image = ImageList9.Images(block(2))
            PictureBox208.Image = ImageList9.Images(block(2))
            PictureBox207.Image = ImageList9.Images(block(2))
            PictureBox247.Image = ImageList9.Images(block(2))
            PictureBox235.Image = ImageList9.Images(block(2))
            PictureBox313.Image = ImageList9.Images(block(2))
            PictureBox311.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox301.Image = ImageList9.Images(block(2))
            PictureBox377.Image = ImageList9.Images(block(2))
            PictureBox375.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox369.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(3))
            PictureBox24.Image = ImageList9.Images(block(3))
            PictureBox16.Image = ImageList9.Images(block(3))
            PictureBox15.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox48.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox83.Image = ImageList9.Images(block(3))
            PictureBox1.Image = ImageList9.Images(block(3))
            PictureBox123.Image = ImageList9.Images(block(3))
            PictureBox122.Image = ImageList9.Images(block(3))
            PictureBox121.Image = ImageList9.Images(block(3))
            PictureBox120.Image = ImageList9.Images(block(3))
            PictureBox177.Image = ImageList9.Images(block(3))
            PictureBox173.Image = ImageList9.Images(block(3))
            PictureBox210.Image = ImageList9.Images(block(3))
            PictureBox206.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox242.Image = ImageList9.Images(block(3))
            PictureBox241.Image = ImageList9.Images(block(3))
            PictureBox240.Image = ImageList9.Images(block(3))
            PictureBox239.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox271.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(block(3))
            PictureBox302.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
            PictureBox336.Image = ImageList9.Images(block(3))
        ElseIf map = 5 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox5.Visible = True
            PictureBox6.Visible = True
            PictureBox7.Visible = True
            PictureBox8.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox22.Visible = True
            PictureBox21.Visible = True
            PictureBox20.Visible = True
            PictureBox19.Visible = True
            PictureBox18.Visible = True
            PictureBox59.Visible = True
            PictureBox57.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox52.Visible = True
            PictureBox51.Visible = True
            PictureBox90.Visible = True
            PictureBox88.Visible = True
            PictureBox87.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox81.Visible = True
            PictureBox80.Visible = True
            PictureBox146.Visible = True
            PictureBox145.Visible = True
            PictureBox1.Visible = True
            PictureBox123.Visible = True
            PictureBox122.Visible = True
            PictureBox121.Visible = True
            PictureBox120.Visible = True
            PictureBox119.Visible = True
            PictureBox117.Visible = True
            PictureBox116.Visible = True
            PictureBox181.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox202.Visible = True
            PictureBox247.Visible = True
            PictureBox246.Visible = True
            PictureBox244.Visible = True
            PictureBox243.Visible = True
            PictureBox242.Visible = True
            PictureBox241.Visible = True
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox235.Visible = True
            PictureBox280.Visible = True
            PictureBox279.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox274.Visible = True
            PictureBox273.Visible = True
            PictureBox272.Visible = True
            PictureBox270.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox303.Visible = True
            PictureBox301.Visible = True
            PictureBox342.Visible = True
            PictureBox341.Visible = True
            PictureBox340.Visible = True
            PictureBox339.Visible = True
            PictureBox338.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox373.Visible = True
            PictureBox372.Visible = True
            PictureBox371.Visible = True
            PictureBox370.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox21.Image = ImageList9.Images(block(1))
            PictureBox20.Image = ImageList9.Images(block(1))
            PictureBox19.Image = ImageList9.Images(block(1))
            PictureBox59.Image = ImageList9.Images(block(1))
            PictureBox54.Image = ImageList9.Images(block(1))
            PictureBox53.Image = ImageList9.Images(block(1))
            PictureBox52.Image = ImageList9.Images(block(1))
            PictureBox181.Image = ImageList9.Images(block(1))
            PictureBox170.Image = ImageList9.Images(block(1))
            PictureBox308.Image = ImageList9.Images(block(1))
            PictureBox307.Image = ImageList9.Images(block(1))
            PictureBox306.Image = ImageList9.Images(block(1))
            PictureBox301.Image = ImageList9.Images(block(1))
            PictureBox341.Image = ImageList9.Images(block(1))
            PictureBox340.Image = ImageList9.Images(block(1))
            PictureBox339.Image = ImageList9.Images(block(1))
            PictureBox104.Image = ImageList9.Images(block(2))
            PictureBox2.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(2))
            PictureBox6.Image = ImageList9.Images(block(2))
            PictureBox7.Image = ImageList9.Images(block(2))
            PictureBox8.Image = ImageList9.Images(block(2))
            PictureBox9.Image = ImageList9.Images(block(2))
            PictureBox26.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(2))
            PictureBox22.Image = ImageList9.Images(block(2))
            PictureBox18.Image = ImageList9.Images(block(2))
            PictureBox55.Image = ImageList9.Images(block(2))
            PictureBox51.Image = ImageList9.Images(block(2))
            PictureBox88.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox86.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox84.Image = ImageList9.Images(block(2))
            PictureBox175.Image = ImageList9.Images(block(2))
            PictureBox174.Image = ImageList9.Images(block(2))
            PictureBox208.Image = ImageList9.Images(block(2))
            PictureBox207.Image = ImageList9.Images(block(2))
            PictureBox276.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox274.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox272.Image = ImageList9.Images(block(2))
            PictureBox309.Image = ImageList9.Images(block(2))
            PictureBox305.Image = ImageList9.Images(block(2))
            PictureBox342.Image = ImageList9.Images(block(2))
            PictureBox338.Image = ImageList9.Images(block(2))
            PictureBox335.Image = ImageList9.Images(block(2))
            PictureBox334.Image = ImageList9.Images(block(2))
            PictureBox375.Image = ImageList9.Images(block(2))
            PictureBox374.Image = ImageList9.Images(block(2))
            PictureBox373.Image = ImageList9.Images(block(2))
            PictureBox372.Image = ImageList9.Images(block(2))
            PictureBox371.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox367.Image = ImageList9.Images(block(2))
            PictureBox370.Image = ImageList9.Images(block(2))
            PictureBox10.Image = ImageList9.Images(block(3))
            PictureBox57.Image = ImageList9.Images(block(3))
            PictureBox90.Image = ImageList9.Images(block(3))
            PictureBox83.Image = ImageList9.Images(block(3))
            PictureBox81.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox145.Image = ImageList9.Images(block(3))
            PictureBox1.Image = ImageList9.Images(block(3))
            PictureBox123.Image = ImageList9.Images(block(3))
            PictureBox122.Image = ImageList9.Images(block(3))
            PictureBox121.Image = ImageList9.Images(block(3))
            PictureBox120.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox169.Image = ImageList9.Images(block(3))
            PictureBox214.Image = ImageList9.Images(block(3))
            PictureBox202.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(block(3))
            PictureBox244.Image = ImageList9.Images(block(3))
            PictureBox243.Image = ImageList9.Images(block(3))
            PictureBox242.Image = ImageList9.Images(block(3))
            PictureBox241.Image = ImageList9.Images(block(3))
            PictureBox240.Image = ImageList9.Images(block(3))
            PictureBox239.Image = ImageList9.Images(block(3))
            PictureBox238.Image = ImageList9.Images(block(3))
            PictureBox237.Image = ImageList9.Images(block(3))
            PictureBox235.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox270.Image = ImageList9.Images(block(3))
            PictureBox303.Image = ImageList9.Images(block(3))
            PictureBox376.Image = ImageList9.Images(block(3))
        ElseIf map = 6 Then
            PictureBox104.Visible = True
            PictureBox2.Visible = True
            PictureBox26.Visible = True
            PictureBox25.Visible = True
            PictureBox17.Visible = True
            PictureBox57.Visible = True
            PictureBox56.Visible = True
            PictureBox55.Visible = True
            PictureBox54.Visible = True
            PictureBox53.Visible = True
            PictureBox51.Visible = True
            PictureBox50.Visible = True
            PictureBox47.Visible = True
            PictureBox90.Visible = True
            PictureBox89.Visible = True
            PictureBox88.Visible = True
            PictureBox87.Visible = True
            PictureBox86.Visible = True
            PictureBox85.Visible = True
            PictureBox84.Visible = True
            PictureBox83.Visible = True
            PictureBox80.Visible = True
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
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox176.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox171.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox210.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True
            PictureBox205.Visible = True
            PictureBox204.Visible = True
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
            PictureBox280.Visible = True
            PictureBox277.Visible = True
            PictureBox276.Visible = True
            PictureBox275.Visible = True
            PictureBox274.Visible = True
            PictureBox273.Visible = True
            PictureBox272.Visible = True
            PictureBox271.Visible = True
            PictureBox270.Visible = True
            PictureBox313.Visible = True
            PictureBox310.Visible = True
            PictureBox309.Visible = True
            PictureBox308.Visible = True
            PictureBox307.Visible = True
            PictureBox306.Visible = True
            PictureBox305.Visible = True
            PictureBox304.Visible = True
            PictureBox303.Visible = True
            PictureBox343.Visible = True
            PictureBox335.Visible = True
            PictureBox334.Visible = True
            PictureBox376.Visible = True
            PictureBox368.Visible = True
            PictureBox367.Visible = True




            PictureBox104.Image = ImageList9.Images(block(1))
            PictureBox2.Image = ImageList9.Images(block(1))
            PictureBox26.Image = ImageList9.Images(block(1))
            PictureBox25.Image = ImageList9.Images(block(1))
            PictureBox53.Image = ImageList9.Images(block(1))
            PictureBox1.Image = ImageList9.Images(block(1))
            PictureBox123.Image = ImageList9.Images(block(1))
            PictureBox122.Image = ImageList9.Images(block(1))
            PictureBox121.Image = ImageList9.Images(block(1))
            PictureBox120.Image = ImageList9.Images(block(1))
            PictureBox178.Image = ImageList9.Images(block(1))
            PictureBox177.Image = ImageList9.Images(block(1))
            PictureBox176.Image = ImageList9.Images(block(1))
            PictureBox175.Image = ImageList9.Images(block(1))
            PictureBox174.Image = ImageList9.Images(block(1))
            PictureBox173.Image = ImageList9.Images(block(1))
            PictureBox211.Image = ImageList9.Images(block(1))
            PictureBox210.Image = ImageList9.Images(block(1))
            PictureBox209.Image = ImageList9.Images(block(1))
            PictureBox208.Image = ImageList9.Images(block(1))
            PictureBox207.Image = ImageList9.Images(block(1))
            PictureBox206.Image = ImageList9.Images(block(1))
            PictureBox205.Image = ImageList9.Images(block(1))
            PictureBox243.Image = ImageList9.Images(block(1))
            PictureBox242.Image = ImageList9.Images(block(1))
            PictureBox241.Image = ImageList9.Images(block(1))
            PictureBox240.Image = ImageList9.Images(block(1))
            PictureBox239.Image = ImageList9.Images(block(1))
            PictureBox307.Image = ImageList9.Images(block(1))
            PictureBox335.Image = ImageList9.Images(block(1))
            PictureBox334.Image = ImageList9.Images(block(1))
            PictureBox368.Image = ImageList9.Images(block(1))
            PictureBox367.Image = ImageList9.Images(block(1))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox54.Image = ImageList9.Images(block(2))
            PictureBox89.Image = ImageList9.Images(block(2))
            PictureBox88.Image = ImageList9.Images(block(2))
            PictureBox87.Image = ImageList9.Images(block(2))
            PictureBox86.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox84.Image = ImageList9.Images(block(2))
            PictureBox83.Image = ImageList9.Images(block(2))
            PictureBox145.Image = ImageList9.Images(block(2))
            PictureBox119.Image = ImageList9.Images(block(2))
            PictureBox244.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox277.Image = ImageList9.Images(block(2))
            PictureBox276.Image = ImageList9.Images(block(2))
            PictureBox275.Image = ImageList9.Images(block(2))
            PictureBox274.Image = ImageList9.Images(block(2))
            PictureBox273.Image = ImageList9.Images(block(2))
            PictureBox272.Image = ImageList9.Images(block(2))
            PictureBox271.Image = ImageList9.Images(block(2))
            PictureBox308.Image = ImageList9.Images(block(2))
            PictureBox306.Image = ImageList9.Images(block(2))
            PictureBox303.Image = ImageList9.Images(block(2))
            PictureBox343.Image = ImageList9.Images(block(2))
            PictureBox376.Image = ImageList9.Images(block(2))
            PictureBox17.Image = ImageList9.Images(block(3))
            PictureBox56.Image = ImageList9.Images(block(3))
            PictureBox55.Image = ImageList9.Images(block(3))
            PictureBox51.Image = ImageList9.Images(block(3))
            PictureBox50.Image = ImageList9.Images(block(3))
            PictureBox47.Image = ImageList9.Images(block(3))
            PictureBox90.Image = ImageList9.Images(block(3))
            PictureBox80.Image = ImageList9.Images(block(3))
            PictureBox146.Image = ImageList9.Images(block(3))
            PictureBox118.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox116.Image = ImageList9.Images(block(3))
            PictureBox179.Image = ImageList9.Images(block(3))
            PictureBox171.Image = ImageList9.Images(block(3))
            PictureBox212.Image = ImageList9.Images(block(3))
            PictureBox204.Image = ImageList9.Images(block(3))
            PictureBox247.Image = ImageList9.Images(block(3))
            PictureBox246.Image = ImageList9.Images(block(3))
            PictureBox245.Image = ImageList9.Images(block(3))
            PictureBox237.Image = ImageList9.Images(block(3))
            PictureBox280.Image = ImageList9.Images(block(3))
            PictureBox270.Image = ImageList9.Images(block(3))
            PictureBox313.Image = ImageList9.Images(block(3))
            PictureBox310.Image = ImageList9.Images(block(3))
            PictureBox309.Image = ImageList9.Images(block(3))
            PictureBox305.Image = ImageList9.Images(block(3))
            PictureBox304.Image = ImageList9.Images(block(3))
        ElseIf map = 7 Then
            PictureBox2.Visible = True
            PictureBox3.Visible = True
            PictureBox4.Visible = True
            PictureBox5.Visible = True
            PictureBox9.Visible = True
            PictureBox10.Visible = True
            PictureBox11.Visible = True
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
            PictureBox181.Visible = True
            PictureBox180.Visible = True
            PictureBox179.Visible = True
            PictureBox178.Visible = True
            PictureBox177.Visible = True
            PictureBox176.Visible = True
            PictureBox175.Visible = True
            PictureBox174.Visible = True
            PictureBox173.Visible = True
            PictureBox172.Visible = True
            PictureBox171.Visible = True
            PictureBox170.Visible = True
            PictureBox169.Visible = True
            PictureBox214.Visible = True
            PictureBox213.Visible = True
            PictureBox212.Visible = True
            PictureBox211.Visible = True
            PictureBox210.Visible = True
            PictureBox209.Visible = True
            PictureBox208.Visible = True
            PictureBox207.Visible = True
            PictureBox206.Visible = True
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
            PictureBox240.Visible = True
            PictureBox239.Visible = True
            PictureBox238.Visible = True
            PictureBox237.Visible = True
            PictureBox236.Visible = True
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
            PictureBox376.Visible = True
            PictureBox375.Visible = True
            PictureBox374.Visible = True
            PictureBox368.Visible = True




            PictureBox4.Image = ImageList9.Images(block(1))
            PictureBox10.Image = ImageList9.Images(block(1))
            PictureBox56.Image = ImageList9.Images(block(1))
            PictureBox54.Image = ImageList9.Images(block(1))
            PictureBox53.Image = ImageList9.Images(block(1))
            PictureBox52.Image = ImageList9.Images(block(1))
            PictureBox50.Image = ImageList9.Images(block(1))
            PictureBox90.Image = ImageList9.Images(block(1))
            PictureBox82.Image = ImageList9.Images(block(1))
            PictureBox1.Image = ImageList9.Images(block(1))
            PictureBox122.Image = ImageList9.Images(block(1))
            PictureBox121.Image = ImageList9.Images(block(1))
            PictureBox120.Image = ImageList9.Images(block(1))
            PictureBox181.Image = ImageList9.Images(block(1))
            PictureBox179.Image = ImageList9.Images(block(1))
            PictureBox177.Image = ImageList9.Images(block(1))
            PictureBox175.Image = ImageList9.Images(block(1))
            PictureBox174.Image = ImageList9.Images(block(1))
            PictureBox173.Image = ImageList9.Images(block(1))
            PictureBox171.Image = ImageList9.Images(block(1))
            PictureBox169.Image = ImageList9.Images(block(1))
            PictureBox247.Image = ImageList9.Images(block(1))
            PictureBox245.Image = ImageList9.Images(block(1))
            PictureBox243.Image = ImageList9.Images(block(1))
            PictureBox241.Image = ImageList9.Images(block(1))
            PictureBox240.Image = ImageList9.Images(block(1))
            PictureBox239.Image = ImageList9.Images(block(1))
            PictureBox237.Image = ImageList9.Images(block(1))
            PictureBox278.Image = ImageList9.Images(block(1))
            PictureBox311.Image = ImageList9.Images(block(1))
            PictureBox309.Image = ImageList9.Images(block(1))
            PictureBox307.Image = ImageList9.Images(block(1))
            PictureBox306.Image = ImageList9.Images(block(1))
            PictureBox305.Image = ImageList9.Images(block(1))
            PictureBox304.Image = ImageList9.Images(block(1))
            PictureBox303.Image = ImageList9.Images(block(1))
            PictureBox375.Image = ImageList9.Images(block(1))
            PictureBox2.Image = ImageList9.Images(block(2))
            PictureBox3.Image = ImageList9.Images(block(2))
            PictureBox5.Image = ImageList9.Images(block(2))
            PictureBox9.Image = ImageList9.Images(block(2))
            PictureBox11.Image = ImageList9.Images(block(2))
            PictureBox26.Image = ImageList9.Images(block(2))
            PictureBox23.Image = ImageList9.Images(block(2))
            PictureBox17.Image = ImageList9.Images(block(2))
            PictureBox57.Image = ImageList9.Images(block(2))
            PictureBox55.Image = ImageList9.Images(block(2))
            PictureBox51.Image = ImageList9.Images(block(2))
            PictureBox49.Image = ImageList9.Images(block(2))
            PictureBox85.Image = ImageList9.Images(block(2))
            PictureBox148.Image = ImageList9.Images(block(2))
            PictureBox146.Image = ImageList9.Images(block(2))
            PictureBox123.Image = ImageList9.Images(block(2))
            PictureBox118.Image = ImageList9.Images(block(2))
            PictureBox116.Image = ImageList9.Images(block(2))
            PictureBox180.Image = ImageList9.Images(block(2))
            PictureBox178.Image = ImageList9.Images(block(2))
            PictureBox170.Image = ImageList9.Images(block(2))
            PictureBox214.Image = ImageList9.Images(block(2))
            PictureBox210.Image = ImageList9.Images(block(2))
            PictureBox206.Image = ImageList9.Images(block(2))
            PictureBox204.Image = ImageList9.Images(block(2))
            PictureBox202.Image = ImageList9.Images(block(2))
            PictureBox246.Image = ImageList9.Images(block(2))
            PictureBox242.Image = ImageList9.Images(block(2))
            PictureBox238.Image = ImageList9.Images(block(2))
            PictureBox280.Image = ImageList9.Images(block(2))
            PictureBox276.Image = ImageList9.Images(block(2))
            PictureBox270.Image = ImageList9.Images(block(2))
            PictureBox310.Image = ImageList9.Images(block(2))
            PictureBox308.Image = ImageList9.Images(block(2))
            PictureBox342.Image = ImageList9.Images(block(2))
            PictureBox336.Image = ImageList9.Images(block(2))
            PictureBox376.Image = ImageList9.Images(block(2))
            PictureBox374.Image = ImageList9.Images(block(2))
            PictureBox368.Image = ImageList9.Images(block(2))
            PictureBox25.Image = ImageList9.Images(block(3))
            PictureBox24.Image = ImageList9.Images(block(3))
            PictureBox22.Image = ImageList9.Images(block(3))
            PictureBox21.Image = ImageList9.Images(block(3))
            PictureBox20.Image = ImageList9.Images(block(3))
            PictureBox19.Image = ImageList9.Images(block(3))
            PictureBox18.Image = ImageList9.Images(block(3))
            PictureBox16.Image = ImageList9.Images(block(3))
            PictureBox15.Image = ImageList9.Images(block(3))
            PictureBox58.Image = ImageList9.Images(block(3))
            PictureBox48.Image = ImageList9.Images(block(3))
            PictureBox91.Image = ImageList9.Images(block(3))
            PictureBox89.Image = ImageList9.Images(block(3))
            PictureBox88.Image = ImageList9.Images(block(3))
            PictureBox87.Image = ImageList9.Images(block(3))
            PictureBox86.Image = ImageList9.Images(block(3))
            PictureBox84.Image = ImageList9.Images(block(3))
            PictureBox83.Image = ImageList9.Images(block(3))
            PictureBox81.Image = ImageList9.Images(block(3))
            PictureBox147.Image = ImageList9.Images(block(3))
            PictureBox145.Image = ImageList9.Images(block(3))
            PictureBox119.Image = ImageList9.Images(block(3))
            PictureBox117.Image = ImageList9.Images(block(3))
            PictureBox176.Image = ImageList9.Images(block(3))
            PictureBox172.Image = ImageList9.Images(block(3))
            PictureBox213.Image = ImageList9.Images(block(3))
            PictureBox212.Image = ImageList9.Images(block(3))
            PictureBox211.Image = ImageList9.Images(block(3))
            PictureBox209.Image = ImageList9.Images(block(3))
            PictureBox208.Image = ImageList9.Images(block(3))
            PictureBox207.Image = ImageList9.Images(block(3))
            PictureBox205.Image = ImageList9.Images(block(3))
            PictureBox203.Image = ImageList9.Images(block(3))
            PictureBox244.Image = ImageList9.Images(block(3))
            PictureBox236.Image = ImageList9.Images(block(3))
            PictureBox279.Image = ImageList9.Images(block(3))
            PictureBox277.Image = ImageList9.Images(block(3))
            PictureBox275.Image = ImageList9.Images(block(3))
            PictureBox274.Image = ImageList9.Images(block(3))
            PictureBox273.Image = ImageList9.Images(block(3))
            PictureBox272.Image = ImageList9.Images(block(3))
            PictureBox271.Image = ImageList9.Images(block(3))
            PictureBox269.Image = ImageList9.Images(block(3))
            PictureBox312.Image = ImageList9.Images(block(3))
            PictureBox302.Image = ImageList9.Images(block(3))
            PictureBox345.Image = ImageList9.Images(block(3))
            PictureBox344.Image = ImageList9.Images(block(3))
            PictureBox343.Image = ImageList9.Images(block(3))
            PictureBox341.Image = ImageList9.Images(block(3))
            PictureBox340.Image = ImageList9.Images(block(3))
            PictureBox339.Image = ImageList9.Images(block(3))
            PictureBox338.Image = ImageList9.Images(block(3))
            PictureBox337.Image = ImageList9.Images(block(3))
            PictureBox335.Image = ImageList9.Images(block(3))
        End If
    End Sub
End Class

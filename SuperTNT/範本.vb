Public Class 範本 '名字自己改
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

    '------------------------------------------1P-控制--------------------------------------------------------------------------------
    '1P角色控制(上)(function)
    Private Sub Character1_up()
        If ch1 = 0 Then
            PictureBox104.Image = ImageList1.Images(0)
        ElseIf ch1 = 1 Then
            PictureBox104.Image = ImageList3.Images(0)
        ElseIf ch1 = 2 Then
            PictureBox104.Image = ImageList4.Images(0)
        ElseIf ch1 = 3 Then
            PictureBox104.Image = ImageList5.Images(1)
        ElseIf ch1 = 4 Then
            PictureBox104.Image = ImageList6.Images(0)
        ElseIf ch1 = 5 Then
            PictureBox104.Image = ImageList7.Images(0)
        ElseIf ch1 = 6 Then
            PictureBox104.Image = ImageList8.Images(0)
        End If
        PictureBox104.Top -= PictureBox144.Height
        y1 -= 1
        udlr1 = 0
        un1 = 1
        If PictureBox104.Top < 0 Then
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
            PictureBox104.Image = ImageList1.Images(1)
        ElseIf ch1 = 1 Then
            PictureBox104.Image = ImageList3.Images(1)
        ElseIf ch1 = 2 Then
            PictureBox104.Image = ImageList4.Images(1)
        ElseIf ch1 = 3 Then
            PictureBox104.Image = ImageList5.Images(0)
        ElseIf ch1 = 4 Then
            PictureBox104.Image = ImageList6.Images(1)
        ElseIf ch1 = 5 Then
            PictureBox104.Image = ImageList7.Images(1)
        ElseIf ch1 = 6 Then
            PictureBox104.Image = ImageList8.Images(1)
        End If
        PictureBox104.Top += PictureBox144.Height
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
            PictureBox104.Image = ImageList1.Images(2)
        ElseIf ch1 = 1 Then
            PictureBox104.Image = ImageList3.Images(2)
        ElseIf ch1 = 2 Then
            PictureBox104.Image = ImageList4.Images(2)
        ElseIf ch1 = 3 Then
            PictureBox104.Image = ImageList5.Images(3)
        ElseIf ch1 = 4 Then
            PictureBox104.Image = ImageList6.Images(2)
        ElseIf ch1 = 5 Then
            PictureBox104.Image = ImageList7.Images(2)
        ElseIf ch1 = 6 Then
            PictureBox104.Image = ImageList8.Images(2)
        End If
        PictureBox104.Left -= PictureBox144.Width
        x1 -= 1
        udlr1 = 1
        un1 = 1
        If PictureBox104.Left < 0 Then
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
            PictureBox104.Image = ImageList1.Images(3)
        ElseIf ch1 = 1 Then
            PictureBox104.Image = ImageList3.Images(3)
        ElseIf ch1 = 2 Then
            PictureBox104.Image = ImageList4.Images(3)
        ElseIf ch1 = 3 Then
            PictureBox104.Image = ImageList5.Images(2)
        ElseIf ch1 = 4 Then
            PictureBox104.Image = ImageList6.Images(3)
        ElseIf ch1 = 5 Then
            PictureBox104.Image = ImageList7.Images(3)
        ElseIf ch1 = 6 Then
            PictureBox104.Image = ImageList8.Images(3)
        End If
        PictureBox104.Left += PictureBox144.Width
        x1 += 1
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
            PictureBox129.Image = ImageList1.Images(0)
        ElseIf ch2 = 1 Then
            PictureBox129.Image = ImageList3.Images(0)
        ElseIf ch2 = 2 Then
            PictureBox129.Image = ImageList4.Images(0)
        ElseIf ch2 = 3 Then
            PictureBox129.Image = ImageList5.Images(1)
        ElseIf ch2 = 4 Then
            PictureBox129.Image = ImageList6.Images(0)
        ElseIf ch2 = 5 Then
            PictureBox129.Image = ImageList7.Images(0)
        ElseIf ch2 = 6 Then
            PictureBox129.Image = ImageList8.Images(0)
        End If
        PictureBox129.Top -= PictureBox144.Height
        y2 -= 1
        udlr2 = 0
        un2 = 1
        If PictureBox129.Top < 0 Then
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
            PictureBox129.Image = ImageList1.Images(1)
        ElseIf ch2 = 1 Then
            PictureBox129.Image = ImageList3.Images(1)
        ElseIf ch2 = 2 Then
            PictureBox129.Image = ImageList4.Images(1)
        ElseIf ch2 = 3 Then
            PictureBox129.Image = ImageList5.Images(0)
        ElseIf ch2 = 4 Then
            PictureBox129.Image = ImageList6.Images(1)
        ElseIf ch2 = 5 Then
            PictureBox129.Image = ImageList7.Images(1)
        ElseIf ch2 = 6 Then
            PictureBox129.Image = ImageList8.Images(1)
        End If
        PictureBox129.Top += PictureBox144.Height
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
            PictureBox129.Image = ImageList1.Images(2)
        ElseIf ch2 = 1 Then
            PictureBox129.Image = ImageList3.Images(2)
        ElseIf ch2 = 2 Then
            PictureBox129.Image = ImageList4.Images(2)
        ElseIf ch2 = 3 Then
            PictureBox129.Image = ImageList5.Images(3)
        ElseIf ch2 = 4 Then
            PictureBox129.Image = ImageList6.Images(2)
        ElseIf ch2 = 5 Then
            PictureBox129.Image = ImageList7.Images(2)
        ElseIf ch2 = 6 Then
            PictureBox129.Image = ImageList8.Images(2)
        End If
        PictureBox129.Left -= PictureBox144.Width
        x2 -= 1
        udlr2 = 1
        un2 = 1
        If PictureBox129.Left < 0 Then
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
            PictureBox129.Image = ImageList1.Images(3)
        ElseIf ch2 = 1 Then
            PictureBox129.Image = ImageList3.Images(3)
        ElseIf ch2 = 2 Then
            PictureBox129.Image = ImageList4.Images(3)
        ElseIf ch2 = 3 Then
            PictureBox129.Image = ImageList5.Images(2)
        ElseIf ch2 = 4 Then
            PictureBox129.Image = ImageList6.Images(3)
        ElseIf ch2 = 5 Then
            PictureBox129.Image = ImageList7.Images(3)
        ElseIf ch2 = 6 Then
            PictureBox129.Image = ImageList8.Images(3)
        End If
        PictureBox129.Left += PictureBox144.Width
        x2 += 1
        udlr2 = 1
        un2 = -1
        Call detection2()
        If exer2 > 0 Then
            exer2 -= 1
        End If
    End Sub
    '---------------------------------------------------------------------------------------------------------------------------------



    '1P爆炸特效(function)
    Private Sub ex1()
        Dim i As Integer
        Randomize()
        i = Int((3 - 0 + 1) * Rnd() + 0)
        If i = 0 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode1.wav", AudioPlayMode.Background)
        ElseIf i = 1 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode2.wav", AudioPlayMode.Background)
        ElseIf i = 2 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode3.wav", AudioPlayMode.Background)
        ElseIf i = 3 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode4.wav", AudioPlayMode.Background)
        End If

        PictureBox125.Visible = True
        PictureBox126.Visible = True
        PictureBox127.Visible = True
        PictureBox128.Visible = True
        PictureBox141.Visible = True
        PictureBox142.Visible = True
        PictureBox143.Visible = True
        PictureBox144.Visible = True
        If tnt1_tick1 = 5 Then
            PictureBox125.Left = PictureBox105.Left + PictureBox144.Width
            PictureBox126.Left = PictureBox105.Left - PictureBox144.Width
            PictureBox127.Left = PictureBox105.Left
            PictureBox128.Left = PictureBox105.Left
            PictureBox125.Top = PictureBox105.Top
            PictureBox126.Top = PictureBox105.Top
            PictureBox127.Top = PictureBox105.Top + PictureBox144.Height
            PictureBox128.Top = PictureBox105.Top - PictureBox144.Height

            PictureBox141.Left = PictureBox105.Left + PictureBox144.Width * 2
            PictureBox142.Left = PictureBox105.Left - PictureBox144.Width * 2
            PictureBox143.Left = PictureBox105.Left
            PictureBox144.Left = PictureBox105.Left
            PictureBox141.Top = PictureBox105.Top
            PictureBox142.Top = PictureBox105.Top
            PictureBox143.Top = PictureBox105.Top + PictureBox144.Height * 2
            PictureBox144.Top = PictureBox105.Top - PictureBox144.Height * 2
        ElseIf tnt1_tick2 = 5 Then
            PictureBox125.Left = PictureBox106.Left + PictureBox144.Width
            PictureBox126.Left = PictureBox106.Left - PictureBox144.Width
            PictureBox127.Left = PictureBox106.Left
            PictureBox128.Left = PictureBox106.Left
            PictureBox125.Top = PictureBox106.Top
            PictureBox126.Top = PictureBox106.Top
            PictureBox127.Top = PictureBox106.Top + PictureBox144.Height
            PictureBox128.Top = PictureBox106.Top - PictureBox144.Height

            PictureBox141.Left = PictureBox106.Left + PictureBox144.Width * 2
            PictureBox142.Left = PictureBox106.Left - PictureBox144.Width * 2
            PictureBox143.Left = PictureBox106.Left
            PictureBox144.Left = PictureBox106.Left
            PictureBox141.Top = PictureBox106.Top
            PictureBox142.Top = PictureBox106.Top
            PictureBox143.Top = PictureBox106.Top + PictureBox144.Height * 2
            PictureBox144.Top = PictureBox106.Top - PictureBox144.Height * 2
        End If
    End Sub
    '2P爆炸特效(function)
    Private Sub ex2()
        Dim i As Integer
        Randomize()
        i = Int((3 - 0 + 1) * Rnd() + 0)
        If i = 0 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode1.wav", AudioPlayMode.Background)
        ElseIf i = 1 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode2.wav", AudioPlayMode.Background)
        ElseIf i = 2 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode3.wav", AudioPlayMode.Background)
        ElseIf i = 3 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode4.wav", AudioPlayMode.Background)
        End If
        '137 ~
        PictureBox132.Visible = True
        PictureBox133.Visible = True
        PictureBox134.Visible = True
        PictureBox135.Visible = True
        PictureBox137.Visible = True
        PictureBox138.Visible = True
        PictureBox139.Visible = True
        PictureBox140.Visible = True
        If tnt2_tick1 = 5 Then
            PictureBox132.Left = PictureBox130.Left + PictureBox144.Width
            PictureBox133.Left = PictureBox130.Left - PictureBox144.Width
            PictureBox137.Left = PictureBox130.Left + PictureBox144.Width * 2
            PictureBox138.Left = PictureBox130.Left - PictureBox144.Width * 2
            PictureBox134.Left = PictureBox130.Left
            PictureBox135.Left = PictureBox130.Left
            PictureBox139.Left = PictureBox130.Left
            PictureBox140.Left = PictureBox130.Left

            PictureBox132.Top = PictureBox130.Top
            PictureBox133.Top = PictureBox130.Top
            PictureBox134.Top = PictureBox130.Top + PictureBox144.Height
            PictureBox135.Top = PictureBox130.Top - PictureBox144.Height
            PictureBox137.Top = PictureBox130.Top
            PictureBox138.Top = PictureBox130.Top
            PictureBox139.Top = PictureBox130.Top + PictureBox144.Height * 2
            PictureBox140.Top = PictureBox130.Top - PictureBox144.Height * 2
        ElseIf tnt2_tick2 = 5 Then
            PictureBox132.Left = PictureBox131.Left + PictureBox144.Width
            PictureBox133.Left = PictureBox131.Left - PictureBox144.Width
            PictureBox134.Left = PictureBox131.Left
            PictureBox135.Left = PictureBox131.Left
            PictureBox132.Top = PictureBox131.Top
            PictureBox133.Top = PictureBox131.Top
            PictureBox134.Top = PictureBox131.Top + PictureBox144.Height
            PictureBox135.Top = PictureBox131.Top - PictureBox144.Height

            PictureBox137.Left = PictureBox131.Left + PictureBox144.Width * 2
            PictureBox138.Left = PictureBox131.Left - PictureBox144.Width * 2
            PictureBox139.Left = PictureBox131.Left
            PictureBox140.Left = PictureBox131.Left
            PictureBox137.Top = PictureBox131.Top
            PictureBox138.Top = PictureBox131.Top
            PictureBox139.Top = PictureBox131.Top + PictureBox144.Height * 2
            PictureBox140.Top = PictureBox131.Top - PictureBox144.Height * 2
        End If
    End Sub

    '1P死亡判斷(function)
    Private Sub die1()
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
    End Sub
    '2P死亡判斷(function)
    Private Sub die2()
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
        End If
    End Sub

    '遊戲結束[死亡](function)
    Private Sub ending2()
        If endy = 0 Then
            If death1 >= 1 And death2 = 0 Then
                '顯示1P輸2P贏
                endy = 2
                PictureBox104.Image = ImageList3.Images(4)
                PictureBox104.Top -= 50
                Timer5.Enabled = True
                If music = 2 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\+9die.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\dead.wav", AudioPlayMode.Background)
                End If
                player.close()
            ElseIf death2 >= 1 And death1 = 0 Then
                '顯示2P輸3P贏
                endy = 2
                PictureBox129.Image = ImageList3.Images(4)
                PictureBox129.Top -= 50
                Timer5.Enabled = True
                If music = 2 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\+9die.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\dead.wav", AudioPlayMode.Background)
                End If
                player.close()
            ElseIf death2 >= 1 And death1 >= 1 Then
                '顯示1P2P輸
                endy = 2
                PictureBox104.Image = ImageList3.Images(4)
                PictureBox129.Image = ImageList3.Images(4)
                PictureBox104.Top -= 50
                PictureBox129.Top -= 50
                If music = 2 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\+9die.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\dead.wav", AudioPlayMode.Background)
                End If

                Timer5.Enabled = True
                player.close()
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
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\decision7.wav", AudioPlayMode.Background)
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
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\decision7.wav", AudioPlayMode.Background)
            endy = 0
            Timer6.Enabled = True
            PictureBox136.Visible = False
            Button1.Visible = False
            Button2.Visible = False
            Button3.Visible = False
            Label20.Visible = False
        End If
    End Sub

    '1P邊界擋玩家執行(function back)
    Private Sub back1()
        If f12 = 0 Then
            If udlr1 = 0 Then
                PictureBox104.Top = PictureBox104.Top + un1 * PictureBox144.Height
                y1 = y1 + un1
            ElseIf udlr1 = 1 Then
                PictureBox104.Left = PictureBox104.Left + un1 * PictureBox144.Width
                x1 = x1 + un1
            End If
        End If
        If exer1 > 0 Then
            exer1 += 1
        End If
    End Sub
    '2P邊界擋玩家執行(function back)
    Private Sub back2()
        If f12 = 0 Then
            If udlr2 = 0 Then
                PictureBox129.Top = PictureBox129.Top + un2 * PictureBox144.Height
                y2 = y2 + un2
            ElseIf udlr2 = 1 Then
                PictureBox129.Left = PictureBox129.Left + un2 * PictureBox144.Width
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

    '1P邊界擋玩家偵測 + 呼叫 function back (function)
    Private Sub detection1()
        If f12 = 0 Then
            If y1 = 14 Then
                Call back1()
            End If
            If x1 = 18 Then
                Call back1()
            End If
        End If
    End Sub
    '2P牆壁擋玩家偵測 + 呼叫 function back (function)
    Private Sub detection2()
        If f12 = 0 Then
            If y2 = 14 Then
                Call back2()
            End If
            If x2 = 18 Then
                Call back2()
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
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\fuse.wav", AudioPlayMode.Background)
                    tnt21 = x2 * 100 + y2
                    PictureBox130.Image = ImageList2.Images(0)
                    PictureBox130.Left = (x2 - 1) * PictureBox144.Width
                    PictureBox130.Top = (y2 - 1) * PictureBox144.Height
                    tnt2_tick1 = 0
                    exer2 = 1
                    Timer3.Enabled = True
                ElseIf tnt2_tick2 < 1 Or tnt2_tick2 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\fuse.wav", AudioPlayMode.Background)
                    tnt22 = x2 * 100 + y2
                    PictureBox131.Image = ImageList2.Images(0)
                    PictureBox131.Left = (x2 - 1) * PictureBox144.Width
                    PictureBox131.Top = (y2 - 1) * PictureBox144.Height
                    tnt2_tick2 = 0
                    exer2 = 1
                    Timer4.Enabled = True
                End If
            ElseIf e.KeyCode = 191 Then
                If tnt1_tick1 < 1 Or tnt1_tick1 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\fuse.wav", AudioPlayMode.Background)
                    tnt11 = x1 * 100 + y1
                    PictureBox105.Image = ImageList2.Images(0)
                    PictureBox105.Left = (x1 - 1) * PictureBox144.Width
                    PictureBox105.Top = (y1 - 1) * PictureBox144.Height
                    tnt1_tick1 = 0
                    exer1 = 1
                    Timer1.Enabled = True
                ElseIf tnt1_tick2 < 1 Or tnt1_tick2 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\fuse.wav", AudioPlayMode.Background)
                    tnt12 = x1 * 100 + y1
                    PictureBox106.Image = ImageList2.Images(0)
                    PictureBox106.Left = (x1 - 1) * PictureBox144.Width
                    PictureBox106.Top = (y1 - 1) * PictureBox144.Height
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
        PictureBox136.Visible = False
        Button1.Visible = False
        Button2.Visible = False
        Button3.Visible = False
        Form2.Close()
        x1 = 17
        y1 = 1
        x2 = 1
        y2 = 13
        xy1 = x1 * 100 + y1
        xy2 = x2 * 100 + y2
        f3 = 0
        f12 = 0
        PictureBox104.BringToFront()
        PictureBox129.BringToFront()
        PictureBox105.BringToFront()
        PictureBox106.BringToFront()
        PictureBox130.BringToFront()
        PictureBox131.BringToFront()

        PictureBox141.SendToBack()
        PictureBox142.SendToBack()
        PictureBox143.SendToBack()
        PictureBox144.SendToBack()
        PictureBox125.SendToBack()
        PictureBox126.SendToBack()
        PictureBox127.SendToBack()
        PictureBox128.SendToBack()

        PictureBox137.SendToBack()
        PictureBox138.SendToBack()
        PictureBox139.SendToBack()
        PictureBox140.SendToBack()
        PictureBox132.SendToBack()
        PictureBox133.SendToBack()
        PictureBox134.SendToBack()
        PictureBox135.SendToBack()
        '1P picture load
        If ch1 = 0 Then
            PictureBox104.Image = ImageList1.Images(0)
        ElseIf ch1 = 1 Then
            PictureBox104.Image = ImageList3.Images(0)
        ElseIf ch1 = 2 Then
            PictureBox104.Image = ImageList4.Images(0)
        ElseIf ch1 = 3 Then
            PictureBox104.Image = ImageList5.Images(0)
        ElseIf ch1 = 4 Then
            PictureBox104.Image = ImageList6.Images(0)
        ElseIf ch1 = 5 Then
            PictureBox104.Image = ImageList7.Images(0)
        ElseIf ch1 = 6 Then
            PictureBox104.Image = ImageList8.Images(0)
        End If
        '2P picture load
        If ch2 = 0 Then
            PictureBox129.Image = ImageList1.Images(0)
        ElseIf ch2 = 1 Then
            PictureBox129.Image = ImageList3.Images(0)
        ElseIf ch2 = 2 Then
            PictureBox129.Image = ImageList4.Images(0)
        ElseIf ch2 = 3 Then
            PictureBox129.Image = ImageList5.Images(0)
        ElseIf ch2 = 4 Then
            PictureBox129.Image = ImageList6.Images(0)
        ElseIf ch2 = 5 Then
            PictureBox129.Image = ImageList7.Images(0)
        ElseIf ch2 = 6 Then
            PictureBox129.Image = ImageList8.Images(0)
        End If
        player.settings.setMode("loop", True)
        If music = 0 Then
            Dim i As Integer
            Randomize()
            i = Int((5 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                player.URL = My.Application.Info.DirectoryPath & "\bgm1.mp3"
            ElseIf i = 1 Then
                player.URL = My.Application.Info.DirectoryPath & "\bgm2.mp3"
            ElseIf i = 2 Then
                player.URL = My.Application.Info.DirectoryPath & "\bgm3.mp3"
            ElseIf i = 3 Then
                player.URL = My.Application.Info.DirectoryPath & "\bgm4.mp3"
            ElseIf i = 4 Then
                player.URL = My.Application.Info.DirectoryPath & "\bgm5.mp3"
            ElseIf i = 5 Then
                player.URL = My.Application.Info.DirectoryPath & "\bgm6.mp3"
            End If
        ElseIf music = 1 Then
            player.URL = My.Application.Info.DirectoryPath & "\Dies Irae.mp3"
        ElseIf music = 2 Then
            player.URL = My.Application.Info.DirectoryPath & "\+9.mp3"
            Dim i As Integer
            Randomize()
            i = Int((3 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode1.wav", AudioPlayMode.Background)
            ElseIf i = 1 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode2.wav", AudioPlayMode.Background)
            ElseIf i = 2 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode3.wav", AudioPlayMode.Background)
            ElseIf i = 3 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\explode4.wav", AudioPlayMode.Background)
            End If
        End If
    End Sub

    '1P TNT[1]引爆timer(Timer_1)
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        tnt1_tick1 += 1
        Label4.Text = "tnt1_tick:" & tnt1_tick1 & " (tick/s)"
        PictureBox105.Visible = True
        If tnt1_tick1 Mod 2 = 0 Then
            PictureBox105.Image = ImageList2.Images(0)
        Else
            PictureBox105.Image = ImageList2.Images(1)
        End If
        If tnt1_tick1 = 5 Then
            PictureBox105.Image = ImageList2.Images(2)
            tnt1xy = tnt11
            Label3.Text = "tnt1xy: " & tnt1xy
            Call ex1()
            Call die1()
            Call die2()
            Call ending2()
        End If
        If tnt1_tick1 > 5 Then
            tnt11 = 0
            Timer1.Enabled = False
            PictureBox105.Visible = False
            PictureBox125.Visible = False
            PictureBox126.Visible = False
            PictureBox127.Visible = False
            PictureBox128.Visible = False
            PictureBox141.Visible = False
            PictureBox142.Visible = False
            PictureBox143.Visible = False
            PictureBox144.Visible = False
        End If

    End Sub
    '1P TNT[2]引爆timer(Timer_2)
    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        tnt1_tick2 += 1
        Label5.Text = "tnt2_tick:" & tnt1_tick2 & " (tick/s)"
        PictureBox106.Visible = True
        If tnt1_tick2 Mod 2 = 0 Then
            PictureBox106.Image = ImageList2.Images(0)
        Else
            PictureBox106.Image = ImageList2.Images(1)
        End If
        If tnt1_tick2 = 5 Then
            Label3.Text = "tnt1xy: " & tnt1xy
            PictureBox106.Image = ImageList2.Images(2)
            tnt1xy = tnt12
            Label3.Text = "tnt1xy: " & tnt1xy
            Call ex1()
            Call die1()
            Call die2()
            Call ending2()
        End If
        If tnt1_tick2 > 5 Then
            tnt12 = 0
            Timer2.Enabled = False
            PictureBox106.Visible = False
            PictureBox125.Visible = False
            PictureBox126.Visible = False
            PictureBox127.Visible = False
            PictureBox128.Visible = False
            PictureBox141.Visible = False
            PictureBox142.Visible = False
            PictureBox143.Visible = False
            PictureBox144.Visible = False
        End If
    End Sub

    '2P TNT[1]引爆timer(Timer_1)
    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
        tnt2_tick1 += 1
        PictureBox130.Visible = True
        If tnt2_tick1 Mod 2 = 0 Then
            PictureBox130.Image = ImageList2.Images(0)
        Else
            PictureBox130.Image = ImageList2.Images(1)
        End If
        If tnt2_tick1 = 5 Then
            Label15.Text = "tnt2xy: " & tnt2xy
            PictureBox130.Image = ImageList2.Images(2)
            tnt2xy = tnt21
            Label15.Text = "tnt2xy: " & tnt2xy
            Call ex2()
            Call die1()
            Call die2()
            Call ending2()
        End If
        If tnt2_tick1 > 5 Then
            tnt21 = 0
            Timer3.Enabled = False
            PictureBox130.Visible = False
            PictureBox132.Visible = False
            PictureBox133.Visible = False
            PictureBox134.Visible = False
            PictureBox135.Visible = False
            PictureBox137.Visible = False
            PictureBox138.Visible = False
            PictureBox139.Visible = False
            PictureBox140.Visible = False
        End If

    End Sub
    '2P TNT[2]引爆timer(Timer_2)
    Private Sub Timer4_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer4.Tick
        tnt2_tick2 += 1
        PictureBox131.Visible = True
        If tnt2_tick2 Mod 2 = 0 Then
            PictureBox131.Image = ImageList2.Images(0)
        Else
            PictureBox131.Image = ImageList2.Images(1)
        End If
        If tnt2_tick2 = 5 Then
            Label15.Text = "tnt2xy: " & tnt2xy
            PictureBox131.Image = ImageList2.Images(2)
            tnt2xy = tnt22
            Label15.Text = "tnt2xy: " & tnt2xy
            Call ex2()
            Call die1()
            Call die2()
            Call ending2()
        End If
        If tnt2_tick2 > 5 Then
            tnt22 = 0
            Timer4.Enabled = False
            PictureBox131.Visible = False
            PictureBox132.Visible = False
            PictureBox133.Visible = False
            PictureBox134.Visible = False
            PictureBox135.Visible = False
            PictureBox137.Visible = False
            PictureBox138.Visible = False
            PictureBox139.Visible = False
            PictureBox140.Visible = False
        End If
    End Sub

    '死亡特效
    Private Sub Timer5_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer5.Tick
        Dim i As Integer
        i += 1
        If i < Me.Height + 100 Then
            If death1 >= 1 Then
                PictureBox104.Top += 10
            End If
            If death2 >= 1 Then
                PictureBox129.Top += 10
            End If
        End If
        If PictureBox129.Top > Me.Height Or PictureBox104.Top > Me.Height Then
            esc = 1
            Call esc_mode()
        End If
    End Sub
    'end
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\decision7.wav", AudioPlayMode.Background)
        start.Close()
    End Sub
    'replay
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\decision7.wav", AudioPlayMode.Background)
        replay1 = 3 '因模式而改變數值
        Me.Close()
    End Sub
    'back
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\decision7.wav", AudioPlayMode.Background)
        Form2.Show()
        Me.Close()

    End Sub
End Class

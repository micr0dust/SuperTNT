Public Class Form7 '名字自己改
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
    Dim timeup As Boolean                       '時間爆炸(布林值)
    Dim color As Integer                        '顏色
    Dim b1, b2 As Integer                       '玩家方塊
    Dim c(11) As Integer                        '色塊陣列
    Dim ti, col, tt, style As Integer                  '時間爆炸

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
            PictureBox104.Image = RR1.Image
        ElseIf ch1 = 4 Then
            PictureBox104.Image = mach1.Image
        ElseIf ch1 = 5 Then
            PictureBox104.Image = sp1.Image
        ElseIf ch1 = 6 Then
            PictureBox104.Image = yfs1.Image
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
            PictureBox104.Image = RR2.Image
        ElseIf ch1 = 4 Then
            PictureBox104.Image = mach2.Image
        ElseIf ch1 = 5 Then
            PictureBox104.Image = sp2.Image
        ElseIf ch1 = 6 Then
            PictureBox104.Image = yfs2.Image
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
            PictureBox104.Image = RR3.Image
        ElseIf ch1 = 4 Then
            PictureBox104.Image = mach3.Image
        ElseIf ch1 = 5 Then
            PictureBox104.Image = sp3.Image
        ElseIf ch1 = 6 Then
            PictureBox104.Image = yfs3.Image
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
            PictureBox104.Image = RR4.Image
        ElseIf ch1 = 4 Then
            PictureBox104.Image = mach4.Image
        ElseIf ch1 = 5 Then
            PictureBox104.Image = sp4.Image
        ElseIf ch1 = 6 Then
            PictureBox104.Image = yfs4.Image
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
            PictureBox129.Image = RR1.Image
        ElseIf ch2 = 4 Then
            PictureBox129.Image = mach1.Image
        ElseIf ch2 = 5 Then
            PictureBox129.Image = sp1.Image
        ElseIf ch2 = 6 Then
            PictureBox129.Image = yfs1.Image
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
            PictureBox129.Image = RR2.Image
        ElseIf ch2 = 4 Then
            PictureBox129.Image = mach2.Image
        ElseIf ch2 = 5 Then
            PictureBox129.Image = sp2.Image
        ElseIf ch2 = 6 Then
            PictureBox129.Image = yfs2.Image
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
            PictureBox129.Image = RR3.Image
        ElseIf ch2 = 4 Then
            PictureBox129.Image = mach3.Image
        ElseIf ch2 = 5 Then
            PictureBox129.Image = sp3.Image
        ElseIf ch2 = 6 Then
            PictureBox129.Image = yfs3.Image
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
            PictureBox129.Image = RR4.Image
        ElseIf ch2 = 4 Then
            PictureBox129.Image = mach4.Image
        ElseIf ch2 = 5 Then
            PictureBox129.Image = sp4.Image
        ElseIf ch2 = 6 Then
            PictureBox129.Image = yfs4.Image
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
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode1.wav", AudioPlayMode.Background)
        ElseIf i = 1 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode2.wav", AudioPlayMode.Background)
        ElseIf i = 2 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode3.wav", AudioPlayMode.Background)
        ElseIf i = 3 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode4.wav", AudioPlayMode.Background)
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
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode1.wav", AudioPlayMode.Background)
        ElseIf i = 1 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode2.wav", AudioPlayMode.Background)
        ElseIf i = 2 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode3.wav", AudioPlayMode.Background)
        ElseIf i = 3 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\explode4.wav", AudioPlayMode.Background)
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
        If invincible = 2 Or invincible = 0 Then
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
        End If
    End Sub
    '2P死亡判斷(function)
    Private Sub die2()
        If invincible = 1 Or invincible = 0 Then
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
        End If
    End Sub

    '遊戲結束[死亡](function)
    Private Sub ending2()
        If endy = 0 Then
            If death1 >= 1 And death2 = 0 Then
                Label6.Text = "WIN"
                Label10.Text = "LOSS"
                endy = 2
                PictureBox104.Image = ImageList3.Images(4)
                PictureBox104.Top -= 50
                Timer5.Enabled = True
                If music = 2 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\+9die.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\dead.wav", AudioPlayMode.Background)
                End If
                player.close()
            ElseIf death2 >= 1 And death1 = 0 Then
                Label10.Text = "WIN"
                Label6.Text = "LOSS"
                endy = 2
                PictureBox129.Image = ImageList3.Images(4)
                PictureBox129.Top -= 50
                Timer5.Enabled = True
                If music = 2 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\+9die.wav", AudioPlayMode.Background)
                Else
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\player\dead.wav", AudioPlayMode.Background)
                End If
                player.close()
            ElseIf death2 >= 1 And death1 >= 1 Then
                Label6.Text = "LOSS"
                Label10.Text = "LOSS"
                endy = 2
                PictureBox104.Image = ImageList3.Images(4)
                PictureBox129.Image = ImageList3.Images(4)
                PictureBox104.Top -= 50
                PictureBox129.Top -= 50
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
            Timer7.Enabled = False
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
            Timer7.Enabled = True
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
            If x1 = 13 Then
                Call back1()
            End If
        End If
        color1()
        color2()
    End Sub
    '2P邊界擋玩家偵測 + 呼叫 function back (function)
    Private Sub detection2()
        If f12 = 0 Then
            If y2 = 14 Then
                Call back2()
            End If
            If x2 = 13 Then
                Call back2()
            End If
        End If
        color1()
        color2()
    End Sub

    '1P所踩顏色 (function)
    Private Sub color1()
        If style = 0 Then
            If xy1 = 102 Or xy1 = 502 Or xy1 = 902 Then
                b1 = c(1)
            ElseIf xy1 = 202 Or xy1 = 602 Or xy1 = 1002 Then
                b1 = c(2)
            ElseIf xy1 = 302 Or xy1 = 402 Or xy1 = 702 Or xy1 = 802 Or xy1 = 1102 Or xy1 = 1202 Or xy1 = 112 Or xy1 = 212 Or xy1 = 512 Or xy1 = 612 Or xy1 = 912 Or xy1 = 1012 Then
                b1 = c(3)
            ElseIf xy1 = 312 Or xy1 = 712 Or xy1 = 1112 Then
                b1 = c(4)
            ElseIf xy1 = 412 Or xy1 = 812 Or xy1 = 1212 Then
                b1 = c(5)
            ElseIf xy1 = 107 Or xy1 = 307 Or xy1 = 507 Or xy1 = 707 Or xy1 = 1107 Or xy1 = 1207 Then
                b1 = c(6)
            ElseIf xy1 = 107 Or xy1 = 307 Or xy1 = 507 Or xy1 = 707 Or xy1 = 1107 Or xy1 = 1207 Then
                b1 = c(6)
            ElseIf xy1 = 204 Or xy1 = 404 Or xy1 = 504 Or xy1 = 604 Or xy1 = 804 Or xy1 = 1004 Or xy1 = 210 Or xy1 = 410 Or xy1 = 510 Or xy1 = 610 Or xy1 = 1010 Then
                b1 = c(7)
            ElseIf xy1 >= 205 And xy1 <= 209 Or xy1 >= 405 And xy1 <= 409 Or xy1 >= 605 And xy1 <= 609 Or xy1 >= 805 And xy1 <= 807 Or xy1 >= 1005 And xy1 <= 1009 Or xy1 = 505 Or xy1 = 509 Or xy1 = 907 Then
                b1 = c(8)
            ElseIf xy1 = 1201 Or xy1 = 1213 Or xy1 = 101 Or xy1 = 113 Then
                b1 = c(10)
            Else
                b1 = c(9)
            End If
        ElseIf style = 1 Then
            If xy1 = 606 Or xy1 = 607 Or xy1 = 706 Or xy1 = 707 Then
                b1 = c(1)
            ElseIf xy1 = 506 Or xy1 = 507 Or xy1 = 605 Or xy1 = 608 Or xy1 = 705 Or xy1 = 708 Or xy1 = 806 Or xy1 = 807 Then
                b1 = c(2)
            ElseIf xy1 = 505 Or xy1 = 508 Or xy1 = 805 Or xy1 = 808 Then
                b1 = c(3)
            ElseIf xy1 = 405 Or xy1 = 406 Or xy1 = 407 Or xy1 = 408 Or xy1 = 504 Or xy1 = 509 Or xy1 = 604 Or xy1 = 609 Or xy1 = 704 Or xy1 = 709 Or xy1 = 804 Or xy1 = 809 Or xy1 = 905 Or xy1 = 906 Or xy1 = 907 Or xy1 = 908 Then
                b1 = c(4)
            ElseIf xy1 = 306 Or xy1 = 307 Or xy1 = 404 Or xy1 = 409 Or xy1 = 603 Or xy1 = 610 Or xy1 = 703 Or xy1 = 710 Or xy1 = 904 Or xy1 = 909 Or xy1 = 1006 Or xy1 = 1007 Then
                b1 = c(5)
            ElseIf xy1 = 101 Or xy1 = 113 Or xy1 = 1201 Or xy1 = 1213 Then
                b1 = c(6)
            ElseIf xy1 = 102 Or xy1 = 112 Or xy1 = 201 Or xy1 = 213 Or xy1 = 1101 Or xy1 = 1113 Or xy1 = 1202 Or xy1 = 1212 Then
                b1 = c(7)
            ElseIf xy1 = 103 Or xy1 = 104 Or xy1 = 110 Or xy1 = 111 Or xy1 = 202 Or xy1 = 212 Or xy1 = 301 Or xy1 = 313 Or xy1 = 401 Or xy1 = 413 Or xy1 = 901 Or xy1 = 913 Or xy1 = 1001 Or xy1 = 1013 Or xy1 = 1102 Or xy1 = 1112 Or xy1 = 1203 Or xy1 = 1204 Or xy1 = 1210 Or xy1 = 1211 Then
                b1 = c(8)
            ElseIf xy1 = 105 Or xy1 = 109 Or xy1 = 203 Or xy1 = 211 Or xy1 = 302 Or xy1 = 312 Or xy1 = 501 Or xy1 = 513 Or xy1 = 801 Or xy1 = 813 Or xy1 = 1002 Or xy1 = 1012 Or xy1 = 1103 Or xy1 = 1111 Or xy1 = 1205 Or xy1 = 1209 Then
                b1 = c(9)
            ElseIf xy1 = 106 Or xy1 = 107 Or xy1 = 108 Or xy1 = 204 Or xy1 = 205 Or xy1 = 206 Or xy1 = 207 Or xy1 = 208 Or xy1 = 209 Or xy1 = 210 Or xy1 = 303 Or xy1 = 304 Or xy1 = 305 Or xy1 = 308 Or xy1 = 309 Or xy1 = 310 Or xy1 = 311 Or xy1 = 402 Or xy1 = 403 Or xy1 = 410 Or xy1 = 411 Or xy1 = 412 Or xy1 = 502 Or xy1 = 503 Or xy1 = 510 Or xy1 = 511 Or xy1 = 512 Or xy1 = 601 Or xy1 = 602 Or xy1 = 611 Or xy1 = 612 Or xy1 = 613 Or xy1 = 701 Or xy1 = 702 Or xy1 = 711 Or xy1 = 712 Or xy1 = 713 Or xy1 = 802 Or xy1 = 803 Or xy1 = 810 Or xy1 = 811 Or xy1 = 812 Or xy1 = 902 Or xy1 = 903 Or xy1 = 910 Or xy1 = 911 Or xy1 = 912 Or xy1 = 1003 Or xy1 = 1004 Or xy1 = 1005 Or xy1 = 1008 Or xy1 = 1009 Or xy1 = 1010 Or xy1 = 1011 Or xy1 = 1104 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1107 Or xy1 = 1108 Or xy1 = 1109 Or xy1 = 1110 Or xy1 = 1206 Or xy1 = 1207 Or xy1 = 1208 Then
                b1 = c(10)
            End If

        ElseIf style = 2 Then
            If xy1 = 1205 Or xy1 = 1206 Or xy1 = 1207 Or xy1 = 1208 Or xy1 = 1209 Then
                b1 = c(1)
            ElseIf xy1 = 101 Or xy1 = 102 Or xy1 = 103 Or xy1 = 104 Or xy1 = 105 Or xy1 = 106 Or xy1 = 107 Or xy1 = 108 Or xy1 = 109 Or xy1 = 110 Or xy1 = 111 Or xy1 = 112 Or xy1 = 201 Or xy1 = 207 Or xy1 = 208 Or xy1 = 210 Or xy1 = 211 Or xy1 = 301 Or xy1 = 303 Or xy1 = 304 Or xy1 = 305 Or xy1 = 307 Or xy1 = 309 Or xy1 = 311 Or xy1 = 401 Or xy1 = 402 Or xy1 = 406 Or xy1 = 407 Or xy1 = 409 Or xy1 = 412 Or xy1 = 501 Or xy1 = 502 Or xy1 = 503 Or xy1 = 504 Or xy1 = 505 Or xy1 = 506 Or xy1 = 507 Or xy1 = 508 Or xy1 = 509 Or xy1 = 510 Or xy1 = 511 Or xy1 = 512 Or xy1 = 601 Or xy1 = 603 Or xy1 = 604 Or xy1 = 607 Or xy1 = 701 Or xy1 = 703 Or xy1 = 705 Or xy1 = 707 Or xy1 = 708 Or xy1 = 709 Or xy1 = 711 Or xy1 = 712 Or xy1 = 801 Or xy1 = 804 Or xy1 = 805 Or xy1 = 807 Or xy1 = 902 Or xy1 = 903 Or xy1 = 904 Or xy1 = 905 Or xy1 = 906 Or xy1 = 907 Or xy1 = 908 Or xy1 = 909 Or xy1 = 910 Or xy1 = 911 Or xy1 = 912 Or xy1 = 1003 Or xy1 = 1004 Or xy1 = 1005 Or xy1 = 1006 Or xy1 = 1007 Or xy1 = 1008 Or xy1 = 1009 Or xy1 = 1010 Or xy1 = 1011 Or xy1 = 1104 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1107 Or xy1 = 1108 Or xy1 = 1109 Or xy1 = 1110 Then
                b1 = c(2)
            ElseIf xy1 = 1204 Then
                b1 = c(3)
            ElseIf xy1 = 202 Or xy1 = 203 Or xy1 = 204 Or xy1 = 205 Or xy1 = 206 Or xy1 = 302 Or xy1 = 306 Or xy1 = 403 Or xy1 = 404 Or xy1 = 405 Or xy1 = 1103 Or xy1 = 1203 Then
                b1 = c(4)
            ElseIf xy1 = 1002 Or xy1 = 1102 Or xy1 = 1202 Then
                b1 = c(5)
            ElseIf xy1 = 209 Or xy1 = 212 Or xy1 = 308 Or xy1 = 310 Or xy1 = 312 Or xy1 = 408 Or xy1 = 410 Or xy1 = 411 Or xy1 = 901 Or xy1 = 1001 Or xy1 = 1101 Or xy1 = 1201 Then
                b1 = c(6)
            ElseIf xy1 = 113 Or xy1 = 213 Or xy1 = 313 Or xy1 = 413 Or xy1 = 513 Or xy1 = 613 Or xy1 = 713 Or xy1 = 813 Or xy1 = 913 Or xy1 = 1013 Or xy1 = 1113 Or xy1 = 1213 Then
                b1 = c(7)
            ElseIf xy1 = 602 Or xy1 = 605 Or xy1 = 606 Or xy1 = 702 Or xy1 = 704 Or xy1 = 706 Or xy1 = 802 Or xy1 = 803 Or xy1 = 806 Or xy1 = 1012 Or xy1 = 1112 Or xy1 = 1212 Then
                b1 = c(8)
            ElseIf xy1 = 1210 Then
                b1 = c(9)
            ElseIf xy1 = 608 Or xy1 = 609 Or xy1 = 610 Or xy1 = 611 Or xy1 = 612 Or xy1 = 710 Or xy1 = 808 Or xy1 = 809 Or xy1 = 810 Or xy1 = 811 Or xy1 = 812 Or xy1 = 1111 Or xy1 = 1211 Then
                b1 = c(10)
            End If
        ElseIf style = 3 Then
            If xy1 = 202 Or xy1 = 206 Or xy1 = 302 Or xy1 = 303 Or xy1 = 304 Or xy1 = 305 Or xy1 = 306 Or xy1 = 402 Or xy1 = 406 Then
                b1 = c(1)
            ElseIf xy1 = 602 Or xy1 = 603 Or xy1 = 604 Or xy1 = 702 Or xy1 = 703 Or xy1 = 704 Or xy1 = 705 Or xy1 = 804 Or xy1 = 805 Or xy1 = 806 Or xy1 = 902 Or xy1 = 903 Or xy1 = 904 Or xy1 = 905 Or xy1 = 1002 Or xy1 = 1003 Or xy1 = 1004 Then
                b1 = c(2)
            ElseIf xy1 = 208 Or xy1 = 209 Or xy1 = 310 Or xy1 = 311 Or xy1 = 412 Or xy1 = 510 Or xy1 = 511 Or xy1 = 608 Or xy1 = 609 Then
                b1 = c(3)
            ElseIf xy1 = 808 Or xy1 = 809 Or xy1 = 810 Or xy1 = 811 Or xy1 = 812 Or xy1 = 910 Or xy1 = 912 Or xy1 = 1010 Or xy1 = 1012 Or xy1 = 1110 Or xy1 = 1111 Or xy1 = 1112 Then
                b1 = c(4)
            ElseIf xy1 = 101 Or xy1 = 113 Or xy1 = 1201 Or xy1 = 1213 Then
                b1 = c(5)
            ElseIf xy1 = 201 Or xy1 = 301 Or xy1 = 401 Or xy1 = 501 Or xy1 = 601 Or xy1 = 701 Or xy1 = 801 Or xy1 = 901 Or xy1 = 1001 Or xy1 = 1101 Then
                b1 = c(6)
            ElseIf xy1 = 102 Or xy1 = 103 Or xy1 = 104 Or xy1 = 105 Or xy1 = 106 Or xy1 = 107 Or xy1 = 108 Or xy1 = 109 Or xy1 = 110 Or xy1 = 111 Or xy1 = 112 Then
                b1 = c(7)
            ElseIf xy1 = 213 Or xy1 = 313 Or xy1 = 413 Or xy1 = 513 Or xy1 = 613 Or xy1 = 713 Or xy1 = 813 Or xy1 = 913 Or xy1 = 1013 Or xy1 = 1113 Then
                b1 = c(8)
            ElseIf xy1 = 1202 Or xy1 = 1203 Or xy1 = 1204 Or xy1 = 1205 Or xy1 = 1206 Or xy1 = 1207 Or xy1 = 1208 Or xy1 = 1209 Or xy1 = 1210 Or xy1 = 1211 Or xy1 = 1212 Then
                b1 = c(9)
            ElseIf xy1 = 203 Or xy1 = 204 Or xy1 = 205 Or xy1 = 207 Or xy1 = 210 Or xy1 = 211 Or xy1 = 212 Or xy1 = 307 Or xy1 = 308 Or xy1 = 309 Or xy1 = 312 Or xy1 = 403 Or xy1 = 404 Or xy1 = 405 Or xy1 = 407 Or xy1 = 408 Or xy1 = 409 Or xy1 = 410 Or xy1 = 411 Or xy1 = 502 Or xy1 = 503 Or xy1 = 504 Or xy1 = 505 Or xy1 = 506 Or xy1 = 507 Or xy1 = 508 Or xy1 = 509 Or xy1 = 512 Or xy1 = 605 Or xy1 = 606 Or xy1 = 607 Or xy1 = 610 Or xy1 = 611 Or xy1 = 612 Or xy1 = 706 Or xy1 = 707 Or xy1 = 708 Or xy1 = 709 Or xy1 = 710 Or xy1 = 711 Or xy1 = 712 Or xy1 = 802 Or xy1 = 803 Or xy1 = 807 Or xy1 = 906 Or xy1 = 907 Or xy1 = 908 Or xy1 = 909 Or xy1 = 911 Or xy1 = 1005 Or xy1 = 1006 Or xy1 = 1007 Or xy1 = 1008 Or xy1 = 1009 Or xy1 = 1011 Or xy1 = 1102 Or xy1 = 1103 Or xy1 = 1104 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1107 Or xy1 = 1108 Or xy1 = 1109 Then
                b1 = c(10)
            End If
        ElseIf style = 4 Then
            If xy1 = 107 Or xy1 = 113 Or xy1 = 213 Or xy1 = 313 Or xy1 = 407 Or xy1 = 507 Or xy1 = 607 Or xy1 = 613 Or xy1 = 713 Or xy1 = 813 Or xy1 = 1107 Or xy1 = 1113 Or xy1 = 1208 Then
                b1 = c(1)
            ElseIf xy1 = 101 Or xy1 = 102 Or xy1 = 103 Or xy1 = 104 Or xy1 = 105 Or xy1 = 106 Or xy1 = 108 Or xy1 = 109 Or xy1 = 110 Or xy1 = 111 Or xy1 = 112 Or xy1 = 201 Or xy1 = 207 Or xy1 = 209 Or xy1 = 301 Or xy1 = 302 Or xy1 = 303 Or xy1 = 304 Or xy1 = 305 Or xy1 = 306 Or xy1 = 307 Or xy1 = 309 Or xy1 = 311 Or xy1 = 401 Or xy1 = 411 Or xy1 = 413 Or xy1 = 501 Or xy1 = 503 Or xy1 = 504 Or xy1 = 505 Or xy1 = 508 Or xy1 = 509 Or xy1 = 510 Or xy1 = 511 Or xy1 = 512 Or xy1 = 513 Or xy1 = 601 Or xy1 = 701 Or xy1 = 702 Or xy1 = 703 Or xy1 = 704 Or xy1 = 705 Or xy1 = 706 Or xy1 = 707 Or xy1 = 709 Or xy1 = 711 Or xy1 = 801 Or xy1 = 805 Or xy1 = 806 Or xy1 = 807 Or xy1 = 901 Or xy1 = 902 Or xy1 = 903 Or xy1 = 905 Or xy1 = 906 Or xy1 = 907 Or xy1 = 908 Or xy1 = 909 Or xy1 = 910 Or xy1 = 911 Or xy1 = 912 Or xy1 = 913 Or xy1 = 1001 Or xy1 = 1007 Or xy1 = 1011 Or xy1 = 1013 Or xy1 = 1101 Or xy1 = 1102 Or xy1 = 1103 Or xy1 = 1104 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1108 Or xy1 = 1109 Or xy1 = 1110 Or xy1 = 1111 Or xy1 = 1112 Or xy1 = 1201 Or xy1 = 1202 Or xy1 = 1203 Or xy1 = 1204 Or xy1 = 1205 Or xy1 = 1206 Or xy1 = 1207 Or xy1 = 1209 Or xy1 = 1210 Or xy1 = 1211 Or xy1 = 1212 Or xy1 = 1213 Then
                b1 = c(2)
            ElseIf xy1 = 208 Or xy1 = 210 Or xy1 = 211 Or xy1 = 212 Or xy1 = 308 Or xy1 = 310 Or xy1 = 312 Or xy1 = 408 Or xy1 = 409 Or xy1 = 410 Or xy1 = 412 Then
                b1 = c(3)
            ElseIf xy1 = 608 Or xy1 = 609 Or xy1 = 610 Or xy1 = 611 Or xy1 = 612 Or xy1 = 708 Or xy1 = 710 Or xy1 = 712 Or xy1 = 808 Or xy1 = 809 Or xy1 = 810 Or xy1 = 811 Or xy1 = 812 Then
                b1 = c(4)
            ElseIf xy1 = 1008 Or xy1 = 1009 Or xy1 = 1010 Or xy1 = 1012 Then
                b1 = c(5)
            ElseIf xy1 = 802 Or xy1 = 803 Or xy1 = 804 Or xy1 = 904 Or xy1 = 1002 Or xy1 = 1003 Or xy1 = 1004 Or xy1 = 1005 Or xy1 = 1006 Then
                b1 = c(6)
            ElseIf xy1 = 202 Or xy1 = 402 Or xy1 = 502 Or xy1 = 602 Then
                b1 = c(7)
            ElseIf xy1 = 204 Or xy1 = 404 Or xy1 = 604 Then
                b1 = c(8)
            ElseIf xy1 = 203 Or xy1 = 403 Or xy1 = 603 Then
                b1 = c(9)
            ElseIf xy1 = 205 Or xy1 = 206 Or xy1 = 405 Or xy1 = 406 Or xy1 = 506 Or xy1 = 605 Or xy1 = 606 Then
                b1 = c(10)
            End If
        ElseIf style = 5 Then
            If xy1 = 202 Or xy1 = 203 Or xy1 = 204 Or xy1 = 205 Or xy1 = 206 Then
                b1 = c(1)
            ElseIf xy1 = 402 Or xy1 = 403 Or xy1 = 404 Or xy1 = 405 Or xy1 = 406 Or xy1 = 502 Or xy1 = 506 Or xy1 = 602 Or xy1 = 603 Or xy1 = 604 Or xy1 = 605 Or xy1 = 606 Then
                b1 = c(2)
            ElseIf xy1 = 802 Or xy1 = 803 Or xy1 = 804 Or xy1 = 904 Or xy1 = 1002 Or xy1 = 1003 Or xy1 = 1004 Or xy1 = 1005 Or xy1 = 1006 Then
                b1 = c(3)
            ElseIf xy1 = 208 Or xy1 = 210 Or xy1 = 212 Or xy1 = 308 Or xy1 = 310 Or xy1 = 312 Or xy1 = 408 Or xy1 = 409 Or xy1 = 410 Or xy1 = 411 Or xy1 = 412 Then
                b1 = c(4)
            ElseIf xy1 = 708 Or xy1 = 709 Or xy1 = 710 Or xy1 = 711 Or xy1 = 712 Or xy1 = 808 Or xy1 = 812 Or xy1 = 908 Or xy1 = 909 Or xy1 = 910 Or xy1 = 911 Or xy1 = 912 Then
                b1 = c(5)
            ElseIf xy1 = 1108 Or xy1 = 1109 Or xy1 = 1110 Or xy1 = 1112 Then
                b1 = c(6)
            ElseIf xy1 = 107 Or xy1 = 108 Or xy1 = 109 Or xy1 = 110 Or xy1 = 111 Or xy1 = 112 Or xy1 = 207 Or xy1 = 209 Or xy1 = 211 Or xy1 = 307 Or xy1 = 309 Or xy1 = 311 Or xy1 = 407 Or xy1 = 507 Or xy1 = 508 Or xy1 = 509 Or xy1 = 510 Or xy1 = 511 Or xy1 = 512 Or xy1 = 607 Or xy1 = 608 Or xy1 = 609 Or xy1 = 610 Or xy1 = 611 Or xy1 = 612 Or xy1 = 707 Or xy1 = 807 Or xy1 = 907 Or xy1 = 1007 Or xy1 = 1008 Or xy1 = 1009 Or xy1 = 1010 Or xy1 = 1011 Or xy1 = 1012 Or xy1 = 1107 Or xy1 = 1111 Or xy1 = 1207 Or xy1 = 1208 Or xy1 = 1209 Or xy1 = 1210 Or xy1 = 1211 Or xy1 = 1212 Then
                b1 = c(7)
            ElseIf xy1 = 102 Or xy1 = 103 Or xy1 = 104 Or xy1 = 105 Or xy1 = 106 Or xy1 = 302 Or xy1 = 303 Or xy1 = 304 Or xy1 = 305 Or xy1 = 306 Or xy1 = 503 Or xy1 = 504 Or xy1 = 505 Or xy1 = 702 Or xy1 = 703 Or xy1 = 704 Or xy1 = 705 Or xy1 = 706 Or xy1 = 805 Or xy1 = 806 Or xy1 = 902 Or xy1 = 903 Or xy1 = 905 Or xy1 = 906 Or xy1 = 1102 Or xy1 = 1103 Or xy1 = 1104 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1202 Or xy1 = 1203 Or xy1 = 1204 Or xy1 = 1205 Or xy1 = 1206 Then
                b1 = c(8)
            ElseIf xy1 = 809 Or xy1 = 810 Or xy1 = 811 Then
                b1 = c(9)
            ElseIf xy1 = 101 Or xy1 = 113 Or xy1 = 201 Or xy1 = 213 Or xy1 = 301 Or xy1 = 313 Or xy1 = 401 Or xy1 = 413 Or xy1 = 501 Or xy1 = 513 Or xy1 = 601 Or xy1 = 613 Or xy1 = 701 Or xy1 = 713 Or xy1 = 801 Or xy1 = 813 Or xy1 = 901 Or xy1 = 913 Or xy1 = 1001 Or xy1 = 1013 Or xy1 = 1101 Or xy1 = 1113 Or xy1 = 1201 Or xy1 = 1213 Then
                b1 = c(10)
            End If
        ElseIf style = 6 Then
            If xy1 = 202 Or xy1 = 203 Or xy1 = 204 Or xy1 = 205 Or xy1 = 206 Then
                b1 = c(1)
            ElseIf xy1 = 402 Or xy1 = 403 Or xy1 = 404 Or xy1 = 405 Or xy1 = 406 Or xy1 = 502 Or xy1 = 506 Or xy1 = 602 Or xy1 = 603 Or xy1 = 604 Or xy1 = 605 Or xy1 = 606 Then
                b1 = c(2)
            ElseIf xy1 = 802 Or xy1 = 803 Or xy1 = 804 Or xy1 = 904 Or xy1 = 1002 Or xy1 = 1003 Or xy1 = 1004 Or xy1 = 1005 Or xy1 = 1006 Then
                b1 = c(3)
            ElseIf xy1 = 208 Or xy1 = 210 Or xy1 = 211 Or xy1 = 212 Or xy1 = 308 Or xy1 = 310 Or xy1 = 312 Or xy1 = 408 Or xy1 = 409 Or xy1 = 410 Or xy1 = 412 Then
                b1 = c(4)
            ElseIf xy1 = 608 Or xy1 = 609 Or xy1 = 708 Or xy1 = 808 Or xy1 = 809 Or xy1 = 810 Or xy1 = 811 Or xy1 = 812 Then
                b1 = c(5)
            ElseIf xy1 = 1008 Or xy1 = 1009 Or xy1 = 1010 Or xy1 = 1012 Then
                b1 = c(6)
            ElseIf xy1 = 108 Or xy1 = 109 Or xy1 = 110 Or xy1 = 111 Or xy1 = 112 Or xy1 = 209 Or xy1 = 309 Or xy1 = 311 Or xy1 = 411 Or xy1 = 508 Or xy1 = 509 Or xy1 = 510 Or xy1 = 511 Or xy1 = 512 Or xy1 = 610 Or xy1 = 611 Or xy1 = 612 Or xy1 = 709 Or xy1 = 710 Or xy1 = 711 Or xy1 = 712 Or xy1 = 908 Or xy1 = 909 Or xy1 = 910 Or xy1 = 911 Or xy1 = 912 Or xy1 = 1011 Or xy1 = 1108 Or xy1 = 1109 Or xy1 = 1110 Or xy1 = 1111 Or xy1 = 1112 Or xy1 = 1208 Or xy1 = 1209 Or xy1 = 1210 Or xy1 = 1211 Or xy1 = 1212 Then
                b1 = c(7)
            ElseIf xy1 = 102 Or xy1 = 103 Or xy1 = 104 Or xy1 = 105 Or xy1 = 106 Or xy1 = 302 Or xy1 = 303 Or xy1 = 304 Or xy1 = 305 Or xy1 = 306 Or xy1 = 503 Or xy1 = 504 Or xy1 = 505 Or xy1 = 702 Or xy1 = 703 Or xy1 = 704 Or xy1 = 705 Or xy1 = 706 Or xy1 = 805 Or xy1 = 806 Or xy1 = 902 Or xy1 = 903 Or xy1 = 905 Or xy1 = 906 Or xy1 = 1102 Or xy1 = 1103 Or xy1 = 1104 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1202 Or xy1 = 1203 Or xy1 = 1204 Or xy1 = 1205 Or xy1 = 1206 Then
                b1 = c(8)
            ElseIf xy1 = 107 Or xy1 = 201 Or xy1 = 213 Or xy1 = 307 Or xy1 = 401 Or xy1 = 413 Or xy1 = 507 Or xy1 = 601 Or xy1 = 613 Or xy1 = 707 Or xy1 = 801 Or xy1 = 813 Or xy1 = 907 Or xy1 = 1001 Or xy1 = 1013 Or xy1 = 1107 Or xy1 = 1201 Or xy1 = 1213 Then
                b1 = c(9)
            ElseIf xy1 = 101 Or xy1 = 113 Or xy1 = 207 Or xy1 = 301 Or xy1 = 313 Or xy1 = 407 Or xy1 = 501 Or xy1 = 513 Or xy1 = 607 Or xy1 = 701 Or xy1 = 713 Or xy1 = 807 Or xy1 = 901 Or xy1 = 913 Or xy1 = 1007 Or xy1 = 1101 Or xy1 = 1113 Or xy1 = 1207 Then
                b1 = c(10)
            End If
        ElseIf style = 7 Then
            If xy1 = 303 Or xy1 = 304 Or xy1 = 305 Or xy1 = 306 Or xy1 = 403 Or xy1 = 407 Or xy1 = 503 Or xy1 = 506 Or xy1 = 508 Or xy1 = 509 Or xy1 = 511 Or xy1 = 603 Or xy1 = 604 Or xy1 = 605 Or xy1 = 608 Or xy1 = 611 Or xy1 = 703 Or xy1 = 706 Or xy1 = 708 Or xy1 = 709 Or xy1 = 711 Or xy1 = 803 Or xy1 = 807 Or xy1 = 903 Or xy1 = 904 Or xy1 = 905 Or xy1 = 906 Then
                b1 = c(1)
            ElseIf xy1 = 101 Or xy1 = 111 Or xy1 = 202 Or xy1 = 210 Or xy1 = 309 Or xy1 = 408 Or xy1 = 808 Or xy1 = 909 Or xy1 = 1002 Or xy1 = 1010 Or xy1 = 1101 Or xy1 = 1111 Then
                b1 = c(2)
            ElseIf xy1 = 404 Or xy1 = 405 Or xy1 = 406 Or xy1 = 504 Or xy1 = 505 Or xy1 = 507 Or xy1 = 606 Or xy1 = 607 Or xy1 = 704 Or xy1 = 705 Or xy1 = 707 Or xy1 = 804 Or xy1 = 805 Or xy1 = 806 Then
                b1 = c(3)
            ElseIf xy1 = 110 Or xy1 = 112 Or xy1 = 209 Or xy1 = 211 Or xy1 = 308 Or xy1 = 310 Or xy1 = 409 Or xy1 = 809 Or xy1 = 908 Or xy1 = 910 Or xy1 = 1009 Or xy1 = 1011 Or xy1 = 1110 Or xy1 = 1112 Or xy1 = 1212 Or xy1 = 1213 Then
                b1 = c(4)
            ElseIf xy1 = 102 Or xy1 = 201 Or xy1 = 203 Or xy1 = 302 Or xy1 = 902 Or xy1 = 1001 Or xy1 = 1003 Or xy1 = 1102 Or xy1 = 1201 Then
                b1 = c(5)
            ElseIf xy1 = 103 Or xy1 = 109 Or xy1 = 113 Or xy1 = 204 Or xy1 = 205 Or xy1 = 206 Or xy1 = 207 Or xy1 = 208 Or xy1 = 212 Or xy1 = 301 Or xy1 = 307 Or xy1 = 311 Or xy1 = 402 Or xy1 = 410 Or xy1 = 411 Or xy1 = 502 Or xy1 = 510 Or xy1 = 602 Or xy1 = 609 Or xy1 = 610 Or xy1 = 702 Or xy1 = 710 Or xy1 = 802 Or xy1 = 810 Or xy1 = 811 Or xy1 = 901 Or xy1 = 907 Or xy1 = 911 Or xy1 = 1004 Or xy1 = 1005 Or xy1 = 1006 Or xy1 = 1007 Or xy1 = 1008 Or xy1 = 1012 Or xy1 = 1103 Or xy1 = 1109 Or xy1 = 1113 Or xy1 = 1202 Or xy1 = 1210 Or xy1 = 1211 Then
                b1 = c(6)
            ElseIf xy1 = 104 Or xy1 = 108 Or xy1 = 213 Or xy1 = 312 Or xy1 = 401 Or xy1 = 801 Or xy1 = 912 Or xy1 = 1013 Or xy1 = 1104 Or xy1 = 1108 Or xy1 = 1203 Or xy1 = 1209 Then
                b1 = c(7)
            ElseIf xy1 = 105 Or xy1 = 107 Or xy1 = 313 Or xy1 = 412 Or xy1 = 413 Or xy1 = 501 Or xy1 = 512 Or xy1 = 513 Or xy1 = 612 Or xy1 = 701 Or xy1 = 712 Or xy1 = 713 Or xy1 = 812 Or xy1 = 813 Or xy1 = 913 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1107 Or xy1 = 1204 Or xy1 = 1205 Or xy1 = 1207 Or xy1 = 1208 Then
                b1 = c(8)
            ElseIf xy1 = 601 Or xy1 = 613 Then
                b1 = c(9)
            ElseIf xy1 = 106 Or xy1 = 1206 Then
                b1 = c(10)
            End If
        ElseIf style = 8 Then
            If xy1 = 202 Or xy1 = 203 Or xy1 = 204 Or xy1 = 302 Or xy1 = 303 Or xy1 = 304 Or xy1 = 309 Or xy1 = 310 Or xy1 = 311 Or xy1 = 312 Or xy1 = 402 Or xy1 = 403 Or xy1 = 404 Or xy1 = 409 Or xy1 = 410 Or xy1 = 411 Or xy1 = 412 Or xy1 = 507 Or xy1 = 508 Or xy1 = 509 Or xy1 = 607 Or xy1 = 608 Or xy1 = 609 Or xy1 = 707 Or xy1 = 708 Or xy1 = 709 Or xy1 = 802 Or xy1 = 803 Or xy1 = 804 Or xy1 = 809 Or xy1 = 810 Or xy1 = 811 Or xy1 = 812 Or xy1 = 902 Or xy1 = 903 Or xy1 = 904 Or xy1 = 909 Or xy1 = 910 Or xy1 = 911 Or xy1 = 912 Or xy1 = 1002 Or xy1 = 1003 Or xy1 = 1004 Then
                b1 = c(1)
            ElseIf xy1 = 110 Or xy1 = 111 Or xy1 = 207 Or xy1 = 210 Or xy1 = 306 Or xy1 = 307 Or xy1 = 406 Or xy1 = 502 Or xy1 = 513 Or xy1 = 603 Or xy1 = 604 Or xy1 = 612 Or xy1 = 613 Or xy1 = 701 Or xy1 = 702 Or xy1 = 704 Or xy1 = 712 Or xy1 = 801 Or xy1 = 1005 Or xy1 = 1007 Or xy1 = 1008 Or xy1 = 1010 Or xy1 = 1011 Or xy1 = 1104 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1107 Or xy1 = 1111 Or xy1 = 1112 Or xy1 = 1203 Or xy1 = 1204 Or xy1 = 1212 Then
                b1 = c(2)
            ElseIf xy1 = 102 Or xy1 = 106 Or xy1 = 108 Or xy1 = 109 Or xy1 = 113 Or xy1 = 201 Or xy1 = 205 Or xy1 = 206 Or xy1 = 208 Or xy1 = 212 Or xy1 = 213 Or xy1 = 305 Or xy1 = 611 Or xy1 = 705 Or xy1 = 710 Or xy1 = 711 Or xy1 = 805 Or xy1 = 806 Or xy1 = 906 Or xy1 = 1102 Or xy1 = 1110 Or xy1 = 1201 Or xy1 = 1202 Or xy1 = 1209 Or xy1 = 1210 Then
                b1 = c(3)
            ElseIf xy1 = 112 Or xy1 = 211 Or xy1 = 501 Or xy1 = 505 Or xy1 = 506 Or xy1 = 601 Or xy1 = 602 Or xy1 = 606 Or xy1 = 813 Or xy1 = 913 Or xy1 = 1009 Or xy1 = 1012 Or xy1 = 1108 Or xy1 = 1109 Or xy1 = 1208 Then
                b1 = c(4)
            ElseIf xy1 = 308 Or xy1 = 407 Or xy1 = 408 Or xy1 = 504 Or xy1 = 605 Or xy1 = 706 Or xy1 = 807 Or xy1 = 808 Or xy1 = 908 Then
                b1 = c(5)
            ElseIf xy1 = 104 Or xy1 = 209 Or xy1 = 510 Or xy1 = 703 Or xy1 = 901 Or xy1 = 1001 Or xy1 = 1101 Or xy1 = 1113 Or xy1 = 1206 Then
                b1 = c(6)
            ElseIf xy1 = 103 Or xy1 = 107 Or xy1 = 313 Or xy1 = 401 Or xy1 = 907 Or xy1 = 1103 Or xy1 = 1213 Then
                b1 = c(7)
            ElseIf xy1 = 105 Or xy1 = 301 Or xy1 = 512 Or xy1 = 905 Or xy1 = 1013 Or xy1 = 1207 Then
                b1 = c(8)
            ElseIf xy1 = 101 Or xy1 = 405 Or xy1 = 413 Or xy1 = 503 Or xy1 = 610 Or xy1 = 1205 Then
                b1 = c(9)
            ElseIf xy1 = 511 Or xy1 = 713 Or xy1 = 1006 Or xy1 = 1211 Then
                b1 = c(10)
            End If
        ElseIf style = 9 Then
            If xy1 = 203 Or xy1 = 210 Or xy1 = 303 Or xy1 = 311 Or xy1 = 403 Or xy1 = 404 Or xy1 = 405 Or xy1 = 406 Or xy1 = 407 Or xy1 = 408 Or xy1 = 409 Or xy1 = 410 Or xy1 = 503 Or xy1 = 603 Or xy1 = 704 Or xy1 = 705 Or xy1 = 706 Or xy1 = 711 Or xy1 = 803 Or xy1 = 807 Or xy1 = 811 Or xy1 = 903 Or xy1 = 907 Or xy1 = 911 Or xy1 = 1003 Or xy1 = 1007 Or xy1 = 1011 Or xy1 = 1103 Or xy1 = 1108 Or xy1 = 1109 Or xy1 = 1110 Then
                b1 = c(1)
            ElseIf xy1 = 102 Or xy1 = 103 Or xy1 = 104 Or xy1 = 109 Or xy1 = 110 Or xy1 = 202 Or xy1 = 204 Or xy1 = 209 Or xy1 = 211 Or xy1 = 302 Or xy1 = 304 Or xy1 = 305 Or xy1 = 306 Or xy1 = 307 Or xy1 = 308 Or xy1 = 309 Or xy1 = 310 Or xy1 = 312 Or xy1 = 402 Or xy1 = 411 Or xy1 = 502 Or xy1 = 504 Or xy1 = 505 Or xy1 = 506 Or xy1 = 507 Or xy1 = 508 Or xy1 = 509 Or xy1 = 510 Or xy1 = 602 Or xy1 = 604 Or xy1 = 605 Or xy1 = 606 Or xy1 = 611 Or xy1 = 703 Or xy1 = 707 Or xy1 = 710 Or xy1 = 712 Or xy1 = 802 Or xy1 = 804 Or xy1 = 805 Or xy1 = 806 Or xy1 = 808 Or xy1 = 810 Or xy1 = 812 Or xy1 = 902 Or xy1 = 904 Or xy1 = 906 Or xy1 = 908 Or xy1 = 910 Or xy1 = 912 Or xy1 = 1002 Or xy1 = 1004 Or xy1 = 1006 Or xy1 = 1008 Or xy1 = 1009 Or xy1 = 1010 Or xy1 = 1012 Or xy1 = 1102 Or xy1 = 1104 Or xy1 = 1107 Or xy1 = 1111 Or xy1 = 1203 Or xy1 = 1208 Or xy1 = 1209 Or xy1 = 1210 Then
                b1 = c(2)
            ElseIf xy1 = 101 Or xy1 = 105 Or xy1 = 108 Or xy1 = 111 Or xy1 = 201 Or xy1 = 205 Or xy1 = 206 Or xy1 = 207 Or xy1 = 208 Or xy1 = 212 Or xy1 = 301 Or xy1 = 313 Or xy1 = 401 Or xy1 = 412 Or xy1 = 501 Or xy1 = 511 Or xy1 = 601 Or xy1 = 607 Or xy1 = 608 Or xy1 = 609 Or xy1 = 610 Or xy1 = 612 Or xy1 = 702 Or xy1 = 708 Or xy1 = 709 Or xy1 = 713 Or xy1 = 801 Or xy1 = 809 Or xy1 = 813 Or xy1 = 901 Or xy1 = 905 Or xy1 = 909 Or xy1 = 913 Or xy1 = 1001 Or xy1 = 1005 Or xy1 = 1013 Or xy1 = 1101 Or xy1 = 1106 Or xy1 = 1112 Or xy1 = 1202 Or xy1 = 1204 Or xy1 = 1207 Or xy1 = 1211 Then
                b1 = c(3)
            ElseIf xy1 = 113 Or xy1 = 1205 Or xy1 = 1213 Then
                b1 = c(4)
            ElseIf xy1 = 107 Or xy1 = 513 Or xy1 = 701 Then
                b1 = c(5)
            ElseIf xy1 = 112 Or xy1 = 1113 Or xy1 = 1206 Then
                b1 = c(6)
            ElseIf xy1 = 106 Or xy1 = 512 Or xy1 = 1201 Then
                b1 = c(7)
            ElseIf xy1 = 1105 Or xy1 = 1212 Then
                b1 = c(8)
            ElseIf xy1 = 213 Or xy1 = 613 Then
                b1 = c(9)
            ElseIf xy1 = 413 Then
                b1 = c(10)
            End If
        ElseIf style = 10 Then
            If xy1 = 713 Or xy1 = 812 Or xy1 = 912 Or xy1 = 913 Or xy1 = 1012 Or xy1 = 1110 Or xy1 = 1212 Or xy1 = 1213 Then
                b1 = c(1)
            ElseIf xy1 = 813 Or xy1 = 911 Or xy1 = 1010 Or xy1 = 1011 Or xy1 = 1013 Or xy1 = 1111 Or xy1 = 1112 Or xy1 = 1113 Or xy1 = 1209 Or xy1 = 1210 Or xy1 = 1211 Then
                b1 = c(2)
            ElseIf xy1 = 113 Or xy1 = 213 Or xy1 = 313 Or xy1 = 413 Or xy1 = 512 Or xy1 = 513 Or xy1 = 612 Or xy1 = 613 Or xy1 = 712 Or xy1 = 811 Then
                b1 = c(3)
            ElseIf xy1 = 110 Or xy1 = 111 Or xy1 = 112 Or xy1 = 210 Or xy1 = 211 Or xy1 = 212 Or xy1 = 310 Or xy1 = 311 Or xy1 = 312 Or xy1 = 410 Or xy1 = 411 Or xy1 = 412 Or xy1 = 509 Or xy1 = 510 Or xy1 = 511 Or xy1 = 609 Or xy1 = 610 Or xy1 = 611 Or xy1 = 708 Or xy1 = 709 Or xy1 = 710 Or xy1 = 711 Or xy1 = 808 Or xy1 = 809 Or xy1 = 810 Or xy1 = 908 Or xy1 = 909 Or xy1 = 910 Or xy1 = 1008 Or xy1 = 1009 Or xy1 = 1108 Or xy1 = 1109 Or xy1 = 1208 Then
                b1 = c(4)
            ElseIf xy1 = 106 Or xy1 = 107 Or xy1 = 108 Or xy1 = 109 Or xy1 = 206 Or xy1 = 207 Or xy1 = 208 Or xy1 = 209 Or xy1 = 306 Or xy1 = 307 Or xy1 = 308 Or xy1 = 309 Or xy1 = 406 Or xy1 = 407 Or xy1 = 408 Or xy1 = 409 Or xy1 = 507 Or xy1 = 508 Or xy1 = 607 Or xy1 = 608 Or xy1 = 707 Or xy1 = 807 Or xy1 = 907 Or xy1 = 1007 Or xy1 = 1106 Or xy1 = 1107 Or xy1 = 1206 Or xy1 = 1207 Then
                b1 = c(5)
            ElseIf xy1 = 205 Or xy1 = 304 Or xy1 = 305 Or xy1 = 403 Or xy1 = 404 Or xy1 = 405 Or xy1 = 504 Or xy1 = 505 Or xy1 = 506 Or xy1 = 605 Or xy1 = 606 Or xy1 = 704 Or xy1 = 705 Or xy1 = 706 Or xy1 = 803 Or xy1 = 804 Or xy1 = 805 Or xy1 = 806 Or xy1 = 902 Or xy1 = 903 Or xy1 = 904 Or xy1 = 905 Or xy1 = 906 Or xy1 = 1004 Or xy1 = 1005 Or xy1 = 1006 Or xy1 = 1105 Then
                b1 = c(6)
            ElseIf xy1 = 101 Or xy1 = 102 Or xy1 = 103 Or xy1 = 201 Or xy1 = 202 Or xy1 = 301 Or xy1 = 401 Or xy1 = 1001 Or xy1 = 1101 Or xy1 = 1201 Or xy1 = 1202 Then
                b1 = c(7)
            ElseIf xy1 = 104 Or xy1 = 105 Or xy1 = 203 Or xy1 = 204 Or xy1 = 302 Or xy1 = 303 Or xy1 = 402 Or xy1 = 502 Or xy1 = 503 Or xy1 = 602 Or xy1 = 604 Or xy1 = 702 Or xy1 = 703 Or xy1 = 1002 Or xy1 = 1102 Or xy1 = 1203 Or xy1 = 1204 Then
                b1 = c(8)
            ElseIf xy1 = 501 Or xy1 = 601 Or xy1 = 701 Or xy1 = 801 Or xy1 = 802 Or xy1 = 901 Or xy1 = 1003 Or xy1 = 1103 Or xy1 = 1104 Or xy1 = 1205 Then
                b1 = c(9)
            ElseIf xy1 = 603 Then
                b1 = c(10)
            End If
        ElseIf style = 11 Then
            If xy1 = 403 Or xy1 = 404 Or xy1 = 405 Or xy1 = 503 Or xy1 = 505 Or xy1 = 603 Or xy1 = 604 Or xy1 = 703 Or xy1 = 705 Or xy1 = 803 Or xy1 = 805 Or xy1 = 904 Or xy1 = 1003 Then
                b1 = c(1)
            ElseIf xy1 = 502 Or xy1 = 602 Or xy1 = 702 Or xy1 = 802 Or xy1 = 804 Or xy1 = 903 Then
                b1 = c(2)
            ElseIf xy1 = 210 Or xy1 = 211 Or xy1 = 310 Or xy1 = 311 Then
                b1 = c(3)
            ElseIf xy1 = 110 Or xy1 = 111 Or xy1 = 209 Or xy1 = 212 Or xy1 = 309 Or xy1 = 312 Or xy1 = 410 Or xy1 = 411 Then
                b1 = c(4)
            ElseIf xy1 = 101 Or xy1 = 102 Or xy1 = 103 Or xy1 = 104 Or xy1 = 105 Or xy1 = 106 Or xy1 = 107 Or xy1 = 108 Or xy1 = 109 Or xy1 = 112 Or xy1 = 201 Or xy1 = 202 Or xy1 = 203 Or xy1 = 204 Or xy1 = 205 Or xy1 = 206 Or xy1 = 207 Or xy1 = 208 Or xy1 = 301 Or xy1 = 302 Or xy1 = 303 Or xy1 = 304 Or xy1 = 305 Or xy1 = 306 Or xy1 = 307 Or xy1 = 308 Or xy1 = 401 Or xy1 = 402 Or xy1 = 501 Or xy1 = 601 Or xy1 = 701 Or xy1 = 801 Or xy1 = 901 Or xy1 = 902 Or xy1 = 1001 Or xy1 = 1002 Or xy1 = 1004 Or xy1 = 1005 Or xy1 = 1006 Or xy1 = 1007 Or xy1 = 1008 Or xy1 = 1009 Or xy1 = 1010 Or xy1 = 1011 Or xy1 = 1012 Or xy1 = 1013 Or xy1 = 1101 Or xy1 = 1102 Or xy1 = 1103 Or xy1 = 1104 Or xy1 = 1105 Or xy1 = 1106 Or xy1 = 1107 Or xy1 = 1108 Or xy1 = 1109 Or xy1 = 1110 Or xy1 = 1111 Or xy1 = 1112 Or xy1 = 1113 Or xy1 = 1201 Or xy1 = 1202 Or xy1 = 1203 Or xy1 = 1204 Or xy1 = 1205 Or xy1 = 1206 Or xy1 = 1207 Or xy1 = 1208 Or xy1 = 1209 Or xy1 = 1210 Or xy1 = 1211 Or xy1 = 1212 Or xy1 = 1213 Then
                b1 = c(5)
            ElseIf xy1 = 504 Or xy1 = 704 Then
                b1 = c(6)
            ElseIf xy1 = 412 Or xy1 = 511 Or xy1 = 512 Or xy1 = 611 Or xy1 = 710 Or xy1 = 711 Or xy1 = 810 Or xy1 = 811 Or xy1 = 812 Or xy1 = 813 Or xy1 = 909 Or xy1 = 910 Or xy1 = 911 Or xy1 = 912 Or xy1 = 913 Then
                b1 = c(7)
            ElseIf xy1 = 313 Or xy1 = 408 Or xy1 = 409 Or xy1 = 413 Or xy1 = 509 Or xy1 = 510 Or xy1 = 513 Or xy1 = 609 Or xy1 = 610 Or xy1 = 612 Or xy1 = 613 Or xy1 = 708 Or xy1 = 709 Or xy1 = 712 Or xy1 = 713 Or xy1 = 808 Or xy1 = 809 Or xy1 = 907 Or xy1 = 908 Then
                b1 = c(8)
            ElseIf xy1 = 113 Or xy1 = 213 Or xy1 = 406 Or xy1 = 407 Or xy1 = 508 Or xy1 = 608 Or xy1 = 706 Or xy1 = 707 Or xy1 = 806 Or xy1 = 807 Or xy1 = 905 Or xy1 = 906 Then
                b1 = c(9)
            ElseIf xy1 = 506 Or xy1 = 507 Or xy1 = 605 Or xy1 = 606 Or xy1 = 607 Then
                b1 = c(10)
            End If

        End If
    End Sub
    '2P所踩顏色 (function)
    Private Sub color2()
        If style = 0 Then
            If xy2 = 102 Or xy2 = 502 Or xy2 = 902 Then
                b2 = c(1)
            ElseIf xy2 = 202 Or xy2 = 602 Or xy2 = 1002 Then
                b2 = c(2)
            ElseIf xy2 = 302 Or xy2 = 402 Or xy2 = 702 Or xy2 = 802 Or xy2 = 1102 Or xy2 = 1202 Or xy2 = 112 Or xy2 = 212 Or xy2 = 512 Or xy2 = 612 Or xy2 = 912 Or xy2 = 1012 Then
                b2 = c(3)
            ElseIf xy2 = 312 Or xy2 = 712 Or xy2 = 1112 Then
                b2 = c(4)
            ElseIf xy2 = 412 Or xy2 = 812 Or xy2 = 1212 Then
                b2 = c(5)
            ElseIf xy2 = 107 Or xy2 = 307 Or xy2 = 507 Or xy2 = 707 Or xy2 = 1107 Or xy2 = 1207 Then
                b2 = c(6)
            ElseIf xy2 = 107 Or xy2 = 307 Or xy2 = 507 Or xy2 = 707 Or xy2 = 1107 Or xy2 = 1207 Then
                b2 = c(6)
            ElseIf xy2 = 204 Or xy2 = 404 Or xy2 = 504 Or xy2 = 604 Or xy2 = 804 Or xy2 = 1004 Or xy2 = 210 Or xy2 = 410 Or xy2 = 510 Or xy2 = 610 Or xy2 = 1010 Then
                b2 = c(7)
            ElseIf xy2 >= 205 And xy2 <= 209 Or xy2 >= 405 And xy2 <= 409 Or xy2 >= 605 And xy2 <= 609 Or xy2 >= 805 And xy2 <= 807 Or xy2 >= 1005 And xy2 <= 1009 Or xy2 = 505 Or xy2 = 509 Or xy2 = 907 Then
                b2 = c(8)
            ElseIf xy2 = 1201 Or xy2 = 1213 Or xy2 = 101 Or xy2 = 113 Then
                b2 = c(10)
            Else
                b2 = c(9)
            End If
        ElseIf style = 1 Then
            If xy2 = 606 Or xy2 = 607 Or xy2 = 706 Or xy2 = 707 Then
                b2 = c(1)
            ElseIf xy2 = 506 Or xy2 = 507 Or xy2 = 605 Or xy2 = 608 Or xy2 = 705 Or xy2 = 708 Or xy2 = 806 Or xy2 = 807 Then
                b2 = c(2)
            ElseIf xy2 = 505 Or xy2 = 508 Or xy2 = 805 Or xy2 = 808 Then
                b2 = c(3)
            ElseIf xy2 = 405 Or xy2 = 406 Or xy2 = 407 Or xy2 = 408 Or xy2 = 504 Or xy2 = 509 Or xy2 = 604 Or xy2 = 609 Or xy2 = 704 Or xy2 = 709 Or xy2 = 804 Or xy2 = 809 Or xy2 = 905 Or xy2 = 906 Or xy2 = 907 Or xy2 = 908 Then
                b2 = c(4)
            ElseIf xy2 = 306 Or xy2 = 307 Or xy2 = 404 Or xy2 = 409 Or xy2 = 603 Or xy2 = 610 Or xy2 = 703 Or xy2 = 710 Or xy2 = 904 Or xy2 = 909 Or xy2 = 1006 Or xy2 = 1007 Then
                b2 = c(5)
            ElseIf xy2 = 101 Or xy2 = 113 Or xy2 = 1201 Or xy2 = 1213 Then
                b2 = c(6)
            ElseIf xy2 = 102 Or xy2 = 112 Or xy2 = 201 Or xy2 = 213 Or xy2 = 1101 Or xy2 = 1113 Or xy2 = 1202 Or xy2 = 1212 Then
                b2 = c(7)
            ElseIf xy2 = 103 Or xy2 = 104 Or xy2 = 110 Or xy2 = 111 Or xy2 = 202 Or xy2 = 212 Or xy2 = 301 Or xy2 = 313 Or xy2 = 401 Or xy2 = 413 Or xy2 = 901 Or xy2 = 913 Or xy2 = 1001 Or xy2 = 1013 Or xy2 = 1102 Or xy2 = 1112 Or xy2 = 1203 Or xy2 = 1204 Or xy2 = 1210 Or xy2 = 1211 Then
                b2 = c(8)
            ElseIf xy2 = 105 Or xy2 = 109 Or xy2 = 203 Or xy2 = 211 Or xy2 = 302 Or xy2 = 312 Or xy2 = 501 Or xy2 = 513 Or xy2 = 801 Or xy2 = 813 Or xy2 = 1002 Or xy2 = 1012 Or xy2 = 1103 Or xy2 = 1111 Or xy2 = 1205 Or xy2 = 1209 Then
                b2 = c(9)
            ElseIf xy2 = 106 Or xy2 = 107 Or xy2 = 108 Or xy2 = 204 Or xy2 = 205 Or xy2 = 206 Or xy2 = 207 Or xy2 = 208 Or xy2 = 209 Or xy2 = 210 Or xy2 = 303 Or xy2 = 304 Or xy2 = 305 Or xy2 = 308 Or xy2 = 309 Or xy2 = 310 Or xy2 = 311 Or xy2 = 402 Or xy2 = 403 Or xy2 = 410 Or xy2 = 411 Or xy2 = 412 Or xy2 = 502 Or xy2 = 503 Or xy2 = 510 Or xy2 = 511 Or xy2 = 512 Or xy2 = 601 Or xy2 = 602 Or xy2 = 611 Or xy2 = 612 Or xy2 = 613 Or xy2 = 701 Or xy2 = 702 Or xy2 = 711 Or xy2 = 712 Or xy2 = 713 Or xy2 = 802 Or xy2 = 803 Or xy2 = 810 Or xy2 = 811 Or xy2 = 812 Or xy2 = 902 Or xy2 = 903 Or xy2 = 910 Or xy2 = 911 Or xy2 = 912 Or xy2 = 1003 Or xy2 = 1004 Or xy2 = 1005 Or xy2 = 1008 Or xy2 = 1009 Or xy2 = 1010 Or xy2 = 1011 Or xy2 = 1104 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1107 Or xy2 = 1108 Or xy2 = 1109 Or xy2 = 1110 Or xy2 = 1206 Or xy2 = 1207 Or xy2 = 1208 Then
                b2 = c(10)
            End If
        ElseIf style = 2 Then
            If xy2 = 1205 Or xy2 = 1206 Or xy2 = 1207 Or xy2 = 1208 Or xy2 = 1209 Then
                b2 = c(1)
            ElseIf xy2 = 101 Or xy2 = 102 Or xy2 = 103 Or xy2 = 104 Or xy2 = 105 Or xy2 = 106 Or xy2 = 107 Or xy2 = 108 Or xy2 = 109 Or xy2 = 110 Or xy2 = 111 Or xy2 = 112 Or xy2 = 201 Or xy2 = 207 Or xy2 = 208 Or xy2 = 210 Or xy2 = 211 Or xy2 = 301 Or xy2 = 303 Or xy2 = 304 Or xy2 = 305 Or xy2 = 307 Or xy2 = 309 Or xy2 = 311 Or xy2 = 401 Or xy2 = 402 Or xy2 = 406 Or xy2 = 407 Or xy2 = 409 Or xy2 = 412 Or xy2 = 501 Or xy2 = 502 Or xy2 = 503 Or xy2 = 504 Or xy2 = 505 Or xy2 = 506 Or xy2 = 507 Or xy2 = 508 Or xy2 = 509 Or xy2 = 510 Or xy2 = 511 Or xy2 = 512 Or xy2 = 601 Or xy2 = 603 Or xy2 = 604 Or xy2 = 607 Or xy2 = 701 Or xy2 = 703 Or xy2 = 705 Or xy2 = 707 Or xy2 = 708 Or xy2 = 709 Or xy2 = 711 Or xy2 = 712 Or xy2 = 801 Or xy2 = 804 Or xy2 = 805 Or xy2 = 807 Or xy2 = 902 Or xy2 = 903 Or xy2 = 904 Or xy2 = 905 Or xy2 = 906 Or xy2 = 907 Or xy2 = 908 Or xy2 = 909 Or xy2 = 910 Or xy2 = 911 Or xy2 = 912 Or xy2 = 1003 Or xy2 = 1004 Or xy2 = 1005 Or xy2 = 1006 Or xy2 = 1007 Or xy2 = 1008 Or xy2 = 1009 Or xy2 = 1010 Or xy2 = 1011 Or xy2 = 1104 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1107 Or xy2 = 1108 Or xy2 = 1109 Or xy2 = 1110 Then
                b2 = c(2)
            ElseIf xy2 = 1204 Then
                b2 = c(3)
            ElseIf xy2 = 202 Or xy2 = 203 Or xy2 = 204 Or xy2 = 205 Or xy2 = 206 Or xy2 = 302 Or xy2 = 306 Or xy2 = 403 Or xy2 = 404 Or xy2 = 405 Or xy2 = 1103 Or xy2 = 1203 Then
                b2 = c(4)
            ElseIf xy2 = 1002 Or xy2 = 1102 Or xy2 = 1202 Then
                b2 = c(5)
            ElseIf xy2 = 209 Or xy2 = 212 Or xy2 = 308 Or xy2 = 310 Or xy2 = 312 Or xy2 = 408 Or xy2 = 410 Or xy2 = 411 Or xy2 = 901 Or xy2 = 1001 Or xy2 = 1101 Or xy2 = 1201 Then
                b2 = c(6)
            ElseIf xy2 = 113 Or xy2 = 213 Or xy2 = 313 Or xy2 = 413 Or xy2 = 513 Or xy2 = 613 Or xy2 = 713 Or xy2 = 813 Or xy2 = 913 Or xy2 = 1013 Or xy2 = 1113 Or xy2 = 1213 Then
                b2 = c(7)
            ElseIf xy2 = 602 Or xy2 = 605 Or xy2 = 606 Or xy2 = 702 Or xy2 = 704 Or xy2 = 706 Or xy2 = 802 Or xy2 = 803 Or xy2 = 806 Or xy2 = 1012 Or xy2 = 1112 Or xy2 = 1212 Then
                b2 = c(8)
            ElseIf xy2 = 1210 Then
                b2 = c(9)
            ElseIf xy2 = 608 Or xy2 = 609 Or xy2 = 610 Or xy2 = 611 Or xy2 = 612 Or xy2 = 710 Or xy2 = 808 Or xy2 = 809 Or xy2 = 810 Or xy2 = 811 Or xy2 = 812 Or xy2 = 1111 Or xy2 = 1211 Then
                b2 = c(10)
            End If
        ElseIf style = 3 Then
            If xy2 = 202 Or xy2 = 206 Or xy2 = 302 Or xy2 = 303 Or xy2 = 304 Or xy2 = 305 Or xy2 = 306 Or xy2 = 402 Or xy2 = 406 Then
                b2 = c(1)
            ElseIf xy2 = 602 Or xy2 = 603 Or xy2 = 604 Or xy2 = 702 Or xy2 = 703 Or xy2 = 704 Or xy2 = 705 Or xy2 = 804 Or xy2 = 805 Or xy2 = 806 Or xy2 = 902 Or xy2 = 903 Or xy2 = 904 Or xy2 = 905 Or xy2 = 1002 Or xy2 = 1003 Or xy2 = 1004 Then
                b2 = c(2)
            ElseIf xy2 = 208 Or xy2 = 209 Or xy2 = 310 Or xy2 = 311 Or xy2 = 412 Or xy2 = 510 Or xy2 = 511 Or xy2 = 608 Or xy2 = 609 Then
                b2 = c(3)
            ElseIf xy2 = 808 Or xy2 = 809 Or xy2 = 810 Or xy2 = 811 Or xy2 = 812 Or xy2 = 910 Or xy2 = 912 Or xy2 = 1010 Or xy2 = 1012 Or xy2 = 1110 Or xy2 = 1111 Or xy2 = 1112 Then
                b2 = c(4)
            ElseIf xy2 = 101 Or xy2 = 113 Or xy2 = 1201 Or xy2 = 1213 Then
                b2 = c(5)
            ElseIf xy2 = 201 Or xy2 = 301 Or xy2 = 401 Or xy2 = 501 Or xy2 = 601 Or xy2 = 701 Or xy2 = 801 Or xy2 = 901 Or xy2 = 1001 Or xy2 = 1101 Then
                b2 = c(6)
            ElseIf xy2 = 102 Or xy2 = 103 Or xy2 = 104 Or xy2 = 105 Or xy2 = 106 Or xy2 = 107 Or xy2 = 108 Or xy2 = 109 Or xy2 = 110 Or xy2 = 111 Or xy2 = 112 Then
                b2 = c(7)
            ElseIf xy2 = 213 Or xy2 = 313 Or xy2 = 413 Or xy2 = 513 Or xy2 = 613 Or xy2 = 713 Or xy2 = 813 Or xy2 = 913 Or xy2 = 1013 Or xy2 = 1113 Then
                b2 = c(8)
            ElseIf xy2 = 1202 Or xy2 = 1203 Or xy2 = 1204 Or xy2 = 1205 Or xy2 = 1206 Or xy2 = 1207 Or xy2 = 1208 Or xy2 = 1209 Or xy2 = 1210 Or xy2 = 1211 Or xy2 = 1212 Then
                b2 = c(9)
            ElseIf xy2 = 203 Or xy2 = 204 Or xy2 = 205 Or xy2 = 207 Or xy2 = 210 Or xy2 = 211 Or xy2 = 212 Or xy2 = 307 Or xy2 = 308 Or xy2 = 309 Or xy2 = 312 Or xy2 = 403 Or xy2 = 404 Or xy2 = 405 Or xy2 = 407 Or xy2 = 408 Or xy2 = 409 Or xy2 = 410 Or xy2 = 411 Or xy2 = 502 Or xy2 = 503 Or xy2 = 504 Or xy2 = 505 Or xy2 = 506 Or xy2 = 507 Or xy2 = 508 Or xy2 = 509 Or xy2 = 512 Or xy2 = 605 Or xy2 = 606 Or xy2 = 607 Or xy2 = 610 Or xy2 = 611 Or xy2 = 612 Or xy2 = 706 Or xy2 = 707 Or xy2 = 708 Or xy2 = 709 Or xy2 = 710 Or xy2 = 711 Or xy2 = 712 Or xy2 = 802 Or xy2 = 803 Or xy2 = 807 Or xy2 = 906 Or xy2 = 907 Or xy2 = 908 Or xy2 = 909 Or xy2 = 911 Or xy2 = 1005 Or xy2 = 1006 Or xy2 = 1007 Or xy2 = 1008 Or xy2 = 1009 Or xy2 = 1011 Or xy2 = 1102 Or xy2 = 1103 Or xy2 = 1104 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1107 Or xy2 = 1108 Or xy2 = 1109 Then
                b2 = c(10)
            End If
        ElseIf style = 4 Then
            If xy2 = 107 Or xy2 = 113 Or xy2 = 213 Or xy2 = 313 Or xy2 = 407 Or xy2 = 507 Or xy2 = 607 Or xy2 = 613 Or xy2 = 713 Or xy2 = 813 Or xy2 = 1107 Or xy2 = 1113 Or xy2 = 1208 Then
                b2 = c(1)
            ElseIf xy2 = 101 Or xy2 = 102 Or xy2 = 103 Or xy2 = 104 Or xy2 = 105 Or xy2 = 106 Or xy2 = 108 Or xy2 = 109 Or xy2 = 110 Or xy2 = 111 Or xy2 = 112 Or xy2 = 201 Or xy2 = 207 Or xy2 = 209 Or xy2 = 301 Or xy2 = 302 Or xy2 = 303 Or xy2 = 304 Or xy2 = 305 Or xy2 = 306 Or xy2 = 307 Or xy2 = 309 Or xy2 = 311 Or xy2 = 401 Or xy2 = 411 Or xy2 = 413 Or xy2 = 501 Or xy2 = 503 Or xy2 = 504 Or xy2 = 505 Or xy2 = 508 Or xy2 = 509 Or xy2 = 510 Or xy2 = 511 Or xy2 = 512 Or xy2 = 513 Or xy2 = 601 Or xy2 = 701 Or xy2 = 702 Or xy2 = 703 Or xy2 = 704 Or xy2 = 705 Or xy2 = 706 Or xy2 = 707 Or xy2 = 709 Or xy2 = 711 Or xy2 = 801 Or xy2 = 805 Or xy2 = 806 Or xy2 = 807 Or xy2 = 901 Or xy2 = 902 Or xy2 = 903 Or xy2 = 905 Or xy2 = 906 Or xy2 = 907 Or xy2 = 908 Or xy2 = 909 Or xy2 = 910 Or xy2 = 911 Or xy2 = 912 Or xy2 = 913 Or xy2 = 1001 Or xy2 = 1007 Or xy2 = 1011 Or xy2 = 1013 Or xy2 = 1101 Or xy2 = 1102 Or xy2 = 1103 Or xy2 = 1104 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1108 Or xy2 = 1109 Or xy2 = 1110 Or xy2 = 1111 Or xy2 = 1112 Or xy2 = 1201 Or xy2 = 1202 Or xy2 = 1203 Or xy2 = 1204 Or xy2 = 1205 Or xy2 = 1206 Or xy2 = 1207 Or xy2 = 1209 Or xy2 = 1210 Or xy2 = 1211 Or xy2 = 1212 Or xy2 = 1213 Then
                b2 = c(2)
            ElseIf xy2 = 208 Or xy2 = 210 Or xy2 = 211 Or xy2 = 212 Or xy2 = 308 Or xy2 = 310 Or xy2 = 312 Or xy2 = 408 Or xy2 = 409 Or xy2 = 410 Or xy2 = 412 Then
                b2 = c(3)
            ElseIf xy2 = 608 Or xy2 = 609 Or xy2 = 610 Or xy2 = 611 Or xy2 = 612 Or xy2 = 708 Or xy2 = 710 Or xy2 = 712 Or xy2 = 808 Or xy2 = 809 Or xy2 = 810 Or xy2 = 811 Or xy2 = 812 Then
                b2 = c(4)
            ElseIf xy2 = 1008 Or xy2 = 1009 Or xy2 = 1010 Or xy2 = 1012 Then
                b2 = c(5)
            ElseIf xy2 = 802 Or xy2 = 803 Or xy2 = 804 Or xy2 = 904 Or xy2 = 1002 Or xy2 = 1003 Or xy2 = 1004 Or xy2 = 1005 Or xy2 = 1006 Then
                b2 = c(6)
            ElseIf xy2 = 202 Or xy2 = 402 Or xy2 = 502 Or xy2 = 602 Then
                b2 = c(7)
            ElseIf xy2 = 204 Or xy2 = 404 Or xy2 = 604 Then
                b2 = c(8)
            ElseIf xy2 = 203 Or xy2 = 403 Or xy2 = 603 Then
                b2 = c(9)
            ElseIf xy2 = 205 Or xy2 = 206 Or xy2 = 405 Or xy2 = 406 Or xy2 = 506 Or xy2 = 605 Or xy2 = 606 Then
                b2 = c(10)
            End If
        ElseIf style = 5 Then
            If xy2 = 202 Or xy2 = 203 Or xy2 = 204 Or xy2 = 205 Or xy2 = 206 Then
                b2 = c(1)
            ElseIf xy2 = 402 Or xy2 = 403 Or xy2 = 404 Or xy2 = 405 Or xy2 = 406 Or xy2 = 502 Or xy2 = 506 Or xy2 = 602 Or xy2 = 603 Or xy2 = 604 Or xy2 = 605 Or xy2 = 606 Then
                b2 = c(2)
            ElseIf xy2 = 802 Or xy2 = 803 Or xy2 = 804 Or xy2 = 904 Or xy2 = 1002 Or xy2 = 1003 Or xy2 = 1004 Or xy2 = 1005 Or xy2 = 1006 Then
                b2 = c(3)
            ElseIf xy2 = 208 Or xy2 = 210 Or xy2 = 212 Or xy2 = 308 Or xy2 = 310 Or xy2 = 312 Or xy2 = 408 Or xy2 = 409 Or xy2 = 410 Or xy2 = 411 Or xy2 = 412 Then
                b2 = c(4)
            ElseIf xy2 = 708 Or xy2 = 709 Or xy2 = 710 Or xy2 = 711 Or xy2 = 712 Or xy2 = 808 Or xy2 = 812 Or xy2 = 908 Or xy2 = 909 Or xy2 = 910 Or xy2 = 911 Or xy2 = 912 Then
                b2 = c(5)
            ElseIf xy2 = 1108 Or xy2 = 1109 Or xy2 = 1110 Or xy2 = 1112 Then
                b2 = c(6)
            ElseIf xy2 = 107 Or xy2 = 108 Or xy2 = 109 Or xy2 = 110 Or xy2 = 111 Or xy2 = 112 Or xy2 = 207 Or xy2 = 209 Or xy2 = 211 Or xy2 = 307 Or xy2 = 309 Or xy2 = 311 Or xy2 = 407 Or xy2 = 507 Or xy2 = 508 Or xy2 = 509 Or xy2 = 510 Or xy2 = 511 Or xy2 = 512 Or xy2 = 607 Or xy2 = 608 Or xy2 = 609 Or xy2 = 610 Or xy2 = 611 Or xy2 = 612 Or xy2 = 707 Or xy2 = 807 Or xy2 = 907 Or xy2 = 1007 Or xy2 = 1008 Or xy2 = 1009 Or xy2 = 1010 Or xy2 = 1011 Or xy2 = 1012 Or xy2 = 1107 Or xy2 = 1111 Or xy2 = 1207 Or xy2 = 1208 Or xy2 = 1209 Or xy2 = 1210 Or xy2 = 1211 Or xy2 = 1212 Then
                b2 = c(7)
            ElseIf xy2 = 102 Or xy2 = 103 Or xy2 = 104 Or xy2 = 105 Or xy2 = 106 Or xy2 = 302 Or xy2 = 303 Or xy2 = 304 Or xy2 = 305 Or xy2 = 306 Or xy2 = 503 Or xy2 = 504 Or xy2 = 505 Or xy2 = 702 Or xy2 = 703 Or xy2 = 704 Or xy2 = 705 Or xy2 = 706 Or xy2 = 805 Or xy2 = 806 Or xy2 = 902 Or xy2 = 903 Or xy2 = 905 Or xy2 = 906 Or xy2 = 1102 Or xy2 = 1103 Or xy2 = 1104 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1202 Or xy2 = 1203 Or xy2 = 1204 Or xy2 = 1205 Or xy2 = 1206 Then
                b2 = c(8)
            ElseIf xy2 = 809 Or xy2 = 810 Or xy2 = 811 Then
                b2 = c(9)
            ElseIf xy2 = 101 Or xy2 = 113 Or xy2 = 201 Or xy2 = 213 Or xy2 = 301 Or xy2 = 313 Or xy2 = 401 Or xy2 = 413 Or xy2 = 501 Or xy2 = 513 Or xy2 = 601 Or xy2 = 613 Or xy2 = 701 Or xy2 = 713 Or xy2 = 801 Or xy2 = 813 Or xy2 = 901 Or xy2 = 913 Or xy2 = 1001 Or xy2 = 1013 Or xy2 = 1101 Or xy2 = 1113 Or xy2 = 1201 Or xy2 = 1213 Then
                b2 = c(10)
            End If
        ElseIf style = 6 Then
            If xy2 = 202 Or xy2 = 203 Or xy2 = 204 Or xy2 = 205 Or xy2 = 206 Then
                b2 = c(1)
            ElseIf xy2 = 402 Or xy2 = 403 Or xy2 = 404 Or xy2 = 405 Or xy2 = 406 Or xy2 = 502 Or xy2 = 506 Or xy2 = 602 Or xy2 = 603 Or xy2 = 604 Or xy2 = 605 Or xy2 = 606 Then
                b2 = c(2)
            ElseIf xy2 = 802 Or xy2 = 803 Or xy2 = 804 Or xy2 = 904 Or xy2 = 1002 Or xy2 = 1003 Or xy2 = 1004 Or xy2 = 1005 Or xy2 = 1006 Then
                b2 = c(3)
            ElseIf xy2 = 208 Or xy2 = 210 Or xy2 = 211 Or xy2 = 212 Or xy2 = 308 Or xy2 = 310 Or xy2 = 312 Or xy2 = 408 Or xy2 = 409 Or xy2 = 410 Or xy2 = 412 Then
                b2 = c(4)
            ElseIf xy2 = 608 Or xy2 = 609 Or xy2 = 708 Or xy2 = 808 Or xy2 = 809 Or xy2 = 810 Or xy2 = 811 Or xy2 = 812 Then
                b2 = c(5)
            ElseIf xy2 = 1008 Or xy2 = 1009 Or xy2 = 1010 Or xy2 = 1012 Then
                b2 = c(6)
            ElseIf xy2 = 108 Or xy2 = 109 Or xy2 = 110 Or xy2 = 111 Or xy2 = 112 Or xy2 = 209 Or xy2 = 309 Or xy2 = 311 Or xy2 = 411 Or xy2 = 508 Or xy2 = 509 Or xy2 = 510 Or xy2 = 511 Or xy2 = 512 Or xy2 = 610 Or xy2 = 611 Or xy2 = 612 Or xy2 = 709 Or xy2 = 710 Or xy2 = 711 Or xy2 = 712 Or xy2 = 908 Or xy2 = 909 Or xy2 = 910 Or xy2 = 911 Or xy2 = 912 Or xy2 = 1011 Or xy2 = 1108 Or xy2 = 1109 Or xy2 = 1110 Or xy2 = 1111 Or xy2 = 1112 Or xy2 = 1208 Or xy2 = 1209 Or xy2 = 1210 Or xy2 = 1211 Or xy2 = 1212 Then
                b2 = c(7)
            ElseIf xy2 = 102 Or xy2 = 103 Or xy2 = 104 Or xy2 = 105 Or xy2 = 106 Or xy2 = 302 Or xy2 = 303 Or xy2 = 304 Or xy2 = 305 Or xy2 = 306 Or xy2 = 503 Or xy2 = 504 Or xy2 = 505 Or xy2 = 702 Or xy2 = 703 Or xy2 = 704 Or xy2 = 705 Or xy2 = 706 Or xy2 = 805 Or xy2 = 806 Or xy2 = 902 Or xy2 = 903 Or xy2 = 905 Or xy2 = 906 Or xy2 = 1102 Or xy2 = 1103 Or xy2 = 1104 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1202 Or xy2 = 1203 Or xy2 = 1204 Or xy2 = 1205 Or xy2 = 1206 Then
                b2 = c(8)
            ElseIf xy2 = 107 Or xy2 = 201 Or xy2 = 213 Or xy2 = 307 Or xy2 = 401 Or xy2 = 413 Or xy2 = 507 Or xy2 = 601 Or xy2 = 613 Or xy2 = 707 Or xy2 = 801 Or xy2 = 813 Or xy2 = 907 Or xy2 = 1001 Or xy2 = 1013 Or xy2 = 1107 Or xy2 = 1201 Or xy2 = 1213 Then
                b2 = c(9)
            ElseIf xy2 = 101 Or xy2 = 113 Or xy2 = 207 Or xy2 = 301 Or xy2 = 313 Or xy2 = 407 Or xy2 = 501 Or xy2 = 513 Or xy2 = 607 Or xy2 = 701 Or xy2 = 713 Or xy2 = 807 Or xy2 = 901 Or xy2 = 913 Or xy2 = 1007 Or xy2 = 1101 Or xy2 = 1113 Or xy2 = 1207 Then
                b2 = c(10)
            End If
        ElseIf style = 7 Then
            If xy2 = 303 Or xy2 = 304 Or xy2 = 305 Or xy2 = 306 Or xy2 = 403 Or xy2 = 407 Or xy2 = 503 Or xy2 = 506 Or xy2 = 508 Or xy2 = 509 Or xy2 = 511 Or xy2 = 603 Or xy2 = 604 Or xy2 = 605 Or xy2 = 608 Or xy2 = 611 Or xy2 = 703 Or xy2 = 706 Or xy2 = 708 Or xy2 = 709 Or xy2 = 711 Or xy2 = 803 Or xy2 = 807 Or xy2 = 903 Or xy2 = 904 Or xy2 = 905 Or xy2 = 906 Then
                b2 = c(1)
            ElseIf xy2 = 101 Or xy2 = 111 Or xy2 = 202 Or xy2 = 210 Or xy2 = 309 Or xy2 = 408 Or xy2 = 808 Or xy2 = 909 Or xy2 = 1002 Or xy2 = 1010 Or xy2 = 1101 Or xy2 = 1111 Then
                b2 = c(2)
            ElseIf xy2 = 404 Or xy2 = 405 Or xy2 = 406 Or xy2 = 504 Or xy2 = 505 Or xy2 = 507 Or xy2 = 606 Or xy2 = 607 Or xy2 = 704 Or xy2 = 705 Or xy2 = 707 Or xy2 = 804 Or xy2 = 805 Or xy2 = 806 Then
                b2 = c(3)
            ElseIf xy2 = 110 Or xy2 = 112 Or xy2 = 209 Or xy2 = 211 Or xy2 = 308 Or xy2 = 310 Or xy2 = 409 Or xy2 = 809 Or xy2 = 908 Or xy2 = 910 Or xy2 = 1009 Or xy2 = 1011 Or xy2 = 1110 Or xy2 = 1112 Or xy2 = 1212 Or xy2 = 1213 Then
                b2 = c(4)
            ElseIf xy2 = 102 Or xy2 = 201 Or xy2 = 203 Or xy2 = 302 Or xy2 = 902 Or xy2 = 1001 Or xy2 = 1003 Or xy2 = 1102 Or xy2 = 1201 Then
                b2 = c(5)
            ElseIf xy2 = 103 Or xy2 = 109 Or xy2 = 113 Or xy2 = 204 Or xy2 = 205 Or xy2 = 206 Or xy2 = 207 Or xy2 = 208 Or xy2 = 212 Or xy2 = 301 Or xy2 = 307 Or xy2 = 311 Or xy2 = 402 Or xy2 = 410 Or xy2 = 411 Or xy2 = 502 Or xy2 = 510 Or xy2 = 602 Or xy2 = 609 Or xy2 = 610 Or xy2 = 702 Or xy2 = 710 Or xy2 = 802 Or xy2 = 810 Or xy2 = 811 Or xy2 = 901 Or xy2 = 907 Or xy2 = 911 Or xy2 = 1004 Or xy2 = 1005 Or xy2 = 1006 Or xy2 = 1007 Or xy2 = 1008 Or xy2 = 1012 Or xy2 = 1103 Or xy2 = 1109 Or xy2 = 1113 Or xy2 = 1202 Or xy2 = 1210 Or xy2 = 1211 Then
                b2 = c(6)
            ElseIf xy2 = 104 Or xy2 = 108 Or xy2 = 213 Or xy2 = 312 Or xy2 = 401 Or xy2 = 801 Or xy2 = 912 Or xy2 = 1013 Or xy2 = 1104 Or xy2 = 1108 Or xy2 = 1203 Or xy2 = 1209 Then
                b2 = c(7)
            ElseIf xy2 = 105 Or xy2 = 107 Or xy2 = 313 Or xy2 = 412 Or xy2 = 413 Or xy2 = 501 Or xy2 = 512 Or xy2 = 513 Or xy2 = 612 Or xy2 = 701 Or xy2 = 712 Or xy2 = 713 Or xy2 = 812 Or xy2 = 813 Or xy2 = 913 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1107 Or xy2 = 1204 Or xy2 = 1205 Or xy2 = 1207 Or xy2 = 1208 Then
                b2 = c(8)
            ElseIf xy2 = 601 Or xy2 = 613 Then
                b2 = c(9)
            ElseIf xy2 = 106 Or xy2 = 1206 Then
                b2 = c(10)
            End If
        ElseIf style = 8 Then
            If xy2 = 202 Or xy2 = 203 Or xy2 = 204 Or xy2 = 302 Or xy2 = 303 Or xy2 = 304 Or xy2 = 309 Or xy2 = 310 Or xy2 = 311 Or xy2 = 312 Or xy2 = 402 Or xy2 = 403 Or xy2 = 404 Or xy2 = 409 Or xy2 = 410 Or xy2 = 411 Or xy2 = 412 Or xy2 = 507 Or xy2 = 508 Or xy2 = 509 Or xy2 = 607 Or xy2 = 608 Or xy2 = 609 Or xy2 = 707 Or xy2 = 708 Or xy2 = 709 Or xy2 = 802 Or xy2 = 803 Or xy2 = 804 Or xy2 = 809 Or xy2 = 810 Or xy2 = 811 Or xy2 = 812 Or xy2 = 902 Or xy2 = 903 Or xy2 = 904 Or xy2 = 909 Or xy2 = 910 Or xy2 = 911 Or xy2 = 912 Or xy2 = 1002 Or xy2 = 1003 Or xy2 = 1004 Then
                b2 = c(1)
            ElseIf xy2 = 110 Or xy2 = 111 Or xy2 = 207 Or xy2 = 210 Or xy2 = 306 Or xy2 = 307 Or xy2 = 406 Or xy2 = 502 Or xy2 = 513 Or xy2 = 603 Or xy2 = 604 Or xy2 = 612 Or xy2 = 613 Or xy2 = 701 Or xy2 = 702 Or xy2 = 704 Or xy2 = 712 Or xy2 = 801 Or xy2 = 1005 Or xy2 = 1007 Or xy2 = 1008 Or xy2 = 1010 Or xy2 = 1011 Or xy2 = 1104 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1107 Or xy2 = 1111 Or xy2 = 1112 Or xy2 = 1203 Or xy2 = 1204 Or xy2 = 1212 Then
                b2 = c(2)
            ElseIf xy2 = 102 Or xy2 = 106 Or xy2 = 108 Or xy2 = 109 Or xy2 = 113 Or xy2 = 201 Or xy2 = 205 Or xy2 = 206 Or xy2 = 208 Or xy2 = 212 Or xy2 = 213 Or xy2 = 305 Or xy2 = 611 Or xy2 = 705 Or xy2 = 710 Or xy2 = 711 Or xy2 = 805 Or xy2 = 806 Or xy2 = 906 Or xy2 = 1102 Or xy2 = 1110 Or xy2 = 1201 Or xy2 = 1202 Or xy2 = 1209 Or xy2 = 1210 Then
                b2 = c(3)
            ElseIf xy2 = 112 Or xy2 = 211 Or xy2 = 501 Or xy2 = 505 Or xy2 = 506 Or xy2 = 601 Or xy2 = 602 Or xy2 = 606 Or xy2 = 813 Or xy2 = 913 Or xy2 = 1009 Or xy2 = 1012 Or xy2 = 1108 Or xy2 = 1109 Or xy2 = 1208 Then
                b2 = c(4)
            ElseIf xy2 = 308 Or xy2 = 407 Or xy2 = 408 Or xy2 = 504 Or xy2 = 605 Or xy2 = 706 Or xy2 = 807 Or xy2 = 808 Or xy2 = 908 Then
                b2 = c(5)
            ElseIf xy2 = 104 Or xy2 = 209 Or xy2 = 510 Or xy2 = 703 Or xy2 = 901 Or xy2 = 1001 Or xy2 = 1101 Or xy2 = 1113 Or xy2 = 1206 Then
                b2 = c(6)
            ElseIf xy2 = 103 Or xy2 = 107 Or xy2 = 313 Or xy2 = 401 Or xy2 = 907 Or xy2 = 1103 Or xy2 = 1213 Then
                b2 = c(7)
            ElseIf xy2 = 105 Or xy2 = 301 Or xy2 = 512 Or xy2 = 905 Or xy2 = 1013 Or xy2 = 1207 Then
                b2 = c(8)
            ElseIf xy2 = 101 Or xy2 = 405 Or xy2 = 413 Or xy2 = 503 Or xy2 = 610 Or xy2 = 1205 Then
                b2 = c(9)
            ElseIf xy2 = 511 Or xy2 = 713 Or xy2 = 1006 Or xy2 = 1211 Then
                b2 = c(10)
            End If
        ElseIf style = 9 Then
            If xy2 = 203 Or xy2 = 210 Or xy2 = 303 Or xy2 = 311 Or xy2 = 403 Or xy2 = 404 Or xy2 = 405 Or xy2 = 406 Or xy2 = 407 Or xy2 = 408 Or xy2 = 409 Or xy2 = 410 Or xy2 = 503 Or xy2 = 603 Or xy2 = 704 Or xy2 = 705 Or xy2 = 706 Or xy2 = 711 Or xy2 = 803 Or xy2 = 807 Or xy2 = 811 Or xy2 = 903 Or xy2 = 907 Or xy2 = 911 Or xy2 = 1003 Or xy2 = 1007 Or xy2 = 1011 Or xy2 = 1103 Or xy2 = 1108 Or xy2 = 1109 Or xy2 = 1110 Then
                b2 = c(1)
            ElseIf xy2 = 102 Or xy2 = 103 Or xy2 = 104 Or xy2 = 109 Or xy2 = 110 Or xy2 = 202 Or xy2 = 204 Or xy2 = 209 Or xy2 = 211 Or xy2 = 302 Or xy2 = 304 Or xy2 = 305 Or xy2 = 306 Or xy2 = 307 Or xy2 = 308 Or xy2 = 309 Or xy2 = 310 Or xy2 = 312 Or xy2 = 402 Or xy2 = 411 Or xy2 = 502 Or xy2 = 504 Or xy2 = 505 Or xy2 = 506 Or xy2 = 507 Or xy2 = 508 Or xy2 = 509 Or xy2 = 510 Or xy2 = 602 Or xy2 = 604 Or xy2 = 605 Or xy2 = 606 Or xy2 = 611 Or xy2 = 703 Or xy2 = 707 Or xy2 = 710 Or xy2 = 712 Or xy2 = 802 Or xy2 = 804 Or xy2 = 805 Or xy2 = 806 Or xy2 = 808 Or xy2 = 810 Or xy2 = 812 Or xy2 = 902 Or xy2 = 904 Or xy2 = 906 Or xy2 = 908 Or xy2 = 910 Or xy2 = 912 Or xy2 = 1002 Or xy2 = 1004 Or xy2 = 1006 Or xy2 = 1008 Or xy2 = 1009 Or xy2 = 1010 Or xy2 = 1012 Or xy2 = 1102 Or xy2 = 1104 Or xy2 = 1107 Or xy2 = 1111 Or xy2 = 1203 Or xy2 = 1208 Or xy2 = 1209 Or xy2 = 1210 Then
                b2 = c(2)
            ElseIf xy2 = 101 Or xy2 = 105 Or xy2 = 108 Or xy2 = 111 Or xy2 = 201 Or xy2 = 205 Or xy2 = 206 Or xy2 = 207 Or xy2 = 208 Or xy2 = 212 Or xy2 = 301 Or xy2 = 313 Or xy2 = 401 Or xy2 = 412 Or xy2 = 501 Or xy2 = 511 Or xy2 = 601 Or xy2 = 607 Or xy2 = 608 Or xy2 = 609 Or xy2 = 610 Or xy2 = 612 Or xy2 = 702 Or xy2 = 708 Or xy2 = 709 Or xy2 = 713 Or xy2 = 801 Or xy2 = 809 Or xy2 = 813 Or xy2 = 901 Or xy2 = 905 Or xy2 = 909 Or xy2 = 913 Or xy2 = 1001 Or xy2 = 1005 Or xy2 = 1013 Or xy2 = 1101 Or xy2 = 1106 Or xy2 = 1112 Or xy2 = 1202 Or xy2 = 1204 Or xy2 = 1207 Or xy2 = 1211 Then
                b2 = c(3)
            ElseIf xy2 = 113 Or xy2 = 1205 Or xy2 = 1213 Then
                b2 = c(4)
            ElseIf xy2 = 107 Or xy2 = 513 Or xy2 = 701 Then
                b2 = c(5)
            ElseIf xy2 = 112 Or xy2 = 1113 Or xy2 = 1206 Then
                b2 = c(6)
            ElseIf xy2 = 106 Or xy2 = 512 Or xy2 = 1201 Then
                b2 = c(7)
            ElseIf xy2 = 1105 Or xy2 = 1212 Then
                b2 = c(8)
            ElseIf xy2 = 213 Or xy2 = 613 Then
                b2 = c(9)
            ElseIf xy2 = 413 Then
                b2 = c(10)
            End If
        ElseIf style = 10 Then
            If xy2 = 713 Or xy2 = 812 Or xy2 = 912 Or xy2 = 913 Or xy2 = 1012 Or xy2 = 1110 Or xy2 = 1212 Or xy2 = 1213 Then
                b2 = c(1)
            ElseIf xy2 = 813 Or xy2 = 911 Or xy2 = 1010 Or xy2 = 1011 Or xy2 = 1013 Or xy2 = 1111 Or xy2 = 1112 Or xy2 = 1113 Or xy2 = 1209 Or xy2 = 1210 Or xy2 = 1211 Then
                b2 = c(2)
            ElseIf xy2 = 113 Or xy2 = 213 Or xy2 = 313 Or xy2 = 413 Or xy2 = 512 Or xy2 = 513 Or xy2 = 612 Or xy2 = 613 Or xy2 = 712 Or xy2 = 811 Then
                b2 = c(3)
            ElseIf xy2 = 110 Or xy2 = 111 Or xy2 = 112 Or xy2 = 210 Or xy2 = 211 Or xy2 = 212 Or xy2 = 310 Or xy2 = 311 Or xy2 = 312 Or xy2 = 410 Or xy2 = 411 Or xy2 = 412 Or xy2 = 509 Or xy2 = 510 Or xy2 = 511 Or xy2 = 609 Or xy2 = 610 Or xy2 = 611 Or xy2 = 708 Or xy2 = 709 Or xy2 = 710 Or xy2 = 711 Or xy2 = 808 Or xy2 = 809 Or xy2 = 810 Or xy2 = 908 Or xy2 = 909 Or xy2 = 910 Or xy2 = 1008 Or xy2 = 1009 Or xy2 = 1108 Or xy2 = 1109 Or xy2 = 1208 Then
                b2 = c(4)
            ElseIf xy2 = 106 Or xy2 = 107 Or xy2 = 108 Or xy2 = 109 Or xy2 = 206 Or xy2 = 207 Or xy2 = 208 Or xy2 = 209 Or xy2 = 306 Or xy2 = 307 Or xy2 = 308 Or xy2 = 309 Or xy2 = 406 Or xy2 = 407 Or xy2 = 408 Or xy2 = 409 Or xy2 = 507 Or xy2 = 508 Or xy2 = 607 Or xy2 = 608 Or xy2 = 707 Or xy2 = 807 Or xy2 = 907 Or xy2 = 1007 Or xy2 = 1106 Or xy2 = 1107 Or xy2 = 1206 Or xy2 = 1207 Then
                b2 = c(5)
            ElseIf xy2 = 205 Or xy2 = 304 Or xy2 = 305 Or xy2 = 403 Or xy2 = 404 Or xy2 = 405 Or xy2 = 504 Or xy2 = 505 Or xy2 = 506 Or xy2 = 605 Or xy2 = 606 Or xy2 = 704 Or xy2 = 705 Or xy2 = 706 Or xy2 = 803 Or xy2 = 804 Or xy2 = 805 Or xy2 = 806 Or xy2 = 902 Or xy2 = 903 Or xy2 = 904 Or xy2 = 905 Or xy2 = 906 Or xy2 = 1004 Or xy2 = 1005 Or xy2 = 1006 Or xy2 = 1105 Then
                b2 = c(6)
            ElseIf xy2 = 101 Or xy2 = 102 Or xy2 = 103 Or xy2 = 201 Or xy2 = 202 Or xy2 = 301 Or xy2 = 401 Or xy2 = 1001 Or xy2 = 1101 Or xy2 = 1201 Or xy2 = 1202 Then
                b2 = c(7)
            ElseIf xy2 = 104 Or xy2 = 105 Or xy2 = 203 Or xy2 = 204 Or xy2 = 302 Or xy2 = 303 Or xy2 = 402 Or xy2 = 502 Or xy2 = 503 Or xy2 = 602 Or xy2 = 604 Or xy2 = 702 Or xy2 = 703 Or xy2 = 1002 Or xy2 = 1102 Or xy2 = 1203 Or xy2 = 1204 Then
                b2 = c(8)
            ElseIf xy2 = 501 Or xy2 = 601 Or xy2 = 701 Or xy2 = 801 Or xy2 = 802 Or xy2 = 901 Or xy2 = 1003 Or xy2 = 1103 Or xy2 = 1104 Or xy2 = 1205 Then
                b2 = c(9)
            ElseIf xy2 = 603 Then
                b2 = c(10)
            End If
        ElseIf style = 11 Then
            If xy2 = 403 Or xy2 = 404 Or xy2 = 405 Or xy2 = 503 Or xy2 = 505 Or xy2 = 603 Or xy2 = 604 Or xy2 = 703 Or xy2 = 705 Or xy2 = 803 Or xy2 = 805 Or xy2 = 904 Or xy2 = 1003 Then
                b2 = c(1)
            ElseIf xy2 = 502 Or xy2 = 602 Or xy2 = 702 Or xy2 = 802 Or xy2 = 804 Or xy2 = 903 Then
                b2 = c(2)
            ElseIf xy2 = 210 Or xy2 = 211 Or xy2 = 310 Or xy2 = 311 Then
                b2 = c(3)
            ElseIf xy2 = 110 Or xy2 = 111 Or xy2 = 209 Or xy2 = 212 Or xy2 = 309 Or xy2 = 312 Or xy2 = 410 Or xy2 = 411 Then
                b2 = c(4)
            ElseIf xy2 = 101 Or xy2 = 102 Or xy2 = 103 Or xy2 = 104 Or xy2 = 105 Or xy2 = 106 Or xy2 = 107 Or xy2 = 108 Or xy2 = 109 Or xy2 = 112 Or xy2 = 201 Or xy2 = 202 Or xy2 = 203 Or xy2 = 204 Or xy2 = 205 Or xy2 = 206 Or xy2 = 207 Or xy2 = 208 Or xy2 = 301 Or xy2 = 302 Or xy2 = 303 Or xy2 = 304 Or xy2 = 305 Or xy2 = 306 Or xy2 = 307 Or xy2 = 308 Or xy2 = 401 Or xy2 = 402 Or xy2 = 501 Or xy2 = 601 Or xy2 = 701 Or xy2 = 801 Or xy2 = 901 Or xy2 = 902 Or xy2 = 1001 Or xy2 = 1002 Or xy2 = 1004 Or xy2 = 1005 Or xy2 = 1006 Or xy2 = 1007 Or xy2 = 1008 Or xy2 = 1009 Or xy2 = 1010 Or xy2 = 1011 Or xy2 = 1012 Or xy2 = 1013 Or xy2 = 1101 Or xy2 = 1102 Or xy2 = 1103 Or xy2 = 1104 Or xy2 = 1105 Or xy2 = 1106 Or xy2 = 1107 Or xy2 = 1108 Or xy2 = 1109 Or xy2 = 1110 Or xy2 = 1111 Or xy2 = 1112 Or xy2 = 1113 Or xy2 = 1201 Or xy2 = 1202 Or xy2 = 1203 Or xy2 = 1204 Or xy2 = 1205 Or xy2 = 1206 Or xy2 = 1207 Or xy2 = 1208 Or xy2 = 1209 Or xy2 = 1210 Or xy2 = 1211 Or xy2 = 1212 Or xy2 = 1213 Then
                b2 = c(5)
            ElseIf xy2 = 504 Or xy2 = 704 Then
                b2 = c(6)
            ElseIf xy2 = 412 Or xy2 = 511 Or xy2 = 512 Or xy2 = 611 Or xy2 = 710 Or xy2 = 711 Or xy2 = 810 Or xy2 = 811 Or xy2 = 812 Or xy2 = 813 Or xy2 = 909 Or xy2 = 910 Or xy2 = 911 Or xy2 = 912 Or xy2 = 913 Then
                b2 = c(7)
            ElseIf xy2 = 313 Or xy2 = 408 Or xy2 = 409 Or xy2 = 413 Or xy2 = 509 Or xy2 = 510 Or xy2 = 513 Or xy2 = 609 Or xy2 = 610 Or xy2 = 612 Or xy2 = 613 Or xy2 = 708 Or xy2 = 709 Or xy2 = 712 Or xy2 = 713 Or xy2 = 808 Or xy2 = 809 Or xy2 = 907 Or xy2 = 908 Then
                b2 = c(8)
            ElseIf xy2 = 113 Or xy2 = 213 Or xy2 = 406 Or xy2 = 407 Or xy2 = 508 Or xy2 = 608 Or xy2 = 706 Or xy2 = 707 Or xy2 = 806 Or xy2 = 807 Or xy2 = 905 Or xy2 = 906 Then
                b2 = c(9)
            ElseIf xy2 = 506 Or xy2 = 507 Or xy2 = 605 Or xy2 = 606 Or xy2 = 607 Then
                b2 = c(10)
            End If
        End If
    End Sub

    '地板顏色改變 (function)
    Private Sub colorchange()
        Dim i As Integer
        Randomize()
        style = Int((11 - 0 + 1) * Rnd() + 0)
        Randomize()
        i = Int((9 - 0 + 1) * Rnd() + 0)
        c(1) = i
        c(2) = i + 1
        c(3) = i + 2
        c(4) = i + 3
        c(5) = i + 4
        c(6) = i + 5
        c(7) = i + 6
        c(8) = i + 7
        c(9) = i + 8
        c(10) = i + 9
        If c(2) > 9 Then
            c(2) -= 10
        End If
        If c(3) > 9 Then
            c(3) -= 10
        End If
        If c(4) > 9 Then
            c(4) -= 10
        End If
        If c(5) > 9 Then
            c(5) -= 10
        End If
        If c(6) > 9 Then
            c(6) -= 10
        End If
        If c(7) > 9 Then
            c(7) -= 10
        End If
        If c(8) > 9 Then
            c(8) -= 10
        End If
        If c(9) > 9 Then
            c(9) -= 10
        End If
        If c(10) > 9 Then
            c(10) -= 10
        End If
        If style = 0 Then
            PictureBox2.Image = ImageList9.Images(c(1))
            PictureBox147.Image = ImageList9.Images(c(1))
            PictureBox279.Image = ImageList9.Images(c(1))

            PictureBox25.Image = ImageList9.Images(c(2))
            PictureBox180.Image = ImageList9.Images(c(2))
            PictureBox312.Image = ImageList9.Images(c(2))

            PictureBox58.Image = ImageList9.Images(c(3))
            PictureBox91.Image = ImageList9.Images(c(3))
            PictureBox213.Image = ImageList9.Images(c(3))
            PictureBox246.Image = ImageList9.Images(c(3))
            PictureBox345.Image = ImageList9.Images(c(3))
            PictureBox378.Image = ImageList9.Images(c(3))
            PictureBox12.Image = ImageList9.Images(c(3))
            PictureBox15.Image = ImageList9.Images(c(3))
            PictureBox117.Image = ImageList9.Images(c(3))
            PictureBox170.Image = ImageList9.Images(c(3))
            PictureBox269.Image = ImageList9.Images(c(3))
            PictureBox302.Image = ImageList9.Images(c(3))

            PictureBox48.Image = ImageList9.Images(c(4))
            PictureBox203.Image = ImageList9.Images(c(4))
            PictureBox335.Image = ImageList9.Images(c(4))

            PictureBox81.Image = ImageList9.Images(c(5))
            PictureBox236.Image = ImageList9.Images(c(5))
            PictureBox368.Image = ImageList9.Images(c(5))

            PictureBox7.Image = ImageList9.Images(c(6))
            PictureBox53.Image = ImageList9.Images(c(6))
            PictureBox122.Image = ImageList9.Images(c(6))
            PictureBox208.Image = ImageList9.Images(c(6))
            PictureBox340.Image = ImageList9.Images(c(6))
            PictureBox373.Image = ImageList9.Images(c(6))

            PictureBox17.Image = ImageList9.Images(c(7))
            PictureBox23.Image = ImageList9.Images(c(7))
            PictureBox83.Image = ImageList9.Images(c(7))
            PictureBox89.Image = ImageList9.Images(c(7))
            PictureBox119.Image = ImageList9.Images(c(7))
            PictureBox145.Image = ImageList9.Images(c(7))
            PictureBox172.Image = ImageList9.Images(c(7))
            PictureBox178.Image = ImageList9.Images(c(7))
            PictureBox244.Image = ImageList9.Images(c(7))
            PictureBox304.Image = ImageList9.Images(c(7))
            PictureBox310.Image = ImageList9.Images(c(7))

            PictureBox18.Image = ImageList9.Images(c(8))
            PictureBox19.Image = ImageList9.Images(c(8))
            PictureBox20.Image = ImageList9.Images(c(8))
            PictureBox21.Image = ImageList9.Images(c(8))
            PictureBox22.Image = ImageList9.Images(c(8))
            PictureBox84.Image = ImageList9.Images(c(8))
            PictureBox85.Image = ImageList9.Images(c(8))
            PictureBox86.Image = ImageList9.Images(c(8))
            PictureBox87.Image = ImageList9.Images(c(8))
            PictureBox88.Image = ImageList9.Images(c(8))
            PictureBox120.Image = ImageList9.Images(c(8))
            PictureBox124.Image = ImageList9.Images(c(8))
            PictureBox173.Image = ImageList9.Images(c(8))
            PictureBox174.Image = ImageList9.Images(c(8))
            PictureBox175.Image = ImageList9.Images(c(8))
            PictureBox176.Image = ImageList9.Images(c(8))
            PictureBox177.Image = ImageList9.Images(c(8))
            PictureBox241.Image = ImageList9.Images(c(8))
            PictureBox242.Image = ImageList9.Images(c(8))
            PictureBox243.Image = ImageList9.Images(c(8))
            PictureBox274.Image = ImageList9.Images(c(8))
            PictureBox305.Image = ImageList9.Images(c(8))
            PictureBox306.Image = ImageList9.Images(c(8))
            PictureBox307.Image = ImageList9.Images(c(8))
            PictureBox308.Image = ImageList9.Images(c(8))
            PictureBox309.Image = ImageList9.Images(c(8))

            PictureBox1.Image = ImageList9.Images(c(10))
            PictureBox3.Image = ImageList9.Images(c(9))
            PictureBox4.Image = ImageList9.Images(c(9))
            PictureBox5.Image = ImageList9.Images(c(9))
            PictureBox6.Image = ImageList9.Images(c(9))
            PictureBox8.Image = ImageList9.Images(c(9))
            PictureBox9.Image = ImageList9.Images(c(9))
            PictureBox10.Image = ImageList9.Images(c(9))
            PictureBox11.Image = ImageList9.Images(c(9))
            PictureBox13.Image = ImageList9.Images(c(10))
            PictureBox14.Image = ImageList9.Images(c(9))
            PictureBox16.Image = ImageList9.Images(c(9))
            PictureBox24.Image = ImageList9.Images(c(9))
            PictureBox26.Image = ImageList9.Images(c(9))
            PictureBox47.Image = ImageList9.Images(c(9))
            PictureBox49.Image = ImageList9.Images(c(9))
            PictureBox50.Image = ImageList9.Images(c(9))
            PictureBox51.Image = ImageList9.Images(c(9))
            PictureBox52.Image = ImageList9.Images(c(9))
            PictureBox54.Image = ImageList9.Images(c(9))
            PictureBox55.Image = ImageList9.Images(c(9))
            PictureBox56.Image = ImageList9.Images(c(9))
            PictureBox57.Image = ImageList9.Images(c(9))
            PictureBox59.Image = ImageList9.Images(c(9))
            PictureBox80.Image = ImageList9.Images(c(9))
            PictureBox82.Image = ImageList9.Images(c(9))
            PictureBox90.Image = ImageList9.Images(c(9))
            PictureBox92.Image = ImageList9.Images(c(9))
            PictureBox116.Image = ImageList9.Images(c(9))
            PictureBox118.Image = ImageList9.Images(c(9))
            PictureBox121.Image = ImageList9.Images(c(9))
            PictureBox123.Image = ImageList9.Images(c(9))
            PictureBox146.Image = ImageList9.Images(c(9))
            PictureBox148.Image = ImageList9.Images(c(9))
            PictureBox169.Image = ImageList9.Images(c(9))
            PictureBox171.Image = ImageList9.Images(c(9))
            PictureBox179.Image = ImageList9.Images(c(9))
            PictureBox181.Image = ImageList9.Images(c(9))
            PictureBox92.Image = ImageList9.Images(c(9))
            PictureBox202.Image = ImageList9.Images(c(9))
            PictureBox204.Image = ImageList9.Images(c(9))
            PictureBox205.Image = ImageList9.Images(c(9))
            PictureBox206.Image = ImageList9.Images(c(9))
            PictureBox207.Image = ImageList9.Images(c(9))
            PictureBox209.Image = ImageList9.Images(c(9))
            PictureBox210.Image = ImageList9.Images(c(9))
            PictureBox211.Image = ImageList9.Images(c(9))
            PictureBox212.Image = ImageList9.Images(c(9))
            PictureBox214.Image = ImageList9.Images(c(9))
            PictureBox235.Image = ImageList9.Images(c(9))
            PictureBox237.Image = ImageList9.Images(c(9))
            PictureBox238.Image = ImageList9.Images(c(9))
            PictureBox239.Image = ImageList9.Images(c(9))
            PictureBox240.Image = ImageList9.Images(c(9))
            PictureBox245.Image = ImageList9.Images(c(9))
            PictureBox247.Image = ImageList9.Images(c(9))
            PictureBox268.Image = ImageList9.Images(c(9))
            PictureBox270.Image = ImageList9.Images(c(9))
            PictureBox271.Image = ImageList9.Images(c(9))
            PictureBox272.Image = ImageList9.Images(c(9))
            PictureBox273.Image = ImageList9.Images(c(9))
            PictureBox275.Image = ImageList9.Images(c(9))
            PictureBox276.Image = ImageList9.Images(c(9))
            PictureBox277.Image = ImageList9.Images(c(9))
            PictureBox278.Image = ImageList9.Images(c(9))
            PictureBox280.Image = ImageList9.Images(c(9))
            PictureBox301.Image = ImageList9.Images(c(9))
            PictureBox303.Image = ImageList9.Images(c(9))
            PictureBox311.Image = ImageList9.Images(c(9))
            PictureBox313.Image = ImageList9.Images(c(9))
            PictureBox334.Image = ImageList9.Images(c(9))
            PictureBox336.Image = ImageList9.Images(c(9))
            PictureBox337.Image = ImageList9.Images(c(9))
            PictureBox338.Image = ImageList9.Images(c(9))
            PictureBox339.Image = ImageList9.Images(c(9))
            PictureBox341.Image = ImageList9.Images(c(9))
            PictureBox342.Image = ImageList9.Images(c(9))
            PictureBox343.Image = ImageList9.Images(c(9))
            PictureBox344.Image = ImageList9.Images(c(9))
            PictureBox346.Image = ImageList9.Images(c(9))
            PictureBox367.Image = ImageList9.Images(c(10))
            PictureBox369.Image = ImageList9.Images(c(9))
            PictureBox370.Image = ImageList9.Images(c(9))
            PictureBox371.Image = ImageList9.Images(c(9))
            PictureBox372.Image = ImageList9.Images(c(9))
            PictureBox374.Image = ImageList9.Images(c(9))
            PictureBox375.Image = ImageList9.Images(c(9))
            PictureBox376.Image = ImageList9.Images(c(9))
            PictureBox377.Image = ImageList9.Images(c(9))
            PictureBox379.Image = ImageList9.Images(c(10))
        ElseIf style = 1 Then
            PictureBox1.Image = ImageList9.Images(c(6))
            PictureBox2.Image = ImageList9.Images(c(7))
            PictureBox3.Image = ImageList9.Images(c(8))
            PictureBox4.Image = ImageList9.Images(c(8))
            PictureBox5.Image = ImageList9.Images(c(9))
            PictureBox6.Image = ImageList9.Images(c(10))
            PictureBox7.Image = ImageList9.Images(c(10))
            PictureBox8.Image = ImageList9.Images(c(10))
            PictureBox9.Image = ImageList9.Images(c(9))
            PictureBox10.Image = ImageList9.Images(c(8))
            PictureBox11.Image = ImageList9.Images(c(8))
            PictureBox12.Image = ImageList9.Images(c(7))
            PictureBox13.Image = ImageList9.Images(c(6))
            PictureBox26.Image = ImageList9.Images(c(7))
            PictureBox25.Image = ImageList9.Images(c(8))
            PictureBox24.Image = ImageList9.Images(c(9))
            PictureBox23.Image = ImageList9.Images(c(10))
            PictureBox22.Image = ImageList9.Images(c(10))
            PictureBox21.Image = ImageList9.Images(c(10))
            PictureBox20.Image = ImageList9.Images(c(10))
            PictureBox19.Image = ImageList9.Images(c(10))
            PictureBox18.Image = ImageList9.Images(c(10))
            PictureBox17.Image = ImageList9.Images(c(10))
            PictureBox16.Image = ImageList9.Images(c(9))
            PictureBox15.Image = ImageList9.Images(c(8))
            PictureBox14.Image = ImageList9.Images(c(7))
            PictureBox59.Image = ImageList9.Images(c(8))
            PictureBox58.Image = ImageList9.Images(c(9))
            PictureBox57.Image = ImageList9.Images(c(10))
            PictureBox56.Image = ImageList9.Images(c(10))
            PictureBox55.Image = ImageList9.Images(c(10))
            PictureBox54.Image = ImageList9.Images(c(5))
            PictureBox53.Image = ImageList9.Images(c(5))
            PictureBox52.Image = ImageList9.Images(c(10))
            PictureBox51.Image = ImageList9.Images(c(10))
            PictureBox50.Image = ImageList9.Images(c(10))
            PictureBox49.Image = ImageList9.Images(c(10))
            PictureBox48.Image = ImageList9.Images(c(9))
            PictureBox47.Image = ImageList9.Images(c(8))
            PictureBox92.Image = ImageList9.Images(c(8))
            PictureBox91.Image = ImageList9.Images(c(10))
            PictureBox90.Image = ImageList9.Images(c(10))
            PictureBox89.Image = ImageList9.Images(c(5))
            PictureBox88.Image = ImageList9.Images(c(4))
            PictureBox87.Image = ImageList9.Images(c(4))
            PictureBox86.Image = ImageList9.Images(c(4))
            PictureBox85.Image = ImageList9.Images(c(4))
            PictureBox84.Image = ImageList9.Images(c(5))
            PictureBox83.Image = ImageList9.Images(c(10))
            PictureBox82.Image = ImageList9.Images(c(10))
            PictureBox81.Image = ImageList9.Images(c(10))
            PictureBox80.Image = ImageList9.Images(c(8))
            PictureBox148.Image = ImageList9.Images(c(9))
            PictureBox147.Image = ImageList9.Images(c(10))
            PictureBox146.Image = ImageList9.Images(c(10))
            PictureBox145.Image = ImageList9.Images(c(4))
            PictureBox124.Image = ImageList9.Images(c(3))
            PictureBox123.Image = ImageList9.Images(c(2))
            PictureBox122.Image = ImageList9.Images(c(2))
            PictureBox121.Image = ImageList9.Images(c(3))
            PictureBox120.Image = ImageList9.Images(c(4))
            PictureBox119.Image = ImageList9.Images(c(10))
            PictureBox118.Image = ImageList9.Images(c(10))
            PictureBox117.Image = ImageList9.Images(c(10))
            PictureBox116.Image = ImageList9.Images(c(9))
            PictureBox181.Image = ImageList9.Images(c(10))
            PictureBox180.Image = ImageList9.Images(c(10))
            PictureBox179.Image = ImageList9.Images(c(5))
            PictureBox178.Image = ImageList9.Images(c(4))
            PictureBox177.Image = ImageList9.Images(c(2))
            PictureBox176.Image = ImageList9.Images(c(1))
            PictureBox175.Image = ImageList9.Images(c(1))
            PictureBox174.Image = ImageList9.Images(c(2))
            PictureBox173.Image = ImageList9.Images(c(4))
            PictureBox172.Image = ImageList9.Images(c(5))
            PictureBox171.Image = ImageList9.Images(c(10))
            PictureBox170.Image = ImageList9.Images(c(10))
            PictureBox169.Image = ImageList9.Images(c(10))
            PictureBox214.Image = ImageList9.Images(c(10))
            PictureBox213.Image = ImageList9.Images(c(10))
            PictureBox212.Image = ImageList9.Images(c(5))
            PictureBox211.Image = ImageList9.Images(c(4))
            PictureBox210.Image = ImageList9.Images(c(2))
            PictureBox209.Image = ImageList9.Images(c(1))
            PictureBox208.Image = ImageList9.Images(c(1))
            PictureBox207.Image = ImageList9.Images(c(2))
            PictureBox206.Image = ImageList9.Images(c(4))
            PictureBox205.Image = ImageList9.Images(c(5))
            PictureBox204.Image = ImageList9.Images(c(10))
            PictureBox203.Image = ImageList9.Images(c(10))
            PictureBox202.Image = ImageList9.Images(c(10))
            PictureBox247.Image = ImageList9.Images(c(9))
            PictureBox246.Image = ImageList9.Images(c(10))
            PictureBox245.Image = ImageList9.Images(c(10))
            PictureBox244.Image = ImageList9.Images(c(4))
            PictureBox243.Image = ImageList9.Images(c(3))
            PictureBox242.Image = ImageList9.Images(c(2))
            PictureBox241.Image = ImageList9.Images(c(2))
            PictureBox240.Image = ImageList9.Images(c(3))
            PictureBox239.Image = ImageList9.Images(c(4))
            PictureBox238.Image = ImageList9.Images(c(10))
            PictureBox237.Image = ImageList9.Images(c(10))
            PictureBox236.Image = ImageList9.Images(c(10))
            PictureBox235.Image = ImageList9.Images(c(9))
            PictureBox280.Image = ImageList9.Images(c(8))
            PictureBox279.Image = ImageList9.Images(c(10))
            PictureBox278.Image = ImageList9.Images(c(10))
            PictureBox277.Image = ImageList9.Images(c(5))
            PictureBox276.Image = ImageList9.Images(c(4))
            PictureBox275.Image = ImageList9.Images(c(4))
            PictureBox274.Image = ImageList9.Images(c(4))
            PictureBox273.Image = ImageList9.Images(c(4))
            PictureBox272.Image = ImageList9.Images(c(5))
            PictureBox271.Image = ImageList9.Images(c(10))
            PictureBox270.Image = ImageList9.Images(c(10))
            PictureBox269.Image = ImageList9.Images(c(10))
            PictureBox268.Image = ImageList9.Images(c(8))
            PictureBox313.Image = ImageList9.Images(c(8))
            PictureBox312.Image = ImageList9.Images(c(9))
            PictureBox311.Image = ImageList9.Images(c(10))
            PictureBox310.Image = ImageList9.Images(c(10))
            PictureBox309.Image = ImageList9.Images(c(10))
            PictureBox308.Image = ImageList9.Images(c(5))
            PictureBox307.Image = ImageList9.Images(c(5))
            PictureBox306.Image = ImageList9.Images(c(10))
            PictureBox305.Image = ImageList9.Images(c(10))
            PictureBox304.Image = ImageList9.Images(c(10))
            PictureBox303.Image = ImageList9.Images(c(10))
            PictureBox302.Image = ImageList9.Images(c(9))
            PictureBox301.Image = ImageList9.Images(c(8))
            PictureBox346.Image = ImageList9.Images(c(7))
            PictureBox345.Image = ImageList9.Images(c(8))
            PictureBox344.Image = ImageList9.Images(c(9))
            PictureBox343.Image = ImageList9.Images(c(10))
            PictureBox342.Image = ImageList9.Images(c(10))
            PictureBox341.Image = ImageList9.Images(c(10))
            PictureBox340.Image = ImageList9.Images(c(10))
            PictureBox339.Image = ImageList9.Images(c(10))
            PictureBox338.Image = ImageList9.Images(c(10))
            PictureBox337.Image = ImageList9.Images(c(10))
            PictureBox336.Image = ImageList9.Images(c(9))
            PictureBox335.Image = ImageList9.Images(c(8))
            PictureBox334.Image = ImageList9.Images(c(7))
            PictureBox379.Image = ImageList9.Images(c(6))
            PictureBox378.Image = ImageList9.Images(c(7))
            PictureBox377.Image = ImageList9.Images(c(8))
            PictureBox376.Image = ImageList9.Images(c(8))
            PictureBox375.Image = ImageList9.Images(c(9))
            PictureBox374.Image = ImageList9.Images(c(10))
            PictureBox373.Image = ImageList9.Images(c(10))
            PictureBox372.Image = ImageList9.Images(c(10))
            PictureBox371.Image = ImageList9.Images(c(9))
            PictureBox370.Image = ImageList9.Images(c(8))
            PictureBox369.Image = ImageList9.Images(c(8))
            PictureBox368.Image = ImageList9.Images(c(7))
            PictureBox367.Image = ImageList9.Images(c(6))

        ElseIf style = 2 Then
            PictureBox1.Image = ImageList9.Images(c(2))
            PictureBox2.Image = ImageList9.Images(c(2))
            PictureBox3.Image = ImageList9.Images(c(2))
            PictureBox4.Image = ImageList9.Images(c(2))
            PictureBox5.Image = ImageList9.Images(c(2))
            PictureBox6.Image = ImageList9.Images(c(2))
            PictureBox7.Image = ImageList9.Images(c(2))
            PictureBox8.Image = ImageList9.Images(c(2))
            PictureBox9.Image = ImageList9.Images(c(2))
            PictureBox10.Image = ImageList9.Images(c(2))
            PictureBox11.Image = ImageList9.Images(c(2))
            PictureBox12.Image = ImageList9.Images(c(2))
            PictureBox13.Image = ImageList9.Images(c(7))
            PictureBox26.Image = ImageList9.Images(c(2))
            PictureBox25.Image = ImageList9.Images(c(4))
            PictureBox24.Image = ImageList9.Images(c(4))
            PictureBox23.Image = ImageList9.Images(c(4))
            PictureBox22.Image = ImageList9.Images(c(4))
            PictureBox21.Image = ImageList9.Images(c(4))
            PictureBox20.Image = ImageList9.Images(c(2))
            PictureBox19.Image = ImageList9.Images(c(2))
            PictureBox18.Image = ImageList9.Images(c(6))
            PictureBox17.Image = ImageList9.Images(c(2))
            PictureBox16.Image = ImageList9.Images(c(2))
            PictureBox15.Image = ImageList9.Images(c(6))
            PictureBox14.Image = ImageList9.Images(c(7))
            PictureBox59.Image = ImageList9.Images(c(2))
            PictureBox58.Image = ImageList9.Images(c(4))
            PictureBox57.Image = ImageList9.Images(c(2))
            PictureBox56.Image = ImageList9.Images(c(2))
            PictureBox55.Image = ImageList9.Images(c(2))
            PictureBox54.Image = ImageList9.Images(c(4))
            PictureBox53.Image = ImageList9.Images(c(2))
            PictureBox52.Image = ImageList9.Images(c(6))
            PictureBox51.Image = ImageList9.Images(c(2))
            PictureBox50.Image = ImageList9.Images(c(6))
            PictureBox49.Image = ImageList9.Images(c(2))
            PictureBox48.Image = ImageList9.Images(c(6))
            PictureBox47.Image = ImageList9.Images(c(7))
            PictureBox92.Image = ImageList9.Images(c(2))
            PictureBox91.Image = ImageList9.Images(c(2))
            PictureBox90.Image = ImageList9.Images(c(4))
            PictureBox89.Image = ImageList9.Images(c(4))
            PictureBox88.Image = ImageList9.Images(c(4))
            PictureBox87.Image = ImageList9.Images(c(2))
            PictureBox86.Image = ImageList9.Images(c(2))
            PictureBox85.Image = ImageList9.Images(c(6))
            PictureBox84.Image = ImageList9.Images(c(2))
            PictureBox83.Image = ImageList9.Images(c(6))
            PictureBox82.Image = ImageList9.Images(c(6))
            PictureBox81.Image = ImageList9.Images(c(2))
            PictureBox80.Image = ImageList9.Images(c(7))
            PictureBox148.Image = ImageList9.Images(c(2))
            PictureBox147.Image = ImageList9.Images(c(2))
            PictureBox146.Image = ImageList9.Images(c(2))
            PictureBox145.Image = ImageList9.Images(c(2))
            PictureBox124.Image = ImageList9.Images(c(2))
            PictureBox123.Image = ImageList9.Images(c(2))
            PictureBox122.Image = ImageList9.Images(c(2))
            PictureBox121.Image = ImageList9.Images(c(2))
            PictureBox120.Image = ImageList9.Images(c(2))
            PictureBox119.Image = ImageList9.Images(c(2))
            PictureBox118.Image = ImageList9.Images(c(2))
            PictureBox117.Image = ImageList9.Images(c(2))
            PictureBox116.Image = ImageList9.Images(c(7))
            PictureBox181.Image = ImageList9.Images(c(2))
            PictureBox180.Image = ImageList9.Images(c(8))
            PictureBox179.Image = ImageList9.Images(c(2))
            PictureBox178.Image = ImageList9.Images(c(2))
            PictureBox177.Image = ImageList9.Images(c(8))
            PictureBox176.Image = ImageList9.Images(c(8))
            PictureBox175.Image = ImageList9.Images(c(2))
            PictureBox174.Image = ImageList9.Images(c(10))
            PictureBox173.Image = ImageList9.Images(c(10))
            PictureBox172.Image = ImageList9.Images(c(10))
            PictureBox171.Image = ImageList9.Images(c(10))
            PictureBox170.Image = ImageList9.Images(c(10))
            PictureBox169.Image = ImageList9.Images(c(7))
            PictureBox214.Image = ImageList9.Images(c(2))
            PictureBox213.Image = ImageList9.Images(c(8))
            PictureBox212.Image = ImageList9.Images(c(2))
            PictureBox211.Image = ImageList9.Images(c(8))
            PictureBox210.Image = ImageList9.Images(c(2))
            PictureBox209.Image = ImageList9.Images(c(8))
            PictureBox208.Image = ImageList9.Images(c(2))
            PictureBox207.Image = ImageList9.Images(c(2))
            PictureBox206.Image = ImageList9.Images(c(2))
            PictureBox205.Image = ImageList9.Images(c(10))
            PictureBox204.Image = ImageList9.Images(c(2))
            PictureBox203.Image = ImageList9.Images(c(2))
            PictureBox202.Image = ImageList9.Images(c(7))
            PictureBox247.Image = ImageList9.Images(c(2))
            PictureBox246.Image = ImageList9.Images(c(8))
            PictureBox245.Image = ImageList9.Images(c(8))
            PictureBox244.Image = ImageList9.Images(c(2))
            PictureBox243.Image = ImageList9.Images(c(2))
            PictureBox242.Image = ImageList9.Images(c(8))
            PictureBox241.Image = ImageList9.Images(c(2))
            PictureBox240.Image = ImageList9.Images(c(10))
            PictureBox239.Image = ImageList9.Images(c(10))
            PictureBox238.Image = ImageList9.Images(c(10))
            PictureBox237.Image = ImageList9.Images(c(10))
            PictureBox236.Image = ImageList9.Images(c(10))
            PictureBox235.Image = ImageList9.Images(c(7))
            PictureBox280.Image = ImageList9.Images(c(6))
            PictureBox279.Image = ImageList9.Images(c(2))
            PictureBox278.Image = ImageList9.Images(c(2))
            PictureBox277.Image = ImageList9.Images(c(2))
            PictureBox276.Image = ImageList9.Images(c(2))
            PictureBox275.Image = ImageList9.Images(c(2))
            PictureBox274.Image = ImageList9.Images(c(2))
            PictureBox273.Image = ImageList9.Images(c(2))
            PictureBox272.Image = ImageList9.Images(c(2))
            PictureBox271.Image = ImageList9.Images(c(2))
            PictureBox270.Image = ImageList9.Images(c(2))
            PictureBox269.Image = ImageList9.Images(c(2))
            PictureBox268.Image = ImageList9.Images(c(7))
            PictureBox313.Image = ImageList9.Images(c(6))
            PictureBox312.Image = ImageList9.Images(c(5))
            PictureBox311.Image = ImageList9.Images(c(2))
            PictureBox310.Image = ImageList9.Images(c(2))
            PictureBox309.Image = ImageList9.Images(c(2))
            PictureBox308.Image = ImageList9.Images(c(2))
            PictureBox307.Image = ImageList9.Images(c(2))
            PictureBox306.Image = ImageList9.Images(c(2))
            PictureBox305.Image = ImageList9.Images(c(2))
            PictureBox304.Image = ImageList9.Images(c(2))
            PictureBox303.Image = ImageList9.Images(c(2))
            PictureBox302.Image = ImageList9.Images(c(8))
            PictureBox301.Image = ImageList9.Images(c(7))
            PictureBox346.Image = ImageList9.Images(c(6))
            PictureBox345.Image = ImageList9.Images(c(5))
            PictureBox344.Image = ImageList9.Images(c(4))
            PictureBox343.Image = ImageList9.Images(c(2))
            PictureBox342.Image = ImageList9.Images(c(2))
            PictureBox341.Image = ImageList9.Images(c(2))
            PictureBox340.Image = ImageList9.Images(c(2))
            PictureBox339.Image = ImageList9.Images(c(2))
            PictureBox338.Image = ImageList9.Images(c(2))
            PictureBox337.Image = ImageList9.Images(c(2))
            PictureBox336.Image = ImageList9.Images(c(10))
            PictureBox335.Image = ImageList9.Images(c(8))
            PictureBox334.Image = ImageList9.Images(c(7))
            PictureBox379.Image = ImageList9.Images(c(6))
            PictureBox378.Image = ImageList9.Images(c(5))
            PictureBox377.Image = ImageList9.Images(c(4))
            PictureBox376.Image = ImageList9.Images(c(3))
            PictureBox375.Image = ImageList9.Images(c(1))
            PictureBox374.Image = ImageList9.Images(c(1))
            PictureBox373.Image = ImageList9.Images(c(1))
            PictureBox372.Image = ImageList9.Images(c(1))
            PictureBox371.Image = ImageList9.Images(c(1))
            PictureBox370.Image = ImageList9.Images(c(9))
            PictureBox369.Image = ImageList9.Images(c(10))
            PictureBox368.Image = ImageList9.Images(c(8))
            PictureBox367.Image = ImageList9.Images(c(7))
        ElseIf style = 3 Then
            PictureBox1.Image = ImageList9.Images(c(5))
            PictureBox2.Image = ImageList9.Images(c(7))
            PictureBox3.Image = ImageList9.Images(c(7))
            PictureBox4.Image = ImageList9.Images(c(7))
            PictureBox5.Image = ImageList9.Images(c(7))
            PictureBox6.Image = ImageList9.Images(c(7))
            PictureBox7.Image = ImageList9.Images(c(7))
            PictureBox8.Image = ImageList9.Images(c(7))
            PictureBox9.Image = ImageList9.Images(c(7))
            PictureBox10.Image = ImageList9.Images(c(7))
            PictureBox11.Image = ImageList9.Images(c(7))
            PictureBox12.Image = ImageList9.Images(c(7))
            PictureBox13.Image = ImageList9.Images(c(5))
            PictureBox26.Image = ImageList9.Images(c(6))
            PictureBox25.Image = ImageList9.Images(c(1))
            PictureBox24.Image = ImageList9.Images(c(10))
            PictureBox23.Image = ImageList9.Images(c(10))
            PictureBox22.Image = ImageList9.Images(c(10))
            PictureBox21.Image = ImageList9.Images(c(1))
            PictureBox20.Image = ImageList9.Images(c(10))
            PictureBox19.Image = ImageList9.Images(c(3))
            PictureBox18.Image = ImageList9.Images(c(3))
            PictureBox17.Image = ImageList9.Images(c(10))
            PictureBox16.Image = ImageList9.Images(c(10))
            PictureBox15.Image = ImageList9.Images(c(10))
            PictureBox14.Image = ImageList9.Images(c(8))
            PictureBox59.Image = ImageList9.Images(c(6))
            PictureBox58.Image = ImageList9.Images(c(1))
            PictureBox57.Image = ImageList9.Images(c(1))
            PictureBox56.Image = ImageList9.Images(c(1))
            PictureBox55.Image = ImageList9.Images(c(1))
            PictureBox54.Image = ImageList9.Images(c(1))
            PictureBox53.Image = ImageList9.Images(c(10))
            PictureBox52.Image = ImageList9.Images(c(10))
            PictureBox51.Image = ImageList9.Images(c(10))
            PictureBox50.Image = ImageList9.Images(c(3))
            PictureBox49.Image = ImageList9.Images(c(3))
            PictureBox48.Image = ImageList9.Images(c(10))
            PictureBox47.Image = ImageList9.Images(c(8))
            PictureBox92.Image = ImageList9.Images(c(6))
            PictureBox91.Image = ImageList9.Images(c(1))
            PictureBox90.Image = ImageList9.Images(c(10))
            PictureBox89.Image = ImageList9.Images(c(10))
            PictureBox88.Image = ImageList9.Images(c(10))
            PictureBox87.Image = ImageList9.Images(c(1))
            PictureBox86.Image = ImageList9.Images(c(10))
            PictureBox85.Image = ImageList9.Images(c(10))
            PictureBox84.Image = ImageList9.Images(c(10))
            PictureBox83.Image = ImageList9.Images(c(10))
            PictureBox82.Image = ImageList9.Images(c(10))
            PictureBox81.Image = ImageList9.Images(c(3))
            PictureBox80.Image = ImageList9.Images(c(8))
            PictureBox148.Image = ImageList9.Images(c(6))
            PictureBox147.Image = ImageList9.Images(c(10))
            PictureBox146.Image = ImageList9.Images(c(10))
            PictureBox145.Image = ImageList9.Images(c(10))
            PictureBox124.Image = ImageList9.Images(c(10))
            PictureBox123.Image = ImageList9.Images(c(10))
            PictureBox122.Image = ImageList9.Images(c(10))
            PictureBox121.Image = ImageList9.Images(c(10))
            PictureBox120.Image = ImageList9.Images(c(10))
            PictureBox119.Image = ImageList9.Images(c(3))
            PictureBox118.Image = ImageList9.Images(c(3))
            PictureBox117.Image = ImageList9.Images(c(10))
            PictureBox116.Image = ImageList9.Images(c(8))
            PictureBox181.Image = ImageList9.Images(c(6))
            PictureBox180.Image = ImageList9.Images(c(2))
            PictureBox179.Image = ImageList9.Images(c(2))
            PictureBox178.Image = ImageList9.Images(c(2))
            PictureBox177.Image = ImageList9.Images(c(10))
            PictureBox176.Image = ImageList9.Images(c(10))
            PictureBox175.Image = ImageList9.Images(c(10))
            PictureBox174.Image = ImageList9.Images(c(3))
            PictureBox173.Image = ImageList9.Images(c(3))
            PictureBox172.Image = ImageList9.Images(c(10))
            PictureBox171.Image = ImageList9.Images(c(10))
            PictureBox170.Image = ImageList9.Images(c(10))
            PictureBox169.Image = ImageList9.Images(c(8))
            PictureBox214.Image = ImageList9.Images(c(6))
            PictureBox213.Image = ImageList9.Images(c(2))
            PictureBox212.Image = ImageList9.Images(c(2))
            PictureBox211.Image = ImageList9.Images(c(2))
            PictureBox210.Image = ImageList9.Images(c(2))
            PictureBox209.Image = ImageList9.Images(c(10))
            PictureBox208.Image = ImageList9.Images(c(10))
            PictureBox207.Image = ImageList9.Images(c(10))
            PictureBox206.Image = ImageList9.Images(c(10))
            PictureBox205.Image = ImageList9.Images(c(10))
            PictureBox204.Image = ImageList9.Images(c(10))
            PictureBox203.Image = ImageList9.Images(c(10))
            PictureBox202.Image = ImageList9.Images(c(8))
            PictureBox247.Image = ImageList9.Images(c(6))
            PictureBox246.Image = ImageList9.Images(c(10))
            PictureBox245.Image = ImageList9.Images(c(10))
            PictureBox244.Image = ImageList9.Images(c(2))
            PictureBox243.Image = ImageList9.Images(c(2))
            PictureBox242.Image = ImageList9.Images(c(2))
            PictureBox241.Image = ImageList9.Images(c(10))
            PictureBox240.Image = ImageList9.Images(c(4))
            PictureBox239.Image = ImageList9.Images(c(4))
            PictureBox238.Image = ImageList9.Images(c(4))
            PictureBox237.Image = ImageList9.Images(c(4))
            PictureBox236.Image = ImageList9.Images(c(4))
            PictureBox235.Image = ImageList9.Images(c(8))
            PictureBox280.Image = ImageList9.Images(c(6))
            PictureBox279.Image = ImageList9.Images(c(2))
            PictureBox278.Image = ImageList9.Images(c(2))
            PictureBox277.Image = ImageList9.Images(c(2))
            PictureBox276.Image = ImageList9.Images(c(2))
            PictureBox275.Image = ImageList9.Images(c(10))
            PictureBox274.Image = ImageList9.Images(c(10))
            PictureBox273.Image = ImageList9.Images(c(10))
            PictureBox272.Image = ImageList9.Images(c(10))
            PictureBox271.Image = ImageList9.Images(c(4))
            PictureBox270.Image = ImageList9.Images(c(10))
            PictureBox269.Image = ImageList9.Images(c(4))
            PictureBox268.Image = ImageList9.Images(c(8))
            PictureBox313.Image = ImageList9.Images(c(6))
            PictureBox312.Image = ImageList9.Images(c(2))
            PictureBox311.Image = ImageList9.Images(c(2))
            PictureBox310.Image = ImageList9.Images(c(2))
            PictureBox309.Image = ImageList9.Images(c(10))
            PictureBox308.Image = ImageList9.Images(c(10))
            PictureBox307.Image = ImageList9.Images(c(10))
            PictureBox306.Image = ImageList9.Images(c(10))
            PictureBox305.Image = ImageList9.Images(c(10))
            PictureBox304.Image = ImageList9.Images(c(4))
            PictureBox303.Image = ImageList9.Images(c(10))
            PictureBox302.Image = ImageList9.Images(c(4))
            PictureBox301.Image = ImageList9.Images(c(8))
            PictureBox346.Image = ImageList9.Images(c(6))
            PictureBox345.Image = ImageList9.Images(c(10))
            PictureBox344.Image = ImageList9.Images(c(10))
            PictureBox343.Image = ImageList9.Images(c(10))
            PictureBox342.Image = ImageList9.Images(c(10))
            PictureBox341.Image = ImageList9.Images(c(10))
            PictureBox340.Image = ImageList9.Images(c(10))
            PictureBox339.Image = ImageList9.Images(c(10))
            PictureBox338.Image = ImageList9.Images(c(10))
            PictureBox337.Image = ImageList9.Images(c(4))
            PictureBox336.Image = ImageList9.Images(c(4))
            PictureBox335.Image = ImageList9.Images(c(4))
            PictureBox334.Image = ImageList9.Images(c(8))
            PictureBox379.Image = ImageList9.Images(c(5))
            PictureBox378.Image = ImageList9.Images(c(9))
            PictureBox377.Image = ImageList9.Images(c(9))
            PictureBox376.Image = ImageList9.Images(c(9))
            PictureBox375.Image = ImageList9.Images(c(9))
            PictureBox374.Image = ImageList9.Images(c(9))
            PictureBox373.Image = ImageList9.Images(c(9))
            PictureBox372.Image = ImageList9.Images(c(9))
            PictureBox371.Image = ImageList9.Images(c(9))
            PictureBox370.Image = ImageList9.Images(c(9))
            PictureBox369.Image = ImageList9.Images(c(9))
            PictureBox368.Image = ImageList9.Images(c(9))
            PictureBox367.Image = ImageList9.Images(c(5))
        ElseIf style = 4 Then
            PictureBox1.Image = ImageList9.Images(c(2))
            PictureBox2.Image = ImageList9.Images(c(2))
            PictureBox3.Image = ImageList9.Images(c(2))
            PictureBox4.Image = ImageList9.Images(c(2))
            PictureBox5.Image = ImageList9.Images(c(2))
            PictureBox6.Image = ImageList9.Images(c(2))
            PictureBox7.Image = ImageList9.Images(c(1))
            PictureBox8.Image = ImageList9.Images(c(2))
            PictureBox9.Image = ImageList9.Images(c(2))
            PictureBox10.Image = ImageList9.Images(c(2))
            PictureBox11.Image = ImageList9.Images(c(2))
            PictureBox12.Image = ImageList9.Images(c(2))
            PictureBox13.Image = ImageList9.Images(c(1))
            PictureBox26.Image = ImageList9.Images(c(2))
            PictureBox25.Image = ImageList9.Images(c(7))
            PictureBox24.Image = ImageList9.Images(c(9))
            PictureBox23.Image = ImageList9.Images(c(8))
            PictureBox22.Image = ImageList9.Images(c(10))
            PictureBox21.Image = ImageList9.Images(c(10))
            PictureBox20.Image = ImageList9.Images(c(2))
            PictureBox19.Image = ImageList9.Images(c(3))
            PictureBox18.Image = ImageList9.Images(c(2))
            PictureBox17.Image = ImageList9.Images(c(3))
            PictureBox16.Image = ImageList9.Images(c(3))
            PictureBox15.Image = ImageList9.Images(c(3))
            PictureBox14.Image = ImageList9.Images(c(1))
            PictureBox59.Image = ImageList9.Images(c(2))
            PictureBox58.Image = ImageList9.Images(c(2))
            PictureBox57.Image = ImageList9.Images(c(2))
            PictureBox56.Image = ImageList9.Images(c(2))
            PictureBox55.Image = ImageList9.Images(c(2))
            PictureBox54.Image = ImageList9.Images(c(2))
            PictureBox53.Image = ImageList9.Images(c(2))
            PictureBox52.Image = ImageList9.Images(c(3))
            PictureBox51.Image = ImageList9.Images(c(2))
            PictureBox50.Image = ImageList9.Images(c(3))
            PictureBox49.Image = ImageList9.Images(c(2))
            PictureBox48.Image = ImageList9.Images(c(3))
            PictureBox47.Image = ImageList9.Images(c(1))
            PictureBox92.Image = ImageList9.Images(c(2))
            PictureBox91.Image = ImageList9.Images(c(7))
            PictureBox90.Image = ImageList9.Images(c(9))
            PictureBox89.Image = ImageList9.Images(c(8))
            PictureBox88.Image = ImageList9.Images(c(10))
            PictureBox87.Image = ImageList9.Images(c(10))
            PictureBox86.Image = ImageList9.Images(c(1))
            PictureBox85.Image = ImageList9.Images(c(3))
            PictureBox84.Image = ImageList9.Images(c(3))
            PictureBox83.Image = ImageList9.Images(c(3))
            PictureBox82.Image = ImageList9.Images(c(2))
            PictureBox81.Image = ImageList9.Images(c(3))
            PictureBox80.Image = ImageList9.Images(c(2))
            PictureBox148.Image = ImageList9.Images(c(2))
            PictureBox147.Image = ImageList9.Images(c(7))
            PictureBox146.Image = ImageList9.Images(c(2))
            PictureBox145.Image = ImageList9.Images(c(2))
            PictureBox124.Image = ImageList9.Images(c(2))
            PictureBox123.Image = ImageList9.Images(c(10))
            PictureBox122.Image = ImageList9.Images(c(1))
            PictureBox121.Image = ImageList9.Images(c(2))
            PictureBox120.Image = ImageList9.Images(c(2))
            PictureBox119.Image = ImageList9.Images(c(2))
            PictureBox118.Image = ImageList9.Images(c(2))
            PictureBox117.Image = ImageList9.Images(c(2))
            PictureBox116.Image = ImageList9.Images(c(2))
            PictureBox181.Image = ImageList9.Images(c(2))
            PictureBox180.Image = ImageList9.Images(c(7))
            PictureBox179.Image = ImageList9.Images(c(9))
            PictureBox178.Image = ImageList9.Images(c(8))
            PictureBox177.Image = ImageList9.Images(c(10))
            PictureBox176.Image = ImageList9.Images(c(10))
            PictureBox175.Image = ImageList9.Images(c(1))
            PictureBox174.Image = ImageList9.Images(c(4))
            PictureBox173.Image = ImageList9.Images(c(4))
            PictureBox172.Image = ImageList9.Images(c(4))
            PictureBox171.Image = ImageList9.Images(c(4))
            PictureBox170.Image = ImageList9.Images(c(4))
            PictureBox169.Image = ImageList9.Images(c(1))
            PictureBox214.Image = ImageList9.Images(c(2))
            PictureBox213.Image = ImageList9.Images(c(2))
            PictureBox212.Image = ImageList9.Images(c(2))
            PictureBox211.Image = ImageList9.Images(c(2))
            PictureBox210.Image = ImageList9.Images(c(2))
            PictureBox209.Image = ImageList9.Images(c(2))
            PictureBox208.Image = ImageList9.Images(c(2))
            PictureBox207.Image = ImageList9.Images(c(4))
            PictureBox206.Image = ImageList9.Images(c(2))
            PictureBox205.Image = ImageList9.Images(c(4))
            PictureBox204.Image = ImageList9.Images(c(2))
            PictureBox203.Image = ImageList9.Images(c(4))
            PictureBox202.Image = ImageList9.Images(c(1))
            PictureBox247.Image = ImageList9.Images(c(2))
            PictureBox246.Image = ImageList9.Images(c(6))
            PictureBox245.Image = ImageList9.Images(c(6))
            PictureBox244.Image = ImageList9.Images(c(6))
            PictureBox243.Image = ImageList9.Images(c(2))
            PictureBox242.Image = ImageList9.Images(c(2))
            PictureBox241.Image = ImageList9.Images(c(2))
            PictureBox240.Image = ImageList9.Images(c(4))
            PictureBox239.Image = ImageList9.Images(c(4))
            PictureBox238.Image = ImageList9.Images(c(4))
            PictureBox237.Image = ImageList9.Images(c(4))
            PictureBox236.Image = ImageList9.Images(c(4))
            PictureBox235.Image = ImageList9.Images(c(1))
            PictureBox280.Image = ImageList9.Images(c(2))
            PictureBox279.Image = ImageList9.Images(c(2))
            PictureBox278.Image = ImageList9.Images(c(2))
            PictureBox277.Image = ImageList9.Images(c(6))
            PictureBox276.Image = ImageList9.Images(c(2))
            PictureBox275.Image = ImageList9.Images(c(2))
            PictureBox274.Image = ImageList9.Images(c(2))
            PictureBox273.Image = ImageList9.Images(c(2))
            PictureBox272.Image = ImageList9.Images(c(2))
            PictureBox271.Image = ImageList9.Images(c(2))
            PictureBox270.Image = ImageList9.Images(c(2))
            PictureBox269.Image = ImageList9.Images(c(2))
            PictureBox268.Image = ImageList9.Images(c(2))
            PictureBox313.Image = ImageList9.Images(c(2))
            PictureBox312.Image = ImageList9.Images(c(6))
            PictureBox311.Image = ImageList9.Images(c(6))
            PictureBox310.Image = ImageList9.Images(c(6))
            PictureBox309.Image = ImageList9.Images(c(6))
            PictureBox308.Image = ImageList9.Images(c(6))
            PictureBox307.Image = ImageList9.Images(c(2))
            PictureBox306.Image = ImageList9.Images(c(5))
            PictureBox305.Image = ImageList9.Images(c(5))
            PictureBox304.Image = ImageList9.Images(c(5))
            PictureBox303.Image = ImageList9.Images(c(2))
            PictureBox302.Image = ImageList9.Images(c(5))
            PictureBox301.Image = ImageList9.Images(c(2))
            PictureBox346.Image = ImageList9.Images(c(2))
            PictureBox345.Image = ImageList9.Images(c(2))
            PictureBox344.Image = ImageList9.Images(c(2))
            PictureBox343.Image = ImageList9.Images(c(2))
            PictureBox342.Image = ImageList9.Images(c(2))
            PictureBox341.Image = ImageList9.Images(c(2))
            PictureBox340.Image = ImageList9.Images(c(1))
            PictureBox339.Image = ImageList9.Images(c(2))
            PictureBox338.Image = ImageList9.Images(c(2))
            PictureBox337.Image = ImageList9.Images(c(2))
            PictureBox336.Image = ImageList9.Images(c(2))
            PictureBox335.Image = ImageList9.Images(c(2))
            PictureBox334.Image = ImageList9.Images(c(1))
            PictureBox379.Image = ImageList9.Images(c(2))
            PictureBox378.Image = ImageList9.Images(c(2))
            PictureBox377.Image = ImageList9.Images(c(2))
            PictureBox376.Image = ImageList9.Images(c(2))
            PictureBox375.Image = ImageList9.Images(c(2))
            PictureBox374.Image = ImageList9.Images(c(2))
            PictureBox373.Image = ImageList9.Images(c(2))
            PictureBox372.Image = ImageList9.Images(c(1))
            PictureBox371.Image = ImageList9.Images(c(2))
            PictureBox370.Image = ImageList9.Images(c(2))
            PictureBox369.Image = ImageList9.Images(c(2))
            PictureBox368.Image = ImageList9.Images(c(2))
            PictureBox367.Image = ImageList9.Images(c(2))
        ElseIf style = 5 Then
            PictureBox1.Image = ImageList9.Images(c(10))
            PictureBox2.Image = ImageList9.Images(c(8))
            PictureBox3.Image = ImageList9.Images(c(8))
            PictureBox4.Image = ImageList9.Images(c(8))
            PictureBox5.Image = ImageList9.Images(c(8))
            PictureBox6.Image = ImageList9.Images(c(8))
            PictureBox7.Image = ImageList9.Images(c(7))
            PictureBox8.Image = ImageList9.Images(c(7))
            PictureBox9.Image = ImageList9.Images(c(7))
            PictureBox10.Image = ImageList9.Images(c(7))
            PictureBox11.Image = ImageList9.Images(c(7))
            PictureBox12.Image = ImageList9.Images(c(7))
            PictureBox13.Image = ImageList9.Images(c(10))
            PictureBox26.Image = ImageList9.Images(c(10))
            PictureBox25.Image = ImageList9.Images(c(1))
            PictureBox24.Image = ImageList9.Images(c(1))
            PictureBox23.Image = ImageList9.Images(c(1))
            PictureBox22.Image = ImageList9.Images(c(1))
            PictureBox21.Image = ImageList9.Images(c(1))
            PictureBox20.Image = ImageList9.Images(c(7))
            PictureBox19.Image = ImageList9.Images(c(4))
            PictureBox18.Image = ImageList9.Images(c(7))
            PictureBox17.Image = ImageList9.Images(c(4))
            PictureBox16.Image = ImageList9.Images(c(7))
            PictureBox15.Image = ImageList9.Images(c(4))
            PictureBox14.Image = ImageList9.Images(c(10))
            PictureBox59.Image = ImageList9.Images(c(10))
            PictureBox58.Image = ImageList9.Images(c(8))
            PictureBox57.Image = ImageList9.Images(c(8))
            PictureBox56.Image = ImageList9.Images(c(8))
            PictureBox55.Image = ImageList9.Images(c(8))
            PictureBox54.Image = ImageList9.Images(c(8))
            PictureBox53.Image = ImageList9.Images(c(7))
            PictureBox52.Image = ImageList9.Images(c(4))
            PictureBox51.Image = ImageList9.Images(c(7))
            PictureBox50.Image = ImageList9.Images(c(4))
            PictureBox49.Image = ImageList9.Images(c(7))
            PictureBox48.Image = ImageList9.Images(c(4))
            PictureBox47.Image = ImageList9.Images(c(10))
            PictureBox92.Image = ImageList9.Images(c(10))
            PictureBox91.Image = ImageList9.Images(c(2))
            PictureBox90.Image = ImageList9.Images(c(2))
            PictureBox89.Image = ImageList9.Images(c(2))
            PictureBox88.Image = ImageList9.Images(c(2))
            PictureBox87.Image = ImageList9.Images(c(2))
            PictureBox86.Image = ImageList9.Images(c(7))
            PictureBox85.Image = ImageList9.Images(c(4))
            PictureBox84.Image = ImageList9.Images(c(4))
            PictureBox83.Image = ImageList9.Images(c(4))
            PictureBox82.Image = ImageList9.Images(c(4))
            PictureBox81.Image = ImageList9.Images(c(4))
            PictureBox80.Image = ImageList9.Images(c(10))
            PictureBox148.Image = ImageList9.Images(c(10))
            PictureBox147.Image = ImageList9.Images(c(2))
            PictureBox146.Image = ImageList9.Images(c(8))
            PictureBox145.Image = ImageList9.Images(c(8))
            PictureBox124.Image = ImageList9.Images(c(8))
            PictureBox123.Image = ImageList9.Images(c(2))
            PictureBox122.Image = ImageList9.Images(c(7))
            PictureBox121.Image = ImageList9.Images(c(7))
            PictureBox120.Image = ImageList9.Images(c(7))
            PictureBox119.Image = ImageList9.Images(c(7))
            PictureBox118.Image = ImageList9.Images(c(7))
            PictureBox117.Image = ImageList9.Images(c(7))
            PictureBox116.Image = ImageList9.Images(c(10))
            PictureBox181.Image = ImageList9.Images(c(10))
            PictureBox180.Image = ImageList9.Images(c(2))
            PictureBox179.Image = ImageList9.Images(c(2))
            PictureBox178.Image = ImageList9.Images(c(2))
            PictureBox177.Image = ImageList9.Images(c(2))
            PictureBox176.Image = ImageList9.Images(c(2))
            PictureBox175.Image = ImageList9.Images(c(7))
            PictureBox174.Image = ImageList9.Images(c(7))
            PictureBox173.Image = ImageList9.Images(c(7))
            PictureBox172.Image = ImageList9.Images(c(7))
            PictureBox171.Image = ImageList9.Images(c(7))
            PictureBox170.Image = ImageList9.Images(c(7))
            PictureBox169.Image = ImageList9.Images(c(10))
            PictureBox214.Image = ImageList9.Images(c(10))
            PictureBox213.Image = ImageList9.Images(c(8))
            PictureBox212.Image = ImageList9.Images(c(8))
            PictureBox211.Image = ImageList9.Images(c(8))
            PictureBox210.Image = ImageList9.Images(c(8))
            PictureBox209.Image = ImageList9.Images(c(8))
            PictureBox208.Image = ImageList9.Images(c(7))
            PictureBox207.Image = ImageList9.Images(c(5))
            PictureBox206.Image = ImageList9.Images(c(5))
            PictureBox205.Image = ImageList9.Images(c(5))
            PictureBox204.Image = ImageList9.Images(c(5))
            PictureBox203.Image = ImageList9.Images(c(5))
            PictureBox202.Image = ImageList9.Images(c(10))
            PictureBox247.Image = ImageList9.Images(c(10))
            PictureBox246.Image = ImageList9.Images(c(3))
            PictureBox245.Image = ImageList9.Images(c(3))
            PictureBox244.Image = ImageList9.Images(c(3))
            PictureBox243.Image = ImageList9.Images(c(8))
            PictureBox242.Image = ImageList9.Images(c(8))
            PictureBox241.Image = ImageList9.Images(c(7))
            PictureBox240.Image = ImageList9.Images(c(5))
            PictureBox239.Image = ImageList9.Images(c(9))
            PictureBox238.Image = ImageList9.Images(c(9))
            PictureBox237.Image = ImageList9.Images(c(9))
            PictureBox236.Image = ImageList9.Images(c(5))
            PictureBox235.Image = ImageList9.Images(c(10))
            PictureBox280.Image = ImageList9.Images(c(10))
            PictureBox279.Image = ImageList9.Images(c(8))
            PictureBox278.Image = ImageList9.Images(c(8))
            PictureBox277.Image = ImageList9.Images(c(3))
            PictureBox276.Image = ImageList9.Images(c(8))
            PictureBox275.Image = ImageList9.Images(c(8))
            PictureBox274.Image = ImageList9.Images(c(7))
            PictureBox273.Image = ImageList9.Images(c(5))
            PictureBox272.Image = ImageList9.Images(c(5))
            PictureBox271.Image = ImageList9.Images(c(5))
            PictureBox270.Image = ImageList9.Images(c(5))
            PictureBox269.Image = ImageList9.Images(c(5))
            PictureBox268.Image = ImageList9.Images(c(10))
            PictureBox313.Image = ImageList9.Images(c(10))
            PictureBox312.Image = ImageList9.Images(c(3))
            PictureBox311.Image = ImageList9.Images(c(3))
            PictureBox310.Image = ImageList9.Images(c(3))
            PictureBox309.Image = ImageList9.Images(c(3))
            PictureBox308.Image = ImageList9.Images(c(3))
            PictureBox307.Image = ImageList9.Images(c(7))
            PictureBox306.Image = ImageList9.Images(c(7))
            PictureBox305.Image = ImageList9.Images(c(7))
            PictureBox304.Image = ImageList9.Images(c(7))
            PictureBox303.Image = ImageList9.Images(c(7))
            PictureBox302.Image = ImageList9.Images(c(7))
            PictureBox301.Image = ImageList9.Images(c(10))
            PictureBox346.Image = ImageList9.Images(c(10))
            PictureBox345.Image = ImageList9.Images(c(8))
            PictureBox344.Image = ImageList9.Images(c(8))
            PictureBox343.Image = ImageList9.Images(c(8))
            PictureBox342.Image = ImageList9.Images(c(8))
            PictureBox341.Image = ImageList9.Images(c(8))
            PictureBox340.Image = ImageList9.Images(c(7))
            PictureBox339.Image = ImageList9.Images(c(6))
            PictureBox338.Image = ImageList9.Images(c(6))
            PictureBox337.Image = ImageList9.Images(c(6))
            PictureBox336.Image = ImageList9.Images(c(7))
            PictureBox335.Image = ImageList9.Images(c(6))
            PictureBox334.Image = ImageList9.Images(c(10))
            PictureBox379.Image = ImageList9.Images(c(10))
            PictureBox378.Image = ImageList9.Images(c(8))
            PictureBox377.Image = ImageList9.Images(c(8))
            PictureBox376.Image = ImageList9.Images(c(8))
            PictureBox375.Image = ImageList9.Images(c(8))
            PictureBox374.Image = ImageList9.Images(c(8))
            PictureBox373.Image = ImageList9.Images(c(7))
            PictureBox372.Image = ImageList9.Images(c(7))
            PictureBox371.Image = ImageList9.Images(c(7))
            PictureBox370.Image = ImageList9.Images(c(7))
            PictureBox369.Image = ImageList9.Images(c(7))
            PictureBox368.Image = ImageList9.Images(c(7))
            PictureBox367.Image = ImageList9.Images(c(10))
        ElseIf style = 6 Then
            PictureBox1.Image = ImageList9.Images(c(10))
            PictureBox2.Image = ImageList9.Images(c(8))
            PictureBox3.Image = ImageList9.Images(c(8))
            PictureBox4.Image = ImageList9.Images(c(8))
            PictureBox5.Image = ImageList9.Images(c(8))
            PictureBox6.Image = ImageList9.Images(c(8))
            PictureBox7.Image = ImageList9.Images(c(9))
            PictureBox8.Image = ImageList9.Images(c(7))
            PictureBox9.Image = ImageList9.Images(c(7))
            PictureBox10.Image = ImageList9.Images(c(7))
            PictureBox11.Image = ImageList9.Images(c(7))
            PictureBox12.Image = ImageList9.Images(c(7))
            PictureBox13.Image = ImageList9.Images(c(10))
            PictureBox26.Image = ImageList9.Images(c(9))
            PictureBox25.Image = ImageList9.Images(c(1))
            PictureBox24.Image = ImageList9.Images(c(1))
            PictureBox23.Image = ImageList9.Images(c(1))
            PictureBox22.Image = ImageList9.Images(c(1))
            PictureBox21.Image = ImageList9.Images(c(1))
            PictureBox20.Image = ImageList9.Images(c(10))
            PictureBox19.Image = ImageList9.Images(c(4))
            PictureBox18.Image = ImageList9.Images(c(7))
            PictureBox17.Image = ImageList9.Images(c(4))
            PictureBox16.Image = ImageList9.Images(c(4))
            PictureBox15.Image = ImageList9.Images(c(4))
            PictureBox14.Image = ImageList9.Images(c(9))
            PictureBox59.Image = ImageList9.Images(c(10))
            PictureBox58.Image = ImageList9.Images(c(8))
            PictureBox57.Image = ImageList9.Images(c(8))
            PictureBox56.Image = ImageList9.Images(c(8))
            PictureBox55.Image = ImageList9.Images(c(8))
            PictureBox54.Image = ImageList9.Images(c(8))
            PictureBox53.Image = ImageList9.Images(c(9))
            PictureBox52.Image = ImageList9.Images(c(4))
            PictureBox51.Image = ImageList9.Images(c(7))
            PictureBox50.Image = ImageList9.Images(c(4))
            PictureBox49.Image = ImageList9.Images(c(7))
            PictureBox48.Image = ImageList9.Images(c(4))
            PictureBox47.Image = ImageList9.Images(c(10))
            PictureBox92.Image = ImageList9.Images(c(9))
            PictureBox91.Image = ImageList9.Images(c(2))
            PictureBox90.Image = ImageList9.Images(c(2))
            PictureBox89.Image = ImageList9.Images(c(2))
            PictureBox88.Image = ImageList9.Images(c(2))
            PictureBox87.Image = ImageList9.Images(c(2))
            PictureBox86.Image = ImageList9.Images(c(10))
            PictureBox85.Image = ImageList9.Images(c(4))
            PictureBox84.Image = ImageList9.Images(c(4))
            PictureBox83.Image = ImageList9.Images(c(4))
            PictureBox82.Image = ImageList9.Images(c(7))
            PictureBox81.Image = ImageList9.Images(c(4))
            PictureBox80.Image = ImageList9.Images(c(9))
            PictureBox148.Image = ImageList9.Images(c(10))
            PictureBox147.Image = ImageList9.Images(c(2))
            PictureBox146.Image = ImageList9.Images(c(8))
            PictureBox145.Image = ImageList9.Images(c(8))
            PictureBox124.Image = ImageList9.Images(c(8))
            PictureBox123.Image = ImageList9.Images(c(2))
            PictureBox122.Image = ImageList9.Images(c(9))
            PictureBox121.Image = ImageList9.Images(c(7))
            PictureBox120.Image = ImageList9.Images(c(7))
            PictureBox119.Image = ImageList9.Images(c(7))
            PictureBox118.Image = ImageList9.Images(c(7))
            PictureBox117.Image = ImageList9.Images(c(7))
            PictureBox116.Image = ImageList9.Images(c(10))
            PictureBox181.Image = ImageList9.Images(c(9))
            PictureBox180.Image = ImageList9.Images(c(2))
            PictureBox179.Image = ImageList9.Images(c(2))
            PictureBox178.Image = ImageList9.Images(c(2))
            PictureBox177.Image = ImageList9.Images(c(2))
            PictureBox176.Image = ImageList9.Images(c(2))
            PictureBox175.Image = ImageList9.Images(c(10))
            PictureBox174.Image = ImageList9.Images(c(5))
            PictureBox173.Image = ImageList9.Images(c(5))
            PictureBox172.Image = ImageList9.Images(c(7))
            PictureBox171.Image = ImageList9.Images(c(7))
            PictureBox170.Image = ImageList9.Images(c(7))
            PictureBox169.Image = ImageList9.Images(c(9))
            PictureBox214.Image = ImageList9.Images(c(10))
            PictureBox213.Image = ImageList9.Images(c(8))
            PictureBox212.Image = ImageList9.Images(c(8))
            PictureBox211.Image = ImageList9.Images(c(8))
            PictureBox210.Image = ImageList9.Images(c(8))
            PictureBox209.Image = ImageList9.Images(c(8))
            PictureBox208.Image = ImageList9.Images(c(9))
            PictureBox207.Image = ImageList9.Images(c(5))
            PictureBox206.Image = ImageList9.Images(c(7))
            PictureBox205.Image = ImageList9.Images(c(7))
            PictureBox204.Image = ImageList9.Images(c(7))
            PictureBox203.Image = ImageList9.Images(c(7))
            PictureBox202.Image = ImageList9.Images(c(10))
            PictureBox247.Image = ImageList9.Images(c(9))
            PictureBox246.Image = ImageList9.Images(c(3))
            PictureBox245.Image = ImageList9.Images(c(3))
            PictureBox244.Image = ImageList9.Images(c(3))
            PictureBox243.Image = ImageList9.Images(c(8))
            PictureBox242.Image = ImageList9.Images(c(8))
            PictureBox241.Image = ImageList9.Images(c(10))
            PictureBox240.Image = ImageList9.Images(c(5))
            PictureBox239.Image = ImageList9.Images(c(5))
            PictureBox238.Image = ImageList9.Images(c(5))
            PictureBox237.Image = ImageList9.Images(c(5))
            PictureBox236.Image = ImageList9.Images(c(5))
            PictureBox235.Image = ImageList9.Images(c(9))
            PictureBox280.Image = ImageList9.Images(c(10))
            PictureBox279.Image = ImageList9.Images(c(8))
            PictureBox278.Image = ImageList9.Images(c(8))
            PictureBox277.Image = ImageList9.Images(c(3))
            PictureBox276.Image = ImageList9.Images(c(8))
            PictureBox275.Image = ImageList9.Images(c(8))
            PictureBox274.Image = ImageList9.Images(c(9))
            PictureBox273.Image = ImageList9.Images(c(7))
            PictureBox272.Image = ImageList9.Images(c(7))
            PictureBox271.Image = ImageList9.Images(c(7))
            PictureBox270.Image = ImageList9.Images(c(7))
            PictureBox269.Image = ImageList9.Images(c(7))
            PictureBox268.Image = ImageList9.Images(c(10))
            PictureBox313.Image = ImageList9.Images(c(9))
            PictureBox312.Image = ImageList9.Images(c(3))
            PictureBox311.Image = ImageList9.Images(c(3))
            PictureBox310.Image = ImageList9.Images(c(3))
            PictureBox309.Image = ImageList9.Images(c(3))
            PictureBox308.Image = ImageList9.Images(c(3))
            PictureBox307.Image = ImageList9.Images(c(10))
            PictureBox306.Image = ImageList9.Images(c(6))
            PictureBox305.Image = ImageList9.Images(c(6))
            PictureBox304.Image = ImageList9.Images(c(6))
            PictureBox303.Image = ImageList9.Images(c(7))
            PictureBox302.Image = ImageList9.Images(c(6))
            PictureBox301.Image = ImageList9.Images(c(9))
            PictureBox346.Image = ImageList9.Images(c(10))
            PictureBox345.Image = ImageList9.Images(c(8))
            PictureBox344.Image = ImageList9.Images(c(8))
            PictureBox343.Image = ImageList9.Images(c(8))
            PictureBox342.Image = ImageList9.Images(c(8))
            PictureBox341.Image = ImageList9.Images(c(8))
            PictureBox340.Image = ImageList9.Images(c(9))
            PictureBox339.Image = ImageList9.Images(c(7))
            PictureBox338.Image = ImageList9.Images(c(7))
            PictureBox337.Image = ImageList9.Images(c(7))
            PictureBox336.Image = ImageList9.Images(c(7))
            PictureBox335.Image = ImageList9.Images(c(7))
            PictureBox334.Image = ImageList9.Images(c(10))
            PictureBox379.Image = ImageList9.Images(c(9))
            PictureBox378.Image = ImageList9.Images(c(8))
            PictureBox377.Image = ImageList9.Images(c(8))
            PictureBox376.Image = ImageList9.Images(c(8))
            PictureBox375.Image = ImageList9.Images(c(8))
            PictureBox374.Image = ImageList9.Images(c(8))
            PictureBox373.Image = ImageList9.Images(c(10))
            PictureBox372.Image = ImageList9.Images(c(7))
            PictureBox371.Image = ImageList9.Images(c(7))
            PictureBox370.Image = ImageList9.Images(c(7))
            PictureBox369.Image = ImageList9.Images(c(7))
            PictureBox368.Image = ImageList9.Images(c(7))
            PictureBox367.Image = ImageList9.Images(c(9))
        ElseIf style = 7 Then
            PictureBox1.Image = ImageList9.Images(c(2))
            PictureBox2.Image = ImageList9.Images(c(5))
            PictureBox3.Image = ImageList9.Images(c(6))
            PictureBox4.Image = ImageList9.Images(c(7))
            PictureBox5.Image = ImageList9.Images(c(8))
            PictureBox6.Image = ImageList9.Images(c(10))
            PictureBox7.Image = ImageList9.Images(c(8))
            PictureBox8.Image = ImageList9.Images(c(7))
            PictureBox9.Image = ImageList9.Images(c(6))
            PictureBox10.Image = ImageList9.Images(c(4))
            PictureBox11.Image = ImageList9.Images(c(2))
            PictureBox12.Image = ImageList9.Images(c(4))
            PictureBox13.Image = ImageList9.Images(c(6))
            PictureBox26.Image = ImageList9.Images(c(5))
            PictureBox25.Image = ImageList9.Images(c(2))
            PictureBox24.Image = ImageList9.Images(c(5))
            PictureBox23.Image = ImageList9.Images(c(6))
            PictureBox22.Image = ImageList9.Images(c(6))
            PictureBox21.Image = ImageList9.Images(c(6))
            PictureBox20.Image = ImageList9.Images(c(6))
            PictureBox19.Image = ImageList9.Images(c(6))
            PictureBox18.Image = ImageList9.Images(c(4))
            PictureBox17.Image = ImageList9.Images(c(2))
            PictureBox16.Image = ImageList9.Images(c(4))
            PictureBox15.Image = ImageList9.Images(c(6))
            PictureBox14.Image = ImageList9.Images(c(7))
            PictureBox59.Image = ImageList9.Images(c(6))
            PictureBox58.Image = ImageList9.Images(c(5))
            PictureBox57.Image = ImageList9.Images(c(1))
            PictureBox56.Image = ImageList9.Images(c(1))
            PictureBox55.Image = ImageList9.Images(c(1))
            PictureBox54.Image = ImageList9.Images(c(1))
            PictureBox53.Image = ImageList9.Images(c(6))
            PictureBox52.Image = ImageList9.Images(c(4))
            PictureBox51.Image = ImageList9.Images(c(2))
            PictureBox50.Image = ImageList9.Images(c(4))
            PictureBox49.Image = ImageList9.Images(c(6))
            PictureBox48.Image = ImageList9.Images(c(7))
            PictureBox47.Image = ImageList9.Images(c(8))
            PictureBox92.Image = ImageList9.Images(c(7))
            PictureBox91.Image = ImageList9.Images(c(6))
            PictureBox90.Image = ImageList9.Images(c(1))
            PictureBox89.Image = ImageList9.Images(c(3))
            PictureBox88.Image = ImageList9.Images(c(3))
            PictureBox87.Image = ImageList9.Images(c(3))
            PictureBox86.Image = ImageList9.Images(c(1))
            PictureBox85.Image = ImageList9.Images(c(2))
            PictureBox84.Image = ImageList9.Images(c(4))
            PictureBox83.Image = ImageList9.Images(c(6))
            PictureBox82.Image = ImageList9.Images(c(6))
            PictureBox81.Image = ImageList9.Images(c(8))
            PictureBox80.Image = ImageList9.Images(c(8))
            PictureBox148.Image = ImageList9.Images(c(8))
            PictureBox147.Image = ImageList9.Images(c(6))
            PictureBox146.Image = ImageList9.Images(c(1))
            PictureBox145.Image = ImageList9.Images(c(3))
            PictureBox124.Image = ImageList9.Images(c(3))
            PictureBox123.Image = ImageList9.Images(c(1))
            PictureBox122.Image = ImageList9.Images(c(3))
            PictureBox121.Image = ImageList9.Images(c(1))
            PictureBox120.Image = ImageList9.Images(c(1))
            PictureBox119.Image = ImageList9.Images(c(6))
            PictureBox118.Image = ImageList9.Images(c(1))
            PictureBox117.Image = ImageList9.Images(c(8))
            PictureBox116.Image = ImageList9.Images(c(8))
            PictureBox181.Image = ImageList9.Images(c(9))
            PictureBox180.Image = ImageList9.Images(c(6))
            PictureBox179.Image = ImageList9.Images(c(1))
            PictureBox178.Image = ImageList9.Images(c(1))
            PictureBox177.Image = ImageList9.Images(c(1))
            PictureBox176.Image = ImageList9.Images(c(3))
            PictureBox175.Image = ImageList9.Images(c(3))
            PictureBox174.Image = ImageList9.Images(c(1))
            PictureBox173.Image = ImageList9.Images(c(6))
            PictureBox172.Image = ImageList9.Images(c(6))
            PictureBox171.Image = ImageList9.Images(c(1))
            PictureBox170.Image = ImageList9.Images(c(8))
            PictureBox169.Image = ImageList9.Images(c(9))
            PictureBox214.Image = ImageList9.Images(c(8))
            PictureBox213.Image = ImageList9.Images(c(6))
            PictureBox212.Image = ImageList9.Images(c(1))
            PictureBox211.Image = ImageList9.Images(c(3))
            PictureBox210.Image = ImageList9.Images(c(3))
            PictureBox209.Image = ImageList9.Images(c(1))
            PictureBox208.Image = ImageList9.Images(c(3))
            PictureBox207.Image = ImageList9.Images(c(1))
            PictureBox206.Image = ImageList9.Images(c(1))
            PictureBox205.Image = ImageList9.Images(c(6))
            PictureBox204.Image = ImageList9.Images(c(1))
            PictureBox203.Image = ImageList9.Images(c(8))
            PictureBox202.Image = ImageList9.Images(c(8))
            PictureBox247.Image = ImageList9.Images(c(7))
            PictureBox246.Image = ImageList9.Images(c(6))
            PictureBox245.Image = ImageList9.Images(c(1))
            PictureBox244.Image = ImageList9.Images(c(3))
            PictureBox243.Image = ImageList9.Images(c(3))
            PictureBox242.Image = ImageList9.Images(c(3))
            PictureBox241.Image = ImageList9.Images(c(1))
            PictureBox240.Image = ImageList9.Images(c(2))
            PictureBox239.Image = ImageList9.Images(c(4))
            PictureBox238.Image = ImageList9.Images(c(6))
            PictureBox237.Image = ImageList9.Images(c(6))
            PictureBox236.Image = ImageList9.Images(c(8))
            PictureBox235.Image = ImageList9.Images(c(8))
            PictureBox280.Image = ImageList9.Images(c(6))
            PictureBox279.Image = ImageList9.Images(c(5))
            PictureBox278.Image = ImageList9.Images(c(1))
            PictureBox277.Image = ImageList9.Images(c(1))
            PictureBox276.Image = ImageList9.Images(c(1))
            PictureBox275.Image = ImageList9.Images(c(1))
            PictureBox274.Image = ImageList9.Images(c(6))
            PictureBox273.Image = ImageList9.Images(c(4))
            PictureBox272.Image = ImageList9.Images(c(2))
            PictureBox271.Image = ImageList9.Images(c(4))
            PictureBox270.Image = ImageList9.Images(c(6))
            PictureBox269.Image = ImageList9.Images(c(7))
            PictureBox268.Image = ImageList9.Images(c(8))
            PictureBox313.Image = ImageList9.Images(c(5))
            PictureBox312.Image = ImageList9.Images(c(2))
            PictureBox311.Image = ImageList9.Images(c(5))
            PictureBox310.Image = ImageList9.Images(c(6))
            PictureBox309.Image = ImageList9.Images(c(6))
            PictureBox308.Image = ImageList9.Images(c(6))
            PictureBox307.Image = ImageList9.Images(c(6))
            PictureBox306.Image = ImageList9.Images(c(6))
            PictureBox305.Image = ImageList9.Images(c(4))
            PictureBox304.Image = ImageList9.Images(c(2))
            PictureBox303.Image = ImageList9.Images(c(4))
            PictureBox302.Image = ImageList9.Images(c(6))
            PictureBox301.Image = ImageList9.Images(c(7))
            PictureBox346.Image = ImageList9.Images(c(2))
            PictureBox345.Image = ImageList9.Images(c(5))
            PictureBox344.Image = ImageList9.Images(c(6))
            PictureBox343.Image = ImageList9.Images(c(7))
            PictureBox342.Image = ImageList9.Images(c(8))
            PictureBox341.Image = ImageList9.Images(c(8))
            PictureBox340.Image = ImageList9.Images(c(8))
            PictureBox339.Image = ImageList9.Images(c(7))
            PictureBox338.Image = ImageList9.Images(c(6))
            PictureBox337.Image = ImageList9.Images(c(4))
            PictureBox336.Image = ImageList9.Images(c(2))
            PictureBox335.Image = ImageList9.Images(c(4))
            PictureBox334.Image = ImageList9.Images(c(6))
            PictureBox379.Image = ImageList9.Images(c(5))
            PictureBox378.Image = ImageList9.Images(c(6))
            PictureBox377.Image = ImageList9.Images(c(7))
            PictureBox376.Image = ImageList9.Images(c(8))
            PictureBox375.Image = ImageList9.Images(c(8))
            PictureBox374.Image = ImageList9.Images(c(10))
            PictureBox373.Image = ImageList9.Images(c(8))
            PictureBox372.Image = ImageList9.Images(c(8))
            PictureBox371.Image = ImageList9.Images(c(7))
            PictureBox370.Image = ImageList9.Images(c(6))
            PictureBox369.Image = ImageList9.Images(c(6))
            PictureBox368.Image = ImageList9.Images(c(4))
            PictureBox367.Image = ImageList9.Images(c(4))
        ElseIf style = 8 Then
            PictureBox1.Image = ImageList9.Images(c(9))
            PictureBox2.Image = ImageList9.Images(c(3))
            PictureBox3.Image = ImageList9.Images(c(7))
            PictureBox4.Image = ImageList9.Images(c(6))
            PictureBox5.Image = ImageList9.Images(c(8))
            PictureBox6.Image = ImageList9.Images(c(3))
            PictureBox7.Image = ImageList9.Images(c(7))
            PictureBox8.Image = ImageList9.Images(c(3))
            PictureBox9.Image = ImageList9.Images(c(3))
            PictureBox10.Image = ImageList9.Images(c(2))
            PictureBox11.Image = ImageList9.Images(c(2))
            PictureBox12.Image = ImageList9.Images(c(4))
            PictureBox13.Image = ImageList9.Images(c(3))
            PictureBox26.Image = ImageList9.Images(c(3))
            PictureBox25.Image = ImageList9.Images(c(1))
            PictureBox24.Image = ImageList9.Images(c(1))
            PictureBox23.Image = ImageList9.Images(c(1))
            PictureBox22.Image = ImageList9.Images(c(3))
            PictureBox21.Image = ImageList9.Images(c(3))
            PictureBox20.Image = ImageList9.Images(c(2))
            PictureBox19.Image = ImageList9.Images(c(3))
            PictureBox18.Image = ImageList9.Images(c(6))
            PictureBox17.Image = ImageList9.Images(c(2))
            PictureBox16.Image = ImageList9.Images(c(4))
            PictureBox15.Image = ImageList9.Images(c(3))
            PictureBox14.Image = ImageList9.Images(c(3))
            PictureBox59.Image = ImageList9.Images(c(8))
            PictureBox58.Image = ImageList9.Images(c(1))
            PictureBox57.Image = ImageList9.Images(c(1))
            PictureBox56.Image = ImageList9.Images(c(1))
            PictureBox55.Image = ImageList9.Images(c(3))
            PictureBox54.Image = ImageList9.Images(c(2))
            PictureBox53.Image = ImageList9.Images(c(2))
            PictureBox52.Image = ImageList9.Images(c(5))
            PictureBox51.Image = ImageList9.Images(c(1))
            PictureBox50.Image = ImageList9.Images(c(1))
            PictureBox49.Image = ImageList9.Images(c(1))
            PictureBox48.Image = ImageList9.Images(c(1))
            PictureBox47.Image = ImageList9.Images(c(7))
            PictureBox92.Image = ImageList9.Images(c(7))
            PictureBox91.Image = ImageList9.Images(c(1))
            PictureBox90.Image = ImageList9.Images(c(1))
            PictureBox89.Image = ImageList9.Images(c(1))
            PictureBox88.Image = ImageList9.Images(c(9))
            PictureBox87.Image = ImageList9.Images(c(2))
            PictureBox86.Image = ImageList9.Images(c(5))
            PictureBox85.Image = ImageList9.Images(c(5))
            PictureBox84.Image = ImageList9.Images(c(1))
            PictureBox83.Image = ImageList9.Images(c(1))
            PictureBox82.Image = ImageList9.Images(c(1))
            PictureBox81.Image = ImageList9.Images(c(1))
            PictureBox80.Image = ImageList9.Images(c(9))
            PictureBox148.Image = ImageList9.Images(c(4))
            PictureBox147.Image = ImageList9.Images(c(2))
            PictureBox146.Image = ImageList9.Images(c(9))
            PictureBox145.Image = ImageList9.Images(c(5))
            PictureBox124.Image = ImageList9.Images(c(4))
            PictureBox123.Image = ImageList9.Images(c(4))
            PictureBox122.Image = ImageList9.Images(c(1))
            PictureBox121.Image = ImageList9.Images(c(1))
            PictureBox120.Image = ImageList9.Images(c(1))
            PictureBox119.Image = ImageList9.Images(c(6))
            PictureBox118.Image = ImageList9.Images(c(10))
            PictureBox117.Image = ImageList9.Images(c(8))
            PictureBox116.Image = ImageList9.Images(c(2))
            PictureBox181.Image = ImageList9.Images(c(4))
            PictureBox180.Image = ImageList9.Images(c(4))
            PictureBox179.Image = ImageList9.Images(c(2))
            PictureBox178.Image = ImageList9.Images(c(2))
            PictureBox177.Image = ImageList9.Images(c(5))
            PictureBox176.Image = ImageList9.Images(c(4))
            PictureBox175.Image = ImageList9.Images(c(1))
            PictureBox174.Image = ImageList9.Images(c(1))
            PictureBox173.Image = ImageList9.Images(c(1))
            PictureBox172.Image = ImageList9.Images(c(9))
            PictureBox171.Image = ImageList9.Images(c(3))
            PictureBox170.Image = ImageList9.Images(c(2))
            PictureBox169.Image = ImageList9.Images(c(2))
            PictureBox214.Image = ImageList9.Images(c(2))
            PictureBox213.Image = ImageList9.Images(c(2))
            PictureBox212.Image = ImageList9.Images(c(6))
            PictureBox211.Image = ImageList9.Images(c(2))
            PictureBox210.Image = ImageList9.Images(c(3))
            PictureBox209.Image = ImageList9.Images(c(5))
            PictureBox208.Image = ImageList9.Images(c(1))
            PictureBox207.Image = ImageList9.Images(c(1))
            PictureBox206.Image = ImageList9.Images(c(1))
            PictureBox205.Image = ImageList9.Images(c(3))
            PictureBox204.Image = ImageList9.Images(c(3))
            PictureBox203.Image = ImageList9.Images(c(2))
            PictureBox202.Image = ImageList9.Images(c(10))
            PictureBox247.Image = ImageList9.Images(c(2))
            PictureBox246.Image = ImageList9.Images(c(1))
            PictureBox245.Image = ImageList9.Images(c(1))
            PictureBox244.Image = ImageList9.Images(c(1))
            PictureBox243.Image = ImageList9.Images(c(3))
            PictureBox242.Image = ImageList9.Images(c(3))
            PictureBox241.Image = ImageList9.Images(c(5))
            PictureBox240.Image = ImageList9.Images(c(5))
            PictureBox239.Image = ImageList9.Images(c(1))
            PictureBox238.Image = ImageList9.Images(c(1))
            PictureBox237.Image = ImageList9.Images(c(1))
            PictureBox236.Image = ImageList9.Images(c(1))
            PictureBox235.Image = ImageList9.Images(c(4))
            PictureBox280.Image = ImageList9.Images(c(6))
            PictureBox279.Image = ImageList9.Images(c(1))
            PictureBox278.Image = ImageList9.Images(c(1))
            PictureBox277.Image = ImageList9.Images(c(1))
            PictureBox276.Image = ImageList9.Images(c(8))
            PictureBox275.Image = ImageList9.Images(c(3))
            PictureBox274.Image = ImageList9.Images(c(7))
            PictureBox273.Image = ImageList9.Images(c(5))
            PictureBox272.Image = ImageList9.Images(c(1))
            PictureBox271.Image = ImageList9.Images(c(1))
            PictureBox270.Image = ImageList9.Images(c(1))
            PictureBox269.Image = ImageList9.Images(c(1))
            PictureBox268.Image = ImageList9.Images(c(4))
            PictureBox313.Image = ImageList9.Images(c(6))
            PictureBox312.Image = ImageList9.Images(c(1))
            PictureBox311.Image = ImageList9.Images(c(1))
            PictureBox310.Image = ImageList9.Images(c(1))
            PictureBox309.Image = ImageList9.Images(c(2))
            PictureBox308.Image = ImageList9.Images(c(10))
            PictureBox307.Image = ImageList9.Images(c(2))
            PictureBox306.Image = ImageList9.Images(c(2))
            PictureBox305.Image = ImageList9.Images(c(4))
            PictureBox304.Image = ImageList9.Images(c(2))
            PictureBox303.Image = ImageList9.Images(c(2))
            PictureBox302.Image = ImageList9.Images(c(4))
            PictureBox301.Image = ImageList9.Images(c(8))
            PictureBox346.Image = ImageList9.Images(c(6))
            PictureBox345.Image = ImageList9.Images(c(3))
            PictureBox344.Image = ImageList9.Images(c(7))
            PictureBox343.Image = ImageList9.Images(c(2))
            PictureBox342.Image = ImageList9.Images(c(2))
            PictureBox341.Image = ImageList9.Images(c(2))
            PictureBox340.Image = ImageList9.Images(c(2))
            PictureBox339.Image = ImageList9.Images(c(4))
            PictureBox338.Image = ImageList9.Images(c(4))
            PictureBox337.Image = ImageList9.Images(c(3))
            PictureBox336.Image = ImageList9.Images(c(2))
            PictureBox335.Image = ImageList9.Images(c(2))
            PictureBox334.Image = ImageList9.Images(c(6))
            PictureBox379.Image = ImageList9.Images(c(3))
            PictureBox378.Image = ImageList9.Images(c(3))
            PictureBox377.Image = ImageList9.Images(c(2))
            PictureBox376.Image = ImageList9.Images(c(2))
            PictureBox375.Image = ImageList9.Images(c(9))
            PictureBox374.Image = ImageList9.Images(c(6))
            PictureBox373.Image = ImageList9.Images(c(8))
            PictureBox372.Image = ImageList9.Images(c(4))
            PictureBox371.Image = ImageList9.Images(c(3))
            PictureBox370.Image = ImageList9.Images(c(3))
            PictureBox369.Image = ImageList9.Images(c(10))
            PictureBox368.Image = ImageList9.Images(c(2))
            PictureBox367.Image = ImageList9.Images(c(7))
        ElseIf style = 9 Then
            PictureBox1.Image = ImageList9.Images(c(3))
            PictureBox2.Image = ImageList9.Images(c(2))
            PictureBox3.Image = ImageList9.Images(c(2))
            PictureBox4.Image = ImageList9.Images(c(2))
            PictureBox5.Image = ImageList9.Images(c(3))
            PictureBox6.Image = ImageList9.Images(c(7))
            PictureBox7.Image = ImageList9.Images(c(5))
            PictureBox8.Image = ImageList9.Images(c(3))
            PictureBox9.Image = ImageList9.Images(c(2))
            PictureBox10.Image = ImageList9.Images(c(2))
            PictureBox11.Image = ImageList9.Images(c(3))
            PictureBox12.Image = ImageList9.Images(c(6))
            PictureBox13.Image = ImageList9.Images(c(4))
            PictureBox26.Image = ImageList9.Images(c(3))
            PictureBox25.Image = ImageList9.Images(c(2))
            PictureBox24.Image = ImageList9.Images(c(1))
            PictureBox23.Image = ImageList9.Images(c(2))
            PictureBox22.Image = ImageList9.Images(c(3))
            PictureBox21.Image = ImageList9.Images(c(3))
            PictureBox20.Image = ImageList9.Images(c(3))
            PictureBox19.Image = ImageList9.Images(c(3))
            PictureBox18.Image = ImageList9.Images(c(2))
            PictureBox17.Image = ImageList9.Images(c(1))
            PictureBox16.Image = ImageList9.Images(c(2))
            PictureBox15.Image = ImageList9.Images(c(3))
            PictureBox14.Image = ImageList9.Images(c(9))
            PictureBox59.Image = ImageList9.Images(c(3))
            PictureBox58.Image = ImageList9.Images(c(2))
            PictureBox57.Image = ImageList9.Images(c(1))
            PictureBox56.Image = ImageList9.Images(c(2))
            PictureBox55.Image = ImageList9.Images(c(2))
            PictureBox54.Image = ImageList9.Images(c(2))
            PictureBox53.Image = ImageList9.Images(c(2))
            PictureBox52.Image = ImageList9.Images(c(2))
            PictureBox51.Image = ImageList9.Images(c(2))
            PictureBox50.Image = ImageList9.Images(c(2))
            PictureBox49.Image = ImageList9.Images(c(1))
            PictureBox48.Image = ImageList9.Images(c(2))
            PictureBox47.Image = ImageList9.Images(c(3))
            PictureBox92.Image = ImageList9.Images(c(3))
            PictureBox91.Image = ImageList9.Images(c(2))
            PictureBox90.Image = ImageList9.Images(c(1))
            PictureBox89.Image = ImageList9.Images(c(1))
            PictureBox88.Image = ImageList9.Images(c(1))
            PictureBox87.Image = ImageList9.Images(c(1))
            PictureBox86.Image = ImageList9.Images(c(1))
            PictureBox85.Image = ImageList9.Images(c(1))
            PictureBox84.Image = ImageList9.Images(c(1))
            PictureBox83.Image = ImageList9.Images(c(1))
            PictureBox82.Image = ImageList9.Images(c(2))
            PictureBox81.Image = ImageList9.Images(c(3))
            PictureBox80.Image = ImageList9.Images(c(10))
            PictureBox148.Image = ImageList9.Images(c(3))
            PictureBox147.Image = ImageList9.Images(c(2))
            PictureBox146.Image = ImageList9.Images(c(1))
            PictureBox145.Image = ImageList9.Images(c(2))
            PictureBox124.Image = ImageList9.Images(c(2))
            PictureBox123.Image = ImageList9.Images(c(2))
            PictureBox122.Image = ImageList9.Images(c(2))
            PictureBox121.Image = ImageList9.Images(c(2))
            PictureBox120.Image = ImageList9.Images(c(2))
            PictureBox119.Image = ImageList9.Images(c(2))
            PictureBox118.Image = ImageList9.Images(c(3))
            PictureBox117.Image = ImageList9.Images(c(7))
            PictureBox116.Image = ImageList9.Images(c(5))
            PictureBox181.Image = ImageList9.Images(c(3))
            PictureBox180.Image = ImageList9.Images(c(2))
            PictureBox179.Image = ImageList9.Images(c(1))
            PictureBox178.Image = ImageList9.Images(c(2))
            PictureBox177.Image = ImageList9.Images(c(2))
            PictureBox176.Image = ImageList9.Images(c(2))
            PictureBox175.Image = ImageList9.Images(c(3))
            PictureBox174.Image = ImageList9.Images(c(3))
            PictureBox173.Image = ImageList9.Images(c(3))
            PictureBox172.Image = ImageList9.Images(c(3))
            PictureBox171.Image = ImageList9.Images(c(2))
            PictureBox170.Image = ImageList9.Images(c(3))
            PictureBox169.Image = ImageList9.Images(c(9))
            PictureBox214.Image = ImageList9.Images(c(5))
            PictureBox213.Image = ImageList9.Images(c(3))
            PictureBox212.Image = ImageList9.Images(c(2))
            PictureBox211.Image = ImageList9.Images(c(1))
            PictureBox210.Image = ImageList9.Images(c(1))
            PictureBox209.Image = ImageList9.Images(c(1))
            PictureBox208.Image = ImageList9.Images(c(2))
            PictureBox207.Image = ImageList9.Images(c(3))
            PictureBox206.Image = ImageList9.Images(c(3))
            PictureBox205.Image = ImageList9.Images(c(2))
            PictureBox204.Image = ImageList9.Images(c(1))
            PictureBox203.Image = ImageList9.Images(c(2))
            PictureBox202.Image = ImageList9.Images(c(3))
            PictureBox247.Image = ImageList9.Images(c(3))
            PictureBox246.Image = ImageList9.Images(c(2))
            PictureBox245.Image = ImageList9.Images(c(1))
            PictureBox244.Image = ImageList9.Images(c(2))
            PictureBox243.Image = ImageList9.Images(c(2))
            PictureBox242.Image = ImageList9.Images(c(2))
            PictureBox241.Image = ImageList9.Images(c(1))
            PictureBox240.Image = ImageList9.Images(c(2))
            PictureBox239.Image = ImageList9.Images(c(3))
            PictureBox238.Image = ImageList9.Images(c(2))
            PictureBox237.Image = ImageList9.Images(c(1))
            PictureBox236.Image = ImageList9.Images(c(2))
            PictureBox235.Image = ImageList9.Images(c(3))
            PictureBox280.Image = ImageList9.Images(c(3))
            PictureBox279.Image = ImageList9.Images(c(2))
            PictureBox278.Image = ImageList9.Images(c(1))
            PictureBox277.Image = ImageList9.Images(c(2))
            PictureBox276.Image = ImageList9.Images(c(3))
            PictureBox275.Image = ImageList9.Images(c(2))
            PictureBox274.Image = ImageList9.Images(c(1))
            PictureBox273.Image = ImageList9.Images(c(2))
            PictureBox272.Image = ImageList9.Images(c(3))
            PictureBox271.Image = ImageList9.Images(c(2))
            PictureBox270.Image = ImageList9.Images(c(1))
            PictureBox269.Image = ImageList9.Images(c(2))
            PictureBox268.Image = ImageList9.Images(c(3))
            PictureBox313.Image = ImageList9.Images(c(3))
            PictureBox312.Image = ImageList9.Images(c(2))
            PictureBox311.Image = ImageList9.Images(c(1))
            PictureBox310.Image = ImageList9.Images(c(2))
            PictureBox309.Image = ImageList9.Images(c(3))
            PictureBox308.Image = ImageList9.Images(c(2))
            PictureBox307.Image = ImageList9.Images(c(1))
            PictureBox306.Image = ImageList9.Images(c(2))
            PictureBox305.Image = ImageList9.Images(c(2))
            PictureBox304.Image = ImageList9.Images(c(2))
            PictureBox303.Image = ImageList9.Images(c(1))
            PictureBox302.Image = ImageList9.Images(c(2))
            PictureBox301.Image = ImageList9.Images(c(3))
            PictureBox346.Image = ImageList9.Images(c(3))
            PictureBox345.Image = ImageList9.Images(c(2))
            PictureBox344.Image = ImageList9.Images(c(1))
            PictureBox343.Image = ImageList9.Images(c(2))
            PictureBox342.Image = ImageList9.Images(c(8))
            PictureBox341.Image = ImageList9.Images(c(3))
            PictureBox340.Image = ImageList9.Images(c(2))
            PictureBox339.Image = ImageList9.Images(c(1))
            PictureBox338.Image = ImageList9.Images(c(1))
            PictureBox337.Image = ImageList9.Images(c(1))
            PictureBox336.Image = ImageList9.Images(c(2))
            PictureBox335.Image = ImageList9.Images(c(3))
            PictureBox334.Image = ImageList9.Images(c(6))
            PictureBox379.Image = ImageList9.Images(c(7))
            PictureBox378.Image = ImageList9.Images(c(3))
            PictureBox377.Image = ImageList9.Images(c(2))
            PictureBox376.Image = ImageList9.Images(c(3))
            PictureBox375.Image = ImageList9.Images(c(4))
            PictureBox374.Image = ImageList9.Images(c(6))
            PictureBox373.Image = ImageList9.Images(c(3))
            PictureBox372.Image = ImageList9.Images(c(2))
            PictureBox371.Image = ImageList9.Images(c(2))
            PictureBox370.Image = ImageList9.Images(c(2))
            PictureBox369.Image = ImageList9.Images(c(3))
            PictureBox368.Image = ImageList9.Images(c(8))
            PictureBox367.Image = ImageList9.Images(c(4))
        ElseIf style = 10 Then
            PictureBox1.Image = ImageList9.Images(c(7))
            PictureBox2.Image = ImageList9.Images(c(7))
            PictureBox3.Image = ImageList9.Images(c(7))
            PictureBox4.Image = ImageList9.Images(c(8))
            PictureBox5.Image = ImageList9.Images(c(8))
            PictureBox6.Image = ImageList9.Images(c(5))
            PictureBox7.Image = ImageList9.Images(c(5))
            PictureBox8.Image = ImageList9.Images(c(5))
            PictureBox9.Image = ImageList9.Images(c(5))
            PictureBox10.Image = ImageList9.Images(c(4))
            PictureBox11.Image = ImageList9.Images(c(4))
            PictureBox12.Image = ImageList9.Images(c(4))
            PictureBox13.Image = ImageList9.Images(c(3))
            PictureBox26.Image = ImageList9.Images(c(7))
            PictureBox25.Image = ImageList9.Images(c(7))
            PictureBox24.Image = ImageList9.Images(c(8))
            PictureBox23.Image = ImageList9.Images(c(8))
            PictureBox22.Image = ImageList9.Images(c(6))
            PictureBox21.Image = ImageList9.Images(c(5))
            PictureBox20.Image = ImageList9.Images(c(5))
            PictureBox19.Image = ImageList9.Images(c(5))
            PictureBox18.Image = ImageList9.Images(c(5))
            PictureBox17.Image = ImageList9.Images(c(4))
            PictureBox16.Image = ImageList9.Images(c(4))
            PictureBox15.Image = ImageList9.Images(c(4))
            PictureBox14.Image = ImageList9.Images(c(3))
            PictureBox59.Image = ImageList9.Images(c(7))
            PictureBox58.Image = ImageList9.Images(c(8))
            PictureBox57.Image = ImageList9.Images(c(8))
            PictureBox56.Image = ImageList9.Images(c(6))
            PictureBox55.Image = ImageList9.Images(c(6))
            PictureBox54.Image = ImageList9.Images(c(5))
            PictureBox53.Image = ImageList9.Images(c(5))
            PictureBox52.Image = ImageList9.Images(c(5))
            PictureBox51.Image = ImageList9.Images(c(5))
            PictureBox50.Image = ImageList9.Images(c(4))
            PictureBox49.Image = ImageList9.Images(c(4))
            PictureBox48.Image = ImageList9.Images(c(4))
            PictureBox47.Image = ImageList9.Images(c(3))
            PictureBox92.Image = ImageList9.Images(c(7))
            PictureBox91.Image = ImageList9.Images(c(8))
            PictureBox90.Image = ImageList9.Images(c(6))
            PictureBox89.Image = ImageList9.Images(c(6))
            PictureBox88.Image = ImageList9.Images(c(6))
            PictureBox87.Image = ImageList9.Images(c(5))
            PictureBox86.Image = ImageList9.Images(c(5))
            PictureBox85.Image = ImageList9.Images(c(5))
            PictureBox84.Image = ImageList9.Images(c(5))
            PictureBox83.Image = ImageList9.Images(c(4))
            PictureBox82.Image = ImageList9.Images(c(4))
            PictureBox81.Image = ImageList9.Images(c(4))
            PictureBox80.Image = ImageList9.Images(c(3))
            PictureBox148.Image = ImageList9.Images(c(9))
            PictureBox147.Image = ImageList9.Images(c(8))
            PictureBox146.Image = ImageList9.Images(c(8))
            PictureBox145.Image = ImageList9.Images(c(6))
            PictureBox124.Image = ImageList9.Images(c(6))
            PictureBox123.Image = ImageList9.Images(c(6))
            PictureBox122.Image = ImageList9.Images(c(5))
            PictureBox121.Image = ImageList9.Images(c(5))
            PictureBox120.Image = ImageList9.Images(c(4))
            PictureBox119.Image = ImageList9.Images(c(4))
            PictureBox118.Image = ImageList9.Images(c(4))
            PictureBox117.Image = ImageList9.Images(c(3))
            PictureBox116.Image = ImageList9.Images(c(3))
            PictureBox181.Image = ImageList9.Images(c(9))
            PictureBox180.Image = ImageList9.Images(c(8))
            PictureBox179.Image = ImageList9.Images(c(10))
            PictureBox178.Image = ImageList9.Images(c(8))
            PictureBox177.Image = ImageList9.Images(c(6))
            PictureBox176.Image = ImageList9.Images(c(6))
            PictureBox175.Image = ImageList9.Images(c(5))
            PictureBox174.Image = ImageList9.Images(c(5))
            PictureBox173.Image = ImageList9.Images(c(4))
            PictureBox172.Image = ImageList9.Images(c(4))
            PictureBox171.Image = ImageList9.Images(c(4))
            PictureBox170.Image = ImageList9.Images(c(3))
            PictureBox169.Image = ImageList9.Images(c(3))
            PictureBox214.Image = ImageList9.Images(c(9))
            PictureBox213.Image = ImageList9.Images(c(8))
            PictureBox212.Image = ImageList9.Images(c(8))
            PictureBox211.Image = ImageList9.Images(c(6))
            PictureBox210.Image = ImageList9.Images(c(6))
            PictureBox209.Image = ImageList9.Images(c(6))
            PictureBox208.Image = ImageList9.Images(c(5))
            PictureBox207.Image = ImageList9.Images(c(4))
            PictureBox206.Image = ImageList9.Images(c(4))
            PictureBox205.Image = ImageList9.Images(c(4))
            PictureBox204.Image = ImageList9.Images(c(4))
            PictureBox203.Image = ImageList9.Images(c(3))
            PictureBox202.Image = ImageList9.Images(c(1))
            PictureBox247.Image = ImageList9.Images(c(9))
            PictureBox246.Image = ImageList9.Images(c(9))
            PictureBox245.Image = ImageList9.Images(c(6))
            PictureBox244.Image = ImageList9.Images(c(6))
            PictureBox243.Image = ImageList9.Images(c(6))
            PictureBox242.Image = ImageList9.Images(c(6))
            PictureBox241.Image = ImageList9.Images(c(5))
            PictureBox240.Image = ImageList9.Images(c(4))
            PictureBox239.Image = ImageList9.Images(c(4))
            PictureBox238.Image = ImageList9.Images(c(4))
            PictureBox237.Image = ImageList9.Images(c(3))
            PictureBox236.Image = ImageList9.Images(c(1))
            PictureBox235.Image = ImageList9.Images(c(2))
            PictureBox280.Image = ImageList9.Images(c(9))
            PictureBox279.Image = ImageList9.Images(c(6))
            PictureBox278.Image = ImageList9.Images(c(6))
            PictureBox277.Image = ImageList9.Images(c(6))
            PictureBox276.Image = ImageList9.Images(c(6))
            PictureBox275.Image = ImageList9.Images(c(6))
            PictureBox274.Image = ImageList9.Images(c(5))
            PictureBox273.Image = ImageList9.Images(c(4))
            PictureBox272.Image = ImageList9.Images(c(4))
            PictureBox271.Image = ImageList9.Images(c(4))
            PictureBox270.Image = ImageList9.Images(c(2))
            PictureBox269.Image = ImageList9.Images(c(1))
            PictureBox268.Image = ImageList9.Images(c(1))
            PictureBox313.Image = ImageList9.Images(c(7))
            PictureBox312.Image = ImageList9.Images(c(8))
            PictureBox311.Image = ImageList9.Images(c(9))
            PictureBox310.Image = ImageList9.Images(c(6))
            PictureBox309.Image = ImageList9.Images(c(6))
            PictureBox308.Image = ImageList9.Images(c(6))
            PictureBox307.Image = ImageList9.Images(c(5))
            PictureBox306.Image = ImageList9.Images(c(4))
            PictureBox305.Image = ImageList9.Images(c(4))
            PictureBox304.Image = ImageList9.Images(c(2))
            PictureBox303.Image = ImageList9.Images(c(2))
            PictureBox302.Image = ImageList9.Images(c(1))
            PictureBox301.Image = ImageList9.Images(c(2))
            PictureBox346.Image = ImageList9.Images(c(7))
            PictureBox345.Image = ImageList9.Images(c(8))
            PictureBox344.Image = ImageList9.Images(c(9))
            PictureBox343.Image = ImageList9.Images(c(9))
            PictureBox342.Image = ImageList9.Images(c(6))
            PictureBox341.Image = ImageList9.Images(c(5))
            PictureBox340.Image = ImageList9.Images(c(5))
            PictureBox339.Image = ImageList9.Images(c(4))
            PictureBox338.Image = ImageList9.Images(c(4))
            PictureBox337.Image = ImageList9.Images(c(1))
            PictureBox336.Image = ImageList9.Images(c(2))
            PictureBox335.Image = ImageList9.Images(c(2))
            PictureBox334.Image = ImageList9.Images(c(2))
            PictureBox379.Image = ImageList9.Images(c(7))
            PictureBox378.Image = ImageList9.Images(c(7))
            PictureBox377.Image = ImageList9.Images(c(8))
            PictureBox376.Image = ImageList9.Images(c(8))
            PictureBox375.Image = ImageList9.Images(c(9))
            PictureBox374.Image = ImageList9.Images(c(5))
            PictureBox373.Image = ImageList9.Images(c(5))
            PictureBox372.Image = ImageList9.Images(c(4))
            PictureBox371.Image = ImageList9.Images(c(2))
            PictureBox370.Image = ImageList9.Images(c(2))
            PictureBox369.Image = ImageList9.Images(c(2))
            PictureBox368.Image = ImageList9.Images(c(1))
            PictureBox367.Image = ImageList9.Images(c(1))
        ElseIf style = 11 Then
            PictureBox1.Image = ImageList9.Images(c(5))
            PictureBox2.Image = ImageList9.Images(c(5))
            PictureBox3.Image = ImageList9.Images(c(5))
            PictureBox4.Image = ImageList9.Images(c(5))
            PictureBox5.Image = ImageList9.Images(c(5))
            PictureBox6.Image = ImageList9.Images(c(5))
            PictureBox7.Image = ImageList9.Images(c(5))
            PictureBox8.Image = ImageList9.Images(c(5))
            PictureBox9.Image = ImageList9.Images(c(5))
            PictureBox10.Image = ImageList9.Images(c(4))
            PictureBox11.Image = ImageList9.Images(c(4))
            PictureBox12.Image = ImageList9.Images(c(5))
            PictureBox13.Image = ImageList9.Images(c(9))
            PictureBox26.Image = ImageList9.Images(c(5))
            PictureBox25.Image = ImageList9.Images(c(5))
            PictureBox24.Image = ImageList9.Images(c(5))
            PictureBox23.Image = ImageList9.Images(c(5))
            PictureBox22.Image = ImageList9.Images(c(5))
            PictureBox21.Image = ImageList9.Images(c(5))
            PictureBox20.Image = ImageList9.Images(c(5))
            PictureBox19.Image = ImageList9.Images(c(5))
            PictureBox18.Image = ImageList9.Images(c(4))
            PictureBox17.Image = ImageList9.Images(c(3))
            PictureBox16.Image = ImageList9.Images(c(3))
            PictureBox15.Image = ImageList9.Images(c(4))
            PictureBox14.Image = ImageList9.Images(c(9))
            PictureBox59.Image = ImageList9.Images(c(5))
            PictureBox58.Image = ImageList9.Images(c(5))
            PictureBox57.Image = ImageList9.Images(c(5))
            PictureBox56.Image = ImageList9.Images(c(5))
            PictureBox55.Image = ImageList9.Images(c(5))
            PictureBox54.Image = ImageList9.Images(c(5))
            PictureBox53.Image = ImageList9.Images(c(5))
            PictureBox52.Image = ImageList9.Images(c(5))
            PictureBox51.Image = ImageList9.Images(c(4))
            PictureBox50.Image = ImageList9.Images(c(3))
            PictureBox49.Image = ImageList9.Images(c(3))
            PictureBox48.Image = ImageList9.Images(c(4))
            PictureBox47.Image = ImageList9.Images(c(8))
            PictureBox92.Image = ImageList9.Images(c(5))
            PictureBox91.Image = ImageList9.Images(c(5))
            PictureBox90.Image = ImageList9.Images(c(1))
            PictureBox89.Image = ImageList9.Images(c(1))
            PictureBox88.Image = ImageList9.Images(c(1))
            PictureBox87.Image = ImageList9.Images(c(9))
            PictureBox86.Image = ImageList9.Images(c(9))
            PictureBox85.Image = ImageList9.Images(c(8))
            PictureBox84.Image = ImageList9.Images(c(8))
            PictureBox83.Image = ImageList9.Images(c(4))
            PictureBox82.Image = ImageList9.Images(c(4))
            PictureBox81.Image = ImageList9.Images(c(7))
            PictureBox80.Image = ImageList9.Images(c(8))
            PictureBox148.Image = ImageList9.Images(c(5))
            PictureBox147.Image = ImageList9.Images(c(2))
            PictureBox146.Image = ImageList9.Images(c(1))
            PictureBox145.Image = ImageList9.Images(c(6))
            PictureBox124.Image = ImageList9.Images(c(1))
            PictureBox123.Image = ImageList9.Images(c(10))
            PictureBox122.Image = ImageList9.Images(c(10))
            PictureBox121.Image = ImageList9.Images(c(9))
            PictureBox120.Image = ImageList9.Images(c(8))
            PictureBox119.Image = ImageList9.Images(c(8))
            PictureBox118.Image = ImageList9.Images(c(7))
            PictureBox117.Image = ImageList9.Images(c(7))
            PictureBox116.Image = ImageList9.Images(c(8))
            PictureBox181.Image = ImageList9.Images(c(5))
            PictureBox180.Image = ImageList9.Images(c(2))
            PictureBox179.Image = ImageList9.Images(c(1))
            PictureBox178.Image = ImageList9.Images(c(1))
            PictureBox177.Image = ImageList9.Images(c(10))
            PictureBox176.Image = ImageList9.Images(c(10))
            PictureBox175.Image = ImageList9.Images(c(10))
            PictureBox174.Image = ImageList9.Images(c(9))
            PictureBox173.Image = ImageList9.Images(c(8))
            PictureBox172.Image = ImageList9.Images(c(8))
            PictureBox171.Image = ImageList9.Images(c(7))
            PictureBox170.Image = ImageList9.Images(c(8))
            PictureBox169.Image = ImageList9.Images(c(8))
            PictureBox214.Image = ImageList9.Images(c(5))
            PictureBox213.Image = ImageList9.Images(c(2))
            PictureBox212.Image = ImageList9.Images(c(1))
            PictureBox211.Image = ImageList9.Images(c(6))
            PictureBox210.Image = ImageList9.Images(c(1))
            PictureBox209.Image = ImageList9.Images(c(9))
            PictureBox208.Image = ImageList9.Images(c(9))
            PictureBox207.Image = ImageList9.Images(c(8))
            PictureBox206.Image = ImageList9.Images(c(8))
            PictureBox205.Image = ImageList9.Images(c(7))
            PictureBox204.Image = ImageList9.Images(c(7))
            PictureBox203.Image = ImageList9.Images(c(8))
            PictureBox202.Image = ImageList9.Images(c(8))
            PictureBox247.Image = ImageList9.Images(c(5))
            PictureBox246.Image = ImageList9.Images(c(2))
            PictureBox245.Image = ImageList9.Images(c(1))
            PictureBox244.Image = ImageList9.Images(c(2))
            PictureBox243.Image = ImageList9.Images(c(1))
            PictureBox242.Image = ImageList9.Images(c(9))
            PictureBox241.Image = ImageList9.Images(c(9))
            PictureBox240.Image = ImageList9.Images(c(8))
            PictureBox239.Image = ImageList9.Images(c(8))
            PictureBox238.Image = ImageList9.Images(c(7))
            PictureBox237.Image = ImageList9.Images(c(7))
            PictureBox236.Image = ImageList9.Images(c(7))
            PictureBox235.Image = ImageList9.Images(c(7))
            PictureBox280.Image = ImageList9.Images(c(5))
            PictureBox279.Image = ImageList9.Images(c(5))
            PictureBox278.Image = ImageList9.Images(c(2))
            PictureBox277.Image = ImageList9.Images(c(1))
            PictureBox276.Image = ImageList9.Images(c(9))
            PictureBox275.Image = ImageList9.Images(c(9))
            PictureBox274.Image = ImageList9.Images(c(8))
            PictureBox273.Image = ImageList9.Images(c(8))
            PictureBox272.Image = ImageList9.Images(c(7))
            PictureBox271.Image = ImageList9.Images(c(7))
            PictureBox270.Image = ImageList9.Images(c(7))
            PictureBox269.Image = ImageList9.Images(c(7))
            PictureBox268.Image = ImageList9.Images(c(7))
            PictureBox313.Image = ImageList9.Images(c(5))
            PictureBox312.Image = ImageList9.Images(c(5))
            PictureBox311.Image = ImageList9.Images(c(1))
            PictureBox310.Image = ImageList9.Images(c(5))
            PictureBox309.Image = ImageList9.Images(c(5))
            PictureBox308.Image = ImageList9.Images(c(5))
            PictureBox307.Image = ImageList9.Images(c(5))
            PictureBox306.Image = ImageList9.Images(c(5))
            PictureBox305.Image = ImageList9.Images(c(5))
            PictureBox304.Image = ImageList9.Images(c(5))
            PictureBox303.Image = ImageList9.Images(c(5))
            PictureBox302.Image = ImageList9.Images(c(5))
            PictureBox301.Image = ImageList9.Images(c(5))
            PictureBox346.Image = ImageList9.Images(c(5))
            PictureBox345.Image = ImageList9.Images(c(5))
            PictureBox344.Image = ImageList9.Images(c(5))
            PictureBox343.Image = ImageList9.Images(c(5))
            PictureBox342.Image = ImageList9.Images(c(5))
            PictureBox341.Image = ImageList9.Images(c(5))
            PictureBox340.Image = ImageList9.Images(c(5))
            PictureBox339.Image = ImageList9.Images(c(5))
            PictureBox338.Image = ImageList9.Images(c(5))
            PictureBox337.Image = ImageList9.Images(c(5))
            PictureBox336.Image = ImageList9.Images(c(5))
            PictureBox335.Image = ImageList9.Images(c(5))
            PictureBox334.Image = ImageList9.Images(c(5))
            PictureBox379.Image = ImageList9.Images(c(5))
            PictureBox378.Image = ImageList9.Images(c(5))
            PictureBox377.Image = ImageList9.Images(c(5))
            PictureBox376.Image = ImageList9.Images(c(5))
            PictureBox375.Image = ImageList9.Images(c(5))
            PictureBox374.Image = ImageList9.Images(c(5))
            PictureBox373.Image = ImageList9.Images(c(5))
            PictureBox372.Image = ImageList9.Images(c(5))
            PictureBox371.Image = ImageList9.Images(c(5))
            PictureBox370.Image = ImageList9.Images(c(5))
            PictureBox369.Image = ImageList9.Images(c(5))
            PictureBox368.Image = ImageList9.Images(c(5))
            PictureBox367.Image = ImageList9.Images(c(5))
        End If
        color1()
        color2()
        Engineering_mode()
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
                    PictureBox130.Image = ImageList2.Images(0)
                    PictureBox130.Left = (x2 - 1) * PictureBox144.Width
                    PictureBox130.Top = (y2 - 1) * PictureBox144.Height
                    tnt2_tick1 = 0
                    exer2 = 1
                    Timer3.Enabled = True
                ElseIf tnt2_tick2 < 1 Or tnt2_tick2 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\fuse.wav", AudioPlayMode.Background)
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
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\fuse.wav", AudioPlayMode.Background)
                    tnt11 = x1 * 100 + y1
                    PictureBox105.Image = ImageList2.Images(0)
                    PictureBox105.Left = (x1 - 1) * PictureBox144.Width
                    PictureBox105.Top = (y1 - 1) * PictureBox144.Height
                    tnt1_tick1 = 0
                    exer1 = 1
                    Timer1.Enabled = True
                ElseIf tnt1_tick2 < 1 Or tnt1_tick2 > 5 Then
                    My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\TNT\fuse.wav", AudioPlayMode.Background)
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
        color1()
        color2()
    End Sub

    '載入遊戲時初始化(onload)
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        PictureBox136.Visible = False
        Button1.Visible = False
        Button2.Visible = False
        Button3.Visible = False
        tt = 20 + dancetime
        ti = tt
        Label9.Text = ti & "秒內"
        colorchange()
        Randomize()
        col = Int((9 - 0 + 1) * Rnd() + 0)
        PictureBox400.Image = ImageList9.Images(col)
        x1 = 12
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

        PictureBox141.BringToFront()
        PictureBox142.BringToFront()
        PictureBox143.BringToFront()
        PictureBox144.BringToFront()
        PictureBox125.BringToFront()
        PictureBox126.BringToFront()
        PictureBox127.BringToFront()
        PictureBox128.BringToFront()

        PictureBox137.BringToFront()
        PictureBox138.BringToFront()
        PictureBox139.BringToFront()
        PictureBox140.BringToFront()
        PictureBox132.BringToFront()
        PictureBox133.BringToFront()
        PictureBox134.BringToFront()
        PictureBox135.BringToFront()
        player.settings.setMode("loop", True)
        If music = 0 Then
            Dim i As Integer
            Randomize()
            i = Int((7 - 0 + 1) * Rnd() + 0)
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
            ElseIf i = 7 Then
                player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgm8.mp3"
            End If
        ElseIf music = 1 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\Dies Irae.mp3"
        ElseIf music = 2 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\+9.mp3"
        ElseIf music = 3 Then
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\special\apple.mp3"
        End If
        Timer7.Enabled = True
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
        Timer7.Enabled = False
    End Sub
    'end
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        start.Close()
    End Sub
    'replay
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        replay1 = 4 '因模式而改變數值
        Me.Close()
    End Sub
    'back
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        Form2.Show()
        Me.Close()

    End Sub
    '時間爆炸
    Private Sub Timer7_Tick(sender As System.Object, e As System.EventArgs) Handles Timer7.Tick
        If ti = 1 Then
            If tt > 4 Then
                tt -= 1
            End If
            ti = tt
            If f12 < 1 Then
                If b1 <> col Then
                    If invincible = 2 Or invincible = 0 Then
                        death1 = 1
                    End If
                End If
                If b2 <> col Then
                    If invincible = 1 Or invincible = 0 Then
                        death2 = 1
                    End If
                End If
                If b1 <> col Or b2 <> col Then
                    ending2()
                End If
            End If
            colorchange()
            Randomize()
            col = Int((9 - 0 + 1) * Rnd() + 0)
            PictureBox400.Image = ImageList9.Images(col)
            If death1 = 0 And death2 = 0 Then
                My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\dancing_mode\remind.wav", AudioPlayMode.Background)
            End If
        End If
        ti -= 1
        Label9.Text = ti & "秒內"

    End Sub
End Class

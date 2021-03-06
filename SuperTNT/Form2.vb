Public Class Form2
    Dim choose As Integer
    Dim shine As Integer
    Dim time As Integer
    Dim i As Integer
    Dim iconn As Integer
    '開啟文字閃爍
    Sub loadin()
        Timer1.Enabled = True
        choose = 0
    End Sub
    '2P選角(function)
    Sub ch()
        ch1 = i
        choose = 1
        Label1.Text = "2P角色選取"
        PictureBox1.Image = ImageList1.Images(1)
        Label1.BackColor = Color.Red
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision4.wav", AudioPlayMode.Background)
    End Sub
    'change gamemode
    Sub gamemode()
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision4.wav", AudioPlayMode.Background)
        If mode = 1 Then
            mode = 6
        ElseIf mode = 7 Then  '()內為模式數
            mode = 2
        End If
        If mode = 4 Then
            PictureBox11.Visible = True
            PictureBox12.Visible = False
            PictureBox14.Visible = False
            PictureBox15.Visible = False
            PictureBox16.Visible = False
            Button1.Text = "普通模式"
            Button7.Visible = True
        ElseIf mode = 5 Then
            PictureBox12.Visible = True
            PictureBox11.Visible = False
            PictureBox14.Visible = False
            PictureBox15.Visible = False
            PictureBox16.Visible = False
            Button1.Text = "TNT雙倍範圍模式"
            Button7.Visible = True
        ElseIf mode = 2 Then
            PictureBox12.Visible = False
            PictureBox11.Visible = False
            PictureBox14.Visible = True
            PictureBox15.Visible = False
            PictureBox16.Visible = False
            Button1.Text = "縮邊模式"
            Button7.Visible = False
        ElseIf mode = 3 Then
            PictureBox12.Visible = False
            PictureBox11.Visible = False
            PictureBox14.Visible = False
            PictureBox15.Visible = True
            PictureBox16.Visible = False
            Button1.Text = "彩色跳舞機模式"
            Button7.Visible = False
        ElseIf mode = 6 Then
            PictureBox12.Visible = False
            PictureBox11.Visible = False
            PictureBox14.Visible = False
            PictureBox15.Visible = False
            PictureBox16.Visible = True
            Button1.Text = "劇情模式"
            Button7.Visible = False
        End If
    End Sub
    '1P選角+呼叫2P選角
    'ch5
    Private Sub PictureBox6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox6.Click
        If choose = 0 Then
            i = 5
            Call ch()
        ElseIf choose = 1 Then
            ch2 = 5
            Timer1.Enabled = False
            began = 1
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
        End If
    End Sub
    'ch3
    Private Sub PictureBox4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox4.Click
        If choose = 0 Then
            i = 3
            Call ch()
        ElseIf choose = 1 Then
            ch2 = 3
            Timer1.Enabled = False
            began = 1
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
        End If
    End Sub
    'ch1
    Private Sub PictureBox2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox2.Click
        If choose = 0 Then
            i = 1
            Call ch()
        ElseIf choose = 1 Then
            ch2 = 1
            Timer1.Enabled = False
            began = 1
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
        End If
    End Sub
    'ch6
    Private Sub PictureBox7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox7.Click
        If choose = 0 Then
            i = 6
            Call ch()
        ElseIf choose = 1 Then
            ch2 = 6
            Timer1.Enabled = False
            began = 1
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
        End If
    End Sub
    'ch4
    Private Sub PictureBox5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox5.Click
        If choose = 0 Then
            i = 4
            Call ch()
        ElseIf choose = 1 Then
            ch2 = 4
            Timer1.Enabled = False
            began = 1
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
        End If
    End Sub
    'ch2
    Private Sub PictureBox3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox3.Click
        If choose = 0 Then
            i = 2
            Call ch()
        ElseIf choose = 1 Then
            ch2 = 2
            Timer1.Enabled = False
            began = 1
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
        End If
    End Sub
    '選角提示文字閃爍
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        shine += 1
        If shine Mod 2 = 0 Then
            Label1.Visible = True
        Else
            Label1.Visible = False
        End If
    End Sub
    '載入初始化
    Private Sub Form2_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Call loadin()
        mode = 4
        PictureBox1.Image = ImageList1.Images(0)
        Label1.BackColor = Color.FromArgb(0, 128, 255)
        If open = True Then
            Label2.Visible = True
            Label3.Visible = True
            Timer3.Enabled = True
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\open\anchor_action.wav", AudioPlayMode.Background)
        Else
            PictureBox13.Visible = False
            player.settings.setMode("loop", True)
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgmlob.mp3"
            Button4.BringToFront()
            Timer3.Enabled = False
            Button6.Visible = True
            Button7.Visible = True
            Timer3.Enabled = False
        End If
        open = False

        If mc = 1 Then
            Button7.Text = "恢復"
        End If
    End Sub
    '選音樂
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        mode += 1
        Call gamemode()
        rule1.Close()
        rule2.Close()
        rule3.Close()
        rule4.Close()
        Form3.Close()
        Form5.Close()
    End Sub
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        mode -= 1
        Call gamemode()
        rule1.Close()
        rule2.Close()
        rule3.Close()
        rule4.Close()
        Form3.Close()
        Form5.Close()
    End Sub
    '開Form3(選音樂界面)
    Private Sub PictureBox10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox10.Click
        Form3.Show()
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
    End Sub
    '開始選角
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision5.wav", AudioPlayMode.Background)
        If mode = 6 Then
            player_name = InputBox("請為主角命名", "玩家匿名", "自己")
            Timer1.Enabled = False
            began = 1
        Else
            PictureBox9.Visible = False
            PictureBox10.Visible = False
            PictureBox11.Visible = False
            PictureBox12.Visible = False
            PictureBox14.Visible = False
            PictureBox15.Visible = False

            Button1.Visible = False
            Button2.Visible = False
            Button3.Visible = False
            Button4.Visible = False
            Button5.Visible = False
            Button6.Visible = False
            Button7.Visible = False

            rule1.Close()
            rule2.Close()
            rule3.Close()
            rule4.Close()
            Form3.Close()
            Form5.Close()
        End If


    End Sub
    '操作說明
    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Form5.Show()
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
    End Sub
    Private Sub PictureBox8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox8.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        start.Close()
    End Sub
    '開始動畫音效
    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
        iconn += 1
        If iconn = 6 Then
            PictureBox13.Visible = False
            Label2.Visible = False
            Label3.Visible = False
            player.settings.setMode("loop", True)
            player.URL = My.Application.Info.DirectoryPath & "\sound\bgm\bgmlob.mp3"
            Button4.BringToFront()
            Button6.BringToFront()
            Button6.Visible = True
            Button7.Visible = True
            Timer3.Enabled = False
        ElseIf iconn = 4 Then
            My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\sound_effect\open\fire.wav", AudioPlayMode.Background)
        End If
    End Sub
    '遊玩說明
    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        If mode = 4 Then
            rule1.Show()
        ElseIf mode = 5 Then
            rule2.Show()
        ElseIf mode = 2 Then
            rule3.Show()
        ElseIf mode = 3 Then
            rule4.Show()
        ElseIf mode = 6 Then
            rule5.Show()
        End If
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
    End Sub
    'mc化
    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        mc += 1
        If mc > 1 Then
            mc = 0
            Button7.Text = "mc化"
        Else
            Button7.Text = "恢復"
        End If
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
    End Sub
End Class
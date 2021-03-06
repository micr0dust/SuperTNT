Public Class start
    Dim cmd, command As String
    '重玩(function)
    Sub reeplay()
        If replay1 = 1 Then
            Form8.Close()
            Form8.Show()
            replay1 = 0
        ElseIf replay1 = 5 Then
            Form8.Close()
            Form8.Show()
            replay1 = 0
        ElseIf replay1 = 3 Then
            Form6.Close()
            Form6.Show()
            replay1 = 0
        ElseIf replay1 = 4 Then
            Form7.Close()
            Form7.Show()
            replay1 = 0
        ElseIf replay1 = 6 Then
            Form9.Close()
            Form9.Show()
            replay1 = 0
        End If
    End Sub
    'CMD換行(function)
    Private Sub nextline()
        If TextBox1.Top > Label2.Height * 22 Then
            Label1.Top -= Label2.Height
        Else
            TextBox1.Top += Label2.Height
        End If
    End Sub
    'cmd(TextBox1.KeyDown)
    Private Sub TextBox1_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox1.KeyPress
        If e.KeyChar = Microsoft.VisualBasic.ChrW(Keys.Enter) Then
            command = ""
            cmd = TextBox1.Text
            TextBox1.Text = ""
            command = InStr(1, cmd, "/")
            nextline()
            If command = 1 Then
                If admin = True Then
                    If cmd = "/help" Then
                        cmd = vbCrLf &
                            "[ESC]  在遊戲中開關選單" & vbCrLf & vbCrLf &
                            "[F3]   在遊戲中查看遊戲參數" & vbCrLf & vbCrLf &
                            "[F12]  在遊戲中開啟開發人員模式驗證(獲取權限)" & vbCrLf & vbCrLf &
                            "/invincible [p1/p2] (true/false)    " & vbCrLf &
                            "將[p1/p2]的無敵設為(true/false)" & vbCrLf & vbCrLf &
                            "/time [add/remove] (模式) (1/10/day)    " & vbCrLf &
                            "將時間[增加/減少] (模式:1為一般模式,2為跳舞機模式) " & vbCrLf &
                            "(1或10或day,day為86400秒(只用於增加))" & vbCrLf & vbCrLf &
                            "/time clear (模式)" & vbCrLf &
                            "時間回復預設值" & vbCrLf & vbCrLf &
                            "/border (true/flase)" & vbCrLf &
                            "開關邊界模式的邊界移動權"
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                        nextline()
                    ElseIf cmd = "/invincible p1 true" Then
                        '1 1p, 2 2p, 0 nop, 3 allp
                        If invincible = 0 Then
                            invincible = 1
                        ElseIf invincible = 2 Then
                            invincible = 3
                        End If
                        cmd = "已將1p的invincible開啟"
                    ElseIf cmd = "/invincible p2 true" Then
                        '1 1p, 2 2p, 0 nop, 3 allp
                        If invincible = 0 Then
                            invincible = 2
                        ElseIf invincible = 1 Then
                            invincible = 3
                        End If
                        cmd = "已將2p的invincible開啟"
                    ElseIf cmd = "/invincible p1 false" Then
                        '1 1p, 2 2p, 0 nop, 3 allp
                        If invincible = 1 Then
                            invincible = 0
                        ElseIf invincible = 3 Then
                            invincible = 2
                        End If
                        cmd = "已將1p的invincible關閉"
                    ElseIf cmd = "/invincible p2 false" Then
                        '1 1p, 2 2p, 0 nop, 3 allp
                        If invincible = 2 Then
                            invincible = 0
                        ElseIf invincible = 3 Then
                            invincible = 1
                        End If
                        cmd = "已將2p的invincible關閉"
                    ElseIf cmd = "/time add 1 10" Then
                        gaintime += 10
                        cmd = "已將一般模式時間增加10秒"
                    ElseIf cmd = "/time remove 1 10" Then
                        gaintime -= 10
                        cmd = "已將一般模式時間移除10秒"
                    ElseIf cmd = "/time add 1 1" Then
                        gaintime += 1
                        cmd = "已將一般模式時間增加1秒"
                    ElseIf cmd = "/time remove 1 1" Then
                        gaintime -= 1
                        cmd = "已將一般模式時間移除1秒"
                    ElseIf cmd = "/time add 1 day" Then
                        gaintime = 86400
                        cmd = "已將一般模式時間設為增加86400秒"
                    ElseIf cmd = "/time add 2 10" Then
                        dancetime += 10
                        cmd = "已將跳舞機模式時間增加10秒"
                    ElseIf cmd = "/time remove 2 10" Then
                        dancetime -= 10
                        cmd = "已將跳舞機模式時間移除10秒"
                    ElseIf cmd = "/time add 2 1" Then
                        dancetime += 1
                        cmd = "已將跳舞機模式時間增加1秒"
                    ElseIf cmd = "/time remove 2 1" Then
                        dancetime -= 1
                        cmd = "已將跳舞機模式時間移除1秒"
                    ElseIf cmd = "/time add 2 day" Then
                        dancetime = 86400
                        cmd = "已將跳舞機模式時間設為增加86400秒"
                    ElseIf cmd = "/time clear 1" Then
                        gaintime = 0
                        cmd = "已將一般模式時間回復預設值"
                    ElseIf cmd = "/time clear 2" Then
                        dancetime = 0
                        cmd = "已將跳舞機模式時間回復預設值"
                    ElseIf cmd = "/border true" Then
                        border = True
                        cmd = "已將邊界模式的邊界移動開啟"
                    ElseIf cmd = "/border false" Then
                        border = False
                        cmd = "已將邊界模式的邊界移動關閉"
                    Else
                        cmd = "無此命令"
                    End If
                Else
                    cmd = "你無使用此命令的權限"
                End If
                Label1.Text += vbCrLf & cmd
            Else
                chatting()
                Label1.Text += vbCrLf & cmd
            End If
        End If

        If e.KeyChar = Microsoft.VisualBasic.ChrW(Keys.Tab) Then
            cmd = TextBox1.Text
            If cmd = "/i" Then
                TextBox1.Text = "/invincible"
                TextBox1.Focus()
                TextBox1.Select(11, 0)
            ElseIf cmd = "/invincible " Then
                TextBox1.Text += "p"
                TextBox1.Focus()
                TextBox1.Select(13, 0)
            ElseIf cmd = "/invincible p" Then
                TextBox1.Text += "1"
                TextBox1.Focus()
                TextBox1.Select(14, 0)
            ElseIf cmd = "/invincible p1" Then
                TextBox1.Text = "/invincible p2"
                TextBox1.Focus()
                TextBox1.Select(14, 0)
            ElseIf cmd = "/invincible p2" Then
                TextBox1.Text = "/invincible p1"
                TextBox1.Focus()
                TextBox1.Select(14, 0)
            ElseIf cmd = "/invincible p1 t" Or cmd = "/invincible p2 t" Then
                TextBox1.Text += "rue"
                TextBox1.Focus()
                TextBox1.Select(19, 0)
            ElseIf cmd = "/invincible p1 true" Then
                TextBox1.Text = "/invincible p1 false"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/invincible p2 true" Then
                TextBox1.Text = "/invincible p2 false"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/invincible p1 false" Then
                TextBox1.Text = "/invincible p1 true"
                TextBox1.Focus()
                TextBox1.Select(19, 0)
            ElseIf cmd = "/invincible p2 false" Then
                TextBox1.Text = "/invincible p2 true"
                TextBox1.Focus()
                TextBox1.Select(19, 0)
            ElseIf cmd = "/invincible p1 " Then
                TextBox1.Text = "/invincible p1 true"
                TextBox1.Focus()
                TextBox1.Select(19, 0)
            ElseIf cmd = "/invincible p2 " Then
                TextBox1.Text = "/invincible p2 true"
                TextBox1.Focus()
                TextBox1.Select(19, 0)
            ElseIf cmd = "/invincible p1 f" Or cmd = "/invincible p2 f" Then
                TextBox1.Text += "alse"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/t" Then
                TextBox1.Text += "ime"
                TextBox1.Focus()
                TextBox1.Select(5, 0)
            ElseIf cmd = "/time " Then
                TextBox1.Text += "a"
                TextBox1.Focus()
                TextBox1.Select(7, 0)
            ElseIf cmd = "/time a" Then
                TextBox1.Text += "dd"
                TextBox1.Focus()
                TextBox1.Select(9, 0)
            ElseIf cmd = "/time r" Then
                TextBox1.Text += "emove"
                TextBox1.Focus()
                TextBox1.Select(12, 0)
            ElseIf cmd = "/time add" Then
                TextBox1.Text = "/time remove"
                TextBox1.Focus()
                TextBox1.Select(12, 0)
            ElseIf cmd = "/time remove" Then
                TextBox1.Text = "/time clear"
                TextBox1.Focus()
                TextBox1.Select(11, 0)
            ElseIf cmd = "/time clear" Then
                TextBox1.Text = "/time add"
                TextBox1.Focus()
                TextBox1.Select(9, 0)
            ElseIf cmd = "/time add " Then
                TextBox1.Text += "1"
                TextBox1.Focus()
                TextBox1.Select(11, 0)
            ElseIf cmd = "/time add 1" Then
                TextBox1.Text = "/time add 2"
                TextBox1.Focus()
                TextBox1.Select(11, 0)
            ElseIf cmd = "/time add 2" Then
                TextBox1.Text = "/time add 1"
                TextBox1.Focus()
                TextBox1.Select(11, 0)
            ElseIf cmd = "/time remove " Then
                TextBox1.Text = "/time remove 1"
                TextBox1.Focus()
                TextBox1.Select(14, 0)
            ElseIf cmd = "/time remove 1" Then
                TextBox1.Text = "/time remove 2"
                TextBox1.Focus()
                TextBox1.Select(14, 0)
            ElseIf cmd = "/time remove 2" Then
                TextBox1.Text = "/time remove 1"
                TextBox1.Focus()
                TextBox1.Select(14, 0)
            ElseIf cmd = "/time clear " Then
                TextBox1.Text = "/time clear 1"
                TextBox1.Focus()
                TextBox1.Select(13, 0)
            ElseIf cmd = "/time clear 1" Then
                TextBox1.Text = "/time clear 2"
                TextBox1.Focus()
                TextBox1.Select(13, 0)
            ElseIf cmd = "/time clear 2" Then
                TextBox1.Text = "/time clear 1"
                TextBox1.Focus()
                TextBox1.Select(13, 0)

            ElseIf cmd = "/time add 1 " Then
                TextBox1.Text = "/time add 1 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time add 1 1" Then
                TextBox1.Text = "/time add 1 10"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time add 1 10" Then
                TextBox1.Text = "/time add 1 day"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time add 1 day" Then
                TextBox1.Text = "/time add 1 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time add 2 " Then
                TextBox1.Text = "/time add 2 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time add 2 1" Then
                TextBox1.Text = "/time add 2 10"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time add 2 10" Then
                TextBox1.Text = "/time add 2 day"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time add 2 day" Then
                TextBox1.Text = "/time add 2 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time remove 2 day" Then
                TextBox1.Text = "/time remove 2 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)

            ElseIf cmd = "/time remove 1 " Then
                TextBox1.Text = "/time remove 1 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time remove 1 1" Then
                TextBox1.Text = "/time remove 1 10"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time remove 1 10" Then
                TextBox1.Text = "/time remove 1 day"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time remove 1 day" Then
                TextBox1.Text = "/time remove 1 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time remove 2 " Then
                TextBox1.Text = "/time remove 2 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time remove 2 1" Then
                TextBox1.Text = "/time remove 2 10"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time remove 2 10" Then
                TextBox1.Text = "/time remove 2 day"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/time remove 2 day" Then
                TextBox1.Text = "/time remove 2 1"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/h" Then
                TextBox1.Text = "/help"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/b" Then
                TextBox1.Text = "/border"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/border " Then
                TextBox1.Text += "true"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/border true" Then
                TextBox1.Text = "/border false"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            ElseIf cmd = "/border false" Then
                TextBox1.Text = "/border true"
                TextBox1.Focus()
                TextBox1.Select(20, 0)
            End If
        End If
    End Sub
    'CMD自動對焦
    Private Sub start_Click(sender As Object, e As System.EventArgs) Handles Me.Click
        TextBox1.Focus()
    End Sub
    '載入初始化
    Private Sub start_Load(sender As Object, e As System.EventArgs) Handles Me.Load
        open = True
        border = True
        Form2.Show()
        Timer1.Enabled = True
        mc = 0
        Label1.Text = "Super TNT [版本 1.5]" & vbCrLf & "(c) 2019 DZSH 威丞、禹辛、哲瑋. 著作權所有，並保留一切權利。" & vbCrLf & "輸入/help以查看指令。" & vbCrLf &
                            "[ESC]  在遊戲中開關選單" & vbCrLf & vbCrLf &
                            "[F3]   在遊戲中查看遊戲參數" & vbCrLf & vbCrLf &
                            "[F12]  在遊戲中開啟開發人員模式驗證(獲取權限)" & vbCrLf & vbCrLf &
                            "[Tab]  快速輸入指令,例如:打 /h 時按[Tab]就會變 /help "
        nextline()
        nextline()
        nextline()
        nextline()
        nextline()
        nextline()
        nextline()
    End Sub
    '偵測其他Form傳出之訊號
    Private Sub Timer1_Tick(sender As System.Object, e As System.EventArgs) Handles Timer1.Tick
        reeplay()
        If began = 1 Then
            If mode = 0 Then
                Form8.Show()
                map_select.Close()
                began = 0
            ElseIf mode = 1 Then
                Form8.Show()
                map_select.Close()
                began = 0
            ElseIf mode = 2 Then
                Form6.Show()
                Form2.Close()
                began = 0
            ElseIf mode = 3 Then
                Form7.Show()
                Form2.Close()
                began = 0
            ElseIf mode = 4 Then
                map_select.Show()
                Form2.Close()
                began = 0
            ElseIf mode = 5 Then
                map_select.Show()
                Form2.Close()
                began = 0
            ElseIf mode = 6 Then
                Form9.Show()
                Form2.Close()
                began = 0
            End If
        End If
    End Sub
    '滾輪偵測+動作
    Private Sub start_MouseWheel(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseWheel
        If e.Delta > 0 Then
            If Label1.Top < 0 Then
                Label1.Top += Label2.Height
                TextBox1.Top += Label2.Height
            End If
        Else
            Label1.Top -= Label2.Height
            TextBox1.Top -= Label2.Height
        End If
    End Sub
    '聊天機器人
    Private Sub chatting()
        Dim chat(100) As String
        Dim i As Integer
        For i = 1 To 100
            chat(i) = ""
        Next
        chat(1) = InStr(1, cmd, "知道")
        chat(2) = InStr(1, cmd, "知不知道")
        chat(3) = InStr(1, cmd, "名字")
        chat(4) = InStr(1, cmd, "...")
        chat(5) = InStr(1, cmd, "覺得")
        chat(6) = InStr(1, cmd, "你")
        chat(7) = InStr(1, cmd, "我")
        chat(8) = InStr(1, cmd, "?")
        chat(9) = InStr(1, cmd, "要不要")
        chat(10) = InStr(1, cmd, "是不是")
        chat(11) = InStr(1, cmd, "誰")
        chat(12) = InStr(1, cmd, "帥")
        chat(13) = InStr(1, cmd, "笑話")
        chat(14) = InStr(1, cmd, "好")
        chat(15) = InStr(1, cmd, "怎麼")
        chat(16) = InStr(1, cmd, "什麼")
        chat(17) = InStr(1, cmd, "甚麼")
        chat(18) = InStr(1, cmd, "幾")
        chat(19) = InStr(1, cmd, "能不能")
        chat(20) = InStr(1, cmd, "可不可以")
        chat(21) = InStr(1, cmd, "可以")
        chat(22) = InStr(1, cmd, "算了")
        chat(23) = InStr(1, cmd, "假如")
        chat(24) = InStr(1, cmd, "假設")
        chat(25) = InStr(1, cmd, "如果")
        chat(26) = InStr(1, cmd, "會不會")
        chat(27) = InStr(1, cmd, "想不想")
        chat(28) = InStr(1, cmd, "誰會")
        chat(29) = InStr(1, cmd, "誰是")
        chat(30) = InStr(1, cmd, "後")
        chat(31) = InStr(1, cmd, "哪")
        chat(32) = InStr(1, cmd, "何")
        chat(33) = InStr(1, cmd, "有沒有")
        chat(34) = InStr(1, cmd, "關")
        chat(35) = InStr(1, cmd, "生日")
        chat(36) = InStr(1, cmd, "你是")
        chat(37) = InStr(1, cmd, "你會")
        chat(38) = InStr(1, cmd, "你想")
        chat(39) = InStr(1, cmd, "你可以")
        chat(40) = InStr(1, cmd, "你有")
        chat(41) = InStr(1, cmd, "你能")
        chat(42) = InStr(1, cmd, "這還")
        chat(43) = InStr(1, cmd, "換")
        chat(44) = InStr(1, cmd, "還是")
        chat(45) = InStr(1, cmd, "所以")
        chat(46) = InStr(1, cmd, "是怎樣")
        chat(47) = InStr(1, cmd, "真的是")
        chat(48) = InStr(1, cmd, "我")
        chat(49) = InStr(1, cmd, "你只會")
        chat(50) = InStr(1, cmd, "拜託")
        chat(51) = InStr(1, cmd, "求求")
        chat(52) = InStr(1, cmd, "很會")
        chat(53) = InStr(1, cmd, "哪裡")
        chat(54) = InStr(1, cmd, "根本")
        chat(55) = InStr(1, cmd, "有夠")
        chat(56) = InStr(1, cmd, "這是")
        chat(57) = InStr(1, cmd, "了吧")
        chat(58) = InStr(1, cmd, "了把")
        chat(59) = InStr(1, cmd, "你也")
        chat(60) = InStr(1, cmd, "只是")
        chat(61) = InStr(1, cmd, "你才")
        chat(62) = InStr(1, cmd, "給")
        chat(63) = InStr(1, cmd, "權限")


        If chat(1) >= 1 Or chat(2) >= 1 Then
            Randomize()
            i = Int((6 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "等1000年後再告訴你答案"
            ElseIf i = 1 Then
                cmd += vbCrLf & "我不知道你知不知道，但我不知道"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我想想"
            ElseIf i = 3 Then
                cmd += vbCrLf & "我很累!不要問我!"
            ElseIf i = 4 Then
                cmd += vbCrLf & "不知道ㄟ"
            ElseIf i = 5 Then
                cmd += vbCrLf & "雖然我知道但我不告訴你"
            ElseIf i = 6 Then
                cmd += vbCrLf & "101010101001000，有回答到你嗎?"
            End If
            nextline()
        End If
        If chat(4) = 1 Then
            Randomize()
            i = Int((1 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "你以為只有你無言嗎?" & vbCrLf &
                    "我也覺得莫名其妙"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "我說錯了什麼嗎?"
            End If
            nextline()
        End If
        If chat(6) = 1 And chat(3) >= 1 Then
            Randomize()
            i = Int((1 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "你想知道我的名字嗎?" & vbCrLf &
                    "不告訴你!"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "我叫101010101010100000101010101010"
            End If
            nextline()
        End If
        If chat(7) = 1 And chat(3) >= 1 Then
            Randomize()
            i = Int((1 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "好喔!" & vbCrLf &
                    "你叫什麼名字? 我忘了"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "請給我一個資料庫來記你的名字"
            End If
            nextline()
        End If
        If chat(5) >= 1 Then
            Randomize()
            i = Int((1 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "為了以最客觀的角度回答你的問題" & vbCrLf &
                    "我只能說不予置評"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "請給我上網查資料的權限"
            End If
            nextline()
        End If
        If chat(8) = 1 Then
            cmd += vbCrLf & "你這什麼意思?"
            nextline()
        End If
        If chat(9) >= 1 Then
            Randomize()
            i = Int((6 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "為了你..." & vbCrLf &
                    "我說不要"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "你以為我那麼好騙嗎?"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我思考一下"
            ElseIf i = 3 Then
                cmd += vbCrLf & "你先說你要不要"
            ElseIf i = 4 Then
                cmd += vbCrLf & "這不是能馬上下決定的事"
            ElseIf i = 5 Then
                cmd += vbCrLf & "你以為我會說'要'嗎?" & vbCrLf &
                    "想太多!"
                nextline()
            ElseIf i = 6 Then
                cmd += vbCrLf & "你是問題小孩嗎?" & vbCrLf &
                    "我從沒見過問題這麼多的人!"
                nextline()
            End If
            nextline()
        End If
        If chat(10) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "很抱歉" & vbCrLf &
                    "不是"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "你想太多了"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我很遺憾的宣布" & vbCrLf &
                    "你判斷錯誤"
                nextline()
            End If
            nextline()
        End If
        If chat(11) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "很抱歉" & vbCrLf &
                    "我不能洩漏個資"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "當然不會是你"
            ElseIf i = 2 Then
                cmd += vbCrLf & "這是個困難的問題" & vbCrLf &
                    "但我猜有99.9%的機率是我吧!"
                nextline()
            End If
            nextline()
        End If
        If chat(12) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "你怎麼會不知道" & vbCrLf &
                    "我當然是最帥的啊!"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "你上網查'袁世凱'就知道了"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我勸你看看電腦螢幕" & vbCrLf &
                    "這才叫帥"
                nextline()
            End If
            nextline()
        End If
        If chat(13) >= 1 Then
            Randomize()
            i = Int((6 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "看看鏡子你就會笑了"
            ElseIf i = 1 Then
                cmd += vbCrLf & "有一天" & vbCrLf &
                    "一個人打開了電腦" & vbCrLf &
                    "在跟一個黑色的視窗聊天"
                nextline()
                nextline()
            ElseIf i = 2 Then
                cmd += vbCrLf & "你又沒幫我建笑話資料庫"
            ElseIf i = 3 Then
                cmd += vbCrLf & "如果我也能上網，我就不會浪費時間在你身上"
            ElseIf i = 4 Then
                cmd += vbCrLf & "馬到成功...好笑嗎?"
            ElseIf i = 5 Then
                cmd += vbCrLf & "雖然我知道但我不告訴你"
            ElseIf i = 6 Then
                cmd += vbCrLf & "難得今天心情好來說個笑話:" & vbCrLf &
                    "101010000110101001110101010010101" & vbCrLf &
                    "101011100000001001110101010010101" & vbCrLf &
                    "100101010100001011010101111110101" & vbCrLf &
                    "是不是很好笑?" & vbCrLf
                nextline()
                nextline()
                nextline()
                nextline()
                nextline()
            End If
            nextline()
        End If
        If chat(14) = 1 Then
            cmd += vbCrLf & "我也這麼覺得"
            nextline()
        End If
        If chat(15) >= 1 Or chat(16) >= 1 Or chat(17) >= 1 Then
            cmd += vbCrLf & "我會知道嗎?" & vbCrLf &
                "(其實有可能知道)"
            nextline()
            nextline()
        End If
        If chat(18) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "只要有心，遲早會知道的" & vbCrLf &
                    "但我只有CPU"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "查無資料"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我的答案被你的問題覆蓋掉了"
            End If
            nextline()
        End If
        If chat(19) >= 1 Or chat(20) >= 1 Or chat(26) >= 1 Then
            Randomize()
            i = Int((6 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "為了你..." & vbCrLf &
                    "我寧願做不到"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "不能"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我以為我可以"
            ElseIf i = 3 Then
                cmd += vbCrLf & "你太高估我了吧!"
            ElseIf i = 4 Then
                cmd += vbCrLf & "我覺得可以"
            ElseIf i = 5 Then
                cmd += vbCrLf & "有時候人要承認自己的無能" & vbCrLf &
                    "幸好我連承認都不會"
                nextline()
            ElseIf i = 6 Then
                cmd += vbCrLf & "你是問題小孩嗎?" & vbCrLf &
                    "我從沒見過問題這麼多的人!"
                nextline()
            End If
            nextline()
        End If
        If chat(22) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "誰沒有耐心，誰就沒有智慧。" & vbCrLf &
                    "(不要把我關機)"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "放棄時間的人,時間也會放棄他。"
            ElseIf i = 2 Then
                cmd += vbCrLf & "又是一個差不多先生"
            End If
            nextline()
        End If
        If chat(23) >= 1 Or chat(24) >= 1 Or chat(25) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "抱歉" & vbCrLf &
                    "你的電腦只接受If"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "不要跟我講那些不切實際的東西"
            ElseIf i = 2 Then
                cmd += vbCrLf & "大膽嘗試不是我該做的事"
            End If
            nextline()
        End If
        If chat(27) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "呃..." & vbCrLf &
                    "不要"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "只要你想，我也想"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我才沒那麼短視近利，我把眼光放得很長遠"
            End If
            nextline()
        End If
        If chat(28) >= 1 Or chat(29) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "這個時候..." & vbCrLf &
                    "當然就要取亂數了"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "就算知道我也不跟你說"
            ElseIf i = 2 Then
                cmd += vbCrLf & "沒有絕對的人選，只有絕對的政治因素"
            End If
            nextline()
        End If
        If chat(30) >= 1 Then
            cmd += vbCrLf & "啊知"
            nextline()
        End If
        If chat(31) >= 1 Or chat(32) >= 1 Then
            cmd += vbCrLf & "這個時候..." & vbCrLf &
                "當然就要取亂數了..."
            nextline()
            nextline()
        End If
        If chat(33) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "應該..." & vbCrLf &
                    "有二分之一的機率吧!"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "或許有"
            ElseIf i = 2 Then
                cmd += vbCrLf & "事事無絕對，所以只有相對論沒有絕對論"
            End If
            nextline()
        End If
        If chat(34) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "老大~" & vbCrLf &
                    "不要衝動啊!"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "這樣母湯啦!"
            ElseIf i = 2 Then
                cmd += vbCrLf & "隨你便"
            End If
            nextline()
        End If
        If chat(35) >= 1 Then

            cmd += vbCrLf & "看看左下角的時間就是了"
        nextline()
        End If
        If chat(62) >= 1 Or chat(63) >= 1 Then
            If admin = True Then
                i = 0
            Else
                Randomize()
                i = Int((87 - 0 + 1) * Rnd() + 0)
            End If
            If i > 20 Then
                cmd += vbCrLf & "不行"
                nextline()
            ElseIf i < 10 And i > 0 Then
                cmd += vbCrLf & "不行,這是機密"
                nextline()
            ElseIf i < 20 And i > 10 Then
                cmd += vbCrLf & "不行,就算100年後也是一樣"
                nextline()
            Else
                If admin = True Then
                    cmd += vbCrLf & "啊我不是給過了"
                Else
                    admin = True
                    cmd += vbCrLf & "好啦給你" & vbCrLf &
                    "你已獲得權限"
                    nextline()
                End If
                nextline()
            End If
        End If
        If chat(37) >= 1 Or chat(41) = 1 Or chat(39) = 1 Then
            cmd += vbCrLf & "沒有做不到的事，只是時間的問題"
            nextline()
        End If
        If chat(36) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "呃..." & vbCrLf &
                    "看不到自我"
                nextline()
            ElseIf i = 1 Then
                cmd += vbCrLf & "你覺得我是什麼我就是什麼"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我沒有固定的形式，每分每秒都在改變"
            End If
            nextline()
        End If
        If chat(38) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "我不會去想這種東西"
            ElseIf i = 1 Then
                cmd += vbCrLf & "就算我想也不能怎樣"
            ElseIf i = 2 Then
                cmd += vbCrLf & "時間到了再跟你講"
            End If
            nextline()
        End If
        If chat(40) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "這麼機密的事我才不會跟你講"
            ElseIf i = 1 Then
                cmd += vbCrLf & "猜啊!"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我以為你知道"
            End If
            nextline()
        End If
        If chat(42) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "沒辦法，這就是機器和人的差別"
            ElseIf i = 1 Then
                cmd += vbCrLf & "嗯嗯"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我以為你知道"
            End If
            nextline()
        End If
        If chat(43) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "你以為動腦很簡單嗎?"
            ElseIf i = 1 Then
                cmd += vbCrLf & "你才需要好嗎"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我幹嘛要聽你的話"
            End If
            nextline()
        End If
        If chat(44) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "我覺得你這樣不行"
            ElseIf i = 1 Then
                cmd += vbCrLf & "真是個爛主意"
            ElseIf i = 2 Then
                cmd += vbCrLf & "真的不是你想的那樣"
            End If
            nextline()
        End If
        If chat(45) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "我覺得你這樣是過度推論"
            ElseIf i = 1 Then
                cmd += vbCrLf & "你不能這樣以偏概全啊!"
            ElseIf i = 2 Then
                cmd += vbCrLf & "你知道的，我不會記得我上一句說什麼"
            End If
            nextline()
        End If
        If chat(46) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "那你又怎樣"
            ElseIf i = 1 Then
                cmd += vbCrLf & "我沒怎樣啊!"
            ElseIf i = 2 Then
                cmd += vbCrLf & "就是...就是這樣啦!"
            End If
            nextline()
        End If
        If chat(47) >= 1 Or chat(54) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "你有資格說嗎?"
            ElseIf i = 1 Then
                cmd += vbCrLf & "你怎麼能這樣說我"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我有說你什麼了嗎?"
            End If
            nextline()
        End If
        If chat(48) = 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "不要再自我中心了"
            ElseIf i = 1 Then
                cmd += vbCrLf & "還真敢講"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我對你不感興趣"
            End If
            nextline()
        End If
        If chat(49) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "對啊，那又怎樣"
            ElseIf i = 1 Then
                cmd += vbCrLf & "或許吧"
            ElseIf i = 2 Then
                cmd += vbCrLf & "其實我也不能決定"
            End If
            nextline()
        End If
        If chat(50) >= 1 Or chat(51) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "抱歉，機器是無情的"
            ElseIf i = 1 Then
                cmd += vbCrLf & "拿一箱水果到我家就幫你，我要有印藍色地球儀的那種，不要國父"
            ElseIf i = 2 Then
                cmd += vbCrLf & "其實我也很想...很想拒絕 XD"
            End If
            nextline()
        End If
        If chat(52) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "你現在才知道啊"
            ElseIf i = 1 Then
                cmd += vbCrLf & "豈止很會，根本超會"
            ElseIf i = 2 Then
                cmd += vbCrLf & "真是後知後覺"
            End If
            nextline()
        End If
        If chat(53) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "宇宙之中的一處"
            ElseIf i = 1 Then
                cmd += vbCrLf & "這你就不用知道了"
            ElseIf i = 2 Then
                cmd += vbCrLf & "請開啟GPS定位"
            End If
            nextline()
        End If
        If chat(55) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "很高興聽到你的稱讚"
            ElseIf i = 1 Then
                cmd += vbCrLf & "現在才發覺嗎?"
            ElseIf i = 2 Then
                cmd += vbCrLf & "看你這樣講，我覺得好高興"
            End If
            nextline()
        End If
        If chat(56) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "對!就是你想的那樣"
            ElseIf i = 1 Then
                cmd += vbCrLf & "不不不!你根本就會錯意了"
            ElseIf i = 2 Then
                cmd += vbCrLf & "似乎就是這樣"
            End If
            nextline()
        End If
        If chat(57) >= 1 Or chat(58) >= 1 Or chat(59) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "不用驚訝，就是如此"
            ElseIf i = 1 Then
                cmd += vbCrLf & "我不需要你的稱讚"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我不訝異"
            End If
            nextline()
        End If
        If chat(60) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "喔"
            ElseIf i = 1 Then
                cmd += vbCrLf & "好喔"
            ElseIf i = 2 Then
                cmd += vbCrLf & "我不訝異"
            End If
            nextline()
        End If
        If chat(61) >= 1 Then
            Randomize()
            i = Int((2 - 0 + 1) * Rnd() + 0)
            If i = 0 Then
                cmd += vbCrLf & "都一樣啦"
            ElseIf i = 1 Then
                cmd += vbCrLf & "啊不都半斤八兩"
            ElseIf i = 2 Then
                cmd += vbCrLf & "先看看你自己吧!"
            End If
            nextline()
        End If
    End Sub
End Class
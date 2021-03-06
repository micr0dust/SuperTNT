Public Class Form3
    Dim choose As Integer

    Sub chose()
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision3.wav", AudioPlayMode.Background)
        If music = 4 Then
            music = 0
        ElseIf music = -1 Then
            music = 3
        End If
        If music = 0 Then
            PictureBox2.Image = ImageList1.Images(0)
            Button1.Text = "預設音樂"
        ElseIf music = 1 Then
            PictureBox2.Image = ImageList1.Images(1)
            Button1.Text = "末日經"
        ElseIf music = 2 Then
            PictureBox2.Image = ImageList1.Images(2)
            Button1.Text = "我以為"
        ElseIf music = 3 Then
            PictureBox2.Image = ImageList1.Images(3)
            Button1.Text = "青蘋果樂園"
        End If
    End Sub

    Private Sub Form3_Load(sender As Object, e As System.EventArgs) Handles Me.Load
        If music = 0 Then
            PictureBox2.Image = ImageList1.Images(0)
            Button1.Text = "預設音樂"
        ElseIf music = 1 Then
            PictureBox2.Image = ImageList1.Images(1)
            Button1.Text = "末日經"
        ElseIf music = 2 Then
            PictureBox2.Image = ImageList1.Images(2)
            Button1.Text = "我以為"
        ElseIf music = 3 Then
            PictureBox2.Image = ImageList1.Images(3)
            Button1.Text = "青蘋果樂園"
        End If
    End Sub

    Private Sub Button2_Click(sender As System.Object, e As System.EventArgs) Handles Button2.Click
        music += 1
        Call chose()
    End Sub

    Private Sub Button3_Click(sender As System.Object, e As System.EventArgs) Handles Button3.Click
        music -= 1
        Call chose()
    End Sub

    Private Sub Button4_Click(sender As System.Object, e As System.EventArgs) Handles Button4.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision1.wav", AudioPlayMode.Background)
        Me.Close()
    End Sub
End Class
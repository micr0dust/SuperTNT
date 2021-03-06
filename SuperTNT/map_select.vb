Public Class map_select

    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click
        map = Int((4 - 0 + 1) * Rnd() + 0)
        began = 1
        random = True
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub

    Private Sub map_select_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        If mode = 4 Then
            mode = 0
        ElseIf mode = 5 Then
            mode = 1
        End If
        random = False
    End Sub

    Private Sub PictureBox7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox7.Click
        map = 0
        began = 1
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub

    Private Sub PictureBox4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox4.Click
        map = 1
        began = 1
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub

    Private Sub PictureBox8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox8.Click
        map = 2
        began = 1
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub

    Private Sub PictureBox3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox3.Click
        map = 4
        began = 1
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub

    Private Sub PictureBox2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox2.Click
        map = 3
        began = 1
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub

    Private Sub PictureBox6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox6.Click
        map = 5
        began = 1
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub

    Private Sub PictureBox5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox5.Click
        map = 6
        began = 1
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub

    Private Sub PictureBox12_Click(sender As System.Object, e As System.EventArgs) Handles PictureBox12.Click
        map = 7
        began = 1
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision8.wav", AudioPlayMode.Background)
    End Sub
End Class
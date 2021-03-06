Public Class rule5

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        My.Computer.Audio.Play(My.Application.Info.DirectoryPath & "\sound\decision\decision7.wav", AudioPlayMode.Background)
        Me.Close()
    End Sub
End Class
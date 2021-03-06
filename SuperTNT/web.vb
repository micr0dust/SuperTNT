Public Class web

    Private Sub web_Load(sender As Object, e As System.EventArgs) Handles Me.Load
        WebBrowser1.Navigate("http://wuilliam.pythonanywhere.com/message/")
    End Sub
End Class
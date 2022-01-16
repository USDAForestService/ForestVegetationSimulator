<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Example
   Inherits System.Windows.Forms.Form

   'Form overrides dispose to clean up the component list.
   <System.Diagnostics.DebuggerNonUserCode()> _
   Protected Overrides Sub Dispose(ByVal disposing As Boolean)
      Try
         If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
         End If
      Finally
         MyBase.Dispose(disposing)
      End Try
   End Sub

   'Required by the Windows Form Designer
   Private components As System.ComponentModel.IContainer

   'NOTE: The following procedure is required by the Windows Form Designer
   'It can be modified using the Windows Form Designer.  
   'Do not modify it using the code editor.
   <System.Diagnostics.DebuggerStepThrough()> _
   Private Sub InitializeComponent()
      Me.ButtonCallFORTRAN = New System.Windows.Forms.Button()
      Me.SuspendLayout()
      '
      'ButtonCallFORTRAN
      '
      Me.ButtonCallFORTRAN.Location = New System.Drawing.Point(37, 32)
      Me.ButtonCallFORTRAN.Name = "ButtonCallFORTRAN"
      Me.ButtonCallFORTRAN.Size = New System.Drawing.Size(214, 23)
      Me.ButtonCallFORTRAN.TabIndex = 3
      Me.ButtonCallFORTRAN.Text = "Call FVS from VB"
      Me.ButtonCallFORTRAN.UseVisualStyleBackColor = True
      '
      'Testing
      '
      Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
      Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
      Me.ClientSize = New System.Drawing.Size(315, 94)
      Me.Controls.Add(Me.ButtonCallFORTRAN)
      Me.Name = "Testing"
      Me.Text = "Testing"
      Me.ResumeLayout(False)

   End Sub
   Friend WithEvents ButtonCallFORTRAN As System.Windows.Forms.Button
End Class

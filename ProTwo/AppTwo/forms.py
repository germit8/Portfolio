from django import forms
from Apptwo.models import User

class LogIn(forms.Form):
    name = forms.CharField()
    email = forms.EmailField()
    text = forms.CharField(widget=forms.Textarea)

class SignUp(forms.ModelForm):
    
    class Meta:
        model = User
        fields = "__all__"
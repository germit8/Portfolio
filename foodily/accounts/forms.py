from django import forms
from accounts.models import CustomUser
from django.contrib.auth.forms import UserCreationForm, UserChangeForm

# forma pro Signup
class CreateCustomUserForm(UserCreationForm):

    class Meta:
        # čerpá z modelu CustomUser
        model = CustomUser
        # tyto políčka + password (automaticky) jsou využita při registraci
        fields = ['first_name', 'last_name', 'username', 'email']
        labels = {
            'first_name':"Your first name",
            'last_name':"Your surname",
            'username':"The name under which users will see you",
            'email':"Your email address"
        }
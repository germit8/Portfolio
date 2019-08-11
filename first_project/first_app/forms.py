from django import forms
from django.core import validators
from django.contrib.auth.models import User
from first_app.models import UserProfileInfo

# tvorba vlsatního validátoru
def check_for_k(value):
    if value[0].upper() != "K":
        raise forms.ValidationError("Needs to start with K")

class FormName(forms.Form):
    name = forms.CharField(validators=[check_for_k])
    email = forms.EmailField()
    verify_email = forms.EmailField(label="Verify your email ")
    text = forms.CharField(widget=forms.Textarea)
    botcatcher = forms.CharField(required=False,
                                widget=forms.HiddenInput,
                                validators=[validators.MaxLengthValidator(0)]) #využití built in validátoru

    # custom made validace, moc často se používat nebude
    # def clean_botcatcher(self):
    #     botcatcher = self.cleaned_data["botcatcher"]
    #     if len(botcatcher) > 0:
    #         raise forms.ValidationError("GOTCHA!")
    #     return botcatcher

    # využití super() k zisku všech dat a následné validaci
    def clean(self):
        all_clean_data = super().clean()
        email = all_clean_data["email"]
        vemail = all_clean_data["verify_email"]

        if vemail != email:
            raise forms.ValidationError("Emails don't match!")

# forma na zisk klasických dat uživatele viz field
class UserForm(forms.ModelForm):
    # tímto uděláme aby password input byl cenzurovaný
    password = forms.CharField(widget=forms.PasswordInput())

    class Meta():
        model = User
        fields = ("username", "email", "password")

# forma na zisk extra informací o profilu, které jsme přidali v našem modelu
class UserProfileInfoForm(forms.ModelForm):
    class Meta():
        model = UserProfileInfo
        fields = ("portfolio", "profile_pic")

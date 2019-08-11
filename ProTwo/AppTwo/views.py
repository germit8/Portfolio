from django.shortcuts import render
from django.http import HttpResponse
from Apptwo.models import User
from . import forms

# Create your views here.
def index(request):
    form = forms.SignUp()

    if request.method == "POST":
        form = forms.SignUp(request.POST)

        if form.is_valid():
        # do stuff
            User.objects.get_or_create(first_name=form.cleaned_data["first_name"], 
                                        second_name=form.cleaned_data["second_name"], 
                                        email=form.cleaned_data["email"])

    my_dict = {"insert_me" : "Do you need help?", "form" : form}
    return render(request, "Apptwo/index.html", context = my_dict)

def users(request):
    user_data = User.objects.order_by("first_name")
    user_dict = {"all_users" : user_data}
    return render(request, "Apptwo/users.html", context = user_dict)

def form_view(request):
    form = forms.LogIn()

    if request.method == "POST":
        form = forms.LogIn(request.POST)

        if form.is_valid():
        # do stuff
            print("Validation complete")
            print("NAME: " + form.cleaned_data["name"])
            print("EMAIL: " + form.cleaned_data["email"])
            print("TEXT: " + form.cleaned_data["text"])

    return render(request, "Apptwo/form.html", context = {"form" : form})
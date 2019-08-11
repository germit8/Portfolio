from django.shortcuts import render
from django.http import HttpResponse, HttpResponseRedirect
from first_app.models import Topic, Webpage, AccessRecord
from . import forms

from django.urls import reverse
from django.contrib.auth.decorators import login_required
from django.contrib.auth import authenticate, login, logout


# Create your views here.
# def index(request):
#     webpages = AccessRecord.objects.order_by('name')
#     date_dict = {'access_records' : webpages}

#     return render(request, "first_app/index.html", context = date_dict)

def form_name_view(request):
    form = forms.FormName()

    if request.method == "POST":
        form = forms.FormName(request.POST)

        if form.is_valid():
            # do something code
            print("Validation success!")
            print("NAME: " + form.cleaned_data["name"])
            print("EMAIL: " + form.cleaned_data["email"])
            print("TEXT: " + form.cleaned_data["text"])

    return render(request, "first_app/form_page.html", {"form" : form})

def index_2(request):
    context_dict = {"text":"Hello", "number":100}
    return render(request, "first_app/index.html", context_dict)

def other(request):
	return render(request, "first_app/other.html")

def relative(request):
	return render(request, "first_app/relative.html")

def registration(request):
    # proměnná využitá pro přeměnu html stránky podle toho, jestli jsem/nejsem zaregistrovaný
    registered = False

    if request.method == "POST":
        # klasické vytvoření nových proměnných pro obě formy, stejné jako vždycky
        user_form = forms.UserForm(request.POST)
        profile_form = forms.UserProfileInfoForm(request.POST)

        # kontrola jestli jsou obě formy platné
        if user_form.is_valid() and profile_form.is_valid():

            user = user_form.save() # pod proměnnou user se uloží data vložená do formy
            user.set_password(user.password) # nastaví se heslo uživatele inputem password
            user.save() # celé se to uloží

            """Zde se uloží informace formy profilu, ale necommitují se, protože obě formy čerpají ze stejné předlohy:
            inbuilt modelu User. Kdyby se to commitnulo, kolidovali by se informace"""
            profile = profile_form.save(commit=False)
            profile.user = user # tento řádek obstará to, aby se informace nekolidovaly

            if 'profile_pic' in request.FILES: # Tohle říká: "Pokud je ve FILES key:value pair profile_pic (pokud se nerovná null)..."
                profile.profile_pic = request.FILES['profile_pic'] # kolonka profilového obrázku se vyplní právě tou value, která se schovává pod key profile_pic

            profile.save() # celé se to uloží

            registered = True # jsme zaregistrovaní

        else:
            print(user_form.errors, profile_form.errors) # pokud nejsou formy platné, vyskočí eror
    
    else:
        user_form = forms.UserForm()
        profile_form = forms.UserProfileInfoForm() # pokud metoda není POST, prostě se to setne do normálu

    return render(request, "first_app/registration.html", {"registered": registered, 
                                                            "user_form": user_form, 
                                                            "profile_form": profile_form}) # klasický context dict render


def user_login(request):

    if request.method == "POST":
        username = request.POST.get('username')
        password = request.POST.get('password') # při loginu se "getuje" hodnota z html z formy-inputu pod name="password/username"

        user = authenticate(username=username, password=password) # authenticate je in built funkce djanga na autentikaci uživatele

        if user: # pokud authenticate() == True...
            if user.is_active: # metoda na ověření aktivity uživatele
                login(request, user) # opět funkce na login djanga
                return HttpResponseRedirect(reverse('index')) # jak HttpResponse, až na to že tě to navrátí tam kam chceš
            else:
                return HttpResponse('Account not active')
        else:
            print("Someone tried to login and failed")
            print("Username: {} and Password: {}".format(username, password))
            return HttpResponse('Invalid login details supplied') # error přihlášení
    else:
        return render(request, 'first_app/login.html', {}) # pokud metoda není post, opět jde všechno do normálu

@login_required # decorator, díky kterému je možné funkci volat pouze je uživatel přihlášený
def user_logout(request):
    logout(request)
    return HttpResponseRedirect(reverse('index')) # jednoduchá funkce na odhlášení

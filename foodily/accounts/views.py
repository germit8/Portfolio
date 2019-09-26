from django.shortcuts import render
from django.views.generic import CreateView, DetailView, UpdateView, DeleteView
from accounts.forms import CreateCustomUserForm
from django.urls import reverse_lazy
from accounts.models import CustomUser
from django.contrib.auth.mixins import LoginRequiredMixin
# Create your views here.

# view pro Signup stránku
class Signup(CreateView):
    # je nutné definovat formu kterou bude využívat
    form_class = CreateCustomUserForm

    """ Je nutné definovat url na kterou uživatele přesměruje po dokončení operace
    reverse_lazy se využívá místo reverse, protože reverse by uživatele přehodilo na stránku
    předtím, než se odešlou data do databáze. """

    success_url = reverse_lazy('login')
    # je nutné definovat path k html souboru, kam se tento view bude promítat
    template_name = "accounts/signup.html"

# view pro detailní informace uživatele
class UserView(DetailView):
    # je nutné definovat model ze kterého view čerpá
    model = CustomUser
    template_name = "accounts/accounts_detail.html"
    # context_data_name = "user_object"

# view pro aktualizaci dat objektu uživatele
# využívá mixin, který kontroluje aby byla operace možná pouze po přihlášení
class UserUpdateInfoView(LoginRequiredMixin, UpdateView):
    model = CustomUser
    # je nutné definovat pole, které je možné upravit
    fields = ['username', 'email']
    template_name = "accounts/accounts_update.html"

from django.shortcuts import render, get_object_or_404, redirect
from django.views import generic
from django.contrib.auth.mixins import LoginRequiredMixin
from django.contrib.auth.decorators import login_required
from foodily import settings
from fridge import forms
from fridge import models
from django.urls import reverse_lazy
# Create your views here.

# FRDIGE VIEWS
class FridgeCreate(LoginRequiredMixin, generic.CreateView):
    model = models.Fridge
    form_class = forms.FridgeCreateForm
    redirect_field_name = "fridge/fridge_detail.html"
    template_name = "fridge/fridge_create.html"

    def form_valid(self, form):
        form.instance.belongs_to = self.request.user
        return super(FridgeCreate, self).form_valid(form)

class FridgeUpdate(LoginRequiredMixin, generic.UpdateView):
    form_class = forms.FridgeCreateForm
    model = models.Fridge
    template_name = "fridge/fridge_update.html"

class FridgeDetail(LoginRequiredMixin, generic.DetailView):
    model = models.Fridge
    template_name = "fridge/fridge_detail.html"

# class FridgeList(LoginRequiredMixin, generic.ListView):
#     model = models.Fridge
#     template_name = "fridge/fridge_list.html"

class FridgeDelete(LoginRequiredMixin, generic.DeleteView):
    model = models.Fridge
    template_name = "fridge/fridge_confirmation_delete.html"
    success_url = reverse_lazy('home')

# FOOD VIEWS
class FoodDetail(LoginRequiredMixin, generic.DetailView):
    model = models.Food
    template_name = "fridge/food_detail.html"

# class FoodDelete(LoginRequiredMixin, generic.DeleteView):
#     model = models.Food
#     template_name = "fridge/food_confirmation_delete.html"
#     success_url = reverse_lazy('fridge:fridge_list')

@login_required
def food_remove_object(request, slug):
    food = get_object_or_404(models.Food, slug=slug)
    food.delete()
    return redirect('fridge:fridge_list')

@login_required
def food_add_quantity(request, slug):
    food = get_object_or_404(models.Food, slug=slug)
    amount_of_food = request.POST["amount"]
    food.add_quantity(int(amount_of_food))
    return redirect('fridge:food_detail', slug=slug)

@login_required
def food_subtract_quantity(request, slug):
    food = get_object_or_404(models.Food, slug=slug)
    amount_of_food = request.POST["amount"]
    food.subtract_quantity(int(amount_of_food))
    return redirect('fridge:food_detail', slug=slug)

# @login_required
# def food_expire(request, slug):
#     food_to_expire = get_object_or_404(models.Food, slug=slug)
#     food_to_expire.expire()
#     return redirect('fridge:fridge_list')


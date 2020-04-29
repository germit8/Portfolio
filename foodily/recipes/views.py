from django.shortcuts import render, get_object_or_404
from django.views import generic
from django.contrib.auth.mixins import LoginRequiredMixin
from recipes import models
from fridge.models import Fridge

# Create your views here.
class RecipesList(LoginRequiredMixin, generic.ListView):
    model = models.Recipe
    template_name = "recipes/recipes_list.html"

class RecipesFilteredList(LoginRequiredMixin, generic.ListView):
    model = models.Recipe
    template_name = "recipes/recipes_list.html"

    def get_queryset(self):
        queryset = super().get_queryset()
        filter_query = self.request.GET["recipe_query"]
        return queryset.filter(name__contains=filter_query)

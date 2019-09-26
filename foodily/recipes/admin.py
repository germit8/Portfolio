from django.contrib import admin
from recipes.models import Ingredient, Recipe
# Register your models here.
admin.site.register(Recipe)
admin.site.register(Ingredient)
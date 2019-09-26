from django.urls import path
from recipes import views

app_name = 'recipes'

urlpatterns = [
    path("recipes_list/", views.RecipesList.as_view(), name="recipes_list"),
    path("recipes_filtered_list/", views.RecipesFilteredList.as_view(), name="recipes_filtered_list"),
]
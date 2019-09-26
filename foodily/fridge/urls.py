from django.urls import path
from fridge import views

app_name = 'fridge'

urlpatterns = [
    # FRIDGE URLS
    path('fridge_create/', views.FridgeCreate.as_view(), name="fridge_create"),
    path('fridge_update/<slug>/', views.FridgeUpdate.as_view(), name="fridge_update"),
    path('fridge_detail/<slug>/', views.FridgeDetail.as_view(), name="fridge_detail"),
    # path('fridge_list/', views.FridgeList.as_view(), name="fridge_list"),
    path('fridge_confirmation_delete/<slug>/', views.FridgeDelete.as_view(), name="fridge_confirmation_delete"),
    # FOOD URLS
    path('food_detail/<slug>/', views.FoodDetail.as_view(), name="food_detail"),
    path('food_confirmation_delete/<slug>/', views.food_remove_object, name="food_confirmation_delete"),
    path('food_detail/<slug>/add_quantity/', views.food_add_quantity, name="food_add_quantity"),
    path('food_detail/<slug>/subtract_quantity/', views.food_subtract_quantity, name="food_subtract_quantity"),
]
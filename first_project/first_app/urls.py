from django.urls import path
from first_app import views

# tvorba url složky v aplikaci a cesta k funkci
urlpatterns = [
    path('', views.index, name="index"),
]
from django.urls import path
from Apptwo import views

urlpatterns = [
    path("", views.index, name="index"),
    path("users", views.users, name="users"),
    path("form", views.form_view, name="form_view"),
]
from django.urls import path
from first_app import views

# template tagging
app_name = "first_app"

urlpatterns = [
    path('formpage/', views.form_name_view, name = "formpage"),
    path('other/', views.other, name = "other"),
    path('relative/', views.relative, name = "relative"),
    path('registration/', views.registration, name = "registration"),
    path('login/', views.user_login, name="user_login")
]

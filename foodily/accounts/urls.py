from django.urls import path
from accounts import views
from django.contrib.auth import views as auth_views

app_name = 'accounts'

urlpatterns = [
    # u LoginView je nutné definovat template_name zde, protože nemá svůj vlastní view
    path('login/', auth_views.LoginView.as_view(template_name="accounts/login.html"), name="login"),
    # u LogoutView to není třeba, protože je to pouze tlačítko a nemá svůj vlastní html
    path('logout/', auth_views.LogoutView.as_view(), name="logout"),
    path('signup/', views.Signup.as_view(), name="signup"),
    path('accounts_detail/<slug>/', views.UserView.as_view(), name="accounts_detail"),
    path('accounts_update/<slug>/', views.UserUpdateInfoView.as_view(), name="accounts_update"),
]
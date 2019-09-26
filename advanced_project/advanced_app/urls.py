from django.urls import path
from advanced_app import views

app_name = "advanced_app"

urlpatterns = [
    path('', views.SchoolListView.as_view(), name="school_list"),
    path('<int:pk>/', views.SchoolDetailView.as_view(), name="school_detail"),
    path('create/', views.SchoolCreateView.as_view(), name="create"),
    path('update/<int:pk>/', views.SchoolUpdateView.as_view(), name="school_update"),
    path('delete/<int:pk>/', views.SchoolDeleteView.as_view(), name="school_delete"),
]
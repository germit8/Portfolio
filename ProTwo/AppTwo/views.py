from django.shortcuts import render
from django.http import HttpResponse
# Create your views here.

def my_app(request):
    return HttpResponse("<em>My Second App</em>")

def help(request):
    variable_dict = {
        "heading": "Help Page",
    }
    return render(request, 'AppTwo/help.html', context=variable_dict)
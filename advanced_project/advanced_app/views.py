from django.shortcuts import render
from django.views.generic import TemplateView, View, ListView, DetailView, CreateView, UpdateView, DeleteView
from . import models
from django.urls import reverse_lazy
# Create your views here.
class IndexView(TemplateView):
    template_name = 'advanced_app/index.html'

    # funkce na vložení context dictionary do templatu
    def get_context_data(self, **kwargs):
        context = super().get_context_data(**kwargs)
        context['inject'] = "Waspoppin"
        return context

class SchoolListView(ListView):
    model = models.School
    # ListView automaaticky vytvoří context dictionary z názvu modelu: dá ho lowercase
    # a k tomu dodá _list --> school_list
    context_object_name = 'schools'
    # pokud to nechci mít automaticky, definuju si to sám viz řádek výš

class SchoolDetailView(DetailView):
    model = models.School
    template_name = 'advanced_app/school_detail.html'
    context_object_name = 'school_detail'
    # DetailView automaticky vrací pouze jméno modelu lowercase bez _list --> school

class SchoolCreateView(CreateView):
    fields = ["name", "principal", "location"]
    model = models.School

class SchoolUpdateView(UpdateView):
    fields = ["name", "principal"]
    model = models.School

class SchoolDeleteView(DeleteView):
    model = models.School
    success_url = reverse_lazy("advanced_app:school_list")
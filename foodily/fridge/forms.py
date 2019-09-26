from django import forms
from fridge.models import Fridge, Food
from foodily import settings

User = settings.AUTH_USER_MODEL

class FridgeCreateForm(forms.ModelForm):

    class Meta:
        model = Fridge
        fields = ['name', 'food_contents']
        labels = {
            'name':"Name for your fridge",
            'food_contents':"Which food does it contain"
        }
        widgets = {
            'food_contents': forms.CheckboxSelectMultiple(),
        }

class FoodCreateForm(forms.ModelForm):

    class Meta:
        model = Food
        fields = ['name', 'quantity', 'description', 'expiration_date']
        labels = {
            'name':"Name of food you are adding",
            'quantity':"Amount of your food",
            'description':"Description of your food (not needed)",
            'expiration_date':"Choose expiration date"
        }
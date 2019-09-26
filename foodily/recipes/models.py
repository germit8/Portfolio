from django.db import models
from django.utils.text import slugify
from django.urls import reverse
from foodily import settings

User = settings.AUTH_USER_MODEL
# Create your models here.
class Recipe(models.Model):
    name = models.CharField(max_length=255, unique=True)
    rating = models.CharField(max_length=255)
    preparation_time = models.CharField(max_length=255)
    servings = models.CharField(max_length=255)
    instructions = models.CharField(max_length=1000)
    slug = models.SlugField()

    def __str__(self):
        return self.name

    def save(self, *args, **kwargs):
        self.slug = slugify(self.name)
        super().save(*args, **kwargs)

    # def get_absolute_url(self):
    #     return reverse('recipes:recipe_detail', kwargs={'slug':self.slug})

class Ingredient(models.Model):
    name = models.CharField(max_length=255)
    recipe = models.ForeignKey(Recipe, related_name="ingredients", on_delete=models.CASCADE)

    def __str__(self):
        return self.name
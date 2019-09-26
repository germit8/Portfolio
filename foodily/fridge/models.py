from django.db import models
from django.utils.text import slugify
from django.utils import timezone
from django.urls import reverse
from foodily import settings

User = settings.AUTH_USER_MODEL
# Create your models here.
class Food(models.Model):
    name = models.CharField(max_length=20, unique=True)
    quantity = models.PositiveIntegerField()
    description = models.TextField(max_length=255, blank=True)
    date_added = models.DateTimeField(default=timezone.now)
    expiration_date = models.DateTimeField()
    is_expired = models.BooleanField(default=False)
    slug = models.SlugField(allow_unicode=True, unique=True)

    def __str__(self):
        return self.name

    def save(self, *args, **kwargs):
        self.slug = slugify(self.name)
        super().save(*args, **kwargs)

    def add_quantity(self, number):
        self.quantity += number
        self.save()

    def subtract_quantity(self, number):
        self.quantity -= number
        self.save()

    # def expire(self):
    #     if self.expiration_date == timezone.now():
    #         self.is_expired = True
    #     if self.is_expired == True:
    #         self.delete()

    def get_absolute_url(self):
        return reverse('fridge:food_detail', kwargs={'slug': self.slug})

class Fridge(models.Model):
    name = models.CharField(max_length=20)
    belongs_to = models.ForeignKey(User, related_name="my_fridge", on_delete=models.CASCADE)
    food_contents = models.ManyToManyField(Food, related_name="in_fridges")
    slug = models.SlugField(allow_unicode=True, unique=True)
    
    def __str__(self):
        return self.name

    def save(self, *args, **kwargs):
        self.slug = slugify(self.name)
        super().save(*args, **kwargs)
  
    def get_absolute_url(self):
        return reverse('fridge:fridge_detail', kwargs={'slug': self.slug})



    



    
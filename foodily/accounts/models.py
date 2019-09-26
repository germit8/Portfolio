from django.db import models
from django.utils.text import slugify
from django.urls import reverse
from django.contrib.auth.models import AbstractUser
# Create your models here.

# Vytvoření custom modelu pro uživatele
class CustomUser(AbstractUser):
    # slug pro pozdější odkazování
    slug = models.SlugField(allow_unicode=True, unique=True)
    # first_name
    # second_name
    # username
    # email
    # password
    # + další, které moc nevyužívám

    def __str__(self):
        return self.username

    # metoda na automatické uložení slugu podle usernamu
    def save(self, *args, **kwargs):
        self.slug = slugify(self.username)
        super().save(*args, **kwargs)
    
    # metoda kterou využívá DetailView a UpdateView --> navrací na accounts_detail pomocí slugu
    def get_absolute_url(self, **kwargs):
        return reverse('accounts:accounts_detail', kwargs={'slug':self.slug})
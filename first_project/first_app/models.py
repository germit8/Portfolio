from django.db import models
from django.contrib.auth.models import User

# Create your models here.
class Topic(models.Model):
    top_name = models.CharField(max_length = 264, unique = True)

    def __str__(self):
        return self.top_name

class Webpage(models.Model):
    topic = models.ForeignKey(Topic, on_delete = models.CASCADE)
    name = models.CharField(max_length = 264, unique = True)
    url = models.URLField(unique = True)

    def __str__(self):
        return self.name

class AccessRecord(models.Model):
    name = models.CharField(max_length = 264, unique = True)
    date = models.DateField()

    def __str__(self):
        return str(self.date)

class UserProfileInfo(models.Model):
    """Model, který čerpá z in-built modelu djanga User. Vytváří se pouze pokud do registrace/přihlášení toho
    chceme víc, než jsou základní kolonky"""
    user = models.OneToOneField(User, on_delete = models.CASCADE)
    # username, first_name, surname, email, password

    portfolio = models.URLField(blank=True) # blank=True --> není nutné vyplnit
    profile_pic = models.ImageField(upload_to="profile_pics", blank=True)

    def __str__(self):
        return self.user.username
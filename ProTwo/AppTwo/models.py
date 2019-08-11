from django.db import models

# Create your models here.
class User(models.Model):
    first_name = models.CharField(max_length = 10, unique = True)
    second_name = models.CharField(max_length = 10, unique = True)
    email = models.CharField(max_length = 30, unique = True)

    def __str__(self):
        return str(self.first_name + " " + self.second_name)
from django.db import models
from django.urls import reverse

# Create your models here.
class School(models.Model):
    name = models.CharField(max_length=20)
    location = models.CharField(max_length=20)
    principal = models.CharField(max_length=20)

    def __str__(self):
        return self.name
    # 2. Tudíž to, co ForeignKey dá za možnosti ve třídě student, je to co returnuje tato funkce

    # funkce na využití PrimaryKey - podle identifikačního čísla objektů z class je pak možné je upravovat/mazat
    def get_absolute_url(self):
        return reverse("advanced_app:school_detail", kwargs={"pk": self.pk})

class Student(models.Model):
    name = models.CharField(max_length=20)
    age = models.PositiveIntegerField()
    school = models.ForeignKey(School, related_name='students', on_delete=models.CASCADE)
    # 1. ForeignKey vezme jako value to, co je ve funkci __str__(self) u toho modelu, ze kterého čerpá

    def __str__(self):
        return self.name
# Generated by Django 2.2.4 on 2019-08-26 11:34

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('fridge', '0001_initial'),
    ]

    operations = [
        migrations.AlterField(
            model_name='fridge',
            name='food_contents',
            field=models.ManyToManyField(related_name='in_fridges', to='fridge.Food'),
        ),
    ]
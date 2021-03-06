# Generated by Django 2.2.4 on 2019-09-24 16:44

from django.db import migrations, models


class Migration(migrations.Migration):

    initial = True

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='Recipe',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=255, unique=True)),
                ('rating', models.CharField(max_length=255)),
                ('preparation_time', models.CharField(max_length=255)),
                ('servings', models.CharField(max_length=255)),
                ('instructions', models.CharField(max_length=1000)),
                ('slug', models.SlugField()),
            ],
        ),
    ]

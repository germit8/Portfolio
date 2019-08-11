# Generated by Django 2.1.7 on 2019-07-30 13:55

from django.db import migrations, models


class Migration(migrations.Migration):

    initial = True

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='User',
            fields=[
                ('id', models.AutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('first_name', models.CharField(max_length=10, unique=True)),
                ('second_name', models.CharField(max_length=10, unique=True)),
                ('email', models.CharField(max_length=30, unique=True)),
            ],
        ),
    ]

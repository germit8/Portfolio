import os
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'ProTwo.settings')

import django
django.setup()

# fake script
import random
from Apptwo.models import User
from faker import Faker

fake = Faker()

def populate(number):
    for entry in range(number):
        names = fake.name().split()
        fake_name = names[0]
        fake_second_name = names[1]
        fake_email = fake.email()

        User.objects.get_or_create(first_name=fake_name, second_name=fake_second_name, email=fake_email)

if __name__ == '__main__':
    print("Populating Users!")
    populate(10)
    print("Populating complete!")
import random

subjects = ["My ", "Your ", "His ", "Hers "]
objects = ["vagina ", "dog ", "apple ", "lasagna "]
verbs = ["stinks ", "is singing ", "tastes ", "is crying "]
adverbs = ["heavily.", "beautifuly.", "noisily.", "smartly."]


def randomText(amount):
    for i in range(amount):
        print(random.choice(subjects) + random.choice(objects) + random.choice(verbs) + random.choice(adverbs))


randomText(5)

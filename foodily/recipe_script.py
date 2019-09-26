import os
import django
os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'foodily.settings')
django.setup()
from recipes.models import Ingredient, Recipe

import bs4
from urllib.request import urlopen
from bs4 import BeautifulSoup as soup

for page_number in range(1, 6):
    # url na scrapování
    my_url = "https://www.allrecipes.com/?page=" + str(page_number)
    print("Going to page number: " + str(page_number))

    # otevření klienta, přečtení a uložení html souboru, uzavření klienta
    client = urlopen(my_url)
    page_html = client.read()
    client.close()

    # zpracování html textu pro oči člověka
    souped_page = soup(page_html, "html.parser")

    # vezme to kontejnery všech produktů
    containers = souped_page.findAll("article", {"class":"fixed-recipe-card"})

    for container in containers:
        recipe_url = container.find("div", {"class":"fixed-recipe-card__info"}).a["href"]

        recipe_client = urlopen(recipe_url)
        recipe_page_html = recipe_client.read()
        recipe_client.close()

        souped_recipe_page = soup(recipe_page_html, "html.parser")
        try:
            recipe_name = souped_recipe_page.find("h1", {"id":"recipe-main-content"}).text
            recipe_rating = souped_recipe_page.find("div", {"class":"rating-stars"}).img["alt"]
            recipe_time = souped_recipe_page.find("span", {"class":"ready-in-time"}).text
            recipe_servings = souped_recipe_page.find("meta", {"id":"metaRecipeServings"})["content"]
            
            recipe_instructions = ""
            recipe_instructions_list = souped_recipe_page.findAll("li", {"class":"step"})
            for instruction in recipe_instructions_list:
                recipe_instructions += instruction.span.text.strip()

            new_recipe = Recipe(name=recipe_name, rating=recipe_rating, preparation_time=recipe_time, servings=recipe_servings, instructions=recipe_instructions)
            new_recipe.save()
       
            recipe_ingredients = []
            recipe_ingredients_list = souped_recipe_page.findAll("li", {"class":"checkList__line"})
            for i in range(len(recipe_ingredients_list) - 3):
                recipe_ingredients.append(recipe_ingredients_list[i].span.text.strip())
            for ingred in recipe_ingredients:
                new_ingredient = Ingredient(name=ingred, recipe=new_recipe)
                new_ingredient.save()
        except:
            print("Didn't find the correct element")



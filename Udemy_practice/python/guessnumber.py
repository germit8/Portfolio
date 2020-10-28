import time
import random
print("Welcome to guess a number. Let's see if you can guess the 3 digit number")

number_list = [random.choice(range(10)) for i in range(3)]
transition = map(str, number_list)
number_string = "".join(transition)
game_on = True

while game_on:
    guess = input("Guess a 3 digit number: ")
    if guess == number_string:
        print("Good job, you have guessed the number")
        game_on = False
    elif guess[0] == number_string[0] or guess[1] == number_string[1] or guess[2] == number_string[2]:
        print("Match")
    elif guess[0] in number_string or guess[1] in number_string or guess[2] in number_string:
        print("Close")
    else:
        print("Nope")

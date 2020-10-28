import time
import random

print("Rock Paper Scissors")


def rps():
    choices = ["rock", "paper", "scissors"]

    time.sleep(1)
    print("Let's start")
    time.sleep(1)
    print("Computer is choosing...")
    ai_choice = random.choice(choices)
    time.sleep(1)
    print("Now you can choose")
    pl_choice = input()

    if pl_choice not in choices:
        time.sleep(1)
        print("This is not an option")
        rps()

    while True:
        if ai_choice == pl_choice:
            time.sleep(1)
            print("Computer chose: " + ai_choice)
            time.sleep(1)
            print("You chose: " + pl_choice)
            time.sleep(1)
            print("You both picked the same")
            rps()
        elif ai_choice == "paper" and pl_choice == "scissors" or ai_choice == "scissors" and pl_choice == "rock" or ai_choice == "rock" and pl_choice == "paper":
            time.sleep(1)
            print("Computer chose: " + ai_choice)
            time.sleep(1)
            print("You chose: " + pl_choice)
            time.sleep(1)
            print("You have won, do you want to play again? (Y/y)")
            answer = input()
            if answer == "y" or answer == "Y":
                rps()
            else:
                quit()
        else:
            time.sleep(1)
            print("Computer chose: " + ai_choice)
            time.sleep(1)
            print("You chose: " + pl_choice)
            time.sleep(1)
            print("You have lost, do you want to play again? (Y/y)")
            answer = input()
            if answer == "y" or answer == "Y":
                rps()
            else:
                quit()


rps()

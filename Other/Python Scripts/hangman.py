import time


def hangman():

    # message variables
    already_guessed_message = "You already guessed this letter."
    guessed_letters_message = "Guessed letters:"
    progress_message = "Type anything to continue or type 'end' to quit"
    play_again_message = "To start a new game, press Y/y"
    win_message = "Congratulations! You have guessed the word."

    # starting and getting a word to be guessed
    print("The game is starting...")
    time.sleep(1)
    print("Type a word to start the game: ")
    guess_word = input()
    guess_word = guess_word.lower()

    # checking if word doesn't have blank lines or is longer than 2 characters
    if " " in guess_word:
        print("It has to be just one word!")
        time.sleep(1)
        hangman()
    elif len(guess_word) < 3:
        print("The word is too short!")
        time.sleep(1)
        hangman()
    else:
        time.sleep(1)
        print("You can start guessing now.")

    # creating identical lists, one will be shown to the player and the second one not
    # appending letters in normal list and stars to the fake one
    # creating variables for lives and list for guessed letters
    word_list = []
    fake_word_list = []
    for i in guess_word:
        word_list.append(i)
        fake_word_list.append("*")
    time.sleep(1)
    fake_word_string = "".join(fake_word_list)
    print(fake_word_string)

    lives = 5
    guessed_letters = []

    # while loop to run the game
    while True:
        # guessing letter
        print("Take a guess: ")
        guess = input()
        if len(guess) == 2:
            print("Guess a single letter or a word with 3 or more characters!")
            time.sleep(1)
            print("Take a guess: ")
            guess = input()
        guess = guess.lower()

        # checking if letter is the word
        if guess in word_list:
            time.sleep(1)
            guessed_letters.append(guess) # appends letters which have been guessed
            print("This letter is in the word.")
            for i in range(int(len(guess_word))):
                if guess == fake_word_list[i]:  # if the letter is already in the fake list, it has been guessed
                    print(already_guessed_message)
                    guessed_letters.pop()  # popping the letter, because i don't want it multiple times
                    print(guessed_letters_message, guessed_letters)
                elif guess == word_list[i]:
                    fake_word_list[i] = guess # letter is correct and not guessed yet
                    fake_word_string = "".join(fake_word_list)
                    print(fake_word_string)
            print(progress_message)

        # you can input a whole word, checking if it is correct
        elif guess == guess_word:
            time.sleep(1)
            print(win_message)
            time.sleep(1)
            print(play_again_message)
            answer = input()
            if answer == "Y" or answer == "y":
                hangman()
            else:
                quit()

        # checking if guess is incorrect and not guessed yet
        elif guess not in word_list and guess not in guessed_letters:
            guessed_letters.append(guess)
            lives -= 1
            if lives == 1:
                time.sleep(1)
                print("You lost a life! You have %d life left" % lives)
            elif lives == 0:
                time.sleep(1)
                print("You are dead, to start a new game pres Y/y")
                answer = input()
                if answer == "Y" or answer == "y":
                    hangman()
                else:
                    quit()
            else:
                time.sleep(1)
                print("You lost a life! You have %d lives left" % lives)
            print(progress_message)

        # checking if guess is incorrect but already guessed
        elif guess not in word_list and guess in guessed_letters:
            time.sleep(1)
            print(already_guessed_message)
            print(guessed_letters_message, guessed_letters)
            print(progress_message)

        # checking if both initial lists are the same, it means victory
        if word_list == fake_word_list:
            time.sleep(1)
            print(win_message)
            time.sleep(1)
            print(play_again_message)
            answer = input()
            if answer == "Y" or answer == "y":
                hangman()
            else:
                quit()
        if input() == "end":
                quit()


hangman()

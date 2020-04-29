import random
random_actions = ["Šel jsem na procházku se psem.",
                    "Posiloval jsem vrchní část těla.",
                    "Posiloval jsem spodní část těla.",
                    "Šel jsem se proběhnout do parku.",
                    "Protahoval jsem se a dělal jógu."]
def main():
    f = open("denik.txt", "w+")

    for i in range(1, 36):
        random_number = random.randint(0, 4)
        random_number_2 = random.randint(0, 4)
        modulo_number = random.randint(1, 2)
        while random_number_2 == random_number:
            random_number_2 = random.randint(0, 4)
        f.write("Deník karantény: den " + str(i) + "\n")
        if modulo_number == 1:
            f.write(random_actions[random_number] + "\n\n")
        else:
            f.write(random_actions[random_number] + "\n")
        if modulo_number % 2 == 0:
            f.write(random_actions[random_number_2] + "\n\n")

    f.close()

if __name__ == "__main__":
    main()
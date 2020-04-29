import pygame
import time
import random

# nastartuje pygame
pygame.init()

# global variables
width = 1705
height = 960

black = (0, 0, 0)
white = (255, 255, 255)
red = (255, 0, 0)
green = (0, 255, 0)
blue = (0, 0, 255)
tyrkys = (0, 200, 200)
purple = (170, 0, 255)

colors = [purple, tyrkys, blue]
# rozměry, název okna a fps
window = pygame.display.set_mode((width, height))
pygame.display.set_caption("Mike In Space")
clock = pygame.time.Clock()

# backgrounds = ["universe.png", "MilkyWay.png", "space.png"]
# nahrání obrázku
charImg = pygame.image.load("MikeWazowski.png")
obstImg = pygame.image.load("meteorit.png")
background = pygame.image.load("MilkyWay.png")


def play_music(name):
    # nesmí chybět kvalitní hudbička
    pygame.mixer.music.load(name)
    pygame.mixer.music.play(-1)


# funkce na vytvoření překážek na uhýbání
def obstacle(obstx, obsty):
    window.blit(obstImg, (obstx, obsty))


# funkce, která dokola "přeplácává" auto na vytvořenou plochu funkcí blit
def char(x, y):
    window.blit(charImg, (x, y))


def display_message(text, points):
    # vytvořím font
    font = pygame.font.Font("begok.ttf", 50)
    # "přetřu" okýnko blitem, vyrenderuju font, kterej bere jako parametry:
    #  text, antialising a barvu, render jako druhý parametr bere lokaci
    window.blit(font.render(text, True, purple), (width / 2 - 250, height / 2))
    if points == 1:
        window.blit(font.render("you have gained 1 point", True, white), (width / 2 - 150, height / 1.5))
    else:
        window.blit(font.render("you have gained %d points" % points, True, white), (width / 2 - 350, height / 1.5))
    # poté samozřejmě musím updatovat displej
    pygame.display.update()
    # dám chvíli čas hráči na oddych
    time.sleep(1.5)
    # hra začíná odznova
    game_loop()


def display_points(points):
    font = pygame.font.Font("begok.ttf", 20)
    window.blit(font.render("points: %d" % points, True, green), (width * 0.01, height * 0.95))


def display_lives(lives):
    font = pygame.font.Font("begok.ttf", 20)
    window.blit(font.render("lives: %d" % lives, True, red), (width * 0.92, height * 0.95))


def crash(points):
    # crash využívá funkci na zprávu
    # tyhle 3 funkce vytvářejí loop, takže se to všechno opakuje stále dokola,
    # pokud nezmáčknu tlačítko quit
    endMess = ["you crashed boi", "get rekt scrub", "you suck dude"]
    display_message(random.choice(endMess), points)


# funkce na loop a běh hry
def game_loop():
    # parametry a pozice auta
    char_w = 150
    char_h = 150
    x = ((width - char_w) / 2)
    y = (height * 0.8)
    char_speed = 175
    # parametry a pozice objektu
    obst_x = random.randrange(0, width)
    obst_y = random.randrange(-600, -150)
    obst_speed = 20
    obst_w = 176
    obst_h = 144
    # body a životy
    points = 0
    lives = 3

    gameExit = False

    while not gameExit:

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                quit()
            # logika pohybu objektu
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    x += -char_speed
                elif event.key == pygame.K_RIGHT:
                    x += char_speed
                elif event.key == pygame.K_UP:
                    y += -char_speed
                elif event.key == pygame.K_DOWN:
                    y += char_speed
        # příprava plochy
        window.blit(background, (0, 0))
        # překážka a její logika
        obstacle(obst_x, obst_y)
        obst_y += obst_speed
        if obst_y > height:
            obst_y = 0
            obst_x = random.randrange(0, (width - obst_w))
            lives -= 1
            if lives < 0:
                crash(points)

        if points >= 15:
            obst_speed = 25
            if points >= 30:
                obst_speed = 30
                if points >= 45:
                    obst_speed = 35
                    # char_speed = 145
                    if points >= 60:
                        obst_speed = 40
                        # char_speed = 165
                        if points >= 75:
                            obst_speed = 55
        # postava a její logika
        char(x, y)
        if x < 0:
            x = 0
        elif x > width - char_w:
            x = width - char_w
        elif y < 0:
            crash(points)
        elif y > height - char_h:
            y = height - char_h

        # kontrola srážky a životů
        if y < obst_y + obst_h:
            if x < obst_x + obst_w and x > obst_x or x + char_w > obst_x and x + char_w < obst_x + obst_w:
                points += 1
                obst_y = 0
                obst_x = random.randrange((width - obst_w))

        display_lives(lives)
        display_points(points)
        # neustálá aktualizace displeje
        pygame.display.update()
        # clock tick využívá global variable, udává FPS
        clock.tick(120)


play_music("EPIC SAX GUY 10 MINS.mp3")
game_loop()

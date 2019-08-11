import math


class Circle():
    def __init__(self, radius):
        self.radius = radius

    def calculate_circumference(self):
        return 2 * math.pi * self.radius

    def calculate_area(self):
        return math.pi * (self.radius ** 2)


class Xsider():
    def __init__(self, side, sides):
        self.side = side
        self.sides = sides

    def calculate_circumference(self):
        return self.side * self.sides

    def calculate_diagonal(self):
        return math.sqrt((self.side ** 2) + (self.side ** 2))


class Square(Xsider):
    def __init__(self, side, sides = 4):
        self.side = side
        self.sides = sides

    def calculate_area(self):
        return self.side ** 2


square = Square(5)
print(square.calculate_circumference())
print(square.calculate_area())
print(square.calculate_diagonal())

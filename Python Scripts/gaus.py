import random

num_1 = 0
num_2 = 0
num_3 = 0
num_4 = 0
num_5 = 0
num_6 = 0
num_times = [x * 1 for x in range(1000)]
for i in num_times:
    result = random.choice(range(1, 7))
    if result == 1:
        num_1 += 1
    elif result == 2:
        num_2 += 1
    elif result == 3:
        num_3 += 1
    elif result == 4:
        num_4 += 1
    elif result == 5:
        num_5 += 1
    else:
        num_6 += 1
print(num_1)
print(num_2)
print(num_3)
print(num_4)
print(num_5)
print(num_6)

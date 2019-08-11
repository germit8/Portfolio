grades = [10, 7, 10, 6, 4, 3, 7, 2, 10, 8, 10]
grades_table = []


class Grades():
    def __init__(self, grades):
        self.grades = grades

    def calculate_average(self):
        numgrades = 0
        for i in grades:
            numgrades += i
        return (numgrades / (len(grades)))

    def grade_median(self):
        sorted_list = sorted(self.grades)
        if len(sorted_list) % 2 != 0:
            indx = len(sorted_list) // 2
            return sorted_list[indx]
        elif len(sorted_list) % 2 == 0:
            indx_1 = int(len(sorted_list) / 2 - 1)
            indx_2 = int(len(sorted_list) / 2)
            med = (sorted_list[indx_1] + sorted_list[indx_2]) / 2
            return med

    def grades_converter(self):
        for i in grades:
            if i == 10 or i == 9:
                grades_table.append("A")
            elif i == 8 or i == 7:
                grades_table.append("B")
            elif i == 6 or i == 5:
                grades_table.append("C")
            elif i == 4 or i == 3:
                grades_table.append("D")
            else:
                grades_table.append("F")
        return grades_table


grade = Grades(grades)
print(grade.calculate_average())
print(grade.grade_median())
print(grade.grades_converter())

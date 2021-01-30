import string
from itertools import permutations

lower_case_letters = string.ascii_lowercase
upper_case_letters = string.ascii_uppercase
digits = string.digits


def permutations_of_lower():
    perm_list = list(permutations(lower_case_letters, 4))
    word_list = []
    for tuple in perm_list:
        word_list.append("".join(tuple))
    return word_list

def create_word(list_of_words):
    new_list = []
    for lettr in upper_case_letters:
        for wrd in list_of_words:
            concat_tuple = "".join(wrd)
            complete_word = lettr + concat_tuple
            new_list.append(complete_word)
    for wordd in new_list:
        if wordd == "Zuzka":
            print("There is a Zuzka!")

create_word(permutations_of_lower())

import pandas as pd
import itertools
FastFood=pd.read_csv("../data/fastfood.csv")
FastFood=FastFood[FastFood['words'].notna()]

final_words=[a.split(";") for a in list(FastFood.words)]
word_list = list(itertools.chain.from_iterable(final_words))

from collections import Counter
freq = pd.DataFrame.from_dict(Counter(word_list), orient='index')
freq.sort_values(by=0, ascending=False)

import re
dictionary = list(freq.index)
dictionary_pos = [x for x in dictionary if re.search("^[^N]", x) ]

import numpy as np
word_embedding = np.zeros([len(FastFood.words), len(dictionary_pos)])

for i,x in enumerate(final_words):
    for j,y in enumerate(dictionary_pos):
        word_embedding[i,j] = sum([z == y for z in x]) - sum([z == "N"+ y for z in x])
    if i % 1000 == 0:
        print(i)

FastFood = pd.concat([FastFood, pd.DataFrame(word_embedding, columns=dictionary_pos)], axis=1)

FastFood.to_csv("../data/fastfood_embedded.csv", index=False)
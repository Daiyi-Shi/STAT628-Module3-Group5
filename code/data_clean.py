import pandas as pd

review = pd.read_json('../data_origin/review_city.json', lines=True)
business = pd.read_json('../data_origin/business_city.json', lines=True)
user = pd.read_json('../data_origin/user_city.json', lines=True)
user = user[["user_id", "average_stars"]]
df_raw = pd.merge(review, business, how='left', on='business_id')
df_raw = pd.merge(df_raw, user, how='left', on='user_id')
df = df_raw.drop(columns=['review_id', 'funny', 'cool', 'date', 'latitude', 'longitude', 'is_open', 'review_count', 'hours'])
df = df.dropna(subset=['categories'])
FastFood = df[df['categories'].str.contains('Fast Food')]
FastFood.reset_index(drop=True, inplace=True)

import nltk
nltk.download('punkt')
nltk.download('stopwords')
nltk.download('wordnet')
nltk.download('averaged_perceptron_tagger')

text = list(FastFood.text)

import string
text = [a.lower().replace('/', ' ') for a in text]

from nltk.corpus import wordnet
tag_dict = {"J": wordnet.ADJ,
            "N": wordnet.NOUN,
            "V": wordnet.VERB,
            "R": wordnet.ADV}

from nltk.stem.wordnet import WordNetLemmatizer
lmtzr = WordNetLemmatizer()

from multiprocessing.dummy import Pool as ThreadPool
pool = ThreadPool()
length = FastFood.shape[0]
npar = 40
step = int(length / npar) + 1

from nltk.tag.stanford import StanfordPOSTagger
_path_to_model = '../stanford-postagger/models/english-left3words-distsim.tagger'
_path_to_jar = '../stanford-postagger/stanford-postagger.jar'
st_list=[StanfordPOSTagger(_path_to_model, _path_to_jar) for num in range(npar)]

def lemmatize_process(n):
    st=st_list[int(n/step)]
    l_words=[]
    cur_row=0
    for a in text[n:(n+step)]:
        token_list=[]
        for x in st.tag(nltk.word_tokenize(a)):
            if x[1][0] in list(tag_dict.keys()):
                token_list.append(lmtzr.lemmatize(x[0], tag_dict.get(x[1][0])))
            else:
                token_list.append(x[0])
        l_words.append(token_list)
        cur_row+=1
        if (cur_row % 20) == 0:
            print(cur_row, flush=True)
    return l_words

results = pool.map(lemmatize_process, list(range(0, length, step)))
pool.close()
pool.join()

import itertools
lemmatized_words = list(itertools.chain.from_iterable(results))

import pickle
f=open('../data/lemmatized_words.dat','wb')
pickle.dump(lemmatized_words,f)
f.close()

from nltk.corpus import stopwords
stop_words = stopwords.words('english')
neg_stop_words = stop_words[131:133] + stop_words[144:160] + stop_words[161:]
neg_words = ['no', 'not', "n't", 'never', 'neither', 'nor', \
             'nothing', 'nobody', 'nowhere', 'none'] + \
             neg_stop_words + \
             [x.replace("'", "") for x in neg_stop_words if "'" in x]

for c1 in range(len(lemmatized_words)):
    neg_flag=False
    for c2 in range(len(lemmatized_words[c1])):
        if lemmatized_words[c1][c2] in [',', '.', ';', '?', '!']:
            neg_flag=False
        if neg_flag & (lemmatized_words[c1][c2] not in stop_words):
            lemmatized_words[c1][c2]="".join(["N", lemmatized_words[c1][c2]])
        if lemmatized_words[c1][c2] in neg_words:
            neg_flag=True

import re
standard_words=[[x for x in a if re.search("^[a-zN]+$", x)] for a in lemmatized_words]

filtered_words = [[word for word in a if word not in stop_words] for a in standard_words]

word_list = list(itertools.chain.from_iterable(filtered_words))
word_list_pos = [x[1:] if re.search("^N", x) else x for x in word_list]
from collections import Counter
freq = pd.DataFrame.from_dict(Counter(word_list_pos), orient='index')

dictionary_pos = list(freq[freq[0] > 30].index)
dictionary = dictionary_pos.copy()
for x in dictionary_pos:
    neg_x="N"+x
    if neg_x not in dictionary_pos:
        dictionary.append(neg_x)

final_words = [[word for word in a if word in dictionary] for a in filtered_words]
newtext = [";".join([word for word in a]) for a in final_words]
FastFood.loc[:,"words"]=newtext

FastFood.to_csv("../data/fastfood_info4.csv", index=False)
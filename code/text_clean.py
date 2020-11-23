"""
We mainly used nltk package to do our text cleaning
"""

import pandas as pd
import json
import nltk
# When first use nltk, run 'nltk.download()'.
from nltk.corpus import stopwords
stop_words_set = set(stopwords.words('english'))
from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet


def transform_tag(nltk_tag):
    if nltk_tag.startswith('J'):
        return wordnet.ADJ
    elif nltk_tag.startswith('V'):
        return wordnet.VERB
    elif nltk_tag.startswith('N'):
        return wordnet.NOUN
    elif nltk_tag.startswith('R'):
        return wordnet.ADV
    else:
        return None


def text_to_words(text):
    text = text.lower()
    # words = [nltk.tokenize.word_tokenize(sentence) for sentence in nltk.tokenize.sent_tokenize(text)]
    words = nltk.tokenize.word_tokenize(text)
    filtered_words = [w for w in words if w not in stop_words_set]
    filtered_words_with_tag = nltk.pos_tag(filtered_words)

    # This part refered to this website
    # https://simonhessner.de/lemmatize-whole-sentences-with-python-and-nltks-wordnetlemmatizer/
    lmtzr = WordNetLemmatizer()
    lemmatized_words = []
    for word, tag in filtered_words_with_tag:
        t_tag = transform_tag(tag)
        if t_tag is None:
            lemmatized_words.append((word, tag))
        else:
            lemmatized_words.append((lmtzr.lemmatize(word, t_tag),tag))

    return lemmatized_words


# example = "This place is absolutely wonderful. The service is efficient, the employees are just pleasant and delightful. I enjoy coming regularly, to the point they have memorized my usual order. Definitely would recommend stopping by for a meal."
# text_to_words(example)


data = pd.read_csv('../data/fastfood_review.csv')
data.review = data.review.apply(lambda text: json.dumps(text_to_words(text)))
data.to_csv('../data/fastfood_review_splited.csv')
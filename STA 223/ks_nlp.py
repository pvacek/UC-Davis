# -*- coding: utf-8 -*-
"""
Created on Tue Feb 27 13:29:03 2018

@author: prvacek
"""

import os
import numpy as np
import pandas as pd
import re
import nltk
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import PCA

#Kickstarter NLP analysis

os.chdir('./2017-18/WQ18/223/Project')

ks714C=pd.read_csv('ks714C.csv')
ksaux=pd.read_csv('ks_auxil714.csv').iloc[:,1:]

ksaux["loghours"]=np.log((ksaux.iloc[:,2]-ksaux.iloc[:,3])/(60*60))
ksaux.blurb=ksaux.blurb.str.replace('\\\\n*',' ')
ksaux=ksaux.rename(columns={'id':'ID'})

ks714C=ks714C.merge(ksaux,on="ID",how='left')
ks714C.blurb=ks714C.blurb.str.replace('"|^[ ]*|[ ]*$','')
ks714C.name=ks714C.name.str.replace(' \(Canceled\)','')
ks714C["text"]=ks714C.name+" "+ks714C.blurb
ks714C.text=ks714C.text.str.replace('"','')

##Some REAL NLP

stopwords = nltk.corpus.stopwords.words('english')

from nltk.stem.snowball import SnowballStemmer
stemmer=SnowballStemmer('english')

def tokenize_and_stem(text):
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [word for sent in nltk.sent_tokenize(text) for word in nltk.wordpunct_tokenize(sent)]
    filtered_tokens = []
    # filter out any tokens not containing letters (e.g., numeric tokens, raw punctuation)
    for token in tokens:
        if re.search('[a-zA-Z]', token):
            filtered_tokens.append(token)
    stems = [stemmer.stem(t) for t in filtered_tokens]
    return stems


def tokenize_only(text):
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [word.lower() for sent in nltk.sent_tokenize(text) for word in nltk.wordpunct_tokenize(sent)]
    filtered_tokens = []
    # filter out any tokens not containing letters (e.g., numeric tokens, raw punctuation)
    for token in tokens:
        if re.search('[a-zA-Z]', token):
            filtered_tokens.append(token)
    return filtered_tokens

titles=ks714C.text.values

totalvocab_stemmed = []
totalvocab_tokenized = []
for i in titles:
    allwords_stemmed = tokenize_and_stem(i) #for each item in 'synopses', tokenize/stem
    totalvocab_stemmed.extend(allwords_stemmed) #extend the 'totalvocab_stemmed' list
    
    allwords_tokenized = tokenize_only(i)
    totalvocab_tokenized.extend(allwords_tokenized)
    
vocab_frame = pd.DataFrame({'words': totalvocab_tokenized}, index = totalvocab_stemmed)

tfidf = TfidfVectorizer(stop_words='english',
                                 use_idf=True, tokenizer=tokenize_and_stem, ngram_range=(1,3))

tfidf_matrix = tfidf.fit_transform(titles)

pca=PCA(n_components=2)
pca_fit=pca.fit(tfidf_matrix.A)
pos = pca.fit_transform(tfidf_matrix.A)

ks714C["score1"]=pos[:,0]
ks714C["score2"]=pos[:,1]

#What text is usually present for a high score1?

normal_raw=" ".join(ks714C.text[np.where((ks714C.score1<0.1) & (ks714C.score2 < 0.1))[0]].values.tolist())
n_text=tokenize_and_stem(normal_raw)
n_freq=nltk.FreqDist(t.lower() for t in nltk.Text([s for s in n_text if s not in stopwords]))

n_freq.most_common(5)

sc1_raw=" ".join(ks714C.text[np.where(ks714C.score1>=0.1)[0]].values.tolist())
sc1_text=tokenize_and_stem(sc1_raw)
sc1_freq=nltk.FreqDist(t.lower() for t in nltk.Text([s for s in sc1_text if s not in stopwords]))

sc1_freq.most_common(5)

sc2_raw=" ".join(ks714C.text[np.where(ks714C.score2>=0.1)[0]].values.tolist())
sc2_text=tokenize_and_stem(sc2_raw)
sc2_freq=nltk.FreqDist(t.lower() for t in nltk.Text([s for s in sc2_text if s not in stopwords]))

sc2_freq.most_common(5)

weird_KS=((ks714C.score1>=0.1)|(ks714C.score2>=0.1))*1

ks714C["category2"]=np.where(weird_KS==1,"Alternative",ks714C.category)

ks714C.to_csv('ks714NLP.csv')


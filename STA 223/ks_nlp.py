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
import matplotlib.pyplot as plt
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.decomposition import PCA, NMF
from sklearn.manifold import MDS
from sklearn import neighbors
import seaborn as sns
from sklearn.metrics.pairwise import cosine_similarity

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
ks714C.text=ks714C.name+" "+ks714C.blurb

#Try the NLP

cv=CountVectorizer(stop_words='english')
pca=PCA(n_components=2)
name_fit=cv.fit_transform(ks714C.name).toarray()
name_pca=pca.fit_transform(name_fit)
blurb_fit=cv.fit_transform(ks714C.blurb).toarray()
blurb_pca=pca.fit_transform(blurb_fit)

ks714C["score1"]=name_pca[:,0]
ks714C["score2"]=name_pca[:,1]
ks714C["score3"]=blurb_pca[:,0]
ks714C["score4"]=blurb_pca[:,1]

#What do the plots look like?
sns.lmplot(x="score1",y="score2",data=ks714C,hue="category",fit_reg=False)
sns.lmplot(x="score3",y="score4",data=ks714C,hue="category",fit_reg=False)
#Food seems to be the noisiest cluster. What does the data look like without food?

ks714CF=ks714C.loc[ks714C.category!="Food",:]

#Product design is the least noisest.

sns.lmplot(x="score1",y="score2",data=ks714CF,hue="category",fit_reg=False)
sns.lmplot(x="score3",y="score4",data=ks714CF,hue="category",fit_reg=False)

#It's easy to tell that score1 is caused by potato salad
ks714C["category2"]=np.where(ks714C.score1>=.5,"Potato Salad",ks714C.category)

##Some REAL NLP

stopwords = nltk.corpus.stopwords.words('english')

from nltk.stem.snowball import SnowballStemmer
stemmer=SnowballStemmer('english')

def tokenize_and_stem(text):
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [word for sent in nltk.sent_tokenize(text) for word in nltk.word_tokenize(sent)]
    filtered_tokens = []
    # filter out any tokens not containing letters (e.g., numeric tokens, raw punctuation)
    for token in tokens:
        if re.search('[a-zA-Z]', token):
            filtered_tokens.append(token)
    stems = [stemmer.stem(t) for t in filtered_tokens]
    return stems


def tokenize_only(text):
    # first tokenize by sentence, then by word to ensure that punctuation is caught as it's own token
    tokens = [word.lower() for sent in nltk.sent_tokenize(text) for word in nltk.word_tokenize(sent)]
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

dist = 1 - cosine_similarity(tfidf_matrix)

from sklearn.cluster import KMeans

num_clusters = 5

km = KMeans(n_clusters=num_clusters)

km.fit(tfidf_matrix)

clusters = km.labels_.tolist()

ks714C["cluster"]=clusters

terms = tfidf.get_feature_names()
terms2 = pd.Series(terms)

order_centroids = km.cluster_centers_.argsort()[:, ::-1]

term_list=[terms2[order_centroids[i,:10]].values for i in range(len(order_centroids))]

#Which topics increase the likelihood of success?
pd.crosstab(ks714C.cluster,ks714C.outcome).apply(lambda r: r/r.sum(), axis=1)

#We need to start WRAPPING UP this part of the project
#Consider developing a final model for NLP
#Topic modeling might be the way to go (e.g. specific topics affect likelihood of success)

#Testing NLP classification

from nltk import word_tokenize

token=tokenize_and_stem(" ".join(ks714C.text.values.tolist()))
text=nltk.Text([t for t in token if t not in stopwords])
all_words=nltk.FreqDist(t.lower() for t in text)
word_features = [a[0] for a in all_words.most_common(100)]

def document_features(document):
    document_words = set(nltk.Text(tokenize_and_stem(document)))
    features = {}
    for word in word_features:
        features['contains({})'.format(word)] = (word in document_words)
    return features

featuresets = [document_features(d) for d in ks714C.name.values]
featuresets = [(featuresets[i],ks714C.outcome.values[i]) for i in range(len(ks714C))]

train_set, test_set = featuresets[:1600], featuresets[1600:]
classifier = nltk.NaiveBayesClassifier.train(train_set)
print(nltk.classify.accuracy(classifier, test_set))

#These words are effective at determining successful and unsuccessful projects!
classifier.show_most_informative_features(20)

#Examine this classifier's relationship with other variables

P_succ=np.array([classifier.prob_classify(document_features(t)).prob(1) for t in ks714C.text.values])

ks714C["textScore"]=np.log(P_succ/(1-P_succ))

ks714C.to_csv('ks714CNLP2.csv')


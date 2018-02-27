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
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.decomposition import PCA
from sklearn import neighbors

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
text_fit=cv.fit_transform(ks714C.text).toarray()
text_pca=pca.fit_transform(text_fit)

ks714C["score1"]=name_pca[:,0]
ks714C["score2"]=name_pca[:,1]
ks714C["score3"]=text_pca[:,0]
ks714C["score4"]=text_pca[:,1]

#Current work, NLP is producing slightly interesting results
#The 'frivolous' titles seem to be popping out, but there's only 100 of them.

X=text_fit
y=ks714C.category.values

clf = neighbors.KNeighborsClassifier(10, weights='distance')
clf.fit(X, y)

nn_preds=clf.predict(X)
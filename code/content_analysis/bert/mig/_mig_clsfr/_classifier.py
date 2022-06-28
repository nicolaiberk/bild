# -*- coding: utf-8 -*-
"""
Created on Mon Jun 21 20:22
@author: Nicolai Berk
"""

# install language model
# python -m spacy download de_core_news_sm

# import
# import csv
import os
import datetime
import pandas as pd
from _functions import TextCleaner
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from imblearn.over_sampling import SMOTE
from sklearn import metrics
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import MultinomialNB
from sklearn import svm
# from sklearn.model_selection import cross_val_score
import joblib
import random
import numpy as np
import copy
import pickle
import nltk
# nltk.download('stopwords')
# from nltk.corpus import stopwords
# sws_german = stopwords.words("german")
import random

os.chdir('../..') # moves from _sc/_mig_clsfr to main project folder
datadir = os.path.expanduser('_dt/_handcoding/')
# datadir = 'drive/MyDrive/rrviol/' # colab

dt = str(datetime.datetime.now().strftime("%Y%m%d-%H%M%S"))

# load data
print('Loading data...')
dta = pd.read_csv(datadir+'handcoding_finished.csv', header=0)
print('\tDone!')


# preprocess
print('Preprocessing articles...')
dta['tokens'], dta['stems'] = TextCleaner(dta.text)
dta['labels'] = dta.mig == 'Ja'
print('\tDone!')


# create dict for different datasets
dta_dict = {form: {vec: {ds: {v: [] for v in ['X', 'y']} for ds in ['train', 'test']} for vec in ['tfidf', 'count']} for form in ['stems', 'tokens']}

vec_dict = {form: {vec: [] for vec in ['tfidf', 'count']} for form in ['stems', 'tokens']}

# vectorize and train-test-split
print('Vectorizing and train-test-split...')

for form in dta_dict:
    for vec in dta_dict[form]:
        
        if vec == 'tfidf':
            vec_dict[form][vec] = TfidfVectorizer(max_df=.5, min_df=5, 
#                                                   stop_words = sws_german
                                                 )
        else:
            vec_dict[form][vec] = CountVectorizer(max_df=.5, min_df=5, 
#                                                   stop_words = sws_german
                                                 )

        # Vectorize
        tempmtrx = vec_dict[form][vec].fit_transform(dta[form])
        
        # train-test-split
        X_train, X_test, y_train, y_test = train_test_split(
            tempmtrx, dta.labels, test_size=0.25, random_state=42)
        
        # assign to dict    
        dta_dict[form][vec]['train']['X'] = copy.deepcopy(X_train)
        dta_dict[form][vec]['test']['X'] = copy.deepcopy(X_test)
        dta_dict[form][vec]['train']['y'] = copy.deepcopy(y_train)
        dta_dict[form][vec]['test']['y'] = copy.deepcopy(y_test)
        
        
print('\tDone!')


        
        

# train classifiers, compare performance
print('Comparative performance of classifiers in cross-validation:\n---\n')

logreg = LogisticRegression()
naive = MultinomialNB()
svm_m = svm.SVC(gamma='scale')
models = [logreg, naive, svm_m]

f1_all = []

## generate index for cross-validation (all train sets have similar n of rows)
slice_id = [random.randint(0, 4) for i in range(dta_dict["stems"]["tfidf"]['train']['X'].shape[0])]
            

for m in models:
    for form in dta_dict:
        for vec in dta_dict[form]:
            
            # load data
            X_temp, y_temp = dta_dict[form][vec]['train']['X'], dta_dict[form][vec]['train']['y']
            
            # define empty containers for results storage
            acc    = []
            precis = []
            recall = []
            f1     = []
            
            for i in range(5):
                
                # split samples
                X_train = X_temp[[1 != sid for sid in slice_id]]
                X_test  = X_temp[[1 == sid for sid in slice_id]]
                y_train = y_temp[[1 != sid for sid in slice_id]]
                y_test  = y_temp[[1 == sid for sid in slice_id]]
                
                # oversample
                X_resampled, y_resampled = SMOTE().fit_resample(X_train, y_train)

            
                # calculate perfomance in cross-validation
                m.fit(X_resampled, y_resampled)
                prediction = m.predict(X_test)
                acc.append(metrics.accuracy_score(y_test, prediction))
                precis.append(metrics.precision_score(y_test, prediction))
                recall.append(metrics.recall_score(y_test, prediction))
                f1.append(metrics.f1_score(y_test, prediction))
                
            print(
                str(m) + ', ' + 
              '\nForm: ' + form + 
              '\nVectoriser: ' + vec + 
              '\n---' +
              '\nAverage accuracy: '  + str(np.mean(acc).round(2)) +
              '\nAverage precision: ' + str(np.mean(precis).round(2)) +
              '\nAverage recall: '    + str(np.mean(recall).round(2)) +
              '\nAverage f1-score: '  + str(np.mean(f1).round(2)) + '\n---\n\n'
            )
            
            
            if len(f1_all) == 0 or f1 > max(f1_all):
                    form_final = form
                    vec_final = vec
                    model_final = m
            
            f1_all.append(f1)


            

# training & validation final classifier

print('### Best performing classifier in crossval: ###\n'+ 
      str(model_final) + ', ' + 
      '\nForm: ' + form_final + 
      '\nVectoriser: ' + vec_final +
      '\n---\n')

## oversample
print('Oversampling for balance...')
X_resampled, y_resampled = SMOTE().fit_resample(dta_dict[form_final][vec_final]['train']['X'], dta_dict[form_final][vec_final]['train']['y'])
print('\tDone!\n')

## fit
model_final.fit(X_resampled, y_resampled)

## predict test set, define observed dependent
test_pred = model_final.predict(dta_dict[form_final][vec_final]['test']['X'])
y_actual = dta_dict[form_final][vec_final]['test']['y']

print('------------------\nPerformance final classifier:\n-------------------')

## performance
print('Accuracy: ' + str(round(metrics.accuracy_score(y_actual, test_pred), 2)) + 
      '\nPrecision: ' + str(round(metrics.precision_score(y_actual, test_pred), 2)) +
      '\nRecall: ' + str(round(metrics.recall_score(y_actual, test_pred), 2)) +
      '\nF1-score: ' + str(round(metrics.f1_score(y_actual, test_pred), 2)) + 
      '\n------------------')

print(metrics.confusion_matrix(y_actual, test_pred))

print('------------------')


# generate predicted probabilities for full set of articles


# save model
pickle.dump(vec_dict[form_final][vec_final], open("_dt/vectorizer.pkl",mode='wb'))
joblib.dump(model_final, '_dt/classifier.pkl')
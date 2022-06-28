import os
import pandas as pd
import csv
import sys
import numpy as np
from transformers import TFAutoModelForSequenceClassification

os.chdir('../..')



csv.field_size_limit(sys.maxsize)

model = TFAutoModelForSequenceClassification.from_pretrained('_dt/mig_clsfr_BERT')
fieldnames = ['date', 'paper', 'title', 'link', 'topic', 'mig_pred', 'mig_proba']



for paper in os.listdir('_dt/Archive/'):
    with open('_dt/Archive/'+paper, mode="r", encoding="utf-8") as fi:
        reader = csv.reader(fi)

        for row in reader:
            # define relative position in row based on title
            titlerow = np.argmax([r == 'title' for r in row])
            linkrow  = np.argmax([r == 'url'   for r in row])
            daterow  = np.argmax([r == 'date'  for r in row])
            textrow  = np.argmax([r == 'text'  for r in row])
            topicrow = np.argmax([r == 'topic' for r in row])
            break
        
        for skip in range(i):
            next(reader)


        with open('_dt/_mig_estimates/_BERT_estimates'+paper, mode="a", encoding="utf-8") as fo:

            writer = csv.DictWriter(fo, lineterminator = '\n', fieldnames = fieldnames)
            writer.writeheader()

            for row in reader:
                i += 1
                mtrx = vectorizer.transform(pd.array([row[textrow]]))
                pred = clf.predict(mtrx)[0]
                proba = clf.predict_proba(mtrx)[0][1]
                writer.writerow({'date':         row[daterow], 
                                 'paper':        paper, 
                                 'title':        row[titlerow], 
                                 'link':         row[linkrow], 
                                 'topic':        row[topicrow],
                                 'mig_pred':     pred,
                                 'mig_proba':    proba})
                print(f'Classified {i} speeches. Current file: {paper}', end="\r")
            print(f'Finished estimating {i} speeches ({paper})\n')

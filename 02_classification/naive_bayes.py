import numpy as np
import pickle
import time
import warnings

from sklearn.metrics import classification_report
from sklearn.naive_bayes import GaussianNB

# filter fscore warnings
warnings.filterwarnings("ignore", message="F-score is ill-defined and being set to 0.0 in labels with no predicted samples.")

# Open file for reading
train = np.genfromtxt("in/train.txt", delimiter=";", skip_header=1)
test  = np.genfromtxt("in/test.txt", delimiter=";", skip_header=1)

# split into train and test
n, col = train.shape
col = col - 1
X_train = train[:,0:col]
y_train = train[:,col]
X_test = test[:,0:col]
y_test = test[:,col]

# Print start time
start = time.time()
print("Start gnb", time.strftime("%d.%m.%y %H:%M:%S"))

# train model
nb = GaussianNB()
nb.fit(X_train, y_train)

# predict labels in training and test
y_train_pred = nb.predict(X_train)
y_test_pred = nb.predict(X_test)

# save model
with open('out/gnb.pkl', 'wb') as f:
    pickle.dump(nb, f)


# print classification reports
print("TRAIN")
print(classification_report(y_train, y_train_pred))
print("TEST")
print(classification_report(y_test, y_test_pred))

# print done message
m, s = divmod(time.time() - start, 60)
h, m = divmod(m, 60)
print("Done gnb", time.strftime("%d.%m.%y %H:%M:%S"), "duration: ", "%d:%02d:%02d" % (h, m, s), "train-samples: ", n)

# GAUSSIANNB
# TRAIN
             # precision    recall  f1-score   support

       # -4.0       0.65      0.37      0.47     22198
       # -3.0       0.13      0.13      0.13     21641
       # -1.0       0.23      0.29      0.26     20701
        # 0.0       0.85      0.98      0.91    181839
        # 1.0       0.48      0.34      0.40     23957
        # 2.0       0.21      0.07      0.11     23405
        # 5.0       0.46      0.51      0.49     16002

# avg / total       0.65      0.69      0.66    309743

# TEST
             # precision    recall  f1-score   support

       # -4.0       0.66      0.37      0.48      5674
       # -3.0       0.13      0.13      0.13      5282
       # -1.0       0.23      0.29      0.25      5182
        # 0.0       0.86      0.98      0.91     45397
        # 1.0       0.47      0.33      0.38      5893
        # 2.0       0.20      0.07      0.11      6002
        # 5.0       0.45      0.50      0.48      4006

# avg / total       0.65      0.68      0.66     77436


# GAUSSIANNB, equal classes
# TRAIN
             # precision    recall  f1-score   support

       # -4.0       0.67      0.37      0.47     22337
       # -3.0       0.21      0.21      0.21     21498
       # -1.0       0.20      0.33      0.25     20687
        # 0.0       0.53      0.97      0.69     20007
        # 1.0       0.51      0.38      0.43     23896
        # 2.0       0.21      0.07      0.11     23601
        # 5.0       0.47      0.51      0.49     15928

# avg / total       0.40      0.39      0.37    147954

# TEST
             # precision    recall  f1-score   support

       # -4.0       0.67      0.37      0.48      5535
       # -3.0       0.21      0.21      0.21      5425
       # -1.0       0.21      0.34      0.26      5196
        # 0.0       0.53      0.97      0.69      4993
        # 1.0       0.51      0.38      0.44      5954
        # 2.0       0.20      0.07      0.11      5806
        # 5.0       0.47      0.51      0.49      4080

# avg / total       0.40      0.39      0.37     36989
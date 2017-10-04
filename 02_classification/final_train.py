import numpy as np
import pickle
import time
import warnings

from sklearn import metrics, neighbors
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC, LinearSVC

# filter fscore warnings
warnings.filterwarnings("ignore", message="F-score is ill-defined and being set to 0.0 in labels with no predicted samples.")

# classifier to train
classifier = "logr"

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

# scale data
scaler = StandardScaler()
scaler.fit(X_train)
X_train = scaler.transform(X_train)
X_test = scaler.transform(X_test)

# Print start time
start = time.time()
print("Start", classifier, time.strftime("%d.%m.%y %H:%M:%S"))

# train model
if(classifier=="knn"):
    k = 4
    mdl = neighbors.KNeighborsClassifier(k, weights="uniform")

elif(classifier=="svm"):
    c = 32768
    g = 2
    mdl = SVC(C=c, gamma=g, kernel="rbf", cache_size=1000)

elif(classifier=="lsvm"):
    c = 8
    mdl = LinearSVC(C=c)

elif(classifier=="mlp"):
    h = 64
    mdl = MLPClassifier(solver="adam", hidden_layer_sizes=(h,), random_state=123)

elif(classifier=="logr"):
    c = 32
    mdl = LogisticRegression(C=c)


mdl.fit(X_train, y_train)

# predict labels in training and test
y_train_pred = mdl.predict(X_train)
y_test_pred = mdl.predict(X_test)

# save model
if(classifier=="knn"):
    with open("out/knn_k"+str(k)+".pkl", 'wb') as f:
        pickle.dump(mdl, f)
    print("Model saved as out/knn_k"+str(k)+".pkl")
    
elif(classifier=="svm"):
    with open("out/svm_c"+str(c)+"_g"+str(g)+".pkl", 'wb') as f:
        pickle.dump(mdl, f)
    print("Model saved as out/svm_c"+str(c)+"_g"+str(g)+".pkl")

elif(classifier=="lsvm"):
    with open("out/lsvm_c"+str(c)+".pkl", 'wb') as f:
        pickle.dump(mdl, f)
    print("Model saved as out/lsvm_c"+str(c)+".pkl")
        
elif(classifier=="mlp"):
    with open("out/mlp_h"+str(h)+".pkl", 'wb') as f:
        pickle.dump(mdl, f)
    print("Model saved as out/mlp_h"+str(h)+".pkl")

elif(classifier=="logr"):
    with open("out/logr_c"+str(c)+".pkl", 'wb') as f:
        pickle.dump(mdl, f)
    print("Model saved as out/logr_c"+str(c)+".pkl")

# print classification reports
print("TRAIN")
print(classification_report(y_train, y_train_pred))
print("TEST")
print(classification_report(y_test, y_test_pred))

# print done message
m, s = divmod(time.time() - start, 60)
h, m = divmod(m, 60)
print("Done", classifier, time.strftime("%d.%m.%y %H:%M:%S"), "duration: ", "%d:%02d:%02d" % (h, m, s), "train-samples: ", n)


# KNN, 7s, 1core, 300k samples
# TRAIN
             # precision    recall  f1-score   support

       # -4.0       1.00      1.00      1.00     22198
       # -3.0       0.99      1.00      0.99     21641
       # -1.0       0.97      0.98      0.98     20701
        # 0.0       1.00      0.99      1.00    181839
        # 1.0       1.00      1.00      1.00     23957
        # 2.0       1.00      1.00      1.00     23405
        # 5.0       1.00      1.00      1.00     16002

# avg / total       1.00      1.00      1.00    309743

# TEST
             # precision    recall  f1-score   support

       # -4.0       1.00      1.00      1.00      5674
       # -3.0       0.99      0.99      0.99      5282
       # -1.0       0.95      0.97      0.96      5182
        # 0.0       1.00      0.99      0.99     45397
        # 1.0       1.00      1.00      1.00      5893
        # 2.0       1.00      1.00      1.00      6002
        # 5.0       1.00      1.00      1.00      4006

# avg / total       0.99      0.99      0.99     77436


# SVM, 5min, 1core, 300k samples
# TRAIN
             # precision    recall  f1-score   support

       # -4.0       1.00      1.00      1.00     22198
       # -3.0       0.99      0.99      0.99     21641
       # -1.0       0.98      0.98      0.98     20701
        # 0.0       1.00      1.00      1.00    181839
        # 1.0       1.00      1.00      1.00     23957
        # 2.0       1.00      1.00      1.00     23405
        # 5.0       1.00      1.00      1.00     16002

# avg / total       1.00      1.00      1.00    309743

# TEST
             # precision    recall  f1-score   support

       # -4.0       1.00      1.00      1.00      5674
       # -3.0       0.99      1.00      0.99      5282
       # -1.0       0.98      0.98      0.98      5182
        # 0.0       1.00      1.00      1.00     45397
        # 1.0       1.00      1.00      1.00      5893
        # 2.0       1.00      1.00      1.00      6002
        # 5.0       1.00      1.00      1.00      4006
        
# avg / total       1.00      1.00      1.00     77436

#LSVM, 7min, 1core, 300k samples
# TRAIN
            # precision    recall  f1-score   support

       # -4.0       0.00      0.00      0.00     22198
       # -3.0       0.00      0.00      0.00     21641
       # -1.0       0.00      0.00      0.00     20701
        # 0.0       0.63      1.00      0.77    181839
        # 1.0       0.00      0.00      0.00     23957
        # 2.0       0.00      0.00      0.00     23405
        # 5.0       0.45      0.62      0.53     16002

# avg / total       0.39      0.62      0.48    309743

# TEST
             # precision    recall  f1-score   support

       # -4.0       0.00      0.00      0.00      5674
       # -3.0       0.00      0.00      0.00      5282
       # -1.0       0.00      0.00      0.00      5182
        # 0.0       0.63      1.00      0.77     45397
        # 1.0       0.00      0.00      0.00      5893
        # 2.0       0.00      0.00      0.00      6002
        # 5.0       0.45      0.64      0.53      4006

# avg / total       0.39      0.62      0.48     77436


# LOGR, 5s, 1core, 300k samples
# TRAIN
             # precision    recall  f1-score   support

       # -4.0       0.90      0.11      0.19     22198
       # -3.0       0.00      0.00      0.00     21641
       # -1.0       0.00      0.00      0.00     20701
        # 0.0       0.63      1.00      0.77    181839
        # 1.0       0.00      0.00      0.00     23957
        # 2.0       0.00      0.00      0.00     23405
        # 5.0       0.53      0.52      0.52     16002

# avg / total       0.46      0.62      0.49    309743

# TEST
             # precision    recall  f1-score   support

       # -4.0       0.91      0.10      0.19      5674
       # -3.0       0.00      0.00      0.00      5282
       # -1.0       0.00      0.00      0.00      5182
        # 0.0       0.63      1.00      0.77     45397
        # 1.0       0.00      0.00      0.00      5893
        # 2.0       0.00      0.00      0.00      6002
        # 5.0       0.52      0.53      0.52      4006

# avg / total       0.46      0.62      0.49     77436

# MLP, 2min, 1core, 300k samples
# TRAIN
             # precision    recall  f1-score   support

       # -4.0       1.00      0.99      0.99     22198
       # -3.0       0.97      0.97      0.97     21641
       # -1.0       0.96      0.90      0.93     20701
        # 0.0       0.99      1.00      0.99    181839
        # 1.0       0.99      1.00      1.00     23957
        # 2.0       1.00      0.99      0.99     23405
        # 5.0       0.99      1.00      0.99     16002

# avg / total       0.99      0.99      0.99    309743

# TEST
             # precision    recall  f1-score   support

       # -4.0       1.00      1.00      1.00      5674
       # -3.0       0.98      0.97      0.97      5282
       # -1.0       0.97      0.89      0.93      5182
        # 0.0       0.99      1.00      0.99     45397
        # 1.0       0.99      1.00      1.00      5893
        # 2.0       1.00      0.99      1.00      6002
        # 5.0       0.99      1.00      0.99      4006

# avg / total       0.99      0.99      0.99     77436



import numpy as np
import pickle
import time
import warnings

from sklearn import neighbors
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC, LinearSVC

# filter fscore warnings
warnings.filterwarnings("ignore", message="Precision and F-score are ill-defined and being set to 0.0 in labels with no predicted samples.")
warnings.filterwarnings("ignore", message="Recall and F-score are ill-defined and being set to 0.0 in labels with no true samples.")

# Open file for reading
train = np.genfromtxt("in/class_train.txt", delimiter=";", skip_header=1)
val_old = np.genfromtxt("in/class_val_old.txt", delimiter=";", skip_header=1)
val_new = np.genfromtxt("in/class_val_new.txt", delimiter=";", skip_header=1)

# assign X and y from datasets
n, col = val_old.shape
col = col - 1
X_train = train[:,0:col]
## validation sets
X_val_old = val_old[:,0:col]
y_val_old = val_old[:,col]
X_val_new = val_new[:,0:col]
y_val_new = val_new[:,col]

# scale data
scaler = StandardScaler()
scaler.fit(X_train)
X_val_old = scaler.transform(X_val_old)
X_val_new = scaler.transform(X_val_new)

# Print start time
start = time.time()
print("Start", time.strftime("%d.%m.%y %H:%M:%S"))

# load model
for classifier in ["knn_k4.pkl", "svm_c32768_g2.pkl", "lsvm_c8.pkl", "logr_c32.pkl", "mlp_h64.pkl"]:
    with open("in/"+classifier, "rb") as f:
        mdl = pickle.load(f)
    # predict labels in validation set
    y_val_old_pred = mdl.predict(X_val_old)
    y_val_new_pred = mdl.predict(X_val_new)
    # print classification reports
    print("")
    print("VAL", classifier)
    print("OLD GL")
    print(classification_report(y_val_old, y_val_old_pred))
    print("NEW GL")
    cm = confusion_matrix(y_val_new, y_val_new_pred, labels=[-4,-3,-2,-1,0,1,2,3,5])
    fmt_string = "".join("%%-%is" % (6) for i in np.zeros(10))
    print(fmt_string % ("",-4,-3,-2,-1,0,1,2,3,5))
    print(fmt_string % tuple(np.insert(cm[2], 0, -2)))
    s = np.insert(np.round(cm[2]/sum(cm[2]),2), 0, -2.2)
    print(fmt_string % tuple(s))
    print(fmt_string % tuple(np.insert(cm[7],0,3)))
    s = np.insert(np.round(cm[7]/sum(cm[7]),2), 0, 3.3)
    print(fmt_string % tuple(s))

# print done message
m, s = divmod(time.time() - start, 60)
h, m = divmod(m, 60)
print("Done", time.strftime("%d.%m.%y %H:%M:%S"), "duration: ", "%d:%02d:%02d" % (h, m, s))

# VAL knn_k4.pkl
# OLD GL
             # precision    recall  f1-score   support

       # -4.0       0.99      0.23      0.38     16719
       # -3.0       0.00      0.00      0.00         0
       # -1.0       0.22      0.09      0.13     18401
        # 0.0       0.87      0.96      0.91    118225
        # 1.0       0.54      0.57      0.55     18925
        # 2.0       0.26      0.12      0.16     17559
        # 5.0       0.00      0.00      0.00         0

# avg / total       0.73      0.69      0.69    189829

# NEW GL
      # -4    -3    -2    -1    0     1     2     3     5
# -2    2654  5478  0     2967  4155  0     3340  0     3
# -2.2  0.14  0.29  0.0   0.16  0.22  0.0   0.18  0.0   0.0
# 3     0     0     0     0     7574  8552  2712  0     64
# 3.3   0.0   0.0   0.0   0.0   0.4   0.45  0.14  0.0   0.0

# VAL svm_c32768_g2.pkl
# OLD GL
             # precision    recall  f1-score   support

       # -4.0       0.00      0.00      0.00     16719
       # -3.0       0.00      0.00      0.00         0
       # -1.0       0.36      0.12      0.17     18401
        # 0.0       0.74      0.79      0.77    118225
        # 1.0       0.39      0.16      0.23     18925
        # 2.0       0.29      0.25      0.27     17559
        # 5.0       0.00      0.00      0.00         0

# avg / total       0.56      0.54      0.54    189829

# NEW GL
      # -4    -3    -2    -1    0     1     2     3     5
# -2    2031  2609  0     1630  8998  0     2231  0     1098
# -2.2  0.11  0.14  0.0   0.09  0.48  0.0   0.12  0.0   0.06
# 3     0     0     0     0     7505  5050  6347  0     0
# 3.3   0.0   0.0   0.0   0.0   0.4   0.27  0.34  0.0   0.0

# VAL lsvm_c8.pkl
# OLD GL
             # precision    recall  f1-score   support

       # -4.0       0.00      0.00      0.00     16719
       # -1.0       0.00      0.00      0.00     18401
        # 0.0       0.65      1.00      0.79    118225
        # 1.0       0.00      0.00      0.00     18925
        # 2.0       0.00      0.00      0.00     17559
        # 5.0       0.00      0.00      0.00         0

# avg / total       0.41      0.62      0.49    189829

# NEW GL
      # -4    -3    -2    -1    0     1     2     3     5
# -2    0     0     0     0     14613 0     0     0     3984
# -2.2  0.0   0.0   0.0   0.0   0.79  0.0   0.0   0.0   0.21
# 3     0     0     0     0     16142 0     0     0     2760
# 3.3   0.0   0.0   0.0   0.0   0.85  0.0   0.0   0.0   0.15

# VAL logr_c32.pkl
# OLD GL
             # precision    recall  f1-score   support

       # -4.0       0.00      0.00      0.00     16719
       # -1.0       0.00      0.00      0.00     18401
        # 0.0       0.65      1.00      0.79    118225
        # 1.0       0.00      0.00      0.00     18925
        # 2.0       0.00      0.00      0.00     17559
        # 5.0       0.00      0.00      0.00         0

# avg / total       0.41      0.62      0.49    189829

# NEW GL
      # -4    -3    -2    -1    0     1     2     3     5
# -2    0     0     0     0     14620 0     0     0     3977
# -2.2  0.0   0.0   0.0   0.0   0.79  0.0   0.0   0.0   0.21
# 3     0     0     0     0     16142 0     89    0     2671
# 3.3   0.0   0.0   0.0   0.0   0.85  0.0   0.0   0.0   0.14

# VAL mlp_h64.pkl
# OLD GL
             # precision    recall  f1-score   support

       # -4.0       0.92      0.44      0.60     16719
       # -3.0       0.00      0.00      0.00         0
       # -1.0       0.97      0.22      0.36     18401
        # 0.0       0.93      0.94      0.94    118225
        # 1.0       0.36      0.49      0.41     18925
        # 2.0       0.29      0.12      0.17     17559
        # 5.0       0.00      0.00      0.00         0

# avg / total       0.82      0.70      0.73    189829

# NEW GL
      # -4    -3    -2    -1    0     1     2     3     5
# -2    4524  2559  0     7044  2326  0     707   0     1437
# -2.2  0.24  0.14  0.0   0.38  0.13  0.0   0.04  0.0   0.08
# 3     1944  314   0     0     7     6658  7310  0     2669
# 3.3   0.1   0.02  0.0   0.0   0.0   0.35  0.39  0.0   0.14
# Done 10.07.17 15:59:35 duration:  0:00:42
import numpy as np
import pickle
import time
import warnings

from sklearn.metrics import classification_report, confusion_matrix
from sklearn.naive_bayes import GaussianNB

# filter fscore warnings
warnings.filterwarnings("ignore", message="Precision and F-score are ill-defined and being set to 0.0 in labels with no predicted samples.")
warnings.filterwarnings("ignore", message="Recall and F-score are ill-defined and being set to 0.0 in labels with no true samples.")

# Open file for reading
val_old = np.genfromtxt("in/class_val_old.txt", delimiter=";", skip_header=1)
val_new = np.genfromtxt("in/class_val_new.txt", delimiter=";", skip_header=1)

# assign X and y from validation datasets
n, col = val_old.shape
col = col - 1
X_val_old = val_old[:,0:col]
y_val_old = val_old[:,col]
X_val_new = val_new[:,0:col]
y_val_new = val_new[:,col]

# Print start time
start = time.time()
print("Start gnb", time.strftime("%d.%m.%y %H:%M:%S"))

with open("in/gnb.pkl", "rb") as f:
    mdl = pickle.load(f)
# predict labels in validation set
y_val_old_pred = mdl.predict(X_val_old)
y_val_new_pred = mdl.predict(X_val_new)
# print classification reports
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
print("Done gnb", time.strftime("%d.%m.%y %H:%M:%S"), "duration: ", "%d:%02d:%02d" % (h, m, s), "train-samples: ", n)

# OLD GL
             # precision    recall  f1-score   support

       # -4.0       1.00      0.33      0.50     16719
       # -3.0       0.00      0.00      0.00         0
       # -1.0       0.16      0.21      0.18     18401
        # 0.0       0.92      1.00      0.96    118225
        # 1.0       0.00      0.00      0.00     18925
        # 2.0       0.51      0.17      0.25     17559
        # 5.0       0.00      0.00      0.00         0

# avg / total       0.72      0.69      0.68    189829

# NEW GL
      # -4    -3    -2    -1    0     1     2     3     5
# -2    0     970   0     5470  2531  4442  4591  0     593
# -2.2  0.0   0.05  0.0   0.29  0.14  0.24  0.25  0.0   0.03
# 3     0     4523  0     4499  2683  4427  10    0     2760
# 3.3   0.0   0.24  0.0   0.24  0.14  0.23  0.0   0.0   0.15
# Done gnb 10.07.17 16:05:37 duration:  0:00:00 train-samples:  189829
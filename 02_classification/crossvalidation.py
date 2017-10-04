import numpy as np
import time
import warnings

from sklearn import metrics, neighbors
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import StratifiedKFold, cross_val_score, GridSearchCV
from sklearn.neural_network import MLPClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC, LinearSVC

# filter fscore warnings
warnings.filterwarnings("ignore", message="F-score is ill-defined and being set to 0.0 in labels with no predicted samples.")

# classifier to cross validate
classifier = "logr"

# Open file for reading
# refX;refY;offsetX;offsetY;dpt
src = np.genfromtxt("in/train.txt", delimiter=";", skip_header=1)

# Load data
n, col = src.shape
col = col - 1
X = src[:,0:col]
y = src[:,col]

# scale data
scaler = StandardScaler()
scaler.fit(X)
X = scaler.transform(X)


# 10-fold stratified cross-validation
cv = StratifiedKFold(n_splits=10, shuffle=True, random_state=123)

# Print start time
start = time.time()
print("Start", classifier, time.strftime("%d.%m.%y %H:%M:%S"))

# KNN
if(classifier=="knn"): 
    param = {"n_neighbors": np.arange(1,35+1)**2}
    mdl = neighbors.KNeighborsClassifier()	
        
# SVM        
elif(classifier=="svm"):
    X = X[0:20000]
    y = y[0:20000]
    n = 20000

    param = {"C": [2**x for x in [-5,-3,-1,1,3,5,7,9,11,13,15]], "gamma": [2**x for x in [-15,-13,-11,-9,-7,-5,-3,-1,1,2,3]]} # weird syntax bc neg pow not allowed in numpy
    mdl = SVC(kernel="rbf", cache_size=1000, random_state=123)
    
# LinearSVM
elif(classifier=="lsvm"):
    X = X[0:20000]
    y = y[0:20000]
    n = 20000

    param = {"C": [2**x for x in [-5,-3,-1,1,3,5,7,9,11,13,15]]}
    mdl = LinearSVC(random_state=123)
    
# MLP
elif(classifier=="mlp"):
    param = {"hidden_layer_sizes": [(1,), (2,), (4,), (8,), (16,), (32,), (64,)]}
    mdl = MLPClassifier(solver="adam", random_state=123)

# LOGR
elif(classifier=="logr"):
    param = {"C": [2**x for x in [-5,-3,-1,1,3,5,7,9,11,13,15]]}
    mdl = LogisticRegression()
        
gscv = GridSearchCV(mdl, param, n_jobs=1, cv=cv, scoring="f1_macro")
gscv.fit(X, y)

# Print results
print(gscv.best_estimator_)
print(gscv.best_score_)

# Save results
train = gscv.cv_results_["mean_train_score"]
test = gscv.cv_results_["mean_test_score"]

if(classifier=="knn"):
    k = gscv.cv_results_["param_n_neighbors"].data
    np.savetxt("out/"+classifier+"_summary.txt", np.column_stack((k,train,test)), delimiter=";", header="k;train;test", fmt="%i;%1.5f;%1.5f", comments="")
    
elif(classifier=="svm"):
    c = gscv.cv_results_["param_C"].data
    g = gscv.cv_results_["param_gamma"].data
    np.savetxt("out/"+classifier+"_summary.txt", np.column_stack((c,g,train,test)), delimiter=";", header="c;gamma;train;test", fmt="%1.6f;%1.6f;%1.5f;%1.5f", comments="")

elif(classifier=="lsvm"):
    c = gscv.cv_results_["param_C"].data
    np.savetxt("out/"+classifier+"_summary.txt", np.column_stack((c,train,test)), delimiter=";", header="c;train;test", fmt="%1.5f;%1.5f;%1.5f", comments="")

elif(classifier=="mlp"):
    h = gscv.cv_results_["param_hidden_layer_sizes"].data
    h = [x[0] for x in h] # turn tuple to int
    np.savetxt("out/"+classifier+"_summary.txt", np.column_stack((h,train,test)), delimiter=";", header="h;train;test", fmt="%i;%1.5f;%1.5f", comments="")

elif(classifier=="logr"):
    c = gscv.cv_results_["param_C"].data
    np.savetxt("out/"+classifier+"_summary.txt", np.column_stack((c,train,test)), delimiter=";", header="c;train;test", fmt="%1.5f;%1.5f;%1.5f", comments="")


# print done message
m, s = divmod(time.time() - start, 60)
h, m = divmod(m, 60)
  
print("Done", classifier, time.strftime("%d.%m.%y %H:%M:%S"), "duration: ", "%d:%02d:%02d" % (h, m, s), "samples: ", n)

# ~2h, 4 cores, 300k samples
# KNeighborsClassifier(algorithm='auto', leaf_size=30, metric='minkowski',
#            metric_params=None, n_jobs=1, n_neighbors=4, p=2,
#            weights='uniform')
# 0.990372574659


# ~1,5h, 4 cores, 20000 obs
# SVC(C=32768, cache_size=1000, class_weight=None, coef0=0.0,
  # decision_function_shape=None, degree=3, gamma=2, kernel='rbf',
  # max_iter=-1, probability=False, random_state=None, shrinking=True,
  # tol=0.001, verbose=False)
# 0.994241388286



# 5min, 4 cores, 20000 obs
# LinearSVC(C=8, class_weight=None, dual=True, fit_intercept=True,
     # intercept_scaling=1, loss='squared_hinge', max_iter=1000,
     # multi_class='ovr', penalty='l2', random_state=None, tol=0.0001,
     # verbose=0)
# 0.208954114182

# 3min, 4cores, 300k obs
# LogisticRegression(C=32, class_weight=None, dual=False, fit_intercept=True,
          # intercept_scaling=1, max_iter=100, multi_class='ovr', n_jobs=1,
          # penalty='l2', random_state=None, solver='liblinear', tol=0.0001,
          # verbose=0, warm_start=False)
# 0.21229890145

# 35min, 4 cores, 300k obs
#MLPClassifier(activation='relu', alpha=0.0001, batch_size='auto', beta_1=0.9,
#       beta_2=0.999, early_stopping=False, epsilon=1e-08,
#       hidden_layer_sizes=(64,), learning_rate='constant',
#       learning_rate_init=0.001, max_iter=200, momentum=0.9,
#       nesterovs_momentum=True, power_t=0.5, random_state=123,
#       shuffle=True, solver='adam', tol=0.0001, validation_fraction=0.1,
#       verbose=False, warm_start=False)
#0.981086213968





import numpy as np
import time

from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import cross_val_score, StratifiedKFold


# Open file for reading
# dpt;X;Y;offsetX;offsetY
train = np.genfromtxt("in/train.txt", delimiter=";", skip_header=1)

# Load data
X_train = train[:,0:3]

# Print start time
start = time.time()
print("Start reg", time.strftime("%d.%m.%y %H:%M:%S"))

# train model
degree = np.arange(1,20+1)

for col in [3,4]:
    y_train = train[:,col] #3: offsetX, 4: offsetY
    mse_results = []
    r_results = []

    for d in degree:
        # fit model
        pip = Pipeline([("polynomial_features", PolynomialFeatures(d)), ("linear_regression", LinearRegression(normalize=True))])
        pip.fit(X_train,y_train)
        
        # compute performance
        mse = cross_val_score(pip, X_train, y_train, scoring="neg_mean_squared_error", cv=10, n_jobs=4)
        mse_results.append(np.mean(mse))
        # r = cross_val_score(pip, X_train, y_train, scoring="r2", cv=10, n_jobs=1)
        # r_results.append(np.mean(r))
        

    # Save results
    sum = "out/regX_summary.txt" if col==3 else "out/regY_summary.txt"
    np.savetxt(sum, np.column_stack((degree, mse_results, r_results)), delimiter=";", header="d;mse", fmt="%i;%1.2f", comments="")

# print done message
m, s = divmod(time.time() - start, 60)
h, m = divmod(m, 60)
  
print("Done reg", time.strftime("%d.%m.%y %H:%M:%S"), "duration: ", "%d:%02d:%02d" % (h, m, s))

# 8:30h, 1core, 2 measures


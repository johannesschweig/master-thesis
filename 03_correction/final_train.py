import numpy as np
import pickle
import time
import warnings

from sklearn.linear_model import LinearRegression

# filter fscore warnings
warnings.filterwarnings("ignore", message="F-score is ill-defined and being set to 0.0 in labels with no predicted samples.")

# Open file for reading
train = np.genfromtxt("in/train.txt", delimiter=";", skip_header=1)

# training set
# dpt;X;Y;offsetX;offsetY
n, col = train.shape
col = col - 1
X_trainX = np.column_stack((train[:,0], train[:,0]*train[:,1])) # offsetX ~ dpt + X dpt
X_trainY = np.column_stack((train[:,0]*train[:,0], train[:,0]*train[:,2])) # offsetY ~ dpt^2 + Y dpt

# Print start time
start = time.time()
print("Start correction", time.strftime("%d.%m.%y %H:%M:%S"))

# train model
mdlX = LinearRegression(normalize=True)
mdlX.fit(X_trainX, train[:,3])
mdlY = LinearRegression(normalize=True)
mdlY.fit(X_trainY, train[:,4])

# save model
with open("out/regX.pkl", 'wb') as f:
    pickle.dump(mdlX, f)
with open("out/regY.pkl", 'wb') as f:
    pickle.dump(mdlY, f)
print("Models saved as out/regX.pkl and out/regY.pkl")
print("offsetX ~ ",mdlX.coef_[0], " * dpt + ", mdlX.coef_[1], " * X dpt + ", mdlX.intercept_)
print("offsetY ~ ",mdlY.coef_[0], " * dpt^2 + ", mdlY.coef_[1], " * Y dpt + ", mdlY.intercept_)

# print done message
m, s = divmod(time.time() - start, 60)
h, m = divmod(m, 60)
print("Done", time.strftime("%d.%m.%y %H:%M:%S"), "duration: ", "%d:%02d:%02d" % (h, m, s), "train-samples: ", n)

# offsetX ~  55.9617242337  * dpt +  -0.0616126932288  * X dpt +  -0.783811703176
# offsetY ~ -5.97264798417  * dpt^2 +  -0.036196647576  * Y dpt +  -6.11623614658
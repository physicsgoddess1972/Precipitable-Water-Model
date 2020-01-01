from csv import *
from numpy import *
from sklearn import *
import cvxopt
from sklearn.model_selection import train_test_split
from matplotlib import pyplot as plt
from sklearn import svm
from sklearn.metrics import confusion_matrix, classification_report
import seaborn as sns
import pandas as pd

## Data Preproccessing
raw_data = []
raw_label = []
with open("./ml_data.csv") as csvfile:
    reader = reader(csvfile, delimiter=",")
    next(reader, None)
    for row in reader:
        raw_data.append(list((float(row[1]),float(row[2]),float(row[3]))))
        raw_label.append(int(row[-1]))
X = array(raw_data)
y = array(raw_label)
y[y == 1] = -1
y[y == 2] = 1
y = ones(len(X)) * y

## Training-Testing Data Partition
X_train, X_test, y_train, y_test = train_test_split(X, y, train_size=0.7, random_state=0)


fig, (ax1,ax2,ax3) = plt.subplots(1,3)
plt.subplots_adjust(left=0.09, right=0.96, wspace=0.25)
ax1.set_title("Full Dataset ")
ax1.scatter(X[:, 0], X[:, 1], X[:,2], c=y, cmap='coolwarm')
ax1.set_ylabel(r"TPW [mm]")
ax1.set_xlabel(r"Temperature [$^o$C]")
ax2.scatter(X_train[:, 0], X_train[:, 1], X_train[:,2], c=y_train, cmap='coolwarm')
ax2.set_title("Training Dataset")
ax2.set_xlabel(r"Temperature [$^o$C]")
ax3.scatter(X_test[:, 0], X_test[:, 1], X_test[:,2], c=y_test, cmap='coolwarm')
ax3.set_title("Testing Dataset")
ax3.set_xlabel(r"Temperature [$^o$C]")
plt.ylim(min(X[:,1]), max(X[:,1]))
plt.xlim(min(X[:,0]), max(X[:,0]))

svc = svm.SVC(kernel='linear', degree=5, C=1).fit(X_train, y_train)

# rbf_svc = svm.SVC(kernel='rbf', gamma=0.7, C=1).fit(X_train, y_train)
# poly_svc = svm.SVC(kernel='poly', degree=3, C=1).fit(X_train, y_train)
# lin_svc = svm.LinearSVC(C=1).fit(X_train, y_train)

x_min, x_max = X_test[:, 0].min() - 1, X_test[:, 0].max() + 1
y_min, y_max = 0, X_test[:, 1].max() + 1

w = svc.coef_[0]
a = -w[0] / w[1]

xx, yy = meshgrid(arange(x_min, x_max, 0.2),
                    arange(y_min, y_max, 0.2))

db_x    = linspace(x_min, x_max)
db_y    = a * db_x - svc.intercept_[0] / w[1]
sv1_y   = a * db_x - (svc.intercept_[0] - 1) / w[1]
sv2_y   = a * db_x - (svc.intercept_[0] + 1) / w[1]

# Original Space (Testing Data)
plt.figure(2)
plt.scatter(X_test[:, 0], X_test[:, 1], X_test[:,2], c=y_test, cmap=plt.cm.coolwarm)
plt.plot(db_x, db_y, 1, color="purple", label="Decision Hyperplane")
plt.plot(db_x, sv1_y,1, linestyle="--", color='k', label="Support Vectors")
plt.plot(db_x, sv2_y,1, linestyle="--", color='k')
plt.ylim(y_min, y_max)
plt.xlim(x_min, x_max)
plt.title("Testing Dataset Temperature vs TPW")
plt.xlabel(r"Temperature [$^o$C]")
plt.ylabel(r"TPW [mm]")

handles, labels = plt.gca().get_legend_handles_labels()
by_label = dict(zip(labels, handles))
plt.legend(by_label.values(), by_label.keys())

# Confusion matrix
plt.figure(3)
y_pred = svc.predict(X_test)
con_mat = array(confusion_matrix(y_test, y_pred))
confusion = pd.DataFrame(con_mat, index=['clear sky', 'overcast'],
                    columns=['predicted clear', 'predicted clouds'])
sns.heatmap(confusion, annot=True, fmt='d', cmap='plasma')
plt.title("Confusion Matrix for Classical SVM")

plt.show()

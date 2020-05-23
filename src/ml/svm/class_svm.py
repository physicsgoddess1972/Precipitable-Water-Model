import cvxopt,os
from csv import *
from numpy import *
import pandas as pd
import random as rd
from seaborn import *
from sklearn import *
from sklearn import svm
from matplotlib import pyplot as plt
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import confusion_matrix, classification_report, precision_score

raw_data    = []
raw_label   = []

## Data import
with open("../../../data/ml/ml_data.csv") as csvfile:
    reader = reader(csvfile, delimiter=",")
    next(reader, None)
    for row in reader:
        raw_data.append(list((float(row[1]),float(row[2]),float(row[3]))))
        raw_label.append(int(row[-1]))

## Shoving data and labels into an array
X = array(raw_data)
y = array(raw_label)

## Redefining data labels to be -1 or 1
y[y == 1] = -1
y[y == 2] = 1
y = ones(len(X)) * y

class svm_analysis:
## Initalization function to retrieve accuracy
    def __init__(self):
        self.result = []
        self.over_fit = []
## Plot of all data and the Training Set
    def pan_data(self, tr_size, rand_ste, plot, suff):
        X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        train_size=tr_size,
                                                        random_state=rand_ste)
        if plot == True:
            plt.figure(1)
            plt.subplot(2,1,1)
            plt.subplots_adjust(hspace=0.35)
            plt.title("Full Dataset Temperature vs TPW")
            plt.scatter(X[:, 0], X[:, 1], X[:,2], c=y, cmap='coolwarm')
            plt.ylabel(r"TPW [mm]")

            plt.subplot(2,1,2)
            plt.scatter(X_train[:, 0], X_train[:, 1], X_train[:,2], c=y_train, cmap='coolwarm')
            plt.title("Training Dataset Temperature vs TPW")
            plt.xlabel(r"Temperature [$^o$C]")
            plt.ylabel(r"TPW [mm]")

            plt.savefig("./output/{}/all_data_{}.png".format(int(tr_size * 100), suff))
            plt.close()

## Plot of Testing Data with support vectors and decision boundary
    def test_plot(self, tr_size, rand_ste, plot, suff):
        X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        train_size=tr_size,
                                                        random_state=rand_ste)

        svc = svm.SVC(kernel='linear', degree=5, C=2).fit(X_train, y_train)
# Minimum and Maximum values for the testing data
        x_min, x_max = X_test[:, 0].min() - 1, X_test[:, 0].max() + 1
        y_min, y_max = 0, X_test[:, 1].max() + 1
# Analysis coefficients
        w = svc.coef_[0]
        a = -w[0] / w[1]

        xx, yy = meshgrid(arange(x_min, x_max, 0.2),
                            arange(y_min, y_max, 0.2))

# X-components of the support vectors and decision boundary
        db_x    = linspace(x_min, x_max)
# X-component of the decision boundary
        db_y    = a * db_x - svc.intercept_[0] / w[1]
# Y-components of the support vectors
        sv1_y   = a * db_x - (svc.intercept_[0] - 1) / w[1]
        sv2_y   = a * db_x - (svc.intercept_[0] + 1) / w[1]

        if plot == True:
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

            plt.savefig("./output/{}/results_{}.png".format(int(tr_size * 100), suff))
            plt.close()

## Confusion matrix
    def confusion(self, tr_size, rand_ste, plot, suff):
        X_train, X_test, y_train, y_test = train_test_split(X, y,
                                                        train_size=tr_size,
                                                        random_state=rand_ste)

        svc = svm.SVC(kernel='linear', degree=5, C=2).fit(X_train, y_train)
        y_pred = svc.predict(X_test)

        con_mat = array(confusion_matrix(y_test, y_pred))
        acc = round(svc.score(X_test, y_test) * 100, 2)

        confusion = pd.DataFrame(con_mat, index=['clear sky', 'overcast'],
                            columns=['predicted clear', 'predicted clouds'])

        self.result.append(acc)
        scores = cross_val_score(svc, X, y, cv=5)
        precision = precision_score(y_test, y_pred)
        print(precision)
        print("Accuracy: {:.2f} (+/- {:.2f})".format(scores.mean(), scores.std()* 2))
        if plot == True:
            plt.figure(3)
            heatmap(confusion, annot=True, fmt='d', cmap='plasma', cbar=False,
                    robust=True, square=True)
            plt.suptitle('Confusion Matrix for Classical SVM', fontsize=16)
            plt.title("Testing Accuracy: {}%".format(acc))

            plt.savefig("./output/{}/con_mat_{}.png".format(int(tr_size * 100), suff))
            plt.close()

if __name__ == '__main__':
## Evaluation of model across a sequence of random states
    def eval():
        acc_set = []
        rdst    = []

        for i in range(0, 10):
            C.confusion(tr_size, i, False, None)

            acc_set.append(C.result)
            rdst.append(i)
        plt.figure(4)
        plt.scatter(rdst, acc_set[0], color='cadetblue')
        plt.xlabel("Random State")
        plt.ylabel("Testing Accuracy")
        plt.title("Evaluation of model with variant random states")

        plt.savefig("./output/{}/rand_ste_eval.png".format(int(tr_size * 100)))
        plt.close()

        max_idx     = acc_set[0].index(max(acc_set[0]))
        min_idx     = acc_set[0].index(min(acc_set[0]))
        good_ste    = rdst[max_idx]
        bad_ste     = rdst[min_idx]
        return good_ste, bad_ste
## Function plots all plots
    def plots(rand_ste, suff):
        C.pan_data(tr_size, rand_ste, True, suff)
        C.test_plot(tr_size,rand_ste, True, suff)
        C.confusion(tr_size,rand_ste, True, suff)

    for i in [0.7]:
## Training set relative size [0 to 1]
        tr_size = i
        path = "./output/{}/".format(int(tr_size * 100))
        try:
            os.mkdir(path)
        except FileExistsError:
            pass
    ## Calls class
        C       = svm_analysis()

        good,bad = eval()

        plots(good, "max_acc")
        print(C.result)

        plots(bad, "min_acc")
        # print(C.result)

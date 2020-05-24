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
from sklearn.metrics import confusion_matrix, classification_report, \
                            precision_score, jaccard_score, matthews_corrcoef

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

        scores      = cross_val_score(svc, X, y, cv=5)
        precision   = round(precision_score(y_test, y_pred),2)
        jaccard     = round(jaccard_score(y_test, y_pred),2)
        matt_corr   = round(matthews_corrcoef(y_test, y_pred),2)
        mean_acc    = round(scores.mean(),2)
        cv_std      = round(scores.std() * 2,2)
        self.result.append([acc, precision, jaccard, matt_corr, mean_acc, cv_std])
        if plot == True:
            plt.figure(3)
            heatmap(confusion, annot=True, fmt='d', cmap='plasma', cbar=False,
                    robust=True, square=True)
            plt.suptitle('Confusion Matrix for Classical SVM', fontsize=16)
            plt.title("Testing Accuracy: {}%".format(acc))

            plt.savefig("./output/{}/con_mat_{}.png".format(int(tr_size * 100), suff))
            plt.close()
class svm_evaluation:
    def __init__(self):
        self.result = []
## Evaluation of model across a sequence of random states
    def eval(self, N, tr_size):
        acc_set = []
        rdst    = []
        pre_set = []
        for i in range(0, N):
            C       = svm_analysis()
            C.confusion(tr_size, i, False, None)
            #print(C.result[0])
            acc_set.append(C.result[0][0])
            rdst.append(i)
            pre_set.append(C.result[0][1])
        plt.figure(4)
        plt.scatter(rdst, acc_set, color='cadetblue')
        plt.xlabel("Random State")
        plt.ylabel("Testing Accuracy")
        plt.title("Evaluation of model with variant random states. N = {}".format(int(N)))

        plt.savefig("./output/{}/rand_ste_eval.png".format(int(tr_size * 100)))
        plt.close()

        max_acc_idx     = acc_set.index(max(acc_set))
        min_acc_idx     = acc_set.index(min(acc_set))

        max_pre_idx     = pre_set.index(max(pre_set))
        min_pre_idx     = pre_set.index(min(pre_set))

        good_acc_ste    = rdst[max_acc_idx]
        bad_acc_ste     = rdst[min_acc_idx]

        good_pre_ste    = rdst[max_pre_idx]
        bad_pre_ste     = rdst[min_pre_idx]

        return good_acc_ste, bad_acc_ste, good_pre_ste, bad_pre_ste
## Function plots all plots
    def plots(self,rand_ste, suff):
        C = svm_analysis()
        C.pan_data(tr_size, rand_ste, True, suff)
        C.test_plot(tr_size,rand_ste, True, suff)
        C.confusion(tr_size,rand_ste, True, suff)

    def other_plots(self,rand_ste, tr_size):
        idn = rand_ste[0]
        inv = rand_ste[1]
        idn_acc = []
        idn_pre = []
        idn_jac = []
        idn_mat = []
        inv_acc = []
        inv_pre = []
        inv_jac = []
        inv_mat = []
        for i in range(0, len(tr_size)):
            C       = svm_analysis()
            C.confusion(tr_size[i], idn[i], False, None)
            idn_met = C.result

            X       = svm_analysis()
            X.confusion(tr_size[i], inv[i], False, None)
            inv_met = X.result
            print(idn_met)
            idn_acc.append(idn_met[0][0])
            idn_pre.append(idn_met[0][1])
            idn_jac.append(idn_met[0][2])
            idn_mat.append(idn_met[0][3])

            inv_acc.append(inv_met[0][0])
            inv_pre.append(inv_met[0][1])
            inv_jac.append(inv_met[0][2])
            inv_mat.append(inv_met[0][3])
# Model A and A^-1
        plt.figure(5)
        ax1 = plt.subplot(4,1,1)
        plt.subplots_adjust(hspace=0.14, right=1, top=1)
#        ax1.yaxis.set_major_locator(plt.NullLocator())
        ax1.xaxis.set_major_formatter(plt.NullFormatter())
#        ax1.xaxis.set_major_locator(plt.MaxNLocator(3))
        ax1.xaxis.set_major_locator(plt.FixedLocator(tr_size))
        plt.scatter(tr_size, idn_acc, color="mediumblue", label="Model A")
        plt.scatter(tr_size, inv_acc, color="red", label=r"Model A$^{-1}$")
        plt.ylabel("Acc [%]")
        plt.legend()

        ax2 = plt.subplot(4, 1, 2, sharex=ax1)
        plt.scatter(tr_size, idn_pre, color="mediumblue")
        plt.scatter(tr_size, inv_pre, color="red")
        plt.ylabel("Prec")
        plt.ylim(0.5, 1)

        ax3 = plt.subplot(4, 1, 3, sharex=ax1)
        plt.scatter(tr_size, idn_jac, color="mediumblue")
        plt.scatter(tr_size, inv_jac, color="red")
        plt.ylabel("Jacc Scr")
        plt.ylim(0.5,1)

        ax4 = plt.subplot(4, 1, 4)
        plt.scatter(tr_size, idn_mat, color="mediumblue")
        plt.scatter(tr_size, inv_mat, color="red")
        plt.xlabel("Training Size")
        plt.ylabel("Matt Coeff")
        plt.ylim(0.5,1)
        ax4.xaxis.set_major_locator(plt.FixedLocator(tr_size))

if __name__ == '__main__':
    tr_list = [0.5, 0.6, 0.7, 0.8, 0.9]

    good_acc_lst    = []
    bad_acc_lst     = []
    good_pre_lst    = []
    bad_pre_lst     = []

    for i in tr_list:
## Training set relative size [0 to 1]
        tr_size = i
        path = "./output/{}/".format(int(tr_size * 100))
        try:
            os.mkdir(path)
        except FileExistsError:
            pass
        D = svm_evaluation()
        good_acc,bad_acc,good_pre,bad_pre = D.eval(100, tr_size)

        good_acc_lst.append(good_acc)
        bad_acc_lst.append(bad_acc)
        good_pre_lst.append(good_pre)
        bad_pre_lst.append(bad_pre)

    print(good_acc_lst)
    D.other_plots([good_acc_lst, bad_acc_lst], tr_list)
    plt.show()
        # D.plots(good, "max_acc")
        # # print(C.result)
        #
        # D.plots(bad, "min_acc")
        # print(C.result)

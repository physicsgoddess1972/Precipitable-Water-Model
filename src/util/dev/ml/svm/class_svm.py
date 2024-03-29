from csv import *
from numpy import *
import pandas as pd
import random as rd
from seaborn import *
import cvxopt,os,time

from matplotlib import pyplot as plt
import matplotlib.gridspec as gridspec

from sklearn import *
from sklearn import svm
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import confusion_matrix, classification_report, \
                            precision_score, jaccard_score, matthews_corrcoef, f1_score

from rich import print, box
from rich.panel import Panel
from rich.progress import track
from rich.table import Table

from rich.progress import (
    BarColumn,
    DownloadColumn,
    TextColumn,
    TransferSpeedColumn,
    TimeRemainingColumn,
    Progress,
    TaskID,
)

progress = Progress(TextColumn("[bold blue]{task.fields[filename]}", justify="right"),
                    BarColumn(bar_width=None),
                    "[progress.percentage]{task.percentage:>3.1f}%",
                    TimeRemainingColumn())


import argparse

parser = argparse.ArgumentParser(description="Classical Support Vector Machine Module")
parser.add_argument("-tr", type=list, help="values of training size divisions\n\t[Default: 0.7]", default=[0.7])
parser.add_argument("-N", type=int, help="Number of random states\n\t[Default: 100]", default=100, dest="N")
parser.add_argument("-dfile", type=str, help="file path of data", dest="dfile", default="../../../data/ml/")

args = parser.parse_args()

raw_data    = []
raw_label   = []

## Data import
with open(str(args.dfile) + "ml_data.csv") as csvfile:
    reader = reader(csvfile, delimiter=",")
    next(reader, None)
    for row in reader:
        raw_data.append(list((float(row[1]),log(float(row[2])),float(row[3]))))
        if row[-1] == "overcast":
            raw_label.append(1)
        else:
            raw_label.append(3)

## Shoving data and labels into an array
X = array(raw_data)
y = array(raw_label)

## Redefining data labels to be -1 or 1
y[y == 1] = -1
y[y == 3] = 1
y = ones(len(X)) * y

# Analysis Class
class svm_analysis:
## Initalization function to retrieve accuracy
    def __init__(self):
        self.result = []

    def closest(self, n, K):
        n = array(n)
        idx = (abs(n - K)).argmin()
        return n[idx], idx

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

            plt.savefig("../figs/ml/{}/all_data_{}.png".format(int(tr_size * 100), suff))
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
        #print(svc.coef_)
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

            plt.savefig("../figs/ml/{}/results_{}.png".format(int(tr_size * 100), suff))
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
        print(y_test)
        scores      = cross_val_score(svc, X, y, cv=5)
        precision   = round(precision_score(y_test, y_pred),2)
        jaccard     = round(jaccard_score(y_test, y_pred),2)
        matt_corr   = round(matthews_corrcoef(y_test, y_pred),2)
        fscore      = f1_score(y_test, y_pred, average='binary')
        mean_acc    = round(scores.mean(),2)
        cv_std      = round(scores.std() * 2,2)

        self.result.append([acc, precision, jaccard, matt_corr, fscore, mean_acc, cv_std])
        if plot == True:
            plt.figure(3)
            heatmap(confusion, annot=True, fmt='d', cmap='coolwarm', cbar=False,
                    robust=True, square=True)
            plt.suptitle('Confusion Matrix for Classical SVM', fontsize=16)
            plt.title("Testing Accuracy: {}%".format(acc))

            plt.savefig("../figs/ml/{}/con_mat_{}.png".format(int(tr_size * 100), suff))
            plt.close()

# Evaluation Class
class svm_evaluation:
    def __init__(self):
        self.result = []
## Function plots all plots
    def plots(self,tr_size, rand_ste, suff):
        C = svm_analysis()
        C.pan_data(tr_size, rand_ste, True, suff)
        C.test_plot(tr_size,rand_ste, True, suff)
        C.confusion(tr_size,rand_ste, True, suff)

    def eval_table(self, acc, pre, matt, jacc, f1s):
        table = Table(show_header=False, box=None, padding=(0,1,0,0))
        table.add_column(width=50, justify="left", style="green", no_wrap=True)
        table.add_column(width=50, justify="left", style="green", no_wrap=True)
        table.add_row("\t\tAverage Accuracy",   "{:.5f}".format(average(acc)/100.))
        table.add_row("\t\tAverage Precision",  "{:.5f}".format(average(pre)))
        table.add_row("\t\t[blue]Average Matthews Coefficient", "[blue]{:.5f}".format(average(matt)))
        table.add_row("\t\t[blue]Average Jaccard Score", "[blue]{:.5f}".format(average(jacc)))
        table.add_row("\t\t[medium_orchid]Average F1 Score", "[medium_orchid]{:.5f}".format(average(f1s)))
        progress.log(table)

## Model Evaluation for N random states
    def eval(self, tr_size, N):
        task_id1 = progress.add_task("download", filename="Random State Evaluation ({})".format(tr_size))

        results = [[] for _ in range(5)]
        for i in range(0, N):
            C       = svm_analysis()
            C.confusion(tr_size, i, False, None)
            results[0].append(C.result[0][0])
            results[1].append(C.result[0][1])
            results[2].append(C.result[0][2])
            results[3].append(C.result[0][3])
            results[4].append(C.result[0][4])
            progress.update(task_id1, advance=100./N,refresh=True)
        avg_fscr, avg_fscr_ste = svm_analysis().closest(results[4], average(results[4]))
        avg_acc, avg_acc_ste = svm_analysis().closest(results[0], average(results[0]))
        self.result.append([avg_fscr_ste, avg_acc_ste])

        svm_evaluation().eval_table(results[0], results[1], results[2], results[3], avg_fscr)

        gs = gridspec.GridSpec(2, 2)
        fig = plt.figure()
        ax = fig.add_subplot(gs[0, 0])
        ax.scatter(arange(0,N), results[2], c=results[3], cmap='winter')
        ax.axhline(average(results[2]), c="black", linestyle="--")
        ax.set_ylabel("Matthew Coefficient")
        ax.set_xlabel("Random State")
        ax.set_ylim(0.5, 1.00+0.01)
        ax.set_xlim(0-1, N)

        ax = fig.add_subplot(gs[0, 1], sharey=ax, sharex=ax)
        ax.scatter(arange(0,N), results[1], c=results[3], cmap='winter')
        ax.axhline(average(results[1]), c="black", linestyle="--")
        ax.set_ylabel("Precision")
        ax.set_xlabel("Random State")

        ax = fig.add_subplot(gs[1, :], sharex=ax)
        ax.scatter(arange(0,N), results[0], c=results[3], cmap='winter')
        ax.axhline(average(results[0]), c="black", linestyle="--")
        ax.set_ylabel("Accuracy [%]")
        ax.set_xlabel("Random State")
        norm = mpl.colors.Normalize(vmin=min(results[3]),vmax=max(results[3]))
        sm = plt.cm.ScalarMappable(cmap=plt.get_cmap('winter',len(results[3])), norm=norm)
        sm.set_array([])
        fig.colorbar(sm,ax=ax).set_label('Jaccard Score')

        plt.tight_layout()
        plt.savefig("../figs/ml/{}/super_eval_{}.pdf".format(int(tr_size*100), N))
        plt.close()

if __name__ == '__main__':
    with progress:
        df = pd.read_csv("https://raw.githubusercontent.com/physicsgoddess1972/Precipitable-Water-Model/master/data/ml/ml_data.csv")
        progress.print(Panel("[bold deep_sky_blue2]Good Morning\nWelcome to the Classical SVM Analysis Module of the Precipitable Water Model. For more information about the model and the purpose of this tool, please visit the [link=https://git.io/fj5Xr]documentation page[/link]"))

        progress.log("[bold white]Script Started")
        task_id = progress.add_task("download", filename="Support Vector Machine Analysis")

        tr_list = [0.6, 0.7, 0.8]
        for i in range(0,len(tr_list)):
            progress.log("\t[bold red]Training Division {}".format(tr_list[i]))
            progress.log("\t\t[orange3]Starting Model Evaluation")

            try:
                os.makedirs("../figs/ml/{}/".format(int(tr_list[i] * 100)))
                progress.log("\t\t[yellow]Directory Generated")
            except FileExistsError:
                progress.log("\t\t[yellow]Directory Already Exists")
                pass

            D = svm_evaluation()
            D.eval(tr_list[i], args.N)
            D.plots(tr_list[i], D.result[0][0], "avg_fscr")
            D.plots(tr_list[i], D.result[0][1], "avg_acc")

            progress.update(task_id, advance=100./(len(tr_list)),refresh=True)
            progress.log("\t[bold red]Model Evaluation Complete")
        progress.log("[bold white]Script Complete")

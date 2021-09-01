import copy
import logging
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import warnings
from csv import *
from seaborn import *
from sklearn.exceptions import UndefinedMetricWarning
from sklearn.metrics import confusion_matrix, classification_report, \
    precision_score, jaccard_score, matthews_corrcoef, f1_score
# from seaborn import *
from sklearn.model_selection import train_test_split
import matplotlib.gridspec as gridspec

logging.disable(logging.WARNING)
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"

import tensorflow.compat.v1 as tf

tf.disable_v2_behavior()
tf.disable_resource_variables()

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
    TaskID
)

progress = Progress(TextColumn("[bold blue]{task.fields[filename]}", justify="right"),
                    BarColumn(bar_width=None),
                    "[progress.percentage]{task.percentage:>3.1f}%",
                    TimeRemainingColumn())

import argparse

parser = argparse.ArgumentParser(description="Classical Support Vector Machine Module")
parser.add_argument("-N", type=int, help="Number of random states\n\t[Default: 100]", default=100, dest="N")
parser.add_argument("-dfile", type=str, help="file path of data", dest="dfile", default="../../../data/ml/")
parser.add_argument("-ffile", type=str, help="file path of figures", dest="ffile", default="../../../figs/ml/")
parser.add_argument("-err", dest='err', action='store_true', default=False)
parser.add_argument("-R", dest='R', action='store_true', default=False)
parser.add_argument("-E", dest='E', action='store_true', default=False)

args = parser.parse_args()

class Utility():
    def __init__(self):
        pass

    def closest(self, n, K):
        n = np.array(n)
        idx = (np.abs(n - K)).argmin()
        return n[idx], idx

class TFClassicalSVMModel():
    def __init__(self):
        pass

    def preproccess(self, rand_ste, train_size):
        raw_data = []
        raw_label = []
        with open(str(args.dfile) + "ml_data.csv") as csvfile:
            next(reader(csvfile, delimiter=","), None)
            for row in reader(csvfile, delimiter=","):
                raw_data.append([float(row[1]), float(row[2])])
                if row[-1] == "overcast":
                    raw_label.append(-1)
                elif row[-1] == "clear sky":
                    raw_label.append(1)

        data = np.array(raw_data)
        label = np.array(raw_label)
        label = np.ones(len(data)) * label
        X_train, X_test, y_train, y_test = train_test_split(data, label,
                                                            train_size=train_size,
                                                            random_state=rand_ste)
        return [X_train, X_test, data], [y_train, y_test, label]

    def process(self, sess):
        # Initialize placeholders
        x_data = tf.placeholder(shape=[None, 2], dtype=tf.float32)
        y_target = tf.placeholder(shape=[None], dtype=tf.float32)

        # Create variables for svm
        A = tf.Variable(tf.random_normal(shape=[2, 1]))
        b = tf.Variable(tf.random_normal(shape=[1, 1]))
        model_output = tf.subtract(tf.matmul(x_data, A), b)

        # Compute SVM Model
        l2_norm = tf.reduce_sum(tf.square(A))
        alpha = tf.constant([0.1])
        classification_term = tf.reduce_mean(tf.maximum(0., tf.subtract(1., tf.multiply(model_output, y_target))))
        loss = tf.add(classification_term, tf.multiply(alpha, l2_norm))

        prediction = tf.sign(model_output)
        accuracy = tf.reduce_mean(tf.cast(tf.equal(prediction, y_target), tf.float32))

        return [sess, loss, accuracy, model_output], [x_data, y_target], [A, b]

    def postprocess(self, epoch, model, other_model, coeff, tr_size, batch_size):
        sess, loss, accuracy, model_output = model
        x_data, y_target = other_model
        A, b = coeff
        # Declare optimizer
        my_opt = tf.train.GradientDescentOptimizer(0.01)
        train_step = my_opt.minimize(loss)

        # Initialize variables
        init = tf.global_variables_initializer()
        sess.run(init)

        loss_vec = []
        train_accuracy = []
        test_accuracy = []

        data, labels = TFClassicalSVMModel().preproccess(100, tr_size)
        task_id1 = progress.add_task("download", filename="Epoch Evaluation ({})".format(tr_size))

        x_vals_train = data[0]
        y_vals_train = labels[0]
        x_vals_test = data[1]
        y_vals_test = labels[1]

        for i in range(epoch):
            rand_index = np.random.choice(len(x_vals_train), size=len(x_vals_train))

            X = x_vals_train[rand_index]
            Y = np.transpose(np.array(y_vals_train[rand_index]))

            sess.run(train_step, feed_dict={x_data: X, y_target: Y})

            temp_loss = sess.run(loss, feed_dict={x_data: X, y_target: Y})
            loss_vec.append(temp_loss)

            train_acc_temp = sess.run(accuracy,
                                      feed_dict={x_data: x_vals_train, y_target: np.transpose(np.array(y_vals_train))})
            train_accuracy.append(train_acc_temp)

            test_acc_temp = sess.run(accuracy,
                                     feed_dict={x_data: x_vals_test, y_target: np.transpose(np.array(y_vals_test))})
            test_accuracy.append(test_acc_temp)

            progress.update(task_id1, advance=100 / int(epoch), refresh=True)
        # Extract coefficients
        [[a1], [a2]] = sess.run(A)
        [[b]] = sess.run(b)
        db_x = x_vals_test  # np.linspace(data[2][:,0].min() - 1, data[2][:,0].max() + 1, num=len(x_vals_test))
        db_m = -a2 / a1
        db_y = (db_m * db_x) - b / a1

        sv1_y = (db_m * db_x) - (b - 1) / a1
        sv2_y = (db_m * db_x) - (b + 1) / a1

        pan_data = data[2]
        test_data = pd.DataFrame()
        test_data["Temp"] = pd.Series(x_vals_test[:, 0])
        test_data["TPW"] = pd.Series(x_vals_test[:, 1])

        lab_data = copy.deepcopy(x_vals_test)
        ld = []
        for i in range(0, len(db_x)):
            if ((lab_data[:, 1][i] - ((db_m * lab_data[:, 0][i]) - b / a1))) > 0:
                ld.append(1)
            elif ((lab_data[:, 1][i] - ((db_m * lab_data[:, 0][i]) - b / a1))) < 0:
                ld.append(-1)

        test_data['pred_cond'] = pd.Series(ld)
        test_data['labl_cond'] = pd.Series(y_vals_test)

        # define the colormap
        cmap = plt.cm.coolwarm
        # extract all colors from the .jet map
        cmaplist = [cmap(i) for i in range(cmap.N)]
        # create the new map
        cmap = cmap.from_list('Custom cmap', cmaplist, 2)
        bounds = np.linspace(-1, 1, 3)
        norm = matplotlib.colors.BoundaryNorm(bounds, 2)

        # Plot batch accuracy
        plt.figure(1)
        plt.plot(test_accuracy, 'k-')
        plt.plot(train_accuracy, 'r--')
        plt.title('Batch Accuracy')
        plt.xlabel('Generation')
        plt.ylabel('Accuracy')

        plt.tight_layout()
        plt.savefig(str(args.ffile) + "{}/batch_acc_{}.pdf".format(int(tr_size * 100), epoch))
        plt.close()

        # Plot loss over time
        plt.figure(2)
        plt.plot(loss_vec, 'k-')
        plt.title('Loss per Generation')
        plt.xlabel('Generation')
        plt.ylabel('Loss')

        plt.tight_layout()
        plt.savefig(str(args.ffile) + "{}/loss_time_{}.pdf".format(int(tr_size * 100), epoch))
        plt.close()
        plt.figure(3)
        plt.scatter(data[0][:, 0], data[0][:, 1], c=labels[0], cmap=cmap, marker="o", label="Training")
        plt.scatter(data[1][:, 0], data[1][:, 1], c=labels[1], cmap=cmap, marker="x", label="Testing")

        plt.plot(db_x, db_y, 'r-', linewidth=2)
        plt.plot(db_x, sv1_y, 'k--', linewidth=2)
        plt.plot(db_x, sv2_y, 'k--', linewidth=2)

        plt.ylim(0, 40)
        plt.title("Full Dataset Temperature vs TPW")
        plt.xlabel(r"Temperature [$^o$C]")
        plt.ylabel(r"TPW [mm]")

        cb = plt.colorbar(spacing='proportional', ticks=bounds)
        cb.set_ticks([-0.4, 0.6])
        cb.ax.tick_params(size=0)
        cb.ax.set_yticklabels(["overcast", "clear sky"], rotation=90)

        handles, labels = plt.gca().get_legend_handles_labels()
        by_label = dict(zip(labels, handles))
        plt.legend(by_label.values(), by_label.keys())

        plt.tight_layout()
        plt.savefig(str(args.ffile) + "{}/full_data_{}.pdf".format(int(tr_size * 100), epoch))
        plt.close()
        return test_data

class TFClassicalSVMEvaluation():
    def __init__(self):
        pass

    def epoch_state(self, result, tr_size, epoch):
        y_test = np.array(result["labl_cond"])
        y_pred = np.array(result["pred_cond"])

        precision = round(precision_score(y_test, y_pred), 3)
        jaccard = round(jaccard_score(y_test, y_pred), 3)
        matt_corr = round(matthews_corrcoef(y_test, y_pred), 3)
        fscore = round(f1_score(y_test, y_pred, average='binary', labels=np.unique(y_pred)), 3)

        con_mat = np.array(confusion_matrix(y_test, y_pred))
        confusion = pd.DataFrame(con_mat, index=['overcast', 'clear sky'],
                                 columns=['predicted clouds', 'predicted clear'])

        heatmap(confusion, annot=True, fmt='d', cmap='coolwarm', cbar=False,
                robust=True, square=True)
        plt.title('Confusion Matrix for Classical SVM', fontsize=16)

        plt.savefig(str(args.ffile) + "{}/con_mat_{}.pdf".format(int(tr_size * 100), epoch))
        plt.close()
        return [precision, jaccard, matt_corr, fscore]

    def rando_state(self, tr_size, N, out):
        task_id1 = progress.add_task("download", filename="Random State Evaluation ({})".format(tr_size))

        results = [[] for _ in range(5)]
        for i in range(0, N):
            C       = TFClassicalSVMEvaluation()
            D       = TFClassicalSVMModel()
            out = D.postprocess(N, )
            result = C.epoch_state(out, tr_size, i)
            results[0].append(result[0])
            results[1].append(result[1])
            results[2].append(result[2])
            results[3].append(result[3])
            # results[4].append(C.result[0][4])
            progress.update(task_id1, advance=100./N,refresh=True)
        avg_fscr, avg_fscr_ste = Utility().closest(results[3], np.average(results[3]))
        # avg_acc, avg_acc_ste = svm_analysis().closest(results[0], average(results[0]))
        # result.append([avg_fscr_ste])

        # svm_evaluation().eval_table(results[0], results[1], results[2], results[3], avg_fscr)

        gs = gridspec.GridSpec(2, 2)
        fig = plt.figure()
        ax = fig.add_subplot(gs[0, 0])
        ax.scatter(np.arange(0,N), results[2], c=results[3], cmap='winter')
        ax.axhline(np.average(results[2]), c="black", linestyle="--")
        ax.set_ylabel("Matthew Coefficient")
        ax.set_xlabel("Random State")
        ax.set_ylim(0.5, 1.00+0.01)
        ax.set_xlim(0-1, N)

        ax = fig.add_subplot(gs[0, 1], sharey=ax, sharex=ax)
        ax.scatter(np.arange(0,N), results[1], c=results[3], cmap='winter')
        ax.axhline(np.average(results[1]), c="black", linestyle="--")
        ax.set_ylabel("Precision")
        ax.set_xlabel("Random State")

        ax = fig.add_subplot(gs[1, :], sharex=ax)
        ax.scatter(np.arange(0,N), results[0], c=results[3], cmap='winter')
        ax.axhline(np.average(results[0]), c="black", linestyle="--")
        ax.set_ylabel("Accuracy [%]")
        ax.set_xlabel("Random State")
        norm = mpl.colors.Normalize(vmin=min(results[3]),vmax=max(results[3]))
        sm = plt.cm.ScalarMappable(cmap=plt.get_cmap('winter',len(results[3])), norm=norm)
        sm.set_array([])
        fig.colorbar(sm,ax=ax).set_label('Jaccard Score')

        plt.tight_layout()
        plt.savefig("../figs/ml/{}/super_eval_{}.pdf".format(int(tr_size*100), N))
        plt.close()
        return

class ErrorTable():
    def errtab(self):
        table = Table(show_header=False, box=None, padding=(0, 1, 0, 0))
        table.add_column(width=50, justify="left", style="red", no_wrap=True)
        table.add_column(width=300, justify="left", style="yellow", no_wrap=False)
        table.add_row("Error (001)",
                      "RuntimeWarning: invalid value encountered in double_scalars\nmcc = cov_ytyp / np.sqrt(cov_ytyt * cov_ypyp)")
        table.add_row(" ", " ")
        table.add_row("Error (002)",
                      "UndefinedMetricWarning: Precision is ill-defined and being set to 0.0 due to no predicted samples. Use `zero_division` parameter to control this behavior")
        table.add_row("---", "---")
        table.add_row("Warning (001)", "Matthew's Correlation Coefficient did not meet QA requirements")
        progress.log(table)

if __name__ == '__main__':
    if args.err == True:
        ErrorTable.errtab()
        quit()
    with progress:
        progress.print(Panel(
            "[bold deep_sky_blue2]Good Morning\nWelcome to the Classical SVM Analysis Module of the Precipitable Water Model. For more information about the model and the purpose of this tool, please visit the [link=https://git.io/fj5Xr]documentation page[/link]"))
        progress.log("[bold white]Script Started")
        task_id = progress.add_task("download", filename="Support Vector Machine Analysis")
        tr_list = [0.6, 0.7, 0.8]
        for i in tr_list:
            progress.log("[bold red]Training Division {}".format(i))
            progress.log("[orange3]Starting Model Evaluation")
            j = 0
            svm_model = TFClassicalSVMModel()
            svm_eval = TFClassicalSVMEvaluation()
            warnings.filterwarnings('error')
            while j < 0.6:
                while True:
                    with tf.Session() as sess:
                        model, placeholder, coeff = svm_model.process(sess)
                        result = svm_model.postprocess(args.N, model, placeholder, coeff, i, 100)
                        try:
                            if args.R == True:
                                eval = svm_eval.rando_state(i, 100, result)
                            else:
                                continue
                            eval = svm_eval.epoch_state(result, i, args.N)
                            j = eval[2]
                            break
                        except RuntimeWarning:
                            progress.log("[white]Error (001). Restarting Evaluation...")
                            continue
                        except UndefinedMetricWarning:
                            progress.log("[white]Error (002). Restarting Evaluation...")
                            continue
                progress.log("[white]Warning (001). Restarting Evaluation...")
            table = Table(show_header=False, box=None, padding=(0, 1, 0, 0))
            table.add_column(width=50, justify="left", style="green", no_wrap=True)
            table.add_column(width=200, justify="left", style="green", no_wrap=True)

            table.add_row("\t[yellow]Precision", "{:.3f}".format(eval[0]))
            table.add_row("\t[green]Jaccard Score", "{:.3f}".format(eval[1]))
            table.add_row("\t[blue]Matt. Coeff.", "[blue]{:.3f}".format(eval[2]))
            table.add_row("\t[medium_orchid]F1 Score", "[medium_orchid]{:.3f}".format(eval[3]))
            progress.log(table)
            progress.log("[orange3]Model Evaluation Complete")
            progress.update(task_id, advance=100. / (len(tr_list)), refresh=True)
        progress.log("[bold white]Script Complete")

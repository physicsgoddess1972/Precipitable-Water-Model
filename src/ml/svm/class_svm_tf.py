from csv import *
import numpy as np
import pandas as pd

# from seaborn import *
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()
tf.disable_resource_variables()
# tf.autograph.set_verbosity(1)
# tf.get_logger().setLevel('ERROR')

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

# import argparse

# parser = argparse.ArgumentParser(description="Classical Support Vector Machine Module")
# # parser.add_argument("-N", type=int, help="Number of random states\n\t[Default: 100]", default=100, dest="N")
# parser.add_argument("-dfile", type=str, help="file path of data", dest="dfile", default="../../../data/ml/")
# args = parser.parse_args()

class TFClassicalSVM():
    def preproccess(self, rand_ste, train_size):
        raw_data = []
        raw_label = []
        with open("../../../data/ml/ml_data.csv") as csvfile:
            next(reader(csvfile, delimiter=","), None)
            for row in reader(csvfile, delimiter=","):
                raw_data.append([float(row[1]),float(row[2])])
                if row[-1] == "overcast":
                    raw_label.append(1)
                else:
                    raw_label.append(3)

        data = np.array(raw_data)
        label = np.array(raw_label)
        label[label == 1] = -1
        label[label == 3] = 1
        label = np.ones(len(data)) * label
        X_train, X_test, y_train, y_test = train_test_split(data, label,
                                                            train_size=train_size,
                                                            random_state=rand_ste)
        return [X_train, X_test], [y_train, y_test]

    def process(self, batch_size):
        sess        = tf.Session(config=tf.ConfigProto(device_count = {'GPU': 0}))

        # Initialize placeholders
        x_data = tf.placeholder(shape=[None, 2], dtype=tf.float32)
        #x_data = tf.Variable(tf.ones(shape=(None, 2)))
        y_target = tf.placeholder(shape=[None, 1], dtype=tf.float32)
        #y_target = tf.Variable(tf.ones(shape=(None, 1)))
        # Create variables for svm
        A = tf.Variable(tf.random_normal(shape=[2,1]), name="A")
        b = tf.Variable(tf.random_normal(shape=[1,1]), name="b")
        model_output = tf.subtract(tf.matmul(x_data, A), b)

        # Compute SVM Model
        l2_norm = tf.reduce_sum(tf.square(A))
        alpha = tf.constant([1.0])
        classification_term = tf.reduce_mean(tf.maximum(0., tf.subtract(1.,tf.multiply(model_output, y_target))))
        loss = tf.add(classification_term, tf.multiply(alpha, l2_norm))


        prediction = tf.sign(model_output)
        accuracy = tf.reduce_mean(tf.cast(tf.equal(prediction, y_target), tf.float32))

        return [sess, loss, accuracy, model_output], [x_data, y_target], [A, b]

    def postprocess(self, epoch, model, other_model, coeff, tr_size):
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
        out = []
        data, labels = TFClassicalSVM().preproccess(1, tr_size)
        task_id1 = progress.add_task("download", filename="Epoch Evaluation ({})".format(tr_size))
        for i in range(epoch):
            x_vals = data[0]
            y_vals = np.transpose([labels[0]])

            rand_x = data[1]
            rand_y = np.transpose([labels[1]])

            sess.run(train_step, feed_dict={x_data: rand_x, y_target: rand_y})

            # output = sess.run(model_output, feed_dict={x_data: rand_x, y_target: rand_y})
            # out.append(output)

            temp_loss = sess.run(loss, feed_dict={x_data: rand_x, y_target: rand_y})
            loss_vec.append(temp_loss)

            train_acc_temp = sess.run(accuracy, feed_dict={x_data: x_vals, y_target: y_vals})
            train_accuracy.append(train_acc_temp)

            test_acc_temp = sess.run(accuracy, feed_dict={x_data: rand_x, y_target: rand_y})
            test_accuracy.append(test_acc_temp)

            progress.update(task_id1, advance=100./epoch,refresh=True)

        # Plot batch accuracy
        plt.figure(1)
        plt.plot(test_accuracy, 'k-')
        plt.plot(train_accuracy, 'r-')
        plt.title('Batch Accuracy')
        plt.xlabel('Generation')
        plt.ylabel('Accuracy')

        plt.tight_layout()
        plt.savefig("../../../figs/ml/{}/batch_acc_{}.pdf".format(int(tr_size*100), epoch))
        plt.close()

        # Plot loss over time
        plt.figure(2)
        plt.plot(loss_vec, 'k-')
        plt.title('Loss per Generation')
        plt.xlabel('Generation')
        plt.ylabel('Loss')

        plt.tight_layout()
        plt.savefig("../../../figs/ml/{}/loss_time_{}.pdf".format(int(tr_size*100), epoch))
        plt.close()

        # plt.figure(3)
        # plt.scatter(data[0][:,0], data[0][:,1], c=labels[0], cmap=plt.cm.coolwarm, marker="o", label="Training")
        # plt.scatter(data[1][:,0], data[1][:,1], c=labels[1], cmap=plt.cm.coolwarm, marker="x", label="Testing")
        # plt.title("Full Dataset Temperature vs TPW")
        # plt.xlabel(r"Temperature [$^o$C]")
        # plt.ylabel(r"TPW [mm]")
        #
        # handles, labels = plt.gca().get_legend_handles_labels()
        # by_label = dict(zip(labels, handles))
        # plt.legend(by_label.values(), by_label.keys())

        # print(out[0])
        # plt.show()

if __name__ == '__main__':
    with progress:
        progress.print(Panel("[bold deep_sky_blue2]Good Morning\nWelcome to the Classical SVM Analysis Module of the Precipitable Water Model. For more information about the model and the purpose of this tool, please visit the [link=https://git.io/fj5Xr]documentation page[/link]"))

        progress.log("[bold white]Script Started")
        task_id = progress.add_task("download", filename="Support Vector Machine Analysis")

        tr_list = [0.6, 0.7, 0.8]
        for i in tr_list:
            svm = TFClassicalSVM()
            model, placeholder, coeff = svm.process(202)
            result = svm.postprocess(1000, model, placeholder, coeff, i)
            progress.update(task_id, advance=100./(len(tr_list)),refresh=True)
        progress.log("[bold white]Script Complete")

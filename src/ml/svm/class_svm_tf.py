from csv import *
import numpy as np
import pandas as pd

# from seaborn import *
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()
#set_context(context='paper')
class TFClassicalSVM():
    def preproccess(self, rand_ste, train_size):
        raw_data = []
        raw_label = []
        with open("../../../data/ml/ml_data.csv") as csvfile:
            next(reader(csvfile, delimiter=","), None)
            for row in reader(csvfile, delimiter=","):
                raw_data.append([float(row[1]),float(row[2])])
                raw_label.append(int(row[-1]))
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
        sess        = tf.Session()

        # Initialize placeholders
        x_data = tf.placeholder(shape=[None, 2], dtype=tf.float32)
        y_target = tf.placeholder(shape=[None, 1], dtype=tf.float32)

        # Create variables for svm
        A = tf.Variable(tf.random_normal(shape=[2,1]))
        b = tf.Variable(tf.random_normal(shape=[1,1]))
        model_output = tf.subtract(tf.matmul(x_data, A), b)

        # Compute SVM Model
        l2_norm = tf.reduce_sum(tf.square(A))
        alpha = tf.constant([1.0])
        classification_term = tf.reduce_mean(tf.maximum(0., tf.subtract(1.,tf.multiply(model_output, y_target))))
        loss = tf.add(classification_term, tf.multiply(alpha, l2_norm))

        prediction = tf.sign(model_output)
        accuracy = tf.reduce_mean(tf.cast(tf.equal(prediction, y_target), tf.float32))

        return [sess, loss, accuracy, model_output], [x_data, y_target], [A, b]

    def postprocess(self, epoch, model, other_model, coeff):
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
        data, labels = TFClassicalSVM().preproccess(1, 0.7)
        for i in range(epoch):
            x_vals = data[0]
            y_vals = np.transpose([labels[0]])

            rand_x = data[1]
            rand_y = np.transpose([labels[1]])

            sess.run(train_step, feed_dict={x_data: rand_x, y_target: rand_y})

            # output = sess.run(model_output, feed_dict={x_data: x_vals, y_target: y_vals})
            # out.append(output)

            temp_loss = sess.run(loss, feed_dict={x_data: rand_x, y_target: rand_y})
            loss_vec.append(temp_loss)

            train_acc_temp = sess.run(accuracy, feed_dict={x_data: x_vals, y_target: y_vals})
            train_accuracy.append(train_acc_temp)

            test_acc_temp = sess.run(accuracy, feed_dict={x_data: rand_x, y_target: rand_y})
            test_accuracy.append(test_acc_temp)


        # Plot batch accuracy
        plt.figure(1)
        plt.plot(test_accuracy, 'k-')
        plt.plot(train_accuracy, 'r-')
        plt.title('Batch Accuracy')
        plt.xlabel('Generation')
        plt.ylabel('Accuracy')

        # Plot loss over time
        plt.figure(2)
        plt.plot(loss_vec, 'k-')
        plt.title('Loss per Generation')
        plt.xlabel('Generation')
        plt.ylabel('Loss')

        plt.figure(3)
        plt.scatter(data[0][:,0], data[0][:,1], c=labels[0], cmap=plt.cm.coolwarm, marker="o", label="Training")
        plt.scatter(data[1][:,0], data[1][:,1], c=labels[1], cmap=plt.cm.coolwarm, marker="x", label="Testing")
        plt.title("Full Dataset Temperature vs TPW")
        plt.xlabel(r"Temperature [$^o$C]")
        plt.ylabel(r"TPW [mm]")

        handles, labels = plt.gca().get_legend_handles_labels()
        by_label = dict(zip(labels, handles))
        plt.legend(by_label.values(), by_label.keys())

        plt.show()

if __name__ == '__main__':
    svm = TFClassicalSVM()

    fuck, other_fuck, coeff = svm.process(202)
    result = svm.postprocess(1, fuck, other_fuck, coeff)

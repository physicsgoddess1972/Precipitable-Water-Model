from csv import *
from numpy import *
import pandas as pd

from seaborn import *
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

import tensorflow.compat.v1 as tf
tf.disable_v2_behavior()

class TFClassicalSVM():
    def preproccess(self, rand_ste, train_size):
        raw_data = []
        raw_label = []
        with open("../../../data/ml/ml_data.csv") as csvfile:
            next(reader(csvfile, delimiter=","), None)
            for row in reader(csvfile, delimiter=","):
                raw_data.append([float(row[1]),float(row[2])])
                raw_label.append(int(row[-1]))
        data = array(raw_data)
        label = array(raw_label)
        label[label == 1] = -1
        label[label == 2] = 1
        label = ones(len(data)) * label
        X_train, X_test, y_train, y_test = train_test_split(data, label,
                                                            train_size=train_size,
                                                            random_state=rand_ste)
        return [X_train, X_test], [y_train, y_test]

    def train(self, epoch, data, labels):
        x_vals      = data[0][0]
        y_vals      = data[0][1]
        batch_size  = 250
        x_data              = tf.placeholder(shape=[None, 2], dtype=tf.float32)
        # y_data              = tf.placeholder(shape=[None, 1], dtype=tf.float32)
        y_target            = tf.placeholder(shape=[None, 1], dtype=tf.float32)
        prediction_grid     = tf.placeholder(shape=[None, 2], dtype=tf.float32)
        b                   = tf.Variable(tf.random_normal(shape=[1,batch_size]))

        kernel              = tf.tensordot(tf.transpose(x_data), y_target, [[1], [0]])
        print(kernel)
        model_output        = tf.tensordot(b, kernel, 1)
        first_term          = tf.reduce_sum(b)
        b_vec_cross         = tf.matmul(tf.transpose(b), b)
        y_target_cross      = tf.matmul(y_target, tf.transpose(y_target))

        second_term         = tf.reduce_sum(tf.multiply(my_kernel, tf.multiply(b_vec_cross,y_target_cross)))
        loss                = tf.negative(tf.subtract(first_term, second_term))

        rA                  = tf.reshape(tf.reduce_sum(tf.square(x_data), 1),[-1,1])
        rB                  = tf.reshape(tf.reduce_sum(tf.square(prediction_grid), 1),[-1,1])

        pred_sq_dist        = tf.add(tf.subtract(rA, tf.multiply(2., tf.matmul(x_data,tf.transpose(prediction_grid)))), tf.transpose(rB))
        pred_kernel         = tf.exp(tf.multiply(gamma, tf.abs(pred_sq_dist)))
        prediction_output   = tf.matmul(tf.multiply(tf.transpose(y_target),b),pred_kernel)

        prediction          = tf.sign(prediction_output-tf.reduce_mean(prediction_output))
        accuracy            = tf.reduce_mean(tf.cast(tf.equal(tf.squeeze(prediction),tf.squeeze(y_target)), tf.float32))

        my_opt              = tf.train.GradientDescentOptimizer(0.01)
        train_step          = my_opt.minimize(loss)
        sess.run(tf.initialize_all_variables())

        loss_vec            = []
        batch_accuracy      = []
        for i in range(epoch):
            rand_index  = random.choice(len(x_vals), size=batch_size)
            X           = x_vals[rand_index]
            Y           = np.transpose([y_vals[rand_index]])
            sess.run(train_step, feed_dict={x_data: X, y_target:Y})
            temp_loss   = sess.run(loss, feed_dict={x_data: X, y_target: Y})
            loss_vec.append(temp_loss)
            acc_temp    = sess.run(accuracy, feed_dict={x_data: X,y_target: Y,prediction_grid:X})
            batch_accuracy.append(acc_temp)

if __name__ == '__main__':
    svm = TFClassicalSVM()
    data, labels = svm.preproccess(1, 0.7)
    result = svm.train(1, data, labels)

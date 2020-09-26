from csv import *
from numpy import *
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
        sess        = tf.Session()
        x_vals = data[0]
        y_vals = labels[0]
        # for i in range(len(data[0])):
        #     x_vals.append(data[0][i][0])
        #     y_vals.append(data[0][i][1])
        # dimension = 2
        N = len(x_vals)
        # grid_step = 1  # default value was 0.02
        #
        # x_dummy = random.random((N, dimension))
        # y_dummy = random.choice(['fear', 'abc'], (N, 1))
        # matrix = hstack((x_dummy, y_dummy))
        #
        # ## SVM con Tensorflow
        # sess = tf.Session()
        # x_vals = array([x[0:dimension] for x in matrix])
        # y_vals = array([1 if y[dimension] == 'fear' else -1 for y in matrix])
        #
        # # Split the train data and testing data
        # train_indices = random.choice(len(x_vals), int(round(len(x_vals)*0.8)), replace=False)
        # test_indices = array(list(set(range(len(x_vals))) - set(train_indices)))
        # x_vals_train = x_vals[train_indices]
        # x_vals_test = x_vals[test_indices]
        # y_vals_train = y_vals[train_indices]
        # y_vals_test = y_vals[test_indices]
        #
        # class1_x = [x[0] for i, x in enumerate(x_vals_train) if y_vals_train[i] == 1]
        # class1_y = [x[1] for i, x in enumerate(x_vals_train) if y_vals_train[i] == 1]
        # class2_x = [x[0] for i, x in enumerate(x_vals_train) if y_vals_train[i] == -1]
        # class2_y = [x[1] for i, x in enumerate(x_vals_train) if y_vals_train[i] == -1]

        # Declare batch size
        batch_size = N

        # Initialize placeholders
        x_data          = tf.placeholder(shape=[2, N], dtype=tf.float32)
        y_target        = tf.placeholder(shape=[1, N], dtype=tf.float32)
        prediction_grid = tf.placeholder(shape=[2, N], dtype=tf.float32)
        # Create variables for svm
        b = tf.Variable(tf.random_normal(shape=[1, batch_size]))

        # Gaussian (RBF) kernel
        # gamma = tf.constant(-10.0)
        # sq_dists = tf.multiply(2., tf.matmul(x_data, tf.transpose(x_data)))
        # my_kernel = tf.exp(tf.multiply(gamma, tf.abs(sq_dists)))
        my_kernel = tf.matmul(tf.transpose(x_data),x_data)

        # Compute SVM Model
        first_term = tf.reduce_sum(b)
        b_vec_cross = tf.matmul(tf.transpose(b), b)
        y_target_cross = tf.matmul(y_target, tf.transpose(y_target))
        second_term = tf.reduce_sum(tf.multiply(tf.multiply(b_vec_cross, y_target_cross),my_kernel))
        loss = tf.subtract(first_term, second_term)

        pred_kernel = tf.matmul(tf.transpose(x_data), prediction_grid)

        prediction_output = tf.matmul(tf.multiply(tf.transpose(y_target), b), pred_kernel)
        prediction = tf.sign(prediction_output - tf.reduce_mean(prediction_output))
        accuracy = tf.reduce_mean(tf.cast(tf.equal(tf.squeeze(prediction), tf.squeeze(y_target)), tf.float32))

        # Declare optimizer
        my_opt = tf.train.GradientDescentOptimizer(0.01)
        train_step = my_opt.minimize(loss)

        # Initialize variables
        init = tf.global_variables_initializer()
        sess.run(init)

        # Training loop
        loss_vec = []
        batch_accuracy = []
        for i in range(epoch):
            rand_index = random.choice(len(x_vals), size=batch_size)
            rand_x = array(x_vals)[rand_index].reshape(2, N)
            rand_y = transpose([array(y_vals)[rand_index]])
            sess.run(train_step, feed_dict={x_data: rand_x, y_target: rand_y})

            temp_loss = sess.run(loss, feed_dict={x_data: rand_x, y_target: rand_y})
            loss_vec.append(temp_loss)

            acc_temp = sess.run(accuracy, feed_dict={x_data: rand_x,
                                                     y_target: rand_y,
                                                     prediction_grid: rand_x})
            batch_accuracy.append(acc_temp)

            if (i + 1) % 75 == 0:
                print('Step #' + str(i + 1))
                print('Loss = ' + str(temp_loss))

        # # Initialize placeholders
        # x_data          = tf.placeholder(shape=[2, None], dtype=tf.float32)
        # y_target        = tf.placeholder(shape=[None, 1], dtype=tf.float32)
        # prediction_grid = tf.placeholder(shape=[None, 2], dtype=tf.float32)
        #
        # # Create variables for svm
        # b = tf.Variable(tf.random_normal(shape=[1, batch_size]))
        #
        # # Kernel (Linear)
        #
        # # Compute SVM Model
        # first_term = tf.reduce_sum(b)
        # b_vec_cross = tf.matmul(tf.transpose(b), b)
        # y_target_cross = tf.matmul(y_target, tf.transpose(y_target))
        # second_term = tf.reduce_sum(tf.multiply(kernel, tf.multiply(b_vec_cross, y_target_cross)))
        # loss = tf.negative(tf.subtract(first_term, second_term))
        #
        # # Gaussian (RBF) prediction kernel
        # rA = tf.reshape(tf.reduce_sum(tf.square(x_data), 1), [-1, 1])
        # rB = tf.reshape(tf.reduce_sum(tf.square(prediction_grid), 1), [-1, 1])
        #
        # # pred_sq_dist = tf.add(tf.subtract(rA, tf.multiply(2., tf.matmul(x_data, tf.transpose(prediction_grid)))), tf.transpose(rB))
        # # pred_kernel = tf.exp(tf.multiply(gamma, tf.abs(pred_sq_dist)))
        # pred_kernel = tf.multiply(x_data, tf.transpose(prediction_grid))
        #
        # prediction_output = tf.matmul(tf.multiply(tf.transpose(y_target), b), pred_kernel)
        # prediction = tf.sign(prediction_output - tf.reduce_mean(prediction_output))
        # accuracy = tf.reduce_mean(tf.cast(tf.equal(tf.squeeze(prediction), tf.squeeze(y_target)), tf.float32))
        #
        # # Declare optimizer
        # my_opt = tf.train.GradientDescentOptimizer(0.01)
        # train_step = my_opt.minimize(loss)
        #
        # # Initialize variables
        # init = tf.global_variables_initializer()
        # sess.run(init)
        #
        # loss_vec            = []
        # batch_accuracy      = []
        # for i in range(epoch):
        #     rand_index  = random.choice(len(x_vals), size=batch_size)
        #     X           = array(x_vals)[rand_index]
        #     Y           = transpose([array(y_vals)[rand_index]])
        #     print(X.shape)
        #     print(x_data.shape)
        #     print(Y.shape)
        #     print(y_target.shape)
        #     sess.run(train_step, feed_dict={x_data: X, y_target: Y})
        #     print("Test_{}".format(epoch))
        #     temp_loss   = sess.run(loss, feed_dict={x_data: X, y_target: Y})
        #     loss_vec.append(temp_loss)
        #     acc_temp    = sess.run(accuracy, feed_dict={x_data: X,y_target: Y,prediction_grid:X})
        #     batch_accuracy.append(acc_temp)

if __name__ == '__main__':
    svm = TFClassicalSVM()
    data, labels = svm.preproccess(1, 0.7)
    result = svm.train(1, data, labels)

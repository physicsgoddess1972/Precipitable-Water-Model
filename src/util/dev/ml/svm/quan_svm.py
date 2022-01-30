from csv import *
from numpy import *
import pandas as pd

from seaborn import *
import matplotlib.pyplot as plt

from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix, classification_report

from qiskit import IBMQ, Aer
from qiskit.providers.aer import noise
from qiskit.aqua import QuantumInstance
from qiskit.aqua.algorithms import QSVM
from qiskit.circuit.library import ZZFeatureMap, ZFeatureMap, PauliFeatureMap
from qiskit.aqua.utils import split_dataset_to_data_and_labels, map_label_to_class_name

import logging
from qiskit.aqua import set_qiskit_aqua_logging
set_qiskit_aqua_logging(logging.DEBUG)


tkn = str(loadtxt("./ibmq.token", dtype=str, unpack=True))

q_mach = 'ibmq_santiago'
q_simu = 'ibmq_qasm_simulator'
q_stat = 'statevector_simulator'

# IBMQ.delete_account()
IBMQ.save_account(tkn, overwrite=True)
provider = IBMQ.load_account()
print("Credintials Loaded")
# thing = IBMQ.get_provider(hub='ibm-q')


device      = provider.get_backend(q_mach)
properties  = device.properties()
backend     = Aer.get_backend(q_stat)

noise_model     = noise.NoiseModel.from_backend(device)
basis_gates     = noise_model.basis_gates
coupling_map    = device.configuration().coupling_map

class quantum_svm:
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

    def run(self, data, label):
## Training-Testing Data Partition
        train_dataset   = {"clear_sky" : asarray(data[0][label[0]==1]),
                            "overcast" : asarray(data[0][label[0]==-1])}
        test_dataset    = {"clear_sky" : asarray(data[1][label[1]==1]),
                            "overcast" : asarray(data[1][label[1]==-1])}
## Quantum Shit
        feature_map = ZZFeatureMap(2, entanglement='linear')
        qu_instance = QuantumInstance(backend,
                                      shots=1,
                                      # noise_model=noise_model,
                                      # basis_gates=basis_gates,
                                      # coupling_map=coupling_map,
                                      # seed_transpiler=10598,
                                      # seed_simulator=10598,
                                      skip_qobj_validation=False)
## Runs the bitch
        #QSVM.BATCH_SIZE = 1000
        qsvm_model =  QSVM(feature_map,
                             training_dataset=train_dataset,
                             test_dataset=test_dataset)
        print([len(data[0]), len(data[1])])
        go       = qsvm_model.run(qu_instance)
        print(go)
        train    = qsvm_model.train(data[0],label[0], qu_instance)
        print(train)
        save     = qsvm_model.save_model("./")
        result   = qsvm_model.result(qu_instance)
        pred     = qsvm_model.predict(qu_instance)
        return result

    def analysis(self, data, label, result, rand_ste):
        ## Kernel Matrix for training and testing (Not a fucking clue what it means)
        plt.figure(1)
        fig, (ax1,ax2) = plt.subplots(1,2)
        kernel_matplot_train = ax1.imshow(asmatrix(result['kernel_matrix_training']),
                                            interpolation='nearest',
                                            origin='upper',
                                            cmap='plasma')
        ax1.set_title("Training Kernel Matrix")
        kernel_matplot_test = ax2.imshow(asmatrix(result['kernel_matrix_testing']),
                                            interpolation='nearest',
                                            origin='upper',
                                            cmap='plasma')
        ax2.set_title("Testing Kernel Matrix")
        plt.savefig('../../../figs/ml/qsvm/kernel_matrix_{}.png'.format(rand_ste))

        # plt.figure(2)
        # sv = result['svm']['support_vectors']
        #
        # x_min, x_max = data[1][:, 0].min() - 1, data[1][:, 0].max() + 1
        # y_min, y_max = 0, data[1][:, 1].max() + 1
        #
        # plt.scatter(data[1][:, 0], data[1][:, 1], c=label[1], cmap=plt.cm.coolwarm)
        # plt.scatter(data[0][:, 0], data[0][:, 1], c=label[0], cmap=plt.cm.coolwarm)
        # plt.plot(sv)
        # plt.plot(linspace(min(sv[0]), max(sv[0]), len(sv)), linspace(min(sv[1]), max(sv[1]), len(sv)), linestyle="--", color='k', label="Support Vectors")
        # plt.title("Testing Dataset Temperature vs TPW")
        # plt.xlabel(r"Temperature [$^o$C]")
        # plt.ylabel(r"TPW [mm]")
        # plt.savefig('test_space.png')
        #
        # plt.figure(2)
        # y_pred = result['predicted_classes']
        # con_mat = array(confusion_matrix(y_test, y_pred))
        # confusion = DataFrame(con_mat, index=['clear sky', 'overcast'],
        #                     columns=['predicted clear', 'predicted clouds'])
        # heatmap(confusion, annot=True, fmt='d', cmap='plasma')
        # plt.savefig("../../../figs/ml/qsvm/con_mat_{}.png".format(rand_ste))
if __name__ == '__main__':
    rand_ste = 1
    Q = quantum_svm()
    data, label = Q.preproccess(rand_ste, 0.7)
    result = Q.run(data, label)
    Q.analysis(data, label, result, rand_ste)
    print("The accuracy of the model is {:.2f}%".format(result['testing_accuracy'] * 100))

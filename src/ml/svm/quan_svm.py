from csv import *
from numpy import *
from qiskit import IBMQ
from qiskit import Aer
import matplotlib.pyplot as plt
from qiskit.aqua import QuantumInstance
from qiskit.aqua.algorithms import QSVM
from qiskit.aqua.utils import split_dataset_to_data_and_labels, map_label_to_class_name
from sklearn.model_selection import train_test_split
from qiskit.aqua.components.feature_maps import SecondOrderExpansion
from qiskit.providers.aer import noise
from sklearn.metrics import confusion_matrix, classification_report
from seaborn import *

import logging
from qiskit.aqua import set_qiskit_aqua_logging
set_qiskit_aqua_logging(logging.DEBUG)


tkn = str(loadtxt("./ibmq.token", dtype=str, unpack=True))

q_mach = 'ibmq_16_melbourne'
q_simu = 'ibmq_qasm_simulator'

# IBMQ.delete_account()
IBMQ.save_account(tkn)
provider = IBMQ.load_account()
print("Credintials Loaded")

device      = provider.get_backend(q_mach)
properties  = device.properties()

# provider = IBMQ.get_provider(hub='ibm-q')
#backend = IBMQ.get_provider(hub='ibm-q').get_backend(q_simu)
backend = Aer.get_backend('qasm_simulator')

## Noise model
gate_times = [
    ('u1', None, 0), ('u2', None, 100), ('u3', None, 200),
    ('cx', [1, 0], 678), ('cx', [1, 2], 547), ('cx', [2, 3], 721),
    ('cx', [4, 3], 733), ('cx', [4, 10], 721), ('cx', [5, 4], 800),
    ('cx', [5, 6], 800), ('cx', [5, 9], 895), ('cx', [6, 8], 895),
    ('cx', [7, 8], 640), ('cx', [9, 8], 895), ('cx', [9, 10], 800),
    ('cx', [11, 10], 721), ('cx', [11, 3], 634), ('cx', [12, 2], 773),
    ('cx', [13, 1], 2286), ('cx', [13, 12], 1504), ('cx', [], 800)
]

noise_model     = noise.device.basic_device_noise_model(properties, gate_lengths=gate_times)
basis_gates     = noise_model.basis_gates
coupling_map    = device.configuration().coupling_map

## Data Preproccessing
raw_data = []
raw_label = []
with open("../../../data/ml/ml_data.csv") as csvfile:
    reader = reader(csvfile, delimiter=",")
    next(reader, None)
    for row in reader:
        raw_data.append(list((float(row[1]),float(row[2]))))
        raw_label.append(int(row[-1]))
X = array(raw_data)
y = array(raw_label)
y[y == 1] = -1
y[y == 2] = 1
y = ones(len(X)) * y


class quantum_svm:
    def run(rand_ste):
## Trainging-Testing Data Partition
        X_train, X_test, y_train, y_test = train_test_split(X, y,
                                            train_size=0.7,
                                            random_state=rand_ste)
        train_dataset   = {"clear_sky" : X_train[y_train==1],
                            "overcast" : X_train[y_train==-1]}
        test_dataset    = {"clear_sky" : X_test[y_test==1],
                            "overcast" : X_test[y_test==-1]}
## Quantum Shit
        feature_map = SecondOrderExpansion(2, depth=3, entanglement='linear')
        qu_instance = QuantumInstance(backend,
                          circuit_caching=False,
                          shots=2,
                          noise_model=noise_model,
                          basis_gates=basis_gates,
                          coupling_map=coupling_map,
                          seed_transpiler=10598,
                          seed_simulator=10598,
                          skip_qobj_deepcopy=True,
                          skip_qobj_validation=False)
        ## Runs the bitch
        result =  QSVM(feature_map,
                        train_dataset,
                        test_dataset).run(qu_instance)
        return result, y_test
    def analysis(result, y_test, rand_ste):
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
    result, y_test = quantum_svm.run(rand_ste)
    quantum_svm.analysis(result, y_test, rand_ste)
    print("The accuracy of the model is {:.2f}%".format(result['testing_accuracy'] * 100))

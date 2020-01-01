import qiskit
from csv import *
from numpy import *
from qiskit import BasicAer
import matplotlib.pyplot as plt
from qiskit.aqua.algorithms import QSVM
from qiskit.aqua import QuantumInstance
from sklearn.model_selection import train_test_split
from qiskit.aqua.components.feature_maps import SecondOrderExpansion

backend = BasicAer.get_backend('qasm_simulator')

## Data Preproccessing
raw_data = []
raw_label = []
with open("./ml_data.csv") as csvfile:
    reader = reader(csvfile, delimiter=",")
    next(reader, None)
    for row in reader:
        raw_data.append(list((float(row[1]),float(row[2]),float(row[3]))))
        raw_label.append(int(row[-1]))
X = array(raw_data)
y = array(raw_label)
y[y == 1] = -1
y[y == 2] = 1
y = ones(len(X)) * y

## Trainging-Testing Data Partition
X_train, X_test, y_train, y_test = train_test_split(X, y, train_size=0.8, random_state=0)
train_dataset   = {"clear_sky" : X_train[y_train==1],
                    "overcast" : X_train[y_train==-1]}
test_dataset    = {"clear_sky" : X_test[y_test==1],
                    "overcast" : X_test[y_test==-1]}
## Quantum Shit
feature_map = SecondOrderExpansion(3, depth=2, entanglement='linear')
qsvm = QSVM(feature_map, train_dataset, test_dataset)
quantum_instance = QuantumInstance(backend, shots=1024)

## Runs the bitch
result = qsvm.run(quantum_instance)

## Kernel Matrix for training and testing (Not a fucking clue what it means)
fig, (ax1,ax2) = plt.subplots(1,2)
kernel_matplot_train = ax1.imshow(asmatrix(result['kernel_matrix_training']),
                                    interpolation='nearest',
                                    origin='upper',
                                    cmap='plasma')
plt.title("Training Kernel Matrix")
kernel_matplot_test = ax2.imshow(asmatrix(result['kernel_matrix_testing']),
                                    interpolation='nearest',
                                    origin='upper',
                                    cmap='plasma')
plt.title("Testing Kernel Matrix")
plt.savefig('kernel_matrix.png')

## Prints Shit
print(y_test)
print(result['svm']['yin'])

## Prints accuracy of model
print("The accuracy of the model is {:.2f}%".format(result['testing_accuracy'] * 100))

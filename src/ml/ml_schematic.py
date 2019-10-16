import h5py
from ann_visualizer.visualize import ann_viz;
from keras.models import model_from_json
import numpy

numpy.random.seed(7)

json_file = open("model.json", 'r')
loaded_model_json = json_file.read()
json_file.close()

model = model_from_json(loaded_model_json)

model.load_weights("model.h5")

ann_viz(model, title="Fuck this shit")


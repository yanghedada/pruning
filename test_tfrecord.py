import os
import tensorflow as tf
from PIL import Image
import matplotlib.pyplot as plt
import numpy as np

cwd = '/home/modric/Downloads/test/'
classes = {'fsk','qpsk'}
writer = tf.python_io.TFRecordWriter("train.tfrecords")

for index, name in enumerate(classes):
    class_path = cwd + name + '/'
    for img_name in os.listdir(class_path):
        img_path = class_path + img_name

        img = Image.open(img_path)
        img = img.resize((128,128))
        img_raw = img.tobytes()
        example = tf.train.Example(features = tf.train.Features(feature={
            "label":tf.train.Feature(int64_list = tf.train.Int64List(value=[index])),
            'img_raw':tf.train.Feature(bytes_list = tf.train.BytesList(value=[img_raw]))
        }))
        writer.write(example.SerializeToString())

writer.close()

def read_and_decode(filename):#read .tfrecords
    filename_queue = tf.train.string_input_producer([filename])#create a queue

    reader = tf.TFRecordeReader()
    _, serialized_example = reader.read(filename_queue)#return filename and file
    features = tf.parse_single_example(serialized_example, features = {
                                                               'label':tf.FixedLenFeature([], tf.int64),
                                                               'img_raw':tf.FixedLenFeature([], tf.string),
                                                           })#take out image and label
    img = tf.decode_raw(features['img_raw']. tf.uint8)
    img = tf.reshape(img, [128, 128, 3])#reshape to 128,128,3
    img = tf.cast(img, tf.float32)*(1./255) - 0.5#throw out the img tensor
    label = tf.cast(features['label'], tf.int32)
    return img, label

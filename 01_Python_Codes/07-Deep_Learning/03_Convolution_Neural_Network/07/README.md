Instructions
--------------
Build a CNN model to classify images of handwritten Devanagari characters.
 
Dataset is an image database of Handwritten Devanagari characters.
There are 46 classes of characters with 2000 examples each.
The dataset is split into training set(85%) and testing set(15%).

The dataset is available at:

https://archive.ics.uci.edu/ml/datasets/Devanagari+Handwritten+Character+Dataset

The data is read into jupyter notebook using ImageDataGenerator from keres.preprocessing.image

A CNN is built to classifiy different hand-written characters in Devanagari script

The structure of the CNN model is as follows:
Conv2D->MaxPooling2D->Conv2D->MaxPooling2D->Flatten->Hidden Layer(512 neurons)->Output layer

Conv2D is a convolutional layer which extracts features like curved edges, straight edges etc.
The output of Conv2D is a feature map, where each feature map extracts a particular feature in the image.

MaxPooling2D layer takes care of spatial invariance and overfitting.

Dropout is used to disable proportion of neurons in a layer for each epoch to avoid overfitting.

softmax activation function is used on output layer since this is multi-class classification.

I have plotted a graph for accuracy after each epoch on training and validation data.

Graph shows there is no problem of overfitting.

The accuracy on test data is 98.37%


 


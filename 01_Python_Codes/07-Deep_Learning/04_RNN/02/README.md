Classify reviews as positive or negative review after preprocessing using Tokenizer from Keras.

Dataset is imdb reviews with no preprocessing done.

A RNN is built using GRU cells.

Structure of model:

Embedding(8)->GRU(16)->GRU(8)->GRU(4)-Dense(1,sigmoid)
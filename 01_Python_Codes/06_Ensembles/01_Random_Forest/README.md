Instructions
--------------------------

Random Forest

Apply random forest algorithm on a cellular network provider's dataset and 
predict if the person will churn or not.

First, a bagging model is built to see if it can do well.
When no purning, overfitting took place.
When purned, recall has gone down.

So, a random forest model is built after parameter tuning using GridSearchCV.
12 columns are selected after checking importance score for each column and 
a final random forest model is built.


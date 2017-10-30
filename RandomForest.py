import csv
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix

def split_dataset(dataset, train_percentage, feature_headers, target_header):
    """
    Split the dataset with train_percentage
    :param dataset:
    :param train_percentage:
    :param feature_headers:
    :param target_header:
    :return: train_x, test_x, train_y, test_y
    """

    # Split dataset into train and test dataset
    train_x, test_x, train_y, test_y = train_test_split(dataset[feature_headers], dataset[target_header],
                                                        train_size=train_percentage)
    return train_x, test_x, train_y, test_y

def random_forest_classifier(features, target):
	clf = RandomForestClassifier()
	clf.fit(features,target)
	return clf

def main():
	dataset = pd.read_csv('WaterPump_accumulation_level_copy.csv')
	column_list = (dataset.columns.values)
	train_x,test_x,train_y,test_y = split_dataset(dataset,0.7,column_list[2:10],column_list[-1])

	trained_model = random_forest_classifier(train_x,train_y)
	prediction = trained_model.predict(test_x)

	for i in range(0,10):
		print ("Actual outcome :: {} and Predict outcome :: {}".format(list(test_y)[i],prediction[i]))

	print ("Train Accuracy :: ",accuracy_score(train_y,trained_model.predict(train_x)))
	print ("Test Accuracy :: ", accuracy_score(test_y,prediction))

if __name__ == '__main__':
	main()



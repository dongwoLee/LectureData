import csv
import pandas as pd

data = pd.read_csv('wholeData.csv')
data = data.values.tolist()

with open('WaterPump_accumulation.csv','a',newline='') as myfile:
    for i in range(len(data)):
        if(data[i][1]=='YangPyung/WaterPump'):
            writer = csv.writer(myfile,delimiter=',')
            writer.writerow(data[i])











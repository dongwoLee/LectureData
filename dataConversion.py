import pandas as pd
import csv

data = pd.read_csv('WaterPump_accumulation.csv')
data = data.values.tolist()

with open('WaterPump_accumulation_level.csv','a',newline='') as myfile:
    writer = csv.writer(myfile,delimiter=',')
    for i in range(len(data)):
        if(0<=data[i][-1] and data[i][-1]<=20):
            data[i].append('1')
        elif(21<=data[i][-1] and data[i][-1]<=40):
            data[i].append('2')
        elif(41<=data[i][-1] and data[i][-1]<=80):
            data[i].append('3')
        elif(81<= data[i][-1] and data[i][-1]<=160):
            data[i].append('4')
        elif(161<= data[i][-1] and data[i][-1]<=320):
            data[i].append('5')
        elif(321<= data[i][-1] and data[i][-1] <= 640):
            data[i].append('6')
        elif(641<= data[i][-1] and data[i][-1]<=1280):
            data[i].append('7')
        elif(data[i][-1]>= 1281):
            data[i].append('8')

        writer.writerow(data[i])













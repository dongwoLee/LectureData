import pandas as pd
import csv

fields = ['Location','Date','Hum','Raf','Tmx','Tav','Tmi','Mosq','level']

data = pd.read_csv('integration.csv')
mosCatch = data['Mosq']

rowData = data.values.tolist()

level = []

for i in range(len(mosCatch)):
    if(0<=mosCatch[i] and mosCatch[i]<=20):
        level.append('1')
    elif(21<=mosCatch[i] and mosCatch[i]<=40):
        level.append('2')
    elif(41<=mosCatch[i] and mosCatch[i]<=80):
        level.append('3')
    elif(81<= mosCatch[i] and mosCatch[i]<=160):
        level.append('4')
    elif(161<= mosCatch[i] and mosCatch[i]<=320):
        level.append('5')
    elif(321<= mosCatch[i] and mosCatch[i] <= 640):
        level.append('6')
    elif(641<= mosCatch[i] and mosCatch[i]<=1280):
        level.append('7')
    elif(mosCatch[i]>= 1281):
        level.append('8')

for j in range(len(rowData)):
    rowData[j].append(level[j])

with open('integration_level.csv','w',newline='') as myfile:
    wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    wr.writerow(fields)
    for k in range(len(rowData)):
        wr.writerow(rowData[k])
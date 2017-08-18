import pandas as pd
import csv

c = pd.read_csv('Yang_WaterPump_2012~2014.csv')
f = pd.read_csv('factor_data_2012~2014.csv')

mosquito_list = c.values.tolist()
factor_list = f.values.tolist()

fields = ['Location','Date','Hum','Raf','Tmx','Tav','Tmi','Mosq']

with open('integration.csv','w',newline='') as myfile:
    wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    wr.writerow(fields)
    for i in range(len(mosquito_list)):
        for j in range(len(factor_list)):
            if(mosquito_list[i][0]==factor_list[j][1]):
                (factor_list[j].append(mosquito_list[i][1]))
                wr.writerow(factor_list[j])






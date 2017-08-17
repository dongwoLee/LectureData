import csv
import pandas as pd
import datetime

startDate=datetime.date(2012,5,7)
endDate = datetime.date(2012,10,31)

dateList = []
dateRange=pd.date_range(startDate,endDate)

for single_date in dateRange:
    dateList.append((single_date.strftime("%Y.%m.%d")))

data = pd.read_csv('Yang_WaterPump.csv')
list = (data.values.tolist())

a=[]

for i in range(len(list)):
    a.append(list[i][0])

b =set(dateList)-set(a)
print (b)











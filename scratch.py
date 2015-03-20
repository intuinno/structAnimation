__author__ = 'intuinno'



import scipy.stats
import numpy as np
import pandas as pd


# SA = {}
# SA[1] = {'count':[1,1,0], 'label':['Velocity','Position', 'Other']}
# SA[2] = {'count':[29, 20, 1], 'label':['Color','Velocity','Other']}
# SA[3] = {'count':[22, 28, 0], 'label':['Shape','Velocity','Other']}
# SA[4] = {'count':[20,29, 1], 'label':['Size','Velocity','Other']}
# SA[5] = {'count':[21,21,8], 'label':['Position','Acc','Other']}
# SA[6] = {'count':[14,32,4], 'label':['Position','Color','Other']}
# SA[7] = {'count':[28,16,6], 'label':['Shape','Position','Other']}
# SA[8] = {'count':[15,26,9], 'label':['Velocity','Acc','Other']}
# SA[9] = {'count':[30,16,4], 'label':['Size','Position','Other']}
# SA[10] = {'count':[1,1,0], 'label':['Shape','Color','Other']}
# SA[11] = {'count':[20,23,7], 'label':['Shape','Size','Other']}
# SA[12] = {'count':[1,1,0], 'label':['Size','Acc','Other']}
# SA[13] = {'count':[31,15,4], 'label':['Color','Size','Other']}
# SA[14] = {'count':[28,19,3], 'label':['Color','Acc','Other']}
# SA[15] = {'count':[24,20,6], 'label':['Shape','Acc','Other']}
#
labelOrder = {'SS':0,'SL':1,'SP':2,'DS':3,'DL':4,'DP':5}
labels = ['SS','SL','SP','DS','DL','DP']

# Counts is a n-by-n matrix where
#  Counts[i,j] = # people who prefer option i over option j
# N is total number of experiment for each comparision


originalData  = pd.read_csv('logs.csv',sep=';')

result = originalData.groupby('tasktype').sum()

Counts = np.zeros([6,6])

for i in range(6):
    for j in range(6):
        if labels[i]+ labels[j] in result.index:
            Counts[i,j] = result.loc[labels[i]+labels[j]][labels[i]]
        elif labels[j]+ labels[i] in result.index:
            Counts[i,j] = result.loc[labels[j]+labels[i]][labels[i]]
        else:
            Counts[i,j] = 1

N = Counts + Counts.T

P = Counts / (N + (N==0))

P[np.eye(6)>0] = 0.5

Z = scipy.stats.norm.ppf(P)

S = (-1) * np.mean(Z, axis=0)

print S

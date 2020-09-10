
import numpy as np                                                                 
import pandas as pd                                                                
import pylab as P                                                                  
                                                                                   
train = pd.read_csv("../input/train.csv", )                                               
test = pd.read_csv("../input/test.csv", dtype={"Age": np.float64, "Fare": np.float64}) 
                                                                                   
print(train[train['Age'] > 60][train['Survived'] == 1][['Sex', 'Pclass', 'Age']])
train['Age'].hist()                                                                
P.show()   

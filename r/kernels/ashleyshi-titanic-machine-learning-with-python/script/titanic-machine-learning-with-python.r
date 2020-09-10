
import pandas as pd

t = pd.read_csv('../input/train.csv')
t.describe()

t.columns

#cleaning Age, fill nas with the median of the age
t['Age']=t['Age'].fillna(t['Age'].median())
t.describe()

# Transfer Sex, Embarked to numerical variable
t.loc[t['Sex']=='male','Sex']=0
t.loc[t['Sex']=='female','Sex']=1
t.loc[t['Embarked']=='S','Embarked']=0
t.loc[t['Embarked']=='C','Embarked']=1
t.loc[t['Embarked']=='Q','Embarked']=2

# play with Cabin
t['Cabin']=t['Cabin'].dropna()

#data exploration
import matplotlib.pyplot as plt
%matplotlib inline

t['relatives']=t['SibSp']+t['Parch']
plt.scatter(t['relatives'],t['Survived'])


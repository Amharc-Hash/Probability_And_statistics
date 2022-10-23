from statistics import LinearRegression
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

import sklearn
import matplotlib.pyplot as plt
import seaborn as sb

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC, LinearSVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import Perceptron
from sklearn.linear_model import SGDClassifier
from sklearn.tree import DecisionTreeClassifier

import warnings
warnings.filterwarnings("ignore")
# E:\2D\Final\Prob_Stat\Analysis_seaborn
df=pd.read_csv("E:/2D/Final/Prob_Stat/Analysis_seaborn/water_potability.csv")
df.head()
df.shape
df.nunique()
df.dtypes
df.describe(include = "all")
df.isnull().sum()
x = df[(df['Potability']==0) & (df['Hardness']<=150)][['ph']].mean()
y = df[(df['Potability']==0) & (df['Hardness']>150)][['ph']].mean()
z = df[(df['Potability']==1) & (df['Hardness']<=150)][['ph']].mean()
o = df[(df['Potability']==1) & (df['Hardness']>150)][['ph']].mean()
for i in range (0,len(df)):
    if (pd.isnull(df['ph'][i]) == True):
        if ((df['Potability'][i]==0) & (df['Hardness'][i]<=150)):
            df['ph'][i] = x
        elif ((df['Potability'][i]==0) & (df['Hardness'][i]>150)):
            df['ph'][i] = y
        elif ((df['Potability'][i]==1) & (df['Hardness'][i]<=150)):
             df['ph'][i] = z
        else:
             df['ph'][i] = o

x = df[(df['Potability']==0)][['Sulfate']].mean()
y = df[(df['Potability']==1)][['Sulfate']].mean()
for i in range (0,len(df)):
    if (pd.isnull(df['Sulfate'][i]) == True):
        if (df['Potability'][i]==0):
            df['Sulfate'][i] = x
        else:
             df['Sulfate'][i] = y

df['Trihalomethanes'].fillna(value = df['Trihalomethanes'].mean() , inplace = True)

df['ph'] = df['ph'].round(decimals = 1)
df["ph"].head()

df["Type of Water"] = ""
for i in range(0,len(df)):
    if (df["ph"][i] > 9):
        df["Type of Water"][i] = "Alkaline water"
    elif (df["ph"][i] <= 9 and df["ph"][i] > 8):
        df["Type of Water"][i] = "Bottled waters labeled as alkaline"
    elif (df["ph"][i] <= 8 and df["ph"][i] > 7.5 ):
        df["Type of Water"][i] = "Ocean water"   
    elif(df["ph"][i] == 7.5 ):
        df["Type of Water"][i] = "Tap water"
    elif(df["ph"][i] < 7.5 and df["ph"][i] >=6.5):
        df["Type of Water"][i] = "Common bottled waters"
    elif(df["ph"][i] < 6.5 and df["ph"][i] >=5.5):
        df["Type of Water"][i] = "Distilled reverse osmosis water"
    else:
        df["Type of Water"][i] = "Acidic water"

df["Type of Hardness"] = ""
for i in range(0,len(df)):
    if (df["Hardness"][i] >=0  and df["Hardness"][i] < 17.1):
        df["Type of Hardness"][i] = "Soft"
    elif (df["Hardness"][i] >= 17.1 and df["Hardness"][i] < 60):
        df["Type of Hardness"][i] = "Slightly hard"
    elif (df["Hardness"][i] >= 60 and df["Hardness"][i] < 120 ):
        df["Type of Hardness"][i] = "Moderately hard"   
    elif(df["Hardness"][i] >= 120 and df["Hardness"][i] < 180):
        df["Type of Hardness"][i] = "Hard"
    else:
         df["Type of Hardness"][i] = "Very Hard"

df.head()
colors = sb.color_palette('twilight')[0:6]
sb.palplot(colors)
labels = ['Non-Potable', 'Potable']
data = [df['Potability'].value_counts()[0],
         df['Potability'].value_counts()[1]
        ]
fig1, ax1 = plt.subplots(figsize=[15,6])
ax1.pie(data, labels=labels,explode=[0.05]*2, autopct='%1.1f%%',pctdistance=0.5, shadow=True, colors = colors)
plt.title("Water Potability", fontsize=20);


fig1, ax = plt.subplots(figsize=[20,10])
ax = sb.boxplot(data=df, orient="h",palette = colors)
sb.despine(offset=10, trim=True)
plt.title("Box Plot of each Column", fontsize=20);


df1 = pd.DataFrame()
df1 = df
df1 = df1.drop("Solids",1)
fig1, ax = plt.subplots(figsize=[20,10])
ax = sb.boxenplot(data=df1, orient="h", palette=colors)
sb.despine(offset=10, trim=True)
plt.title("Boxen Plot of each Column except Solids", fontsize=20);

fig1, ax = plt.subplots(figsize=[20,10])
ax = sb.boxplot(data=df1, orient="h", palette=colors)
sb.despine(offset=10, trim=True)
plt.title("Box Plot of each Column except Solids", fontsize=20);

ax = sb.pairplot(df, hue="Potability",diag_kind="kde",kind="scatter",palette="twilight")

g = sb.clustermap(df.corr(), center=0, cmap="twilight",
                   dendrogram_ratio=(.2, .2),
                   cbar_pos=(.01, .32, .03, .2), figsize=(15, 15))
plt.title("Cluster Map", fontsize=20);

g = sb.displot(
    data=df, y="Hardness", hue="Potability", col="Type of Hardness", palette = "twilight",col_wrap=2,
    kind="kde", height=4, aspect=2,rug=True)

g = sb.displot(
    data=df, y="ph", hue="Potability", col="Type of Water", palette = "twilight", col_wrap=3,
    kind="ecdf", height=4, aspect=1.2,rug=True)

sb.displot(data=df, x="Solids", y="Type of Water", col="Potability",cmap = "twilight",
    log_scale=(True, False), col_wrap=4, height=6, aspect=.95)

g = sb.jointplot(x="Solids", y="Conductivity", data=df, cmap = "twilight",
    kind="kde",palette =colors, height =8, aspect=0.95)

g = sb.jointplot(x="Organic_carbon", y="Sulfate", data=df,
                  kind="hex",cmap ="twilight", height =8)

g = sb.JointGrid(data=df, x="Turbidity", y="Trihalomethanes", space=0, ratio=17)
g.plot_joint(sb.scatterplot, size=df["Potability"], sizes=(3, 5),
             color="brown", alpha=.6, legend=False)
g.plot_marginals(sb.rugplot, height=15, alpha=1,color="Black")

corr = df.corr()
mask = np.triu(np.ones_like(corr, dtype=bool))
f, ax = plt.subplots(figsize=(11, 9))

sb.heatmap(corr, mask=mask, cmap="twilight", vmax=.3, center=0,
            square=True, linewidths=.5, cbar_kws={"shrink": .5}, annot =True)

data = pd.get_dummies(df, columns = ['Type of Water', 'Type of Hardness'])
data.head()
X = data.drop(['Potability'], axis = 1)
Y = data['Potability']
X_train, X_test, y_train, y_test = train_test_split(X,Y,test_size=0.75, random_state=83)
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.fit_transform(X_test)
logreg = LogisticRegression()
logreg.fit(X_train, y_train)

acc_log = round(logreg.score(X_train, y_train) * 100, 2)
acc_log


plt.show()
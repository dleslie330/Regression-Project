import pandas as pd
import seaborn as sb
import matplotlib.pyplot as plt

data = pd.read_csv('Real estate.csv',sep=",",header=0,usecols=range(1,8),dtype=float)
sb.pairplot(data)

plt.savefig('pairwise plot.png')

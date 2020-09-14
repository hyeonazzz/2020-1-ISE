import numpy as np
import pandas as pd 
import statsmodels.api as sm; 

df = pd.read_csv('C:/Users/Minji/Desktop/0701P.csv')

df['intercept']=1
lm=sm.OLS(df['RS40009'], df[['intercept', 'Alarm']])
result=lm.fit()
print(result.summary()) 
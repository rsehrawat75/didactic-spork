#!/usr/bin/env python
# coding: utf-8

# In[54]:


import pandas as pd
import numpy as np


# In[55]:


df=pd.read_csv('default of credit card clients.csv',index_col='ID')


# In[56]:


df.tail()


# In[57]:


df.shape


# In[58]:


df.info()


# In[59]:


df.describe()


# In[60]:


char_var=['SEX','EDUCATION','MARRIAGE','PAY_1','PAY_2','PAY_3','PAY_4','PAY_5','PAY_6']
for x in char_var:
    print(df[x].value_counts(),"\n")


# In[61]:


from sklearn.model_selection import train_test_split


# In[62]:


X=df.iloc[:,:23]
X


# In[63]:


Y=df.loc[:,'dpnm']
Y


# In[64]:


X_train, X_test, y_train, y_test = train_test_split(X, Y,test_size = 0.3, random_state=2)


# In[65]:


from sklearn.preprocessing import MinMaxScaler


# In[66]:


m = MinMaxScaler()


# In[67]:


m.fit_transform(X_train)


# In[68]:


m.transform(X_test)


# In[69]:


X_train.shape


# In[70]:


X_train = pd.DataFrame(m.transform(X_train), columns=X_train.columns)
X_test = pd.DataFrame(m.transform(X_test), columns=X_test.columns)


# In[71]:


X_train


# In[79]:


y_test.value_counts()


# In[81]:


7006/(7006+1994)


# In[73]:


from sklearn.linear_model import LogisticRegression
lore = LogisticRegression(random_state=2, n_jobs=-1,class_weight={0:16358,1:4642}) #n_jobs = -1 will do the parallel computations using all the cores in the processor
lore.fit(X_train, y_train)


# In[74]:


lore.score(X_test,y_test)


# In[75]:


y_class = lore.predict(X_test)
y_class


# In[76]:


y_class = pd.Series(y_class, index = y_test.index)


# In[77]:


ds = pd.DataFrame({"Actual": y_test, "Predicted": y_class})


# In[84]:


table = pd.DataFrame(pd.crosstab(ds.Actual, ds.Predicted))
table


# In[ ]:





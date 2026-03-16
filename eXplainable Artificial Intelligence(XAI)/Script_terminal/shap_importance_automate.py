#!/usr/bin/env python
# coding: utf-8

# # Chargement des dataset Input_Space et Output_Space

# In[1]:


import pandas as pd
input_space = pd.read_excel('Input_Space.xlsx',engine='openpyxl')
output_space = pd.read_excel('Output_Space.xlsx',engine='openpyxl')
########  Filtrer les données avec la Variance   
import pandas as pd
from sklearn.feature_selection import VarianceThreshold
vt = VarianceThreshold() # Threshold default is 0
_ = vt.fit(input_space)
var_vector_mask = vt.get_support()
    #var_vector_mask 
input_space = input_space.loc[:, var_vector_mask]


# # fonction permettant de multiplier les colonnes entre elles deux a deux 

# In[2]:


#####  fonction permettant de multiplier les colonnes entre elles deux a deux
def ft_combinatorial(input_space):
    from itertools import combinations
    #df=input_space
    cc = list(combinations(input_space.columns,2))
    input_space= pd.concat([input_space[c[1]]*input_space[c[0]] for c in cc], axis=1, keys=cc)
    input_space.columns = input_space.columns.map('*'.join)
    return input_space   


# In[3]:


ft=ft_combinatorial(input_space)
input_space=pd.concat([input_space,ft],axis=1,join='inner')
col_names=input_space.columns


# In[4]:


#####  Creation des fakes variables 
import numpy as np
Vars=input_space.columns
input_space[[v + "_fake" for v in Vars]]=input_space.iloc[np.random.permutation(input_space.index)].reset_index(drop=True)


# In[5]:


###### Application de la standardisation avec “StandardScaler” dans le package #preprocessing.
import pandas as pd    
from sklearn.preprocessing import StandardScaler
ss = StandardScaler()
output_space = pd.DataFrame(ss.fit_transform(output_space),columns =output_space.columns)
input_space = pd.DataFrame(ss.fit_transform(input_space),columns =input_space.columns)


# # Fonction permettant de comparer les variables avec leur fakes

# In[6]:


###### Fonction permettant de comparer les variables avec leur fakes
def ft_from_fake(ft_input,var_names):
    ft=ft_input
    for i in var_names: 
        #print(ft.loc[i,'feature_importance_vals']
        if   ft.loc[i,col] <= ft.loc[i+"_fake",col]:
            ft.loc[i,col]=0
            ft.drop(ft.tail(len(col_names)).index,inplace = True)
            return ft


# In[7]:


###############  Random Forest model and SHAP for Cost prediction
     ##### Random Forest model
from sklearn.model_selection import cross_validate
from sklearn.model_selection import cross_val_score
from sklearn.metrics import make_scorer
from sklearn.metrics import mean_squared_error
from sklearn.ensemble import RandomForestRegressor
import shap
import numpy as np
import matplotlib.pyplot as plt
feature_importance=pd.DataFrame(np.nan, index=input_space.columns, columns=output_space.columns)
for  col in list(output_space.columns.values):
    Output= output_space.to_dict('series')
    #print(Output[col])
    X = input_space
    model = RandomForestRegressor(n_estimators =100, random_state =0)
    Score_col=cross_val_score(model, X ,Output[col], cv=10, scoring=make_scorer(mean_squared_error))
    model_col=model.fit(X, Output[col])  
    ##### SHAP explaination
    shap_values_col= shap.TreeExplainer(model_col).shap_values(X)
    vals_col= np.abs(shap_values_col.mean(0))
    feature_importance[col]=vals_col
    
########################### SHAP VALUES IMPORTANCE DATAFRAME
#feature_importance.set_index(input_space.columns, inplace = True)
ft_from_fake(feature_importance,col_names)
feature_importance = 1.15*feature_importance
feature_importance[feature_importance > 1] = 1
feature_importance


# In[8]:


print(feature_importance.max()) 


# je remarque que les shap values sont plus petites si je compare aux autres notebook.

# # Summary plot  xlim=[0,1]

# In[10]:


import pandas as pd 
import matplotlib.pyplot as plt 
fig = plt.figure(figsize = (9, 9))
for i, col in enumerate(feature_importance.columns):
    ax = fig.add_subplot(3, 3, i+1)
    feature_importance[col].plot(kind='barh')
    plt.xticks(fontsize=7,rotation=45)
    plt.yticks(fontsize=6,color ='0.2')
   # plt.xlim([0, 1])
    plt.title(col+ ":FI SHAP",fontsize=8)
plt.tight_layout(pad=1,h_pad=1.4)   
plt.show()


# # Summary plot avec xlim=[0,0.05]

# In[10]:


import pandas as pd 
import matplotlib.pyplot as plt 
fig = plt.figure(figsize = (9, 9))
for i, col in enumerate(feature_importance.columns):
    ax = fig.add_subplot(3, 3, i+1)
    feature_importance[col].plot(kind='barh')
    plt.xticks(fontsize=7,rotation=45)
    plt.yticks(fontsize=6,color ='0.2')
    plt.xlim([0, 0.05])
    plt.title(col+ ":FI SHAP",fontsize=8)
plt.tight_layout(pad=1,h_pad=1.4)   
plt.show()


# In[11]:


import pandas as pd 
import matplotlib.pyplot as plt 
import seaborn as sns
fig = plt.figure(figsize = (9, 9))
for i, col in enumerate(feature_importance.columns):
    ax = fig.add_subplot(3, 3, i+1)
    sns.barplot(x=feature_importance[col], y=feature_importance.index)
    plt.xticks(fontsize=7,rotation=45)
    plt.yticks(fontsize=6,color ='0.2')
    plt.xlim([0,0.05])
    plt.title(col+ ":FI SHAP",fontsize=8)
plt.tight_layout(pad=1,h_pad=1.4)
plt.show()


# # Summary plot variable ranking

# In[12]:


import pandas as pd 
import matplotlib.pyplot as plt 
fig = plt.figure(figsize = (9, 9))
for i, col in enumerate(feature_importance.columns):
    ax = fig.add_subplot(3, 3, i+1)
    feature_importance[col].sort_values().plot(kind='barh')
    plt.xticks(fontsize=7,rotation=45)
    plt.yticks(fontsize=6,color ='0.2')
    plt.xlim([0, 0.05])
    plt.title(col+ ":FI SHAP",fontsize=8)
plt.tight_layout(pad=1,h_pad=1.4)   
plt.show()


# In[13]:


import pandas as pd 
import matplotlib.pyplot as plt 
import seaborn as sns
fig = plt.figure(figsize = (9, 9))
for i, col in enumerate(feature_importance.columns):
    ax = fig.add_subplot(3, 3, i+1)
    sns.barplot(x=feature_importance[col], y=feature_importance.index, order=feature_importance[col].sort_values(ascending = False).index)
    plt.xticks(fontsize=7,rotation=45)
    plt.yticks(fontsize=6,color ='0.2')
    plt.xlim([0, 0.05])
    plt.title(col+ ":FI SHAP",fontsize=8)
plt.tight_layout(pad=1,h_pad=1.4)
plt.show()


# In[5]:


import numpy as np
feature_importance=pd.DataFrame(np.nan, index=input_space.columns, columns=output_space.columns)
feature_importance.shape


# In[ ]:





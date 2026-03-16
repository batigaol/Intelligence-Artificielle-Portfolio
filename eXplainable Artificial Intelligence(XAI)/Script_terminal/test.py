#!/usr/bin/env python
# coding: utf-8
import sys
import multiprocessing
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt 
from math import ceil
### Commentaires
from itertools import combinations
###
from sklearn.preprocessing import StandardScaler
###
###############  Random Forest model and SHAP for Cost prediction
##### Random Forest modele
from sklearn.model_selection import cross_validate, cross_val_score
#from sklearn.model_selection import cross_val_score
from sklearn.metrics import make_scorer,mean_squared_error
#from sklearn.metrics import mean_squared_error
from sklearn.ensemble import RandomForestRegressor
####
import shap
###
from sklearn.feature_selection import VarianceThreshold
import seaborn as sns

# Read the command-line argument passed to the interpreter when invoking the script
fichier_1 = sys.argv[1]
fichier_2 = sys.argv[2]
pareto_path_1 = sys.argv[3]
pareto_path_2 = sys.argv[4]
matrix_path= sys.argv[5]
user_input = sys.argv[6]
### Choices
yes_choices = ['yes', 'y']
no_choices = ['no', 'n']

# # Chargement des dataset Input_Space et Output_Space
input_space = pd.read_excel(fichier_1,engine='openpyxl')
output_space = pd.read_excel(fichier_2,engine='openpyxl')
########  Filtrer les données avec la Variance   
vt = VarianceThreshold() # Threshold default is 0
_ = vt.fit(input_space)
var_vector_mask = vt.get_support()
    #var_vector_mask 
input_space = input_space.loc[:, var_vector_mask]

#####  fonction permettant de multiplier les colonnes entre elles deux a deux
def ft_combinatorial(input_space):
    from itertools import combinations
    #df=input_space
    cc = list(combinations(input_space.columns,2))
    input_space= pd.concat([input_space[c[1]]*input_space[c[0]] for c in cc], axis=1, keys=cc)
    input_space.columns = input_space.columns.map('*'.join)
    return input_space   
############################################################""    
#####  Activation des interactions entre les feature
if user_input.lower() in yes_choices:
    ft=ft_combinatorial(input_space)
    input_space=pd.concat([input_space,ft],axis=1,join='inner')
#col_names=input_space.columns
elif user_input.lower() in no_choices:
    input_space=input_space 

#####  Creation des fakes variables 
Vars=input_space.columns
input_space[[v + "_fake" for v in Vars]]=input_space.iloc[np.random.permutation(input_space.index)].reset_index(drop=True)

###### Application de la standardisation avec “StandardScaler” dans le package #preprocessing.
ss = StandardScaler()
output_space = pd.DataFrame(ss.fit_transform(output_space),columns =output_space.columns)
input_space = pd.DataFrame(ss.fit_transform(input_space),columns =input_space.columns)

###### Fonction permettant de comparer les variables avec leur fakes
def ft_from_fake(ft_input):
    ft=ft_input
    for i in Vars:
        for col in output_space.columns:
            if   ft.loc[i,col] <= ft.loc[i+ "_fake",col]:
                 ft.loc[i,col]=0
    ft.drop(ft.tail(len(Vars)).index,inplace = True)
    return ft

###############  Random Forest model and SHAP for Cost prediction
feature_importance=pd.DataFrame(np.nan, index=input_space.columns, columns=output_space.columns)
Output= output_space.to_dict('series')
X = input_space
for  col in list(output_space.columns.values):
    model = RandomForestRegressor(n_estimators =900,max_depth =70,random_state =101)
    #Score_col=cross_val_score(model, X ,Output[col], cv=10, scoring=make_scorer(mean_squared_error))
    #mean_mse, std_mse = np.mean(Score_col), np.std(Score_col)
    model_col=model.fit(X, Output[col])  
    ##### SHAP explaination
    shap_values_col= shap.TreeExplainer(model_col).shap_values(X)
    vals_col= np.abs(shap_values_col).mean(0)
    feature_importance[col]=vals_col
    
########################### SHAP VALUES IMPORTANCE DATAFRAME
feature_importance=1.15*ft_from_fake(feature_importance)
feature_importance[feature_importance > 1] = 1
#########################################################################################
########################### Deuxieme Passe avec SHAP
######  de supprimer  les variables fakes
#X= pd.DataFrame(X)
for col in X.columns:
    if  '_fake' in col:
        del X[col]
#########################
feature_importance_clean=pd.DataFrame(np.nan, index=input_space.columns, columns=output_space.columns)
#feature_importance_clean=pd.DataFrame()
i=feature_importance.index
d = {}
Index={}
list_of_datasets=list()
#Output_clean= output_space.to_dict('series')
feature_importance_clean=pd.DataFrame(np.nan, index=input_space.columns, columns=output_space.columns)
for  col in list(feature_importance.columns.values):
    index =feature_importance[col]==0
    result = i[index]
    Index[col]=result.tolist()
    #print(Index['Cost'])
    d[col]=pd.DataFrame(X.drop(Index[col], axis=1))
    #var=d[col].columns
    #d[col][[v + "_fake" for v in var]]=d[col].iloc[np.random.permutation(d[col].index)].reset_index(drop=True)
    model_col=model.fit(d[col], Output[col])
    shap_values_col= shap.TreeExplainer(model_col).shap_values(d[col])
    vals_col=pd.Series(np.abs(shap_values_col).mean(0),index=d[col].columns)
    feature_importance_clean[col]=pd.DataFrame(pd.concat([vals_col[lambda x: x != ''].rename(col)], axis=1))  
    
 ########################### SHAP VALUES IMPORTANCE DATAFRAME
feature_importance_clean=1.15*feature_importance_clean
feature_importance_clean[feature_importance_clean > 1] = 1
#feature_importance_clean 

#### Visualisation et sauvegarde du pareto graph
size=len(output_space.columns)
taille=ceil(len(output_space.columns)/3)
fig = plt.figure(figsize = (5*size,5*size))
for i, col in enumerate(feature_importance_clean.columns):
    ax = fig.add_subplot(taille, 3, i+1)
    #feature_importance[col].plot(kind='barh')
    sns.barplot(x=feature_importance_clean[col], y=feature_importance_clean.index,color='#1874CD')
    plt.xticks(np.linspace(0,1,6,endpoint=True),fontsize=5*size,rotation=45)
    plt.yticks(fontsize=5*size,color ='0.2')
    plt.xlim([0,1])
    ax.set(xlabel=None)
    plt.title("VI:"+ col,fontsize=5*size+5)
    plt.grid(axis='x',color='0.7',linewidth=0.5)
plt.tight_layout(pad=1,h_pad=1.4)   
plt.ioff()
plt.savefig(pareto_path_1)

fig = plt.figure(figsize = (5*size,5*size))
for i, col in enumerate(feature_importance_clean.columns):
    ax = fig.add_subplot(taille, 3, i+1)
    #feature_importance[col].plot(kind='barh')
    sns.barplot(x=feature_importance_clean[col], y=feature_importance_clean.index,order=feature_importance_clean[col].sort_values(ascending = False).index,color='#1874CD')
    plt.xticks(np.linspace(0,1,6,endpoint=True),fontsize=5*size,rotation=45)
    plt.yticks(fontsize=5*size,color ='0.2')
    plt.xlim([0,1])
    ax.set(xlabel=None)
    plt.title("VI:"+ col,fontsize=5*size+5)
    plt.grid(axis='x',color='0.7',linewidth=0.5)
plt.tight_layout(pad=1,h_pad=1.4)   
plt.ioff()
plt.savefig(pareto_path_2)

#### Visualisation et sauvegarde du matrix des shap values
# Generate a mask for the upper triangle
#mask = np.triu(np.ones_like(feature_importance, dtype=bool))
f, ax = plt.subplots(figsize=(5*size,5*size))
sns.set(font_scale=5) 
heatmap =sns.heatmap(feature_importance_clean.T.round(2),
                      #mask = mask,
                      square = True,
                      linewidths = .8,
                      cmap ='Blues',
                      cbar_kws = {'shrink': .7,
                               'ticks' : [0.0, 0.2, 0.4, 0.6, 0.8, 1]},
                      vmin = 0,
                      vmax = 1,
                      annot = True,
                      annot_kws = {"size": 5*size+10})
#add the column names as labels            
plt.xticks(fontsize=5*size+5,rotation=45)
plt.yticks(fontsize=5*size+5,rotation=45)
plt.ioff()
plt.savefig(matrix_path)

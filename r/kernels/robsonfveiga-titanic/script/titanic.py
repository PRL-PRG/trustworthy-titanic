#!/usr/bin/env python
# coding: utf-8

# # Titanic Analysis (Under onstruction)

# #### Importação das bibliotecas
# 
# Primeiro é feito a importação das bibliotecas necessárias

# In[ ]:


py.offline.init_notebook_mode();


# #### Lendo o dataset
# Realizando a leitura e inserindo em um dataframe que será utilizado como base para todas manipulações

# In[ ]:


data_csv = csv.reader(open("../input/train.csv",newline=''));

header = np.array(next(data_csv));
data=[]                          
for row in data_csv:      
    data.append(row);             
    
data = p.DataFrame(np.array(data),columns=header);


# #### Convertendo os dados
# Convertendo os dados para valores os quais tornam o trabalho mais facil e performatico

# In[ ]:


data.Survived = p.to_numeric(data.Survived);
data.Pclass   = p.to_numeric(data.Pclass);


# ### Tipos de variáveis
# 
# <table class="table" >
#     <thead>
#         <tr>
#             <th >Variable Name</th>
#             <th>Description</th>
#             <th >Type</th>
#         </tr>
#     </thead>
#     <tbody>
#         <tr class="odd">
#             <td align="left">Survived</td>
#             <td align="left">Survived (1) or died (0)</td>
#             <td style="color:#587D96;">Qualitative / Nominal</td>
#         </tr>
#         <tr class="even">
#             <td align="left">Pclass</td>
#             <td align="left">Passenger’s class</td>
#             <td style="color:#587D96;">Qualitative / Ordinal</td>
#         </tr>
#         <tr class="odd">
#             <td align="left">Name</td>
#             <td align="left">Passenger’s name</td>
#             <td style="color:#587D96;">Qualitative / Nominal</td>
#         </tr>
#         <tr class="even">
#             <td align="left">Sex</td>
#             <td align="left">Passenger’s sex</td>
#             <td style="color:#587D96;">Qualitative / Nominal</td>
#         </tr>
#         <tr class="odd">
#             <td align="left">Age</td>
#             <td align="left">Passenger’s age</td>
#             <td style="color:#A6246E">Quantitative / Continuous</td>
#         </tr>
#         <tr class="even">
#             <td align="left">SibSp</td>
#             <td align="left">Number of siblings/spouses aboard</td>
#             <td style="color:#A6246E">Quantitative / Discrete</td>
#         </tr>
#         <tr class="odd">
#             <td align="left">Parch</td>
#             <td align="left">Number of parents/children aboard</td>
#             <td style="color:#A6246E">Quantitative / Discrete</td>
#         </tr>
#         <tr class="even">
#             <td align="left">Ticket</td>
#             <td align="left">Ticket number</td>
#             <td style="color:#587D96;">Qualitative / Nominal</td>
#         </tr>
#         <tr class="odd">
#             <td align="left">Fare</td>
#             <td align="left">Fare</td>
#            <td style="color:#A6246E">Quantitative / Continuous</td>
#         </tr>
#         <tr class="even">
#             <td align="left">Cabin</td>
#             <td align="left">Cabin</td>
#             <td style="color:#587D96;">Qualitative / Nominal</td>
#         </tr>
#         <tr class="odd">
#             <td align="left">Embarked</td>
#             <td align="left">Port of embarkation</td>
#             <td style="color:#587D96;">Qualitative / Nominal</td>
#         </tr>
#     </tbody>
# </table>

# ### Contadores
# 
# Neste momento quero analisar qual qualidade de passageiro era mais presente para em uma analise seguinte correlacionar as informações

# In[ ]:



number_passengers           = np.shape(data)[0];

#Seuvived
number_survived             = np.shape(data[data["Survived"]==1])[0];
number_no_survived          = np.shape(data[data["Survived"]==0])[0];

#Sex
number_female               = np.shape(data[data["Sex"]=="female"])[0];
number_male                 = np.shape(data[data["Sex"]=="male"])[0];

#Socio Economic Status
number_class_1                     = np.shape(data[data["Pclass"]==1])[0];
number_class_2                     = np.shape(data[data["Pclass"]==2])[0];
number_class_3                     = np.shape(data[data["Pclass"]==3])[0];


# Verificando se existe gaps nas colunas de classe e sexo dos passageiros

# In[ ]:


print("column sex is whole filled:   " + str(np.count_nonzero(data["Sex"])==number_passengers));
print("column class is whole filled: " + str(np.count_nonzero(data["Pclass"])==number_passengers));


# In[ ]:


labels        = np.array(["Female","Male","Class 1","Class 2","Class 3"]);
values     = np.array([number_female,number_male,number_class_1,number_class_2,number_class_3]);
valuesFrame     = p.DataFrame(values,labels);


#Plot configs
plotConfig = [go.Bar(
            x=labels,
            y=values
    )]

layout = go.Layout(
    title='Total passengers by Qualitative / Ordinal',
    
   
)

fig = go.Figure(data=plotConfig, layout=layout)


py.offline.iplot(fig)


# Analisando o grafico acima podemos perceber quer entre os passageiros uma caracteristica muito presente era sexo masculino e pessoas de classe 3. Portantos no navio havia muitos homens e pessoas de classe baixa. Agora vamos ver como estas informações se relacionam.

# In[ ]:


number_female_suvived               = np.shape(data[data["Sex"]=="female"])[0];
number_male                 = np.shape(data[data["Sex"]=="male"])[0];


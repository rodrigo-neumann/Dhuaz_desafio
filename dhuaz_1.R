# install.packages("arules")
# install.packages("arulesViz")
# install.packages("tidyverse")
# install.packages("knitr")
#install.packages("RColorBrewer")
# install.packages("stringr")
install.packages("ggpubr")

library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyverse)
library(arules)
library(arulesViz)
library(readxl)
library(RColorBrewer)
library(tidyr)
library(ggpubr)


require(dplyr)
require("forecast")
require(ggplot2)
require(caret)
require(lubridate)
require(fastDummies)

#--------- DATA extraction----------------------
raw_data=read.csv("./Dhuaz desafio/Dhuaz_desafio/data/base_desafio.csv", sep = "|")

#---------Data exploration----------------------
#------ Looking for missign data
raw_data %>% is.na() %>% table()
(raw_data=="") %>% table()

raw_data$product_category_name %>% table() %>% sort(decreasing = T) #We have one missing category, some categories are very under represented

raw_data$order_purchase_timestamp %>% as.Date() %>% summary() # Almost two years of data

raw_data$seller_id %>% unique() %>% length() # 2750 different sellers
raw_data$seller_id %>% table() %>% sort(decreasing = T) %>% head() # top seller by the number o products sold


#-----Volume de venda------------------------------------------------------------------------------------------------------

volume_venda_dia=raw_data %>% 
  group_by(dia=floor_date(order_purchase_timestamp %>% as.Date(), unit = 'days'))  %>% 
  dplyr::summarise(volume_vendas=sum(price))

volume_venda_mes=raw_data %>% 
  group_by(mes=floor_date(order_purchase_timestamp %>% as.Date(), unit = 'months'))  %>% 
  dplyr::summarise(volume_vendas=sum(price))

volume_venda_ano=raw_data %>% 
  group_by(ano=floor_date(order_purchase_timestamp %>% as.Date(), unit = 'years'))  %>% 
  dplyr::summarise(volume_vendas=sum(price))

volume_venda_wday=raw_data %>% 
  group_by(wday=wday(order_purchase_timestamp %>% as.Date(), label = T))  %>% 
  dplyr::summarise(volume_vendas=sum(price)
                   ,dias=length(unique(floor_date(order_purchase_timestamp %>% as.Date(), unit = 'days'))))
volume_venda_wday$media=volume_venda_wday$volume_vendas/volume_venda_wday$dias

ggplot()+geom_line(aes(x=volume_venda_dia$dia,y=volume_venda_dia$volume_vendas),size=0.5)+
  ggtitle("Volume de vendas diario") +
  xlab("Dia") + ylab("Vendas (R$")

ggplot()+geom_line(aes(x=volume_venda_mes$mes,y=volume_venda_mes$volume_vendas),size=1.5)+
  ggtitle("Volume de vendas mensal") +
  xlab("Mes") + ylab("Vendas (R$")
  
#-------principais produtos,vendeores,cidades------------------------------------------------------------------------------------

group_seller=raw_data %>% group_by(seller_id) %>%  summarise(n_vendas=n()
                                                                ,n_vendas_15_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(15) )
                                                                ,n_vendas_30_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(30) )
                                                                ,n_vendas_60_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(60) )
                                                                ,n_vendas_90_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(90) )
                                                                ,valor_vendas= sum(price)
                                                                ,valor_vendas_15_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(15),price,0))
                                                                ,valor_vendas_30_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(30),price,0))
                                                                ,valor_vendas_60_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(60),price,0))
                                                                ,valor_vendas_90_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(90),price,0))
)

group_seller_category=raw_data %>% group_by(seller_id,product_category_name) %>%  summarise(n_vendas=n()
                                                             ,n_vendas_15_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(15) )
                                                             ,n_vendas_30_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(30) )
                                                             ,n_vendas_60_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(60) )
                                                             ,n_vendas_90_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(90) )
                                                             ,valor_vendas= sum(price)
                                                             ,valor_vendas_15_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(15),price,0))
                                                             ,valor_vendas_30_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(30),price,0))
                                                             ,valor_vendas_60_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(60),price,0))
                                                             ,valor_vendas_90_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(90),price,0))
)


group_location=raw_data %>% group_by(seller_state,seller_city) %>%  summarise(n_vendas=n()
                                                                ,n_vendas_15_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(15) )
                                                                ,n_vendas_30_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(30) )
                                                                ,n_vendas_60_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(60) )
                                                                ,n_vendas_90_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(90) )
                                                                ,valor_vendas= sum(price)
                                                                ,valor_vendas_15_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(15),price,0))
                                                                ,valor_vendas_30_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(30),price,0))
                                                                ,valor_vendas_60_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(60),price,0))
                                                                ,valor_vendas_90_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(90),price,0))
)


group_product=raw_data %>% group_by(product_id) %>%  summarise(n_vendas=n()
                                                 ,n_vendas_15_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(15) )
                                                 ,n_vendas_30_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(30) )
                                                 ,n_vendas_60_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(60) )
                                                 ,n_vendas_90_dias=sum(order_purchase_timestamp>=as.Date("2018-06-30")-days(90) )
                                                 ,valor_vendas= sum(price)
                                                 ,valor_vendas_15_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(15),price,0))
                                                 ,valor_vendas_30_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(30),price,0))
                                                 ,valor_vendas_60_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(60),price,0))
                                                 ,valor_vendas_90_dias= sum(ifelse(order_purchase_timestamp>=as.Date("2018-06-30")-days(90),price,0))
                                                 )






#------------------------items comprados juntos-------------------------------------------------------
multi_items=filter(raw_data,order_id %in% filter(raw_data,order_item_id==2)$order_id)  

multi_items %>% group_by(order_id)  %>%  
  dplyr::summarise(items=paste(product_id,collapse = ",")) %>% 
  select(items) %>%  write.csv("./Dhuaz desafio/Dhuaz_desafio/data/trans_orders.csv",row.names = F,quote = F)

tr=arules::read.transactions("./Dhuaz desafio/Dhuaz_desafio/data/trans_orders.csv", format = 'basket', sep=',')
summary(tr)

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

rules=apriori(tr, parameter = list(supp=0.0001, conf=0.01,maxlen=21))
summary(rules)
rules_df=as(rules,"data.frame")

#--------------------------Prediction----------------------------------------------------------------
category_vendas=raw_data %>% group_by(product_category_name) %>% summarise(vendas=sum(price)) 

# top 5 categories: relogios_presentes,beleza_saude,cama_mesa_banho,esporte_lazer,informatica_acessorios

dataset=raw_data %>% group_by(product_category_name,mes=month(order_purchase_timestamp,label = T)
                              ,ano=year(order_purchase_timestamp)
                              ,data=floor_date(as.Date(order_purchase_timestamp),unit = "months")) %>%  
  dplyr::summarise(n_vendas=n()
                   , soma_vendas=sum(price)
                   )


dataset$n=as.numeric((dataset$data- (dataset$data %>% min())), units="days")
dataset=dataset[order(dataset$data),]
dataset$predict=0
dataset$upper=0
dataset$lower=0

dataset_relogios=dataset %>% filter(product_category_name=="relogios_presentes")
dataset_beleza=dataset %>% filter(product_category_name=="beleza_saude")
dataset_mesa=dataset %>% filter(product_category_name=="cama_mesa_banho")
dataset_esporte=dataset %>% filter(product_category_name=="esporte_lazer")
dataset_info=dataset %>% filter(product_category_name=="informatica_acessorios")


holt_es_int<-function(dataset)
{
S=nrow(dataset)
for (i in 2:S)
{
  print(i)
  model <- holt(y=dataset$soma_vendas[1:i]
                ,h = 1)
  autoplot(model)
  dataset[i+1,]$predict=predict(model)$mean
  dataset[i+1,]$lower=model$lower[2]
  dataset[i+1,]$upper=model$upper[2]
}
return(dataset)  
}
dataset_relogios=holt_es_int(dataset_relogios)
dataset_relogios[nrow(dataset_relogios),]$data=as.Date('2018-07-01')
dataset_beleza=holt_es_int(dataset_beleza)
dataset_beleza[nrow(dataset_beleza),]$data=as.Date('2018-07-01')
dataset_mesa=holt_es_int(dataset_mesa)
dataset_mesa[nrow(dataset_mesa),]$data=as.Date('2018-07-01')
dataset_esporte=holt_es_int(dataset_esporte)
dataset_esporte[nrow(dataset_esporte),]$data=as.Date('2018-07-01')
dataset_info=holt_es_int(dataset_info)
dataset_info[nrow(dataset_info),]$data=as.Date('2018-07-01')


A=ggplot()+geom_line(aes(x=dataset_relogios$data,y=dataset_relogios$soma_vendas, color="real"),size=1)+
  geom_line(aes(x=dataset_relogios$data,y=dataset_relogios$predict, color="media"),size=1)+
  geom_line(aes(x=dataset_relogios$data,y=dataset_relogios$upper, color="95%"),size=1)+
  geom_line(aes(x=dataset_relogios$data,y=dataset_relogios$lower, color="5%"),size=1)+
  ggtitle("relogios_presentes") + xlab("Mes") + ylab("Vendas (R$)")

B=ggplot()+geom_line(aes(x=dataset_beleza$data,y=dataset_beleza$soma_vendas, color="real"),size=1)+
  geom_line(aes(x=dataset_beleza$data,y=dataset_beleza$predict, color="media"),size=1)+
  geom_line(aes(x=dataset_beleza$data,y=dataset_beleza$upper, color="95%"),size=1)+
  geom_line(aes(x=dataset_beleza$data,y=dataset_beleza$lower, color="5%"),size=1)+
  ggtitle("beleza_saude") + xlab("Mes") + ylab("Vendas (R$)")

C=ggplot()+geom_line(aes(x=dataset_mesa$data,y=dataset_mesa$soma_vendas, color="real"),size=1)+
  geom_line(aes(x=dataset_mesa$data,y=dataset_mesa$predict, color="media"),size=1)+
  geom_line(aes(x=dataset_mesa$data,y=dataset_mesa$upper, color="95%"),size=1)+
  geom_line(aes(x=dataset_mesa$data,y=dataset_mesa$lower, color="5%"),size=1)+
  ggtitle("cama_mesa_banho") + xlab("Mes") + ylab("Vendas (R$)")

D=ggplot()+geom_line(aes(x=dataset_esporte$data,y=dataset_esporte$soma_vendas, color="real"),size=1)+
  geom_line(aes(x=dataset_esporte$data,y=dataset_esporte$predict, color="media"),size=1)+
  geom_line(aes(x=dataset_esporte$data,y=dataset_esporte$upper, color="95%"),size=1)+
  geom_line(aes(x=dataset_esporte$data,y=dataset_esporte$lower, color="5%"),size=1)+
  ggtitle("esporte_lazer") + xlab("Mes") + ylab("Vendas (R$)")

E=ggplot()+geom_line(aes(x=dataset_info$data,y=dataset_info$soma_vendas, color="real"),size=1)+
  geom_line(aes(x=dataset_info$data,y=dataset_info$predict, color="media"),size=1)+
  geom_line(aes(x=dataset_info$data,y=dataset_info$upper, color="95%"),size=1)+
  geom_line(aes(x=dataset_info$data,y=dataset_info$lower, color="5%"),size=1)+
  ggtitle("informatica_acessorios") + xlab("Mes") + ylab("Vendas (R$)")


ggarrange(A,B,C,D,E 
          ,ncol = 2, nrow = 3)

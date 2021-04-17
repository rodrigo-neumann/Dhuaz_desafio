require(dplyr)
require("forecast")
require(ggplot2)
require(caret)
require(lubridate)
require(fastDummies)

#--------- DATA extraction----------------------
raw_data=read.csv("./Dhuaz desafio/Dhuaz_desafio/base_desafio.csv", sep = "|")

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

ggplot()+geom_line(aes(x=volume_venda_dia$dia,y=volume_venda_dia$volume_vendas),size=0.5)

ggplot()+geom_line(aes(x=volume_venda_mes$mes,y=volume_venda_mes$volume_vendas),size=1.5)
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


group_producct=raw_data %>% group_by(product_id) %>%  summarise(n_vendas=n()
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




#--------items comprados juntos -------------------------------------------------------------------------------------

filter(raw_data, order_item_id %in% (raw_data %>% filter(order_item_id>1))$order_item_id) %>% nrow()

n_products=raw_data %>% group_by(order_id) %>% dplyr::summarise(n_products=n())
n_products_2=filter(n_products,n_products>=2)
n_products_plus=filter(n_products,n_products>2)

products=(raw_data %>% filter(order_id %in% n_products_2$order_id))$product_id %>% unique()

S=length(products)
pedidos_dummy=raw_data
for (i in 1:S)
{
  #print(i)
  #raw_data[["f64fc82a96c3d672cedd416653f65f06"]]=raw_data$product_id=="f64fc82a96c3d672cedd416653f65f06"
  pedidos_dummy[[products[i]]]=pedidos_dummy$product_id==products[i]
}

pedidos_dummy_s=pedidos_dummy %>% select(names(pedidos_dummy)[11:7208])

pedidos_resumo=pedidos_dummy_s[,1:10] %>% group_by(across(everything())) %>% dplyr::summarise(n=n())




#-----------------------------------------------------------------------------------------------------------
raw_data %>% names()
data_dummies=raw_data  %>% fastDummies::dummy_columns(select_columns = c("seller_city"
                                                                        ,"product_category_name"
                                                                        ,"seller_state")
                                        , remove_selected_columns = T)


raw_data %>% group_by(order_id) %>% dplyr::summarise()






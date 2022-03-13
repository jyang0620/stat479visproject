library(readr)
library(tidyverse)
library(ggplot2)

ds_sal = read_csv("https://uwmadison.box.com/shared/static/l2n9u9d97yxzibvd71y7a370kt0pj1pu.csv")
costoliv = read_csv("https://uwmadison.box.com/shared/static/6xphn35fqqq8svwr3y9iis1cpxjqfo8b.csv")
head(ds_sal)
names(ds_sal)

ds_salfix = ds_sal %>%
  separate(Location, c('City','State'),sep = ', ')
ds_salfix[127,]$City = 'Los Angeles'
ds_salfix[127,]$State = 'CA'
ds_salfix$cost.of.living = NA

for(i in seq(length(ds_salfix$City))){
  if (ds_salfix[i,]$City %in% costoliv$City) {
    ix = which(costoliv$City == ds_salfix[i,]$City)
    for (j in ix){
      if(ds_salfix[i,]$State == costoliv[j,]$State){
        print(i)
        print(ds_salfix[i,]$City)
        print(j)
        print(costoliv[j,]$State)
        ds_salfix[i,]$cost.of.living = costoliv[j,]$`Cost of Living Index`
      }
    }
  }
}


ds_salfix %>%
  drop_na(cost.of.living) %>%
  ggplot() +
  geom_point(aes(`Avg Salary(K)`, cost.of.living, col = State))

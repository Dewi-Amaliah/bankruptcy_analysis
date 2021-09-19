# Code for Data Cleaning

---
  title: "High Dimensional Data Analysis - Group Assignment 18"
author: "Group 18"
date: "08/09/2021"
output:
  bookdown::html_document2:
  fig_height: 5
fig_width: 8
toc: yes
toc_depth: 1
toc_float:
  collapsed: false
number_sections: false
code_folding: show
theme: readable
---
  
  ```{r setup, include=FALSE, message = FALSE, warning = FALSE}
knitr::opts_chunk$set(echo = TRUE, 
                      message = FALSE, 
                      warning = FALSE, 
                      error = FALSE)
library(tidyverse)
library(dplyr)
library(visdat)
library(splitstackshape)
library(lubridate)
library(patchwork)
library(tidyverse)
library(dplyr)
library(learningtower)
library(kableExtra)
library(FactoMineR)
library(factoextra)
library(patchwork)
library(corrplot)
library(ggrepel)
bankruptcy <- read_rds(here::here("data/Bankruptcy.rds"))
```


# Overview

```{r}
glimpse(bankruptcy)
```

```{r}
vis_dat(bankruptcy)
```


- Note: We can see that FirmEnd does not have any missing values, in fact it has many NULL values that is not detected. Hence, we will change it to NA. 


```{r}
bankruptcy <- bankruptcy %>%
  mutate(FirmEnd = ifelse(FirmEnd == "", 
                          NA, 
                          FirmEnd))
```

- From the `glimpse` output, we also notice that `SICMajGroup`, which is the industry classification code, has a very long name, so we will separate it to the code and the meaning. 

```{r}
bankruptcy <- bankruptcy %>%
  separate(SICMajGroup, 
           into = c("SIC", "SICMajGroup"), 
           sep = "\\s", 
           extra = "merge") %>%
  mutate(SIC = as.factor(SIC))
```


- Now, let us check again the missing values in data 

```{r}
vis_dat(bankruptcy)
```

```{r}
vis_miss(bankruptcy)
```

Assets, CPI, Employees, Ebit, Liab, FilingRate, GDP, PrimeFiling, Sales

# Variable Discussion 

- The following variables do not need any changes
+ Name: Name of the firm
+ Assets: Total assets (in millions of dollars)
+ CityFiled: City where filing took place
+ CPI U.S CPI at the time of filing
+ DENYOther: CityFiled, categorized as Wilmington (DE), New York (NY) or all other cities (OT)
+ FilingRate: Total number of other bankruptcy filings in the year of this filing
+ GDP: Gross Domestic Product for the Quarter in which the case was filed
+ HeadCityPop: The population of the firms headquarters city


- Variables with a lot of missing values are:
  + FirmEnd: Short description of the event that ended the firm’s existence - We plan to omit this variable for further analysis
+ EmplUnion: Number of union employees before bankruptcy - We plan to omit this variable for further analysis



- The DaysIn (Length of bankruptcy process) variable has about 0.92% missing values. In order to fill the NA's we used the following approach:


- **DaysIn**

Now let us move to `DaysIn` variable 

```{r}
na_daysin <- filter(bankruptcy, is.na(DaysIn))
na_daysin
```

Based on google search:

- [AP Industries, Inc.](https://casetext.com/case/in-re-ap-industries-inc?__cf_chl_jschl_tk__=pmd_0e9LcvX3fGC0nT5WJVBShbHDvK9GY2AMZnekCjslbN0-1631333530-0-gqNtZGzNAjujcnBszQd9) filed the bankruptcy on 27/03/1990, disposition made in 26/07/1990 equivalent to 121 days

- [Daisy Systems Corp.](https://caselaw.findlaw.com/us-9th-circuit/1020075.html) filed the bankruptcy on 30/05/1991, the court decided on 24/09/1996 equivalent to 1944 days

- Since there is no data for the remaining two, **we will impute the days in bankruptcy**, based on firm types median. (Hunt International Resources Corp., SIC = 20; McCrory Corp., SIC = 53)

```{r}
bankruptcy %>% group_by(SIC) %>%
  summarise(median = median(DaysIn, na.rm = TRUE)) %>% 
    dplyr::filter(SIC %in% c(20, 53))
```


```{r}
bankruptcy <- bankruptcy %>%
  mutate(DaysIn = ifelse(Name == "Hunt International Resources Corp.", 305,
                                    ifelse(Name == "AP Industries, Inc.", 121,
                                           ifelse(Name == "Daisy Systems Corp.", 1944,
                                                  ifelse(Name == "McCrory Corp.", 683, DaysIn)))))

summary(bankruptcy$DaysIn)
```

- There is no suspicious anomaly. The bankcruptcy process can take a long time. 


- **HeadCourtCityToDE** (The distance in miles from the firms headquarters city to the city in which the case was filed)

- There are some missing values in `HeadCourtCityToDE`, may be we could impute it using the values in CityFiled, DENYOther, or HeadStAtFiling. One of the row missing is Aruba, so we just google the distance between these cities an impute it (1126 miles). The other two are Canada. 

- We search the address for the firm name and google the distance.
Loewen Group, Inc (British Columbia to Wilmington = 2942).
Philip Services Corp. (Ontario to Wilmington = 1234)


```{r}
na_distance <- bankruptcy %>%
  filter(is.na(HeadCourtCityToDE))

na_distance

bankruptcy <- bankruptcy %>%
  mutate(HeadCourtCityToDE = ifelse(Name == "Divi Hotels, N.V.", 1126,
                                    ifelse(Name == "Loewen Group, Inc.", 2942,
                                           ifelse(Name == "Philip Services Corp. (1999)", 1234,
                                                  HeadCourtCityToDE))))

summary(bankruptcy$HeadCourtCityToDE)
```

- The minimun distance is 1 , when inspected the state of headquarter and the city filed its in the same state, so it made sense.


- **Employees** (Number of employees before bankruptcy)

```{r}
na_employees <- filter(bankruptcy, is.na(Employees))
na_employees
```

- The number of employees of County Seat, Inc. is missing. According to [Chicago Tribune](https://www.chicagotribune.com/news/ct-xpm-1996-11-29-9611290135-story.html), the store has an average 5-10 employees and it has 740 stores. We will take 7 employees per store. So the employees before bankruptcy are approximately 5180. 

```{r}
bankruptcy <- bankruptcy %>%
  mutate(Employees = ifelse(Name == "County Seat, Inc.", 5180, Employees))

summary(bankruptcy$Employees)
```

- Is it possible that companies only have 1 employees? 

```{r}
ggplot(bankruptcy, aes(x = Employees)) +
  geom_histogram()
```

- **Sales** (Sales before bankruptcy (in dollars))

- The missing values in Sales is imputed by its median of sales grouped by SIC

```{r}
na_sales <- filter(bankruptcy, is.na(Sales))
na_sales

bankruptcy %>%
  group_by(SIC) %>%
  summarise(median = median(Sales, na.rm = TRUE)) %>% 
  dplyr::filter(SIC == 56)

bankruptcy <- bankruptcy %>%
  mutate(Sales = ifelse(Name == "County Seat, Inc.", 1645170944, Sales))
```

**Ebit and Liab**

```{r}
na_ebit <- filter(bankruptcy, is.na(Ebit))
na_liab <- filter(bankruptcy, is.na(Liab))
na_ebit
na_liab
```

Options for treat the missing values in `Liab` and `Ebit`:

- 1. Omit the missing values in these variables


```{r}
bank_data_op1 <- bankruptcy %>%
  filter(!is.na(Ebit) & !is.na(Liab))
```

- 2. Impute with its median grouped by SIC 

```{r}
bank_data_op2 <- bankruptcy %>% group_by(SIC) %>%
  mutate(Ebit = ifelse(is.na(Ebit), median(Ebit,na.rm=TRUE), Ebit))

bank_data_op2 <- bank_data_op2 %>% group_by(SIC) %>%
  mutate(Liab = ifelse(is.na(Liab), median(Liab,na.rm=TRUE), Liab))
```

```{r}
vis_dat(bank_data_op1)
vis_dat(bank_data_op2)
```

# Sample MDS for diff Data Cleaning Approaches

- Let say we use bank_data_op1

```{r}
data_clean <- bank_data_op1 %>%
  select(-FirmEnd, -EmplUnion) %>%
  mutate(DENYOther = as.factor(DENYOther),
         MonthFiled = as.factor(MonthFiled),
         YearFiled = as.factor(YearFiled))
row.names(data_clean) <- data_clean$Name
ggplot(data_clean, aes(x = Ebit)) +
  geom_histogram()
```


```{r}
vis_dat(data_clean)
```

# Data Clean Option 1

```{r}
#data w/o missing values, number obs as in original data 

dd <- data_clean %>% 
  ungroup() %>% 
  select_if(is.numeric) %>%
  scale %>% dist(method = "manhattan")

rownames(data_clean) -> attributes(dd)$Labels

cmds <- cmdscale(dd,eig = T)

cmds$points %>%
as.data.frame() %>%
rownames_to_column(var = 'Names')-> df

p1 <- ggplot(df,
             aes(x=V1,
                 y=V2,
                 label=`Names`)) + 
  geom_text(size=2) # xlim(0, 30) + ylim(-20,20)
```


# Data Clean Option 2

```{r}
data_clean2 <- bank_data_op2 %>%
  select(-FirmEnd, -EmplUnion) %>%
  mutate(DENYOther = as.factor(DENYOther),
         MonthFiled = as.factor(MonthFiled),
         YearFiled = as.factor(YearFiled)) 
row.names(data_clean2) <- data_clean2$Name

#data w/o missing values, number obs as in original data

dd2 <- data_clean2 %>%  
  ungroup() %>% 
  select_if(is.numeric) %>%
  scale %>% dist(method = "manhattan")


rownames(data_clean2) -> attributes(dd2)$Labels

cmds2 <- cmdscale(dd2,eig = T)

cmds2$points %>%
as.data.frame() %>%
rownames_to_column(var = 'Names')-> df2

df2 <- left_join(df2, data_clean2, by = c("Names" = "Name"))

p2 <- ggplot(df2,
             aes(x=V1,
                 y=V2,
                 label=`Names`, 
                 color = DENYOther)) + 
  geom_text(size=2) # xlim(0, 30) + ylim(-20,20)
```

# Data Clean Option 3

```{r}
data <- read_rds(here::here("data/Bankruptcy.rds"))

data_clean3 <- data %>%
  select(-FirmEnd, -EmplUnion) %>%
  drop_na() %>%
  mutate(DENYOther = as.factor(DENYOther),
         MonthFiled = as.factor(MonthFiled),
         YearFiled = as.factor(YearFiled)) 

row.names(data_clean3) <- data_clean3$Name

#data w/o missing values, number obs as in original data

dd3 <- data_clean3 %>%  
  ungroup() %>% 
  select_if(is.numeric) %>%
  scale %>% dist(method = "manhattan")


rownames(data_clean3) -> attributes(dd3)$Labels

cmds3 <- cmdscale(dd3,eig = T)

cmds3$points %>%
as.data.frame() %>%
rownames_to_column(var = 'Names')-> df3

df3 <- left_join(df3, data_clean3, by = c("Names" = "Name"))

p3 <- ggplot(df3,
             aes(x=V1,
                 y=V2,
                 label=`Names`, 
                 color = DENYOther)) + 
  geom_text(size=2) # xlim(0, 30) + ylim(-20,20)
```

# Plots

```{r}
p1
```

```{r}
p2
```

```{r}
p3
```




# PCA 

```{r}
data <- read_rds(here::here("data/Bankruptcy.rds"))

data_clean3 <- data %>%
  select(-FirmEnd, -EmplUnion) %>%
  drop_na() %>%
  mutate(DENYOther = as.factor(DENYOther),
         MonthFiled = as.factor(MonthFiled),
         YearFiled = as.factor(YearFiled))
```


```{r}
data_clean%>%
  select_if(is.numeric)%>% #Only use numeric variables
  prcomp(scale. = TRUE)->pca #Do pca 
summary(pca)
```


```{r}
screeplot(pca,type="lines")
```


```{r}
biplot(pca)
```

# Diff PCA

```{r}
pca_data <- data_clean %>%
  dplyr::select(Assets, CPI, Employees, Ebit, Liab, FilingRate, GDP, PrimeFiling, Sales)

pca <- prcomp(pca_data[,c(1:9)], center = TRUE,scale. = TRUE)

pca
```

```{r}
eig.value <- get_eigenvalue(pca)
eig.value %>%  kable(caption = "Eigen Values") %>% 
kable_styling(c("striped", "hover"))
```


```{r}
fviz_eig(pca, addlabels = TRUE)
```

```{r}
var <- get_pca_var(pca)
var
```

```{r}
fviz_pca_var(pca, col.var = "black")
```

# Aarathy branch 


# Data exploration 
A = cor(data_clean%>%select_if(is.numeric))
corrplot(A, method = 'number') 
data_clean%>%
  select_if(is.numeric)%>% #Only use numeric variables
  prcomp(scale. = TRUE)->pca #Do pca 
summary(pca)
screeplot(pca,type="lines")
biplot(pca)
data_clean<- data_clean%>%
  mutate(abbreviation=abbreviate(Name))
rownames(pca$x)<-pull(data_clean,abbreviation)
library(ggfortify)
autoplot(pca,label = TRUE, label.size = 3,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
```

We can see that theres an outlier, so lets remove it. 

```{r}
nooutlier<- bank_data_op1%>%
    mutate(abb=abbreviate(Name))
nooutlier <- nooutlier%>%
  dplyr::select(c(abb,Assets, CPI, Employees, Ebit, Liab, FilingRate, GDP, PrimeFiling, Sales))%>%
  dplyr::filter(abb!="TIn.")
nooutlier%>%
  select_if(is.numeric)%>% #Only use numeric variables
  prcomp(scale. = TRUE)->pca_no #Do pca 
summary(pca_no)
screeplot(pca_no,type="lines")
rownames(pca_no$x)<-pull(nooutlier,abb)
autoplot(pca_no,label = TRUE, label.size = 2.5,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)
```

#### Without var select
1. Same Direction -> Positive Correlation 

Employees + Ebit + Liab + Assests + Sales 

Filing + GDP + CPI 

2. 90 degree -> weak relationship 

3. Opposite Direction 

Prime filing has negative correlation with cpi + gdp


```{r}
library(ggplot2)
library(ggrepel)
soec_pca_pcs <- as_tibble(pca_no$x[, 1:2])%>%
  mutate(cnt=nooutlier$abb)
soec_pca_var <-
  tibble(n = 1:length(pca_no$sdev), evl = pca_no$sdev ^ 2)
soec_pca_evc <- as_tibble(pca_no$rotation[, 1:2]) %>%
  mutate(
    origin = rep(0, 9),
    variable = colnames(nooutlier%>%select_if(is.numeric)),
    varname = rownames(pca_no$rotation)
  ) %>%
  mutate(
    PC1s = PC1 * (soec_pca_var$evl[1] * 2.5),
    PC2s = PC2 * (soec_pca_var$evl[2] * 2.5)
  )
pca_p <- ggplot() +
  geom_segment(data = soec_pca_evc,
               aes(
                 x = origin,
                 xend = PC1s,
                 y = origin,
                 yend = PC2s
               ),
               colour = "orange") +
  geom_text_repel(
    data = soec_pca_evc,
    aes(
      x = PC1s,
      y = PC2s,
      label = variable,
      nudge_y = sign(PC2) * 0.2
    ),
    colour = "orange",
    nudge_x = 0.2
  ) +
  geom_point(data = soec_pca_pcs, aes(x = PC1, y = PC2)) +
  geom_text(
    data = filter(soec_pca_pcs, abs(PC1) > 5),
    aes(x = PC1, y = PC2, label = cnt),
    nudge_y = 0.1,
    nudge_x = -0.1
  ) +
  geom_text(
    data = filter(soec_pca_pcs, abs(PC2) > 1.69),
    aes(x = PC1, y = PC2, label = cnt),
    nudge_y = 0.1,
    nudge_x = -0.1
  ) +
  xlab("PC1") + ylab("PC2") 
pca_p+
  ggtitle("Biplot of PC1 & PC2")
```





```{r}
soec_pc_loadings <- as_tibble(pca_no$rotation[,1]) %>%
  mutate(variable = rownames(pca_no$rotation), 
         indx = 1:nrow(pca_no$rotation),
         ymin=rep(0, nrow(pca_no$rotation)))
ggplot(soec_pc_loadings) + 
geom_hline(yintercept=c(-1/sqrt(nrow(pca_no$rotation)),
                          1/sqrt(nrow(pca_no$rotation))), colour="red")+
  geom_errorbar(aes(x=indx, ymin=ymin, ymax=value), width = 0) +
  geom_point(aes(x=indx, y=value)) + 
  scale_x_continuous(breaks = c(1:9))+
  ggtitle("Loadings for PC1")
```


```{r}
library(boot)
compute_PC1 <- function(data, index) {
  pc1 <- prcomp(data[index, ], center = TRUE, scale = TRUE)$rotation[, 1]
  if (sign(pc1[1]) < 0)
    pc1 <- -pc1
  return(pc1)
}
PC1_boot <- boot(data = nooutlier%>%select_if(is.numeric), compute_PC1, R = 1000)
colnames(PC1_boot$t) <- colnames(nooutlier%>%select_if(is.numeric))
PC1_boot_ci <- as_tibble(PC1_boot$t) %>%
  gather(var, coef) %>%
  mutate(var = factor(
    var,
    levels = c(
     "Assets"  ,    "CPI", "Employees", "Ebit" ,"Liab" ,"FilingRate" , "GDP"  ,      
"PrimeFiling" ,"Sales" 
    )
  )) %>%
  group_by(var) %>%
  summarise(
    q2.5 = quantile(coef, 0.025),
    q5 = median(coef),
    q97.5 = quantile(coef, 0.975)
  ) %>%
  mutate(t0 = PC1_boot$t0)
ggplot(PC1_boot_ci, aes(x = var, y = t0)) +
  geom_hline(yintercept = 1 / sqrt(9),
             linetype = 2,
             colour = "red") +
  geom_point() +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.1) +
  geom_hline(yintercept = 0,
             size = 3,
             colour = "white") +
  xlab("") + ylab("coefficient") +
  ggtitle("Decoding variable importance using PC1 loadings")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




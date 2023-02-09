
## Reading the data 
df=read.csv('cleaned_uic.csv')

## Column Names
names(df)

## Storing all questions in a list 


qns<-names(df)[6:63]


## Checking the frequency of demographic variables 
sort(table(df$major_name),decreasing = T)

sort(table(df$race_desc),decreasing = T)

sort(table(df$student_status),decreasing = T)



## Creating New columns for race and major by grouping things together 

df$URM<-ifelse(
  (
    df$race_desc %in% c('Black or African American','American Indian or Alaska Native','Native Hawaiian or Other Pacific Islander')
  ),
  'Yes','No'
  )
sort(table(df$URM),decreasing = T)

df$new_major<-ifelse(
  (
    df$major_name %in% c('Mechanical Engineering','Engineering Physics','Chemical Engineering','Civil Engineering','Industrial Engineering')
  ),
  'Others',
          ifelse(df$major_name %in% c('Computer Science','Computer Science & Design','Data Science'),'Computer Science and Specializations',df$major_name)
)

sort(table(df$new_major),decreasing = T)

### Create a function that computes kruskal wallis test and returns p-value

kw.test<-function(statistic,group,df){
  test=kruskal.test(formula(paste(statistic,'~',group )),data=df)
  return(test$p.value)
  
}


# Mann Whitney U Test

wrs.test<-function(statistic,group,df){
  #statistic=df[statistic]
  #group=df[group]
  test=pairwise.wilcox.test(df[,statistic],df[,group], p.adjust.method = "BH")
  return(knitr::kable(test$p.value))
  
}

## Create a function to return array of p-values for kruskal wallis test

kw.arr<-function(qnarr,group,df){
  n=length(qnarr)
  out=numeric(n)
  for ( i in 1:n){
    out[i]=kw.test(qnarr[i],group,df)
    
    
  }
  
  return(out)
}

library(dplyr)
library(knitr)

## A summary function that returns summary statistic by group and question 
summ_func<-function(group,qn,df){
  x<- group_by(df, df[group]) %>%
    summarise(
      count = n(),
      mean = mean(!!as.name(qn), na.rm = TRUE),
      sd = sd(!!as.name(qn), na.rm = TRUE),
      median = median(!!as.name(qn), na.rm = TRUE),
      IQR = IQR(!!as.name(qn), na.rm = TRUE)
    )
  
  return(knitr::kable(x) )   
}



### URM 


URM_Kruskal<-kw.arr(qns,'URM',df)

urmqns<-qns[URM_Kruskal<=0.05]

for (qn in urmqns){
  print(qn)
  print(summ_func('URM',qn,df))  
}

## New Major 

NwMaj_Kruskal<-kw.arr(qns,'new_major',df)

newmajqns<-qns[NwMaj_Kruskal<=0.05]


test =wrs.test("Q8_29","new_major",df)

test

for (qn in newmajqns){
  print(qn)
  print(wrs.test(qn,"new_major",df))
  print(summ_func('new_major',qn,df))  
}


## Student Status 

studstat_kruskal=kw.arr(qns,'student_status',df)
studstat_qns<-qns[studstat_kruskal<=0.05]
studstat_qns

for (qn in studstat_qns){
  print(qn)
  print(wrs.test(qn,'student_status',df))
  print(summ_func('student_status',qn,df))  
}


## Group for Electrical 

df$elec_group=ifelse(df$major_name=='Electrical Engineering',df$major_name,'Other Majors')




eg_Kruskal<-kw.arr(qns,'elec_group',df)

eg_qns<-qns[eg_Kruskal<=0.05]

for (qn in eg_qns){
  print(qn)
  print(summ_func('elec_group',qn,df))  
}

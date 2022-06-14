#install.packages("easypackages")
library(easypackages)
install.packages("rmdformats")


#install.packages(c("boot","ggm","ggplot2","polycor","Hmisc","dplyr",
#"readxl","devtools","tidyverse","lubridate",
#"ggridges","wesanderson","RColorBrewer","knitr",
#"kableExtra","hrbrthemes","statsr","stargazer","psych",
#"corrplot","corrr","GGally","ggcorrplot","PerformanceAnalytics",
#"pander","broom","purrr","kableExtra","egg","vtable","qwraps2","rticles",
#"rmdformats","plotly"))



libraries("boot","ggm","ggplot2","polycor","Hmisc","dplyr",
          "readxl","devtools","tidyverse","lubridate",
          "ggridges","wesanderson","RColorBrewer","knitr",
          "kableExtra","hrbrthemes","statsr","stargazer","psych",
          "corrplot","corrr","GGally","ggcorrplot","PerformanceAnalytics",
          "pander","broom","purrr","kableExtra","egg", "vtable",
          "qwraps2","rticles","rmdformats", "plotly")




dir.create("Correlation_Matrices_html")
dir.create("Summary_Tables")
dir.create("Correlation_Plots")
dir.create("Regression_Tables")
dir.create("Graficas_Definitivas")



#Cargando datos

Datos_Cornel <- read_excel("Datos_Cornel.xlsx")
#Esta base no tiene al participante (MC4PB-HIK) ni los participantes en Rojo

#Exploración inicial

head(Datos_Cornel) #Primeras 10 filas

dim(Datos_Cornel) #Número de filas y número de columnas

str(Datos_Cornel) #Estructura de la bas (Tipos de variables)


#Names

names(Datos_Cornel)

Datos_Cornel<- Datos_Cornel %>% 
  rename(NL_RCTot_Pre_25=`NL_RCTot_Pre(/25)`,
         NL_RCTot_Post_25=`NL_RCTot_Post(/25)`)
         
#NAS

#Total NAS:

sum(is.na(Datos_Cornel))


#NAS por columna

map(Datos_Cornel, ~sum(is.na(.)))  


#NA graph

Datos_Cornel  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "Variables", values_to="Missing") %>%
  count(Variables, Missing) %>%
  ggplot(aes(y=Variables,x=n,fill=Missing))+
  geom_col()+
  scale_fill_brewer(palette = "Accent")+
  labs(title="NAS en la base por variables")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))



#Export

Plot0<- Datos_Cornel  %>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "Variables", values_to="Missing") %>%
  count(Variables, Missing) %>%
  ggplot(aes(y=Variables,x=n,fill=Missing))+
  geom_col()+
  scale_fill_brewer(palette = "Accent")+
  labs(title="NAS en la base por variables")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


ggsave(plot = Plot0, filename = "./Graficas_Definitivas/Barplot_NAS.png", 
       width = 11, 
       height = 7,
       type = "cairo",
       dpi = "retina")



#Summary Statistics

#Pongo también Summary porque incluye el conteo de NAS
summary(Datos_Cornel)

stargazer(as.data.frame(Datos_Cornel),
          type = "text",
          median = TRUE,
          title = "Summary Statistics",
          out="./Summary_Tables/Summary_Statistics.html")


    #Txt
    stargazer(as.data.frame(Datos_Cornel),
          median = TRUE,
          title = "Summary Statistics",
          out="./Summary_Tables/Summary_Statistics.txt")

###Comparaciones

## Cornel Pre y post Con y  Sin NAS

#Acá creo dos bases distintas. Una solo con los valores completos
#Y otra que incluye NAs.

Cornel_Pre_Post_ConNAS<- Datos_Cornel %>% 
  select(TotalCornel_Pre,TotalCornel_Post)

Cornel_Pre_Post_SinNAS<- Datos_Cornel %>% 
  select(TotalCornel_Pre,TotalCornel_Post) %>% 
  na.omit()
 

#(Sin NAS) Pre

Datos_Cornel_Pre <- Cornel_Pre_Post_SinNAS %>% 
    select(TotalCornel_Pre) %>%
  mutate(Time=as.factor("Pre"))


Datos_Cornel_Pre <- Datos_Cornel_Pre %>% 
  rename(Score_Cornel=TotalCornel_Pre) 

  

#(Sin NAS) Post
Datos_Cornel_Post <- Cornel_Pre_Post_SinNAS %>% 
  select(TotalCornel_Post) %>% 
  mutate(Time=as.factor("Post"))

Datos_Cornel_Post <- Datos_Cornel_Post %>% 
  rename(Score_Cornel=TotalCornel_Post) 


#(Sin NAS) Pre & Post

Datos_Cornel_Pre_Post_SinNAS<-rbind(Datos_Cornel_Pre,Datos_Cornel_Post)



#Boxplot COrnel Pre vs Post

Datos_Cornel_Pre_Post_SinNAS %>% 
  ggplot(aes(y=Score_Cornel,x=Time, fill= Time))+
  geom_boxplot(alpha = 0.6)+
  geom_jitter(alpha = 0.6)+
  scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
  labs(title="Cornel Scores",
       subtitle = "Pre vs Post",
       y="Score",
       x="Test")+
  stat_summary(fun.y=mean, geom="point", 
               shape=20, size=5, color="white", 
               fill="red") +
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

    #Eport

      Plot1<- Datos_Cornel_Pre_Post_SinNAS %>% 
        ggplot(aes(y=Score_Cornel,x=Time, fill= Time))+
        geom_boxplot(alpha = 0.6)+
        geom_jitter(alpha = 0.6)+
        scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
        labs(title="Cornel Scores",
             subtitle = "Pre vs Post",
             y="Score",
             x="Test")+
        stat_summary(fun.y=mean, geom="point", 
                     shape=20, size=5, color="white", 
                     fill="red") +
        theme_classic()+
        theme(axis.text.x = element_text( size = 13),#Tamaños de letra
              axis.title.y = element_text( size = 15),
              axis.title.x = element_text( size = 15),
              plot.title = element_text(size = 20,face = "bold"),
              plot.subtitle = element_text(size = 15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 13))
      
      
      ggsave(plot = Plot1, filename = "./Graficas_Definitivas/Boxplot_Cornel_Pre_Vs_Post.png", 
             width = 11, 
             height = 7,
             type = "cairo",
             dpi = "retina")

ggplotly(Plot1)
      
      
      
#Table (Match values)

#Este valor puede ser diferente al de la tabla general porque acá, 
#solo se incluyen los valores de los participantes
#que presentaron ambas pruebas

stargazer(as.data.frame(Cornel_Pre_Post_SinNAS),
          type = "text",
          median = TRUE,
          title = "Summary Statistics Cornel",
          out="./Summary_Tables/Summary_Statistics_Cornel.html")

    #Txt
    stargazer(as.data.frame(Cornel_Pre_Post_SinNAS),
          median = TRUE,
          title = "Summary Statistics Cornel",
          out="./Summary_Tables/Summary_Statistics_Cornel.txt")



## Moyenne Pre y post Con y  Sin NAS

#Acá creo dos bases distintas. Una solo con los valores completos
#Y otra que incluye NAs.

Moyenne_Pre_Post_ConNAS<- Datos_Cornel %>% 
  select(GCB_Pre_Moyenne, GCB_Post_Moyenne)

Moyenne_Pre_Post_SinNAS<- Datos_Cornel %>% 
  select(GCB_Pre_Moyenne, GCB_Post_Moyenne) %>% 
  na.omit()


#Pre

Datos_Moyenne_Pre <- Moyenne_Pre_Post_SinNAS %>% 
  select(GCB_Pre_Moyenne) %>%
  mutate(Time=as.factor("Pre"))

Datos_Moyenne_Pre <- Datos_Moyenne_Pre %>% 
  rename(Score_Moyenne=GCB_Pre_Moyenne) 


#Post

Datos_Moyenne_Post <- Moyenne_Pre_Post_SinNAS %>% 
  select(GCB_Post_Moyenne) %>%
  mutate(Time=as.factor("Post"))

Datos_Moyenne_Post <- Datos_Moyenne_Post %>% 
  rename(Score_Moyenne=GCB_Post_Moyenne) 



#Rbind Pre & Post Moyenne

Datos_Moyenne_Pre_Post_SinNAS<-rbind(Datos_Moyenne_Pre,
                                    Datos_Moyenne_Post)



#Boxplot

Datos_Moyenne_Pre_Post_SinNAS %>% 
  ggplot(aes(y=Score_Moyenne,x=Time, fill= Time))+
  geom_boxplot(alpha = 0.6)+
  geom_jitter(alpha = 0.6)+
  scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
  labs(title="Generic Conspiracy Belief Scores",
       subtitle = "Pre vs Post",
       y="Score",
       x="Test")+
  stat_summary(fun.y=mean, geom="point", 
               shape=20, size=5, color="white", 
               fill="red") +
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


      #Export
        
    Plot2 <- Datos_Moyenne_Pre_Post_SinNAS %>% 
      ggplot(aes(y=Score_Moyenne,x=Time, fill= Time))+
      geom_boxplot(alpha = 0.6)+
      geom_jitter(alpha = 0.6)+
      scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
      labs(title="Generic Conspiracy Belief Scores",
           subtitle = "Pre vs Post",
           y="Score",
           x="Test")+
      stat_summary(fun.y=mean, geom="point", 
                   shape=20, size=5, color="white", 
                   fill="red") +
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),#Tamaños de letra
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))

    ggsave(plot = Plot2, filename = "./Graficas_Definitivas/Boxplot_Moyenne_Pre_Post.png", 
       width = 11, 
       height = 7,
       type = "cairo",
       dpi = "retina")

#Table (Match values)
    
stargazer(as.data.frame(Moyenne_Pre_Post_SinNAS),
              type = "text",
              median = TRUE,
              title = "Summary Statistics Moyenne",
              out="./Summary_Tables/Summary_Statistics_Moyenne.html")

    #Txt
    stargazer(as.data.frame(Moyenne_Pre_Post_SinNAS),
          median = TRUE,
          title = "Summary Statistics Moyenne",
          out="./Summary_Tables/Summary_Statistics_Moyenne.txt")

##New Leash

##New Leash Pre y post Con y Sin NAS

#Acá creo dos bases distintas. Una solo con los valores completos
#Y otra que incluye NAs.
View(Datos_Cornel)
    
NL_Pre_Post_ConNAS<- Datos_Cornel %>% 
  select(NL_RCTot_Pre_25,NL_RCTot_Post_25)


NL_Pre_Post_SinNAS<- Datos_Cornel %>% 
  select(NL_RCTot_Pre_25,NL_RCTot_Post_25) %>% 
  na.omit()


#Pre

Datos_NL_Pre <- NL_Pre_Post_SinNAS %>% 
  select(NL_RCTot_Pre_25) %>%
  mutate(Time=as.factor("Pre"))

Datos_NL_Pre <- Datos_NL_Pre %>% 
  rename(Score_NL=NL_RCTot_Pre_25) 


#Post

Datos_NL_Post <- NL_Pre_Post_SinNAS %>% 
  select(NL_RCTot_Post_25) %>%
  mutate(Time=as.factor("Post"))

Datos_NL_Post <- Datos_NL_Post %>% 
  rename(Score_NL=NL_RCTot_Post_25) 


#Rbind Pre & Post NL

Datos_NL_Pre_Post_SinNAS<-rbind(Datos_NL_Pre,
                                Datos_NL_Post)


#Boxplot

Datos_NL_Pre_Post_SinNAS %>% 
  ggplot(aes(y=Score_NL,x=Time, fill= Time))+
  geom_boxplot(alpha = 0.6)+
  geom_jitter(alpha = 0.6)+
  scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
  labs(title="New Leash Scores",
       subtitle = "Pre vs Post",
       y="Score",
       x="Test")+
  stat_summary(fun.y=mean, geom="point", 
               shape=20, size=5, color="white", 
               fill="red") +
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),#Tamaños de letra
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


      #Export
      
      Plot3<-Datos_NL_Pre_Post_SinNAS %>% 
        ggplot(aes(y=Score_NL,x=Time, fill= Time))+
        geom_boxplot(alpha = 0.6)+
        geom_jitter(alpha = 0.6)+
        scale_fill_manual(values = c("#F652A0", "#36EEE0"))+
        labs(title="New Leash Scores",
             subtitle = "Pre vs Post",
             y="Score",
             x="Test")+
        stat_summary(fun.y=mean, geom="point", 
                     shape=20, size=5, color="white", 
                     fill="red") +
        theme_classic()+
        theme(axis.text.x = element_text( size = 13),#Tamaños de letra
              axis.title.y = element_text( size = 15),
              axis.title.x = element_text( size = 15),
              plot.title = element_text(size = 20,face = "bold"),
              plot.subtitle = element_text(size = 15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 13)) 
          
      ggsave(plot = Plot3, filename = "./Graficas_Definitivas/Boxplot_New_Leash_Pre_Post.png", 
             width = 11, 
             height = 7,
             type = "cairo",
             dpi = "retina")

      
#Table (Match values)
      
  stargazer(as.data.frame(NL_Pre_Post_SinNAS),
                type = "text",
                median = TRUE,
                title = "Summary Statistics New Leash",
                out="./Summary_Tables/Summary_Statistics_New_Leash.html")
  

  #Txt
  stargazer(as.data.frame(NL_Pre_Post_SinNAS),
            median = TRUE,
            title = "Summary Statistics New Leash",
            out="./Summary_Tables/Summary_Statistics_New_Leash.txt")
      

    

##Correlaciones
Corelations <- Datos_Cornel %>% 
  select(GCB_Pre_Moyenne,TotalCornel_Pre,NL_RCTot_Pre_25)
      
#Corplot
      
ggcorrplot(cor(Corelations,use = "pairwise.complete.obs"),
           hc.order=TRUE, type='lower',lab = TRUE,
           title = "Corelations",
           ggtheme = ggplot2::theme_minimal(),
           colors = c("#F652A0","white","#36EEE0"))

    #Export
  
    ggcorrplot<- ggcorrplot(cor(Corelations,use = "pairwise.complete.obs"),
                                                      hc.order=TRUE, type='lower',lab = TRUE,
                                                      title = "Corelations",
                                                      ggtheme = ggplot2::theme_minimal(),
                                                      colors = c("#F652A0","white","#36EEE0"))

    ggsave(plot = ggcorrplot, filename = "./Correlation_Plots/Corplot.png", 
       width = 9, 
       height = 7,
       type = "cairo",
       dpi = "retina")

    
   #Corelation Matrix Stargazer
    
    Correlation_Matrix <-Datos_Cornel %>% 
      select(2:7) %>% 
      cor(use = "pairwise.complete.obs")
    
    
    stargazer(Correlation_Matrix,
              title="Correlation Matrix",
              type = "text",
              out = "./Correlation_Matrices_html/Correlation_Matrix.html")
    
    
    #txt
    stargazer(Correlation_Matrix,
              title="Correlation Matrix",
              out = "./Correlation_Matrices_html/Correlation_Matrix.txt")

#Scatterplots 

  ##GCB_Pre_Moyenne vs TotalCornel_Pre

  Corelations%>%
  select(GCB_Pre_Moyenne,TotalCornel_Pre) %>% 
  na.omit() %>% 
  ggplot(aes(y=GCB_Pre_Moyenne,x= TotalCornel_Pre))+
  geom_point(alpha = 0.5)+
  geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
  labs(title="GCB_Pre_Moyenne vs Cornel Pre")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


    #Export

    Scatter_1<- Corelations%>%
    select(GCB_Pre_Moyenne,TotalCornel_Pre) %>% 
    na.omit() %>% 
    ggplot(aes(y=GCB_Pre_Moyenne,x= TotalCornel_Pre))+
    geom_point(alpha = 0.5)+
    geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
    labs(title="GCB_Pre_Moyenne vs Cornel Pre")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))

     ggsave(plot =Scatter_1, 
       filename = "./Graficas_Definitivas/Scatter_GCB_Pre_Moyenne_vs_Cornel_Pre.png", 
       width = 9, height = 7,
       type= "cairo",
       dpi = "retina")


    #Regression table:

    #Cuando hago las tablas con Stargazer pongo "Text" en type
    #Para que la tabla me aparezca en el programa, pero dejo
    #el formato de out en ".html"

    m1_GCB_VS_TotalCornel_Pre<-lm(GCB_Pre_Moyenne ~ TotalCornel_Pre,
                      data = Datos_Cornel)
    
    summary(m1_GCB_VS_TotalCornel_Pre)

    #Export

    stargazer(m1_GCB_VS_TotalCornel_Pre,
          ci = TRUE,
          type = "text",
          title="GCB Pre Moyenne ~ Total Cornel Pre",
          align=TRUE,
          style = "all",
          out = "Regression_Tables/m1_GCB_VS_TotalCornel_Pre.html")


#Cornel Post VS NL_RCTot_Pre_25

  Corelations%>%
  select(GCB_Pre_Moyenne,NL_RCTot_Pre_25) %>% 
  na.omit() %>% 
  ggplot(aes(y=GCB_Pre_Moyenne,x= NL_RCTot_Pre_25))+
  geom_point(alpha = 0.5)+
  geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
  labs(title="GCB_Pre_Moyenne vs NL_RCTot_Pre_25")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))
  
  #Export
  
  Scatter_2 <-  Corelations%>%
    select(GCB_Pre_Moyenne,NL_RCTot_Pre_25) %>% 
    na.omit() %>% 
    ggplot(aes(y=GCB_Pre_Moyenne,x= NL_RCTot_Pre_25))+
    geom_point(alpha = 0.5)+
    geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
    labs(title="GCB_Pre_Moyenne vs NL_RCTot_Pre_25")+
    theme_classic()+
    theme(axis.text.x = element_text( size = 13),
          axis.title.y = element_text( size = 15),
          axis.title.x = element_text( size = 15),
          plot.title = element_text(size = 20,face = "bold"),
          plot.subtitle = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 13))
  
  
  ggsave(plot =Scatter_2, 
         filename = "./Graficas_Definitivas/Scatter_GCB_Pre_Moyenne vs NL_RCTot_Pre_25.png", 
         width = 9, height = 7,
         type= "cairo",
         dpi = "retina")
  

    #Regression table:


    m1_GCB_VS_NL_RCTot_Pre<-lm( GCB_Pre_Moyenne ~ NL_RCTot_Pre_25,
                               data = Datos_Cornel)

    summary(m1_GCB_VS_NL_RCTot_Pre)

    #Export

    stargazer(m1_GCB_VS_NL_RCTot_Pre,
          ci = TRUE,
          type = "text",
          title="GCB Pre Moyenne ~ NL RCTot Pre 25",
          align=TRUE,
          style = "all",
          out = "Regression_Tables/m1_GCB_VS_NL_RCTot_Pre.html")

    
    
#Cornel Pre VS NL_RCTot_Pre_25

Corelations%>%
  select(TotalCornel_Pre,NL_RCTot_Pre_25) %>% 
  na.omit() %>% 
  ggplot(aes(y=NL_RCTot_Pre_25,x= TotalCornel_Pre))+
  geom_point(alpha = 0.5)+
  geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
  labs(title="NL_RCTot_Pre_25 vs TotalCornel_Pre")+
  theme_classic()+
  theme(axis.text.x = element_text( size = 13),
        axis.title.y = element_text( size = 15),
        axis.title.x = element_text( size = 15),
        plot.title = element_text(size = 20,face = "bold"),
        plot.subtitle = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))


    #Export
    Scatter_3 <- Corelations%>%
      select(TotalCornel_Pre,NL_RCTot_Pre_25) %>% 
      na.omit() %>% 
      ggplot(aes(y=NL_RCTot_Pre_25,x= TotalCornel_Pre))+
      geom_point(alpha = 0.5)+
      geom_smooth(method=lm, color="darkred", fill="#36EEE0")+
      labs(title="NL_RCTot_Pre_25 vs TotalCornel_Pre")+
      theme_classic()+
      theme(axis.text.x = element_text( size = 13),
            axis.title.y = element_text( size = 15),
            axis.title.x = element_text( size = 15),
            plot.title = element_text(size = 20,face = "bold"),
            plot.subtitle = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13))

    
    ggsave(plot =Scatter_3, 
           filename = "./Graficas_Definitivas/Scatter_NL_RCTot_Pre_25_vs_TotalCornel_Pre.png", 
           width = 9, height = 7,
           type= "cairo",
           dpi = "retina")

#Regression table:


m1_NL_RCTot_Pre_25_VS_TotalCornel_Pre <-lm(NL_RCTot_Pre_25 ~ TotalCornel_Pre,
                            data = Datos_Cornel)

summary(m1_NL_RCTot_Pre_25_VS_TotalCornel_Pre)

#Export

stargazer(m1_NL_RCTot_Pre_25_VS_TotalCornel_Pre,
          ci = TRUE,
          type = "text",
          title="NL RCTot Pre 25 ~ TotalCornel Pre",
          align=TRUE,
          style = "all",
          out = "Regression_Tables/m1_NL_RCTot_Pre_25_VS_TotalCornel_Pre.html")


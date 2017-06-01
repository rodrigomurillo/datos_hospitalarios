library(FactoMineR)
library(ggplot2)
library(dplyr)
library(factoextra)
library(gplots)
library(stringr)

rm(list=ls())
setwd("C:/Users/stuka/Desktop/datos_hospitalarios/")
eventos <- readRDS("./DB/Datos_hospitalarios.rds")

tab <- as.data.frame(head(sort(table(eventos$DscAfecprinGrouped),decreasing=T),10))
tab$Var1 <- tab$Var1 %>% as.character() 
tab$Var1 <- c("Efecto tóxico del contacto con animales venenosos","Herida de la cabeza","Herida de la muñeca y de la mano","Síndrome del maltrato","Traumatismos superficiales que afectan múltiples regiones del cuerpo","Traumatismo superficial de la cabeza","Traumatismo de regiones no especificadas del cuerpo","Fractura del antebrazo","Herida de la pierna","Traumatismo intracraneal")
tab$Var1 <- tab$Var1 %>% factor()
tab <- tab[order(-tab$Freq),]
     
ggplot(tab,aes(x=reorder(Var1,-Freq),y=Freq)) + geom_col() +
  ggtitle("Top 10 Afecciones principales") +
  labs(x="Afecciones",y="# Casos") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  geom_text(
            aes(label=format(Freq, big.mark = ",", scientific = FALSE)),
            size=6,hjust=0.5, vjust=-0.5) +
  scale_y_continuous(limits = c(0,max(tab$Freq)*1.05),labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90,size=12,vjust=0.3))

tab <- as.data.frame(head(sort(table(eventos$DscCausaExtGrouped),decreasing=T),10))
tab$Var1 <- tab$Var1 %>% as.character() 
#paste(tab$Var1,collapse="\",\"")
tab$Var1 <- c("Contacto traumático con escorpión","Otros maltratos","Otras caídas en el mismo nivel","Caída en el mismo nivel por deslizamiento, tropezón y traspié","Golpe contra o golpeado por otros objetos","Caída no especificada","Mordedura o ataque de perro","Exposición a factores no especificados","Agresión con fuerza corporal","Ocupante de automóvil lesionado en otros accidentes de transporte, y en los no especificados")
tab$Var1 <- tab$Var1 %>% factor()
tab <- tab[order(-tab$Freq),]

ggplot(tab,aes(x=reorder(Var1,-Freq),y=Freq)) + geom_col() +
  ggtitle("Top 10 Causas principales") +
  labs(x="Causas Externas",y="# Casos") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  geom_text(
    aes(label=format(Freq, big.mark = ",", scientific = FALSE)),
    size=6,hjust=0.5, vjust=-0.5) +
  scale_y_continuous(limits = c(0,max(tab$Freq)*1.05),labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90,size=12,vjust=0.3))

ggplot(eventos[which(!is.na(eventos$CodSexo)),],aes(x=CodSexo,fill=CodSexo)) + geom_bar() +
  ggtitle("Sexo del afectado") +
  labs(x="Sexo",y="# Casos") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  geom_text(stat='count',
    aes(label=format(..count.., big.mark = ",", scientific = FALSE)),
    size=6,hjust=1.5, vjust=0) +
  guides(fill=FALSE) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90,size=12,vjust=0.3))

ggplot(eventos[which(!is.na(eventos$Edad_Cut)),],aes(x=Edad_Cut,fill=Edad_Cut)) + geom_bar() +
  ggtitle("Edad del afectado") +
  labs(x="Edad",y="# Casos") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  geom_text(stat='count',
            aes(label=format(..count.., big.mark = ",", scientific = FALSE)),
            size=6,hjust=1.1, vjust=0.5) +
  guides(fill=FALSE) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90,size=12,vjust=0))

ggplot(eventos[which(!is.na(eventos$CodEscolaridad)),],aes(x=CodEscolaridad,fill=CodEscolaridad)) + geom_bar() +
  ggtitle("Escolaridad del afectado") +
  labs(x="Escolaridad",y="# Casos") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  geom_text(stat='count',
            aes(label=format(..count.., big.mark = ",", scientific = FALSE)),
            size=4,hjust=0.8, vjust=0.5) +
  guides(fill=FALSE) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90,size=12,vjust=0))

ggplot(eventos[which(!is.na(eventos$CodLeeEscribe)),],aes(x=CodLeeEscribe,fill=CodLeeEscribe)) + geom_bar() +
  ggtitle("Alfabetismo del afectado") +
  labs(x="Sabe leer y escribir",y="# Casos") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  geom_text(stat='count',
            aes(label=format(..count.., big.mark = ",", scientific = FALSE)),
            size=6,hjust=1.1, vjust=0) +
  guides(fill=FALSE) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90,size=12,vjust=0.3))

ggplot(eventos[which(!is.na(eventos$CodDerechohabiencia)),],aes(x=reorder(CodDerechohabiencia,table(eventos$CodDerechohabiencia)[eventos$CodDerechohabiencia]),fill=CodDerechohabiencia)) + geom_bar() +
  ggtitle("Derechohabiencia del afectado") +
  labs(x="Tipo de seguro",y="# Casos") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  geom_text(stat='count',
            aes(label=format(..count.., big.mark = ",", scientific = FALSE)),
            size=4,hjust=0.6, vjust=0.5) +
  guides(fill=FALSE) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(axis.text.x = element_text(angle=90,size=12,vjust=0.3))


temp <- eventos
temp <- temp[!is.na(temp$CodSexo),]
temp <- temp[!is.na(temp$CodEscolaridad),]
temp <- temp[!is.na(temp$CodLeeEscribe),]
temp <- temp[!is.na(temp$CodAgente),]
temp <- temp[!is.na(temp$CodDerechohabienciaRegroup),]
temp <- temp[!is.na(temp$Edad_Cut),]
temp_sample <- temp[base::sample(temp$ID,300000,replace=F),]

tab <- table(temp$CodEscolaridad,temp$Edad_Cut)

cols <-c("Edad_Cut","CodEscolaridad")
mca <- MCA(eventos[c(cols)])

cats = apply(eventos[c(cols)], 2, function(x) nlevels(as.factor(x)))
cats = cats +1

mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))
mca_obs_df = data.frame(mca$ind$coord)


# Grafica de las categorias de las variables
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("Grafica de ACM de variables de STUDENT en R con FactoMineR")
  
data(housetasks)
head(housetasks)
class(housetasks)



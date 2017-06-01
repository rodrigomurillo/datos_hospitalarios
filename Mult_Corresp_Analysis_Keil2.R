library(FactoMineR)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggrepel)

getwd()
setwd("C:/Users/stuka/Desktop/datos_hospitalarios/")
db <- readRDS("./DB/Datos_hospitalarios.rds")

tab2 <- as.data.frame(head(sort(table(db$DscAfecprinGrouped),decreasing=T),10))
tab2$Var1 <- tab$Var1 %>% as.character() 
tab$Var1 <- c("Efecto tóxico del contacto con animales venenosos","Herida de la cabeza","Herida de la muñeca y de la mano","Síndrome del maltrato","Traumatismos superficiales que afectan múltiples regiones del cuerpo","Traumatismo superficial de la cabeza","Traumatismo de regiones no especificadas del cuerpo","Fractura del antebrazo","Herida de la pierna","Traumatismo intracraneal")
tab$Var1 <- tab$Var1 %>% factor()
tab <- tab[order(-tab$Freq),]

db_anal <- db[which(db$DscDiagnosticoGrouped %in% tab$Var1),]


##################### Causas

tab <- as.data.frame(head(sort(table(db$DscCausaExtGrouped),decreasing=T),10))
tab$Var1 <- tab$Var1 %>% as.character() 
#paste(tab$Var1,collapse="\",\"")
tab$Var1 <- c("Contacto traumático con escorpión","Otros maltratos","Otras caídas en el mismo nivel","Caída en el mismo nivel por deslizamiento, tropezón y traspié","Golpe contra o golpeado por otros objetos","Caída no especificada","Mordedura o ataque de perro","Exposición a factores no especificados","Agresión con fuerza corporal","Ocupante de automóvil lesionado en otros accidentes de transporte, y en los no especificados")
tab$Var1 <- tab$Var1 %>% factor()
tab <- tab[order(-tab$Freq),]

db_anal2 <- db_anal

###################################### Test 2
# Interesante pero genera muchas broncas

tab <- as.data.frame(head(sort(table(db$DscCausaExtGrouped),decreasing=T),10))
tab$Var1 <- tab$Var1 %>% as.character() 

tab2 <- as.data.frame(head(sort(table(db$DscAfecprinGrouped),decreasing=T),10))
tab2$Var1 <- tab2$Var1 %>% as.character() 

db_anal <- db[which((db$DscCausaExtGrouped %in% tab$Var1) & (db$DscAfecprinGrouped %in% tab2$Var1)),]

test <- data.frame(paste0(db_anal$DscCausaExtGrouped,"-",db_anal$DscAfecprinGrouped))
names(test) <- "Nombre"
tab3 <- as.data.frame(sort(table(test$Nombre),decreasing=T))
write.csv(tab3,"test_keil.csv")
tab <- as.data.frame(head(sort(table(db$DscCausaExtGrouped),decreasing=T),10))

##################################################### db_anal2

db_anal2$CodDerechohabienciaRegroup2 <-as.character(db_anal2$CodDerechohabienciaRegroup)
db_anal2$CodDerechohabienciaRegroup2 <- ifelse(db_anal2$CodDerechohabienciaRegroup2 %in% c("SE IGNORA","NINGUNA"),"No Tiene",db_anal2$CodDerechohabienciaRegroup2)
db_anal2$CodDerechohabienciaRegroup2 <- ifelse(db_anal2$CodDerechohabienciaRegroup2 %in% c("FUERZAS ARMADAS","IMSS","SEGURO ESTATAL","SEGURO PRIVADO"),"Tiene",db_anal2$CodDerechohabienciaRegroup2)
db_anal2$CodDerechohabienciaRegroup2 <- ifelse(db_anal2$CodDerechohabienciaRegroup2 %in% c("PROGRAMA SOCIAL"),"Prog Soc",db_anal2$CodDerechohabienciaRegroup2)
db_anal2$CodDerechohabienciaRegroup2 <- factor(db_anal2$CodDerechohabienciaRegroup2)
table(db_anal2$CodDerechohabienciaRegroup2)
db_anal2$CodDerechohabienciaRegroup2 <- factor(db_anal2$CodDerechohabienciaRegroup2,levels=c("No Tiene","Prog Soc","Tiene"), ordered=T)


db_anal2$Edad_Group <-as.character(db_anal2$Edad_Cut)
db_anal2$Edad_Group <- ifelse(db_anal2$Edad_Group %in% c("[  0,  5)"),"Bebe",db_anal2$Edad_Group)
db_anal2$Edad_Group <- ifelse(db_anal2$Edad_Group %in% c("[  5, 12)"),"Niño_a",db_anal2$Edad_Group)
db_anal2$Edad_Group <- ifelse(db_anal2$Edad_Group %in% c("[ 12, 18)"),"Adolesc",db_anal2$Edad_Group)
db_anal2$Edad_Group <- ifelse(db_anal2$Edad_Group %in% c("[ 18, 30)"),"Joven",db_anal2$Edad_Group)
db_anal2$Edad_Group <- ifelse(db_anal2$Edad_Group %in% c("[ 30, 55)"),"Adulto",db_anal2$Edad_Group)
db_anal2$Edad_Group <- ifelse(db_anal2$Edad_Group %in% c("[ 55,130]"),"Adulto Mayor",db_anal2$Edad_Group)
table(db_anal2$Edad_Group)

db_anal2$Edad_Group <- factor(db_anal2$Edad_Group,levels=c("Bebe","Niño_a","Adolesc","Joven","Adulto","Adulto Mayor"), ordered=T)


db_anal2$Escolaridad_Grouped <-as.character(db_anal2$CodEscolaridad)
db_anal2$Escolaridad_Grouped
db_anal2$Escolaridad_Grouped <- ifelse(db_anal2$Escolaridad_Grouped  %in% c("PRIMARIA","SECUNDARIA"),"PRIM_SEC",db_anal2$Escolaridad_Grouped)
db_anal2$Escolaridad_Grouped <- ifelse(db_anal2$Escolaridad_Grouped  %in% c("OTRA","BACHILLERATO"),"PREPA",db_anal2$Escolaridad_Grouped)
db_anal2$Escolaridad_Grouped <- ifelse(db_anal2$Edad_Group  %in% c("Bebe","Niño_a","Adolesc"),"ES MENOR",db_anal2$Escolaridad_Grouped)

table(db_anal2$Escolaridad_Grouped)



db_anal2$Escolaridad_Grouped <- factor(db_anal2$Escolaridad_Grouped,levels=c("ES MENOR","NINGUNA","PRIM_SEC","PREPA","SUPERIOR"), ordered=T)

db_anal2 <- db_anal2[!is.na(db_anal2$CodSexo),]
db_anal2 <- db_anal2[!is.na(db_anal2$Edad_Group),]
db_anal2 <- db_anal2[!is.na(db_anal2$Escolaridad_Grouped),]

# Combinar Edad y Escolaridad
db_anal2$Socio_Dem <- as.factor(paste(db_anal2$CodSexo,db_anal2$Edad_Cut, db_anal2$CodEscolaridad,db_anal2$CodDerechohabienciaRegroup, sep = "_"))

tab4 <- as.data.frame(sort(table(db_anal2$Socio_Dem),decreasing=T))
tab4$Var1 <- tab4$Var1 %>% as.character() 

tab5 <- as.data.frame(sort(table(db_anal2$DscAfecprinGrouped),decreasing=T))
tab5$Var1 <- tab5$Var1 %>% as.character() 
tab5 <- tab5[which(tab5$Freq != 0),]

tab6 <- as.data.frame(sort(table(db_anal2$DscCausaExtGrouped),decreasing=T))
tab6$Var1 <- tab6$Var1 %>% as.character() 
tab6 <- tab6[which(tab6$Freq != 0),]

write.csv(tab4,"SocioDem_Cat.csv")
write.csv(tab5,"Afec_Cat.csv")
write.csv(tab6,"Causa_Cat.csv")

Afec_Cat <- read.csv("Afec_Cat.csv")
Afec_Cat$Freq <- NULL
db_anal3 <- left_join(db_anal2,Afec_Cat)

###########################3 Recargar catalogos
Afec_Cat <- read.csv("Afec_Cat.csv")
Afec_Cat$Freq <- NULL
db_anal2 <- left_join(db_anal2,Afec_Cat)

Causa_Cat <- read.csv("Causa_Cat.csv")
Causa_Cat$Freq <- NULL
db_anal2 <- left_join(db_anal2,Causa_Cat)


cols <-c("CodSexo","Edad_Group","Escolaridad_Grouped","CodDerechohabienciaRegroup2","Afeccion","Causa")
rm(mca)
mca <- MCA(db_anal2[c(cols)], graph = F)
tab <- cbind(rownames(mca$eig),mca$eig$`percentage of variance`)
write.csv(tab,"dimensiones.csv")

cats = apply(db_anal2[c(cols)], 2, function(x) nlevels(as.factor(x)))


mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))
mca_obs_df = data.frame(mca$ind$coord)


# Grafica de las categorias de las variables
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_label_repel(aes(colour = Variable),fontface="bold",size=3) + 
  ggtitle("Grafica de MCA: Afeccion, Causa y Socio demograficas - Dim 1 vs Dim 2 = Varianza explicada 17%") +
  xlab("Dimensión 1 - 10% de la varianza") +
  ylab("Dimensión 2 - 7% de la varianza") +
  theme(legend.position="bottom")
ggsave("./Images/Grafica_Afec_Causa_Sociodem_dim1_2.png", plot=last_plot(), width = 45, height = 45/2.5, units = "cm")

ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.3, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_label_repel(aes(colour = Variable),fontface="bold",size=3) + 
  ggtitle("Grafica de MCA: Afeccion, Causa y Socio demograficas - Dim 1 vs Dim 3 = Varianza explicada 16.5%") +
  xlab("Dimensión 1 - 10% de la varianza") +
  ylab("Dimensión 3 - 6.5% de la varianza") +
  theme(legend.position="bottom")

ggplot(data = mca_vars_df, aes(x = Dim.2, y = Dim.3, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_label_repel(aes(colour = Variable),fontface="bold",size=3) + 
  ggtitle("Grafica de MCA: Afeccion, Causa y Socio demograficas - Dim 2 vs Dim 3 = Varianza explicada 13.5%") +
  xlab("Dimensión 2 - 7% de la varianza") +
  ylab("Dimensión 3 - 6.5% de la varianza") +
  theme(legend.position="bottom")

?geom_text_repel
#------------------------
cols <-c("CodArea","CodSexo","CodDerechohabienciaRegroup")
rm(mca)
mca <- MCA(db[c(cols)], graph = F)
mca$eig

cats = apply(db[c(cols)], 2, function(x) nlevels(as.factor(x)))


mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))
mca_obs_df = data.frame(mca$ind$coord)


# Grafica de las categorias de las variables
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("Grafica de MCA: Área afectada, Sexo y tipo de seguro")
ggsave("../Images/Grafica_Sexo_Area_Seguro.png", plot=last_plot(), width = 45, height = 45/2.5, units = "cm")

#-------------Causas-----
db <- readRDS("../DB/Datos_hospitalarios_completa.rds")

db <- db[!is.na(db$CodSexo),]
db <- db[!is.na(db$CodEscolaridad),]
db <- db[!is.na(db$CodLeeEscribe),]
db <- db[!is.na(db$CodArea),]
db <- db[!is.na(db$Edad_Cut),]
db <- db[!is.na(db$CodDerechohabienciaRegroup),]

# Combinar Edad y Escolaridad
db$Edad_Escolaridad <- as.factor(paste(db$Edad_Cut, db$CodEscolaridad, sep = "_"))
db$Edad_Lee <- as.factor(paste(db$Edad_Cut, db$CodLeeEscribe, sep = "_"))

causas <- names(sort(summary(db$DscCausaExt), decreasing = T)[2:11])

db_causas <- db[db$DscCausaExt == causas,]


cols <-c("CodSexo","DscCausaExt","Edad_Escolaridad")
rm(mca)
mca <- MCA(db_causas[c(cols)], graph = F)
mca$eig

cats = apply(db_causas[c(cols)], 2, function(x) nlevels(as.factor(x)))

mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))
mca_obs_df = data.frame(mca$ind$coord)

# Grafica de las categorias de las variables
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("Grafica de MCA: Sexo, Causa, Edad y Escolaridad")
ggsave("../Images/Grafica_Sexo_Causa_Edad_Escolaridad.png", plot=last_plot(), width = 45, height = 45/2.5, units = "cm")


cols <-c("CodSexo","DscCausaExt","Edad_Lee")
rm(mca)
mca <- MCA(db_causas[c(cols)], graph = F)
mca$eig

cats = apply(db_causas[c(cols)], 2, function(x) nlevels(as.factor(x)))


mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))
mca_obs_df = data.frame(mca$ind$coord)


# Grafica de las categorias de las variables
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("Grafica de MCA: Sexo, Causa, Edad y Alfabetismo")
ggsave("./Images/Grafica_Sexo_Causa_Edad_Lee.png", plot=last_plot(), width = 45, height = 45/2.5, units = "cm")

#------Beneficiarios de los seguros

cols <-c("CodDerechohabienciaRegroup","CodEscolaridad","Edad_Cut")
rm(mca)
mca <- MCA(db_causas[c(cols)], graph = F)
mca$eig

cats = apply(db_causas[c(cols)], 2, function(x) nlevels(as.factor(x)))


mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))
mca_obs_df = data.frame(mca$ind$coord)


# Grafica de las categorias de las variables
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("Grafica de MDS")


#-----Análisis FInal

causas <- names(sort(summary(db$DscAfecprinGrouped), decreasing = T)[1:10])
db_aux <- db[db$DscAfecprinGrouped == causas,]


cols <-c("DscAfecprinGrouped","CodEscolaridad","Edad_Cut")
rm(mca)
mca <- MCA(db_aux[c(cols)], graph = F)
mca$eig

cats = apply(db_aux[c(cols)], 2, function(x) nlevels(as.factor(x)))


mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))
mca_obs_df = data.frame(mca$ind$coord)


# Grafica de las categorias de las variables
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("Grafica de MDS")
ggsave("./Images/Grafica_Sexo_Causa_Edad_Lee.png", plot=last_plot(), width = 45, height = 45/2.5, units = "cm")

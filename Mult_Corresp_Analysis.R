library(FactoMineR)
library(ggplot2)
library(lubridate)

db <- readRDS("../DB/Datos_hospitalarios_completa.rds")

db <- db[!is.na(db$CodSexo),]
db <- db[!is.na(db$CodEscolaridad),]
db <- db[!is.na(db$CodLeeEscribe),]
db <- db[!is.na(db$CodArea),]
db <- db[!is.na(db$Edad_Cut),]
db <- db[!is.na(db$CodDerechohabienciaRegroup),]

set.seed(110183)
db <- db[sample(nrow(db), 1e6, replace = F),]


cols <-c("CodArea","CodSexo","Edad_Cut")
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
  ggtitle("Grafica de MCA: Área afectada, Sexo y edad")
ggsave("../Images/Grafica_Sexo_Area_Edad.png", plot=last_plot(), width = 45, height = 45/2.5, units = "cm")

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
ggsave("../Images/Grafica_Sexo_Causa_Edad_Lee.png", plot=last_plot(), width = 45, height = 45/2.5, units = "cm")

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



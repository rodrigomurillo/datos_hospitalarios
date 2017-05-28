library(FactoMineR)
library(ggplot2)

db <- readRDS("../DB/Datos_hospitalarios_completa.rds")

db <- db[!is.na(db$CodSexo),]
db <- db[!is.na(db$CodEscolaridad),]
db <- db[!is.na(db$CodLeeEscribe),]
db <- db[!is.na(db$CodArea),]

# db$Seguro <- ifelse(db$CodDerechohabiencia == "SEGURO GRATUIDAD" | db$CodDerechohabiencia == "PROSPERA")

cols <-c("CodSexo","CodLeeEscribe","CodEscolaridad","CodDerechohabiencia","CodArea")
mca <- MCA(db[c(cols)])

cats = apply(db[c(cols)], 2, function(x) nlevels(as.factor(x)))


mca_vars_df = data.frame(mca$var$coord, Variable = rep(names(cats), cats))
mca_obs_df = data.frame(mca$ind$coord)


# Grafica de las categorias de las variables
ggplot(data = mca_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_text(aes(colour = Variable)) + 
  ggtitle("Grafica de ACM de variables de STUDENT en R con FactoMineR")




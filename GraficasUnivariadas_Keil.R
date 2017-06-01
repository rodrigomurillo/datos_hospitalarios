####################### Analisis Exploratorio
library(Hmisc)
library(BAMMtools)

for(i in 3:ncol(eventos)){
  clase <- class(eventos[[i]])
  if(clase %in% c("factor","ordered")){
    ggplot(eventos,aes(x=reorder(eventos[[i]],-table(eventos[[i]])[eventos[[i]]]))) + geom_bar() +
      ggtitle(names(eventos)[i]) +
      labs(x=names(eventos)[i],y="COnteo") +
      theme(axis.text.x = element_text(angle=90,size=8))
    ggsave(paste0("./Graph/",names(eventos)[i],".png"))
  }
  else{
    if(clase %in% c("Date","numeric","integer")){
      ggplot(eventos,aes(x=eventos[[i]])) + geom_bar() +
        ggtitle(names(eventos)[i]) +
        labs(x=names(eventos)[i],y="COnteo") +
        theme(axis.text.x = element_text(angle=90,size=8))
    }
    else{
      if(clase %in% c("Duration")){
        ggplot(eventos,aes(x=eventos[[i]]@.Data)) + geom_bar() +
          ggtitle(names(eventos)[i]) +
          labs(x=names(eventos)[i],y="COnteo") +
          theme(axis.text.x = element_text(angle=90,size=8))
      }
    }
  }
}

names(eventos)
ggplot(eventos,aes(x=eventos[[42]]@.Data)) + geom_bar() +
  ggtitle(names(eventos)[42]) +
  labs(x=names(eventos)[42],y="COnteo") +
  theme(axis.text.x = element_text(angle=90,size=8))

ggplot(eventos,aes(x=reorder(eventos[[42]]@.Data,-table(eventos[[42]]@.Data)[eventos[[42]]@.Data]))) + geom_bar() +
  ggtitle(names(eventos)[4]) +
  labs(x=names(eventos)[4],y="COnteo") +
  theme(axis.text.x = element_text(angle=90,size=8))

sapply(sapply(eventos,class),first)
names(eventos)

eventos$Edad_Cut <- cut2(eventos$Edad,c(5,12,18,30,55))
eventos$EdadAgresor_Cut <- cut2(eventos$EdadAgresor,c(5,12,18,30,55))
table(eventos$CodSexo)
table(eventos$CodLeeEscribe)
table(eventos$CodEscolaridad)
table(eventos$CodDerechohabiencia)
eventos$CodDerechohabienciaRegroup <- ifelse(eventos$CodDerechohabiencia %in% c("SEGURO GRATUIDAD","PROSPERA","SEGURO POPULAR"),"PROGRAMA SOCIAL",eventos$CodDerechohabiencia)
eventos$CodDerechohabienciaRegroup <- ifelse(eventos$CodDerechohabiencia %in% c("GOBIERNO ESTATAL","ISSSTE","PEMEX"),"SEGURO ESTATAL",eventos$CodDerechohabienciaRegroup)
eventos$CodDerechohabienciaRegroup <- ifelse(eventos$CodDerechohabiencia %in% c("IMSS"),"IMSS",eventos$CodDerechohabienciaRegroup)
eventos$CodDerechohabienciaRegroup <- ifelse(eventos$CodDerechohabiencia %in% c("SEGURO PRIVADO"),"SEGURO PRIVADO",eventos$CodDerechohabienciaRegroup)
eventos$CodDerechohabienciaRegroup <- ifelse(eventos$CodDerechohabiencia %in% c("SEDENA","SEMAR"),"FUERZAS ARMADAS",eventos$CodDerechohabienciaRegroup)
eventos$CodDerechohabienciaRegroup <- ifelse(eventos$CodDerechohabiencia %in% c("NINGUNA"),"NINGUNA",eventos$CodDerechohabienciaRegroup)
eventos$CodDerechohabienciaRegroup <- ifelse(eventos$CodDerechohabiencia %in% c("SE IGNORA"),"SE IGNORA",eventos$CodDerechohabienciaRegroup)
table(eventos$CodDerechohabienciaRegroup)

eventos$CodDerechohabienciaRegroup <- factor(eventos$CodDerechohabienciaRegroup,levels=c("SE IGNORA","NINGUNA","PROGRAMA SOCIAL","SEGURO ESTATAL","IMSS","SEGURO PRIVADO","FUERZAS ARMADAS"),ordered=TRUE)

###############################3 ANalisis final
eventos <- db_anal2

for(i in 3:ncol(eventos)){
  clase <- class(eventos[[i]])
  if(clase %in% c("factor","ordered")){
    ggplot(eventos,aes(x=reorder(eventos[[i]],-table(eventos[[i]])[eventos[[i]]]))) + geom_bar() +
      ggtitle(names(eventos)[i]) +
      labs(x=names(eventos)[i],y="COnteo") +
      theme(axis.text.x = element_text(angle=90,size=8))
    ggsave(paste0("./Graph/",names(eventos)[i],".png"))
  }
  else{
    if(clase %in% c("Date","numeric","integer")){
      ggplot(eventos,aes(x=eventos[[i]])) + geom_bar() +
        ggtitle(names(eventos)[i]) +
        labs(x=names(eventos)[i],y="COnteo") +
        theme(axis.text.x = element_text(angle=90,size=8))
    }
    else{
      if(clase %in% c("Duration")){
        ggplot(eventos,aes(x=eventos[[i]]@.Data)) + geom_bar() +
          ggtitle(names(eventos)[i]) +
          labs(x=names(eventos)[i],y="COnteo") +
          theme(axis.text.x = element_text(angle=90,size=8))
      }
    }
  }
}

sapply(sapply(db_anal2,class),first)

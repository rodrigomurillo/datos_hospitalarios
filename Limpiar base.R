library(plyr)

# Leer tablas
eventos <- read.csv("./DB/Lesiones_2010_a_2015/D_EVENTO_2015.csv", header=T, encoding = "Latin-1")

efectos <- read.csv("./DB/Lesiones_2010_a_2015/D_BAJOEFECTOS_2015.csv", header=T, encoding = "Latin-1")
# A qué organismo están afiliados
derechohabiencia <- read.csv("./DB/Lesiones_2010_a_2015/D_DERECHOHABIENCIA_2015.CSV", header=T, encoding = "Latin-1")
# No encontré el catálogo
diagnosticos <- read.csv("./DB/Lesiones_2010_a_2015/D_DIAGNOSTICOS_2015.csv", header = T, encoding = "Latin-1")
# Tipo de atención médica que se les brindó
atencion <- read.csv("./DB/Lesiones_2010_a_2015/D_TIPOATENCION_2015.csv", header = T, encoding = "Latin-1")
violencia <- read.csv("./DB/Lesiones_2010_a_2015/D_TIPOVIOLENCIA_2015.csv", header = T, encoding = "Latin-1")


db <- merge(eventos, violencia, by = "ID")
db <- merge(db, atencion, by = "ID")

# Leer los catálogos
catalogos <- list.files("./DB/Catalogos Lesiones 2010 a 2014/", pattern="*.csv", full.names=TRUE)

# Tipo de Edad
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cClaveEdad.csv", header = T)
db$CodClaveEdad <- as.factor(mapvalues(db$CodClaveEdad, from=aux$CodClaveEdad, to= as.character(aux$Descripcion)))
# Sexo
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cSexo.csv", header = T)
db$CodSexo <- as.factor(mapvalues(db$CodSexo, from=aux$CodSex, to= as.character(aux$Descripcion)))
# Emabarazada
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cEmbarazada.csv", header = T)
db$CodEmbarazada <- as.factor(mapvalues(db$CodEmbarazada, from=aux$CodEmbarazada, to= as.character(aux$Descripcion)))
# Escribe
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cLeeEscribe.csv", header = T)
db$CodLeeEscribe <- as.factor(mapvalues(db$CodLeeEscribe, from=aux$CodLeeEscribe, to= as.character(aux$Descripcion)))
# Estados
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cEstados.csv", header = T)
db$CodEstado <- as.factor(mapvalues(db$CodEstado, from=aux$CodEstado, to= as.character(aux$Descripcion)))
# ¿Festivo?
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cFestivo.csv", header = T)
db$CodFestivo <- as.factor(mapvalues(db$CodFestivo, from=aux$CodFestivo, to= as.character(aux$Descripcion)))
# Discapacidad
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cDiscapacidad.csv", header = T)
db$CodDiscapacidad <- as.factor(mapvalues(db$CodDiscapacidad, from=aux$CodDiscapacidad, to= as.character(aux$Descripcion)))
# Intencionalidad
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cIntencionalidad.csv", header = T)
db$CodIntencionalidad <- as.factor(mapvalues(db$CodIntencionalidad, from=aux$CodIntencionalidad, to= as.character(aux$Descripcion)))
# Cuántas veces
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cVez.csv", header = T)
db$CodVez <- as.factor(mapvalues(db$CodVez, from=aux$CodVez, to= as.character(aux$Descripcion)))
# Sitio del ataque
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cSitio.csv", header = T)
db$CodSitio <- as.factor(mapvalues(db$CodSitio, from=aux$CodSitio, to= as.character(aux$Descripcion)))
# sort(summary(db$CodSitio),decreasing = T)
# Objeto con el que se atacó
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cAgente.csv", header = T)
db$CodAgente <- as.factor(mapvalues(db$CodAgente, from=aux$CodAgente, to= as.character(aux$Descripcion)))
# Ocupante del vehículo que resultó herido
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cLesionadoVehiculo.csv", header = T)
db$CodLesionadoVehiculo <- as.factor(mapvalues(db$CodLesionadoVehiculo, from=aux$CodLesionadoVehiculo, to= as.character(aux$Descripcion)))
# Área lastimada
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cArea.csv", header = T)
db$CodArea <- as.factor(mapvalues(db$CodArea, from=aux$CodArea, to= as.character(aux$Descripcion)))
# Consecuencia
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cConsecuencia.csv", header = T)
db$CodConsecuencia <- as.factor(mapvalues(db$CodConsecuencia, from=aux$CodConsecuencia, to= as.character(aux$Descripcion)))
# ¿Prehospitalaria?
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cPrehospitalaria.csv", header = T)
db$CodPrehospitalaria <- as.factor(mapvalues(db$CodPrehospitalaria, from=aux$CodPrehospitalaria, to= as.character(aux$Descripcion)))
# Número de agresores
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cAgresor.csv", header = T)
db$CodAgresor <- as.factor(mapvalues(db$CodAgresor, from=aux$CodAgresor, to= as.character(aux$Descripcion)))
# Género del agresor
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cSexo.csv", header = T)
db$CodSexoAgresor <- as.factor(mapvalues(db$CodSexoAgresor, from=aux$CodSexo, to= as.character(aux$Descripcion)))
# Parentesco del agresor
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cParentesco.csv", header = T)
db$CodParentescoAgresor <- as.factor(mapvalues(db$CodParentescoAgresor, from=aux$CodParentesco, to= as.character(aux$Descripcion)))
# Destino de la víctima
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cDestino.csv", header = T)
db$CodDestino <- as.factor(mapvalues(db$CodDestino, from=aux$CodDestino, to= as.character(aux$Descripcion)))
# Responsable de dar la atención
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cResponsable.csv", header = T)
db$CodResponsable <- as.factor(mapvalues(db$CodResponsable, from=aux$CodResponsable, to= as.character(aux$Descripcion)))
# Tipo de violencia
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cTipoViolencia.csv", header = T)
db$CodTipoViolencia <- as.factor(mapvalues(db$CodTipoViolencia, from=aux$CodTipoViolencia, to= as.character(aux$Descripcion)))
# Tipo de atención
aux <- read.csv("./DB/Catalogos Lesiones 2010 a 2014/cTipoAtencion.csv", header = T)
db$CodTipoAtencion <- as.factor(mapvalues(db$CodTipoAtencion, from=aux$CodTipoAtencion, to= as.character(aux$Descripcion)))

# Gráficas
saveRDS(db, file = "./DB/Datos_hospitalarios.rds")


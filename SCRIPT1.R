# importando a base 

consumo <- read.table("C:\\INPE\\Estudo Access\\Total_teste.CSV",header=TRUE,sep=";")

header(consumo)

consumo

# ver a estrutura do data frame - aula 1 R professor 

str(consumo)

# Fazer um sumario pela coluna SETOR, E DEPOIS MOSTRA O GRAFICO DE BARRAS (LIVRO)
# ERRO CONSUMO("SETOR")
quantidade <- tapply(rep(1,65535),consumo("SETOR"),sum)

# aula 8 FGV Youtube - instalando pacotes
install.packages("survey")
require(survey)

# aula 9 FGV Youtube - ver o diretorio de trabalho
getwd()

# ----------
# sumario - pega media, mediana, minimo, maximo, primeiro e terceiro quartis de 
# todas a colunas
summary(consumo)

# exemplo variancia  "Coluna Setor"
var(consumo[,4])
# 35113,2

# exemplo desvio padrao "Coluna Setor"
sd(consumo[,4])
# 187.3852  

# confere o desvio padrao pela a raiz quadrada
sqrt(35113.2)

# media "Coluna Setor"
mean(consumo[,4])
#205,5164

# calculo do coeficiente de variancao (>30% dados dispersos da media)
cv<-function(x){coef<-sd(x)/mean(x)*100
return(coef)}

cv(consumo[,4])
#91.17771 
# dados muito dispersos > 30%

#coeficiente de assimetria (os dados estão dispersos em relacao a moda)
#biblioteca
#package moments
require(moments)

skewness(consumo[,4])
#3.234053

# histograma para confirmar 
hist(consumo[,4],col='red',main="Frequencia Coluna Setor",xlab="Setor")

barplot(consumo[,4],main="Frequencia Coluna Setor",col=topo.colors(9))

#-------
# selecionando da base somente as residenciaS
library(dplyr)
require(dplyr)

resid<-filter(consumo,ECRES>0,is.na(TPLIGACAO) | TPLIGACAO=='P',
              STLIGACAO=='R' | STLIGACAO=='C')

resid<-filter(consumo,(ECRES>0 & is.na(TPLIGACAO) & STLIGACAO=='R') |
                (ECRES>0 & is.na(TPLIGACAO) & STLIGACAO=='C') |
                (ECRES>0 & TPLIGACAO=='P'   & STLIGACAO=='R') |
                (ECRES>0 & TPLIGACAO=='P'   & STLIGACAO=='C')  )



#-------- filtro por residencia ok
summary(consumo)
str(consumo)
resid1<-dplyr::filter(consumo,ECRES>0)

resid2<-dplyr::filter(resid1,ECCOM<1)

resid3<-dplyr::filter(resid2,ECIND<1)

resid4<-dplyr::filter(resid3,ECPUB<1)

summary(resid4)
str(resid4)

#------- filtro por tipo de ligacao - excluir G,C,F  ok
resid5<-dplyr::filter(resid4,TPLIGACAO!='G' & TPLIGACAO!='C' & TPLIGACAO!='F')

summary(resid5)
str(resid5)

#--- filtro por status de ligacao - excluir S,H,M,D  ok
resid<-dplyr::filter(resid5,STLIGACAO!='E' & STLIGACAO!='S' & STLIGACAO!='H' & STLIGACAO!='M' & STLIGACAO!='D')

summary(resid)
str(resid)

# --- apaga os data.frames gerados para teste - depois de confirmado o resid final (ok)
rm(resid1)
rm(resid2)
rm(resid3)
rm(resid4)
rm(resid5)

#---- criacao das colunas de consumo mensal por residencia 
resid["CMRRJAN"]<-resid$CMRJAN / resid$ECRES
resid["CMRRFEV"]<-resid$CMRFEV / resid$ECRES
resid["CMRRMAR"]<-resid$CMRMAR / resid$ECRES
resid["CMRRABR"]<-resid$CMRABR / resid$ECRES
resid["CMRRMAI"]<-resid$CMRMAI / resid$ECRES
resid["CMRRJUN"]<-resid$CMRJUN / resid$ECRES
resid["CMRRJUL"]<-resid$CMRJUL / resid$ECRES
resid["CMRRAGO"]<-resid$CMRAGO / resid$ECRES
resid["CMRRSET"]<-resid$CMRSET / resid$ECRES
resid["CMRROUT"]<-resid$CMROUT / resid$ECRES
resid["CMRRNOV"]<-resid$CMRNOV / resid$ECRES
resid["CMRRDEZ"]<-resid$CMRDEZ / resid$ECRES

#-- conferencia
summary(resid)
str(resid)

resid[31,]
resid[49186,]


#---- OK - calcula a media de consumo por residencia anual
resid$MEDRANU <- rowMeans(resid[c('CMRRJAN','CMRRFEV','CMRRMAR',
                                  'CMRRABR','CMRRMAI','CMRRJUN',
                                  'CMRRJUL','CMRRAGO','CMRRSET',
                                  'CMRROUT','CMRRNOV','CMRRDEZ')], na.rm=TRUE)


#--- HISTOGRAMA DA MEDIA POR RESIDENCIA
summary(resid)

hist(resid[,118],col='red',main="Frequencia Media por Residencia",
     xlab="Media-Residencia",cex.main=2)


med_max <-which(resid$MEDRANU>600)
resid[med_max,]


plot(resid$SETOR ~ resid$MEDRANU,
     main="Setor X Media Consumo Anual",
     col="blue",xlab="Media Consumo Residencia",ylab="Setor")

#------ Criação coluna Desvio Padrao
#------
resid$DPRANU <- sqrt(rowMeans(resid[c('CMRRJAN','CMRRFEV','CMRRMAR',
                                      'CMRRABR','CMRRMAI','CMRRJUN',
                                      'CMRRJUL','CMRRAGO','CMRRSET',
                                      'CMRROUT','CMRRNOV','CMRRDEZ')], na.rm=TRUE))

#--- HISTOGRAMA DESVIO PADRAO POR RESIDENCIA
summary(resid)

hist(resid[,119],col='red',main="Desvio Padrao por Residencia",
     xlab="Desvio Padrao-Residencia",cex.main=2)


dp_max <-which(resid$DPRANU>24)
resid[dp_max,]

sqrt(600.9167)

dp_maior20 <-which(resid$DPRANU>20)
resid[maior20,]

plot(resid$SETOR ~ resid$DPRANU,
     main="Setor X Desvio Padrao ",
     col="blue",xlab="Desvio Padrao por Residencia",ylab="Setor")



#------ SOM
require(kohonen)
str(resid)

colnames(resid)

resid.setmddp <- c("MEDRANU","DPRANU")


resid.SOM1 <- som(scale(resid[resid.setmddp]), grid = somgrid(6, 4, "rectangular"))

summary(resid.SOM1)
str(resid.SOM1)


plot(resid.SOM1)


#---- teste plot 
plot(resid.SOM1, type = "counts", palette.name = colors, heatkey = TRUE)

plot(resid.SOM1,"changes")
plot(resid.SOM1,"counts")
plot(resid.SOM1,"codes")

plot(resid.SOM1, type = "counts", property = resid.SOM1$MEDRANU[,2], 
     main=names(resid.SOM1$MEDRANU)[2], palette.name = colors) 


#-- -SOM hexagonal
data_train <- resid[, c(118,119)] #setor, media, desvio padrao

#resid.SOM2 <- som(scale(resid[resid.setmddp]), grid = somgrid(6, 6, "hexagonal"))

data_train_matrix <- as.matrix(scale(data_train)) 

som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal") 

resid.SOM2 <- som(data_train_matrix,    grid=som_grid,    rlen=100, 
                  alpha=c(0.05,0.01),    keep.data = TRUE ) 

summary(resid.SOM2) 

plot(resid.SOM2)

#---

plot(resid.SOM2, type = "property", property = getCodes(resid.SOM2,1)[,2], 
     main=colnames(getCodes(resid.SOM2,1))[2], palette.name = colors) 

plot(resid.SOM2, type = "property", property = getCodes(resid.SOM2,1)[,3], 
     main=colnames(getCodes(resid.SOM2,1))[3], palette.name = colors) 

######------ outros tipos
par(mfrow = c(1, 2))
plot(resid.SOM2, type = "mapping", pchs = 20, main = "Mapping Type SOM")
plot(resid.SOM2, main = "Default SOM Plot")

plot(resid.SOM2)
plot(resid.SOM2, type = "dist.neighbours", property = getCodes(resid.SOM2,1)[,2],
     main=colnames(getCodes(resid.SOM2,1))[2], palette.name = colors) 


#--- NOVO TESTE PROFESSOR 
#---- SOM SUPERVISIONADO - VARIOS DADOS  
require(kohonen)
summary(resid)

SOM3 =  data.frame(CMRRJAN=resid$CMRRJAN,CMRRFEV=resid$CMRRFEV,
                   CMRRMAR=resid$CMRRMAR,CMRRABR=resid$CMRRABR,
                   CMRRMAI=resid$CMRRMAI,CMRRJUN=resid$CMRRJUN,
                   CMRRJUL=resid$CMRRJUL,CMRRAGO=resid$CMRRAGO,
                   CMRRSET=resid$CMRRSET,CMRROUT=resid$CMRROUT,
                   CMRRNOV=resid$CMRRNOV,CMMRDEZ=resid$CMRRDEZ)

str(SOM3)
SOM3 = scale(SOM3)


str(resid[106:117])
str(resid(106:117))


#--- 
set.seed(12)

SOM3.supersom <- supersom(SOM3, somgrid(12, 12, "hexagonal")) 

plot(SOM3.supersom, type = "mapping",  pch = 1, main = "All", keepMargins = TRUE)



SOM3.prediction <- predict(SOM3.supersom)


plot(SOM3.supersom, type = "property",  property = SOM3.prediction , main = "Consumo Agua Anual",keepMargins = TRUE)

plot(SOM3.supersom, type = "property",  property = SOM3.prediction ,palette.name = rainbow, main = "Consumo Agua Anual", keepMargins = TRUE)


consumo.supersom <- predict(SOM3.supersom)$unit.prediction 
consumo.predict <- as.numeric(classmat2classvec(consumo.supersom))

plot(SOM3.supersom, type = "property",  property = consumo.supersom , main = "Consumo Agua Anual",palette.name = rainbow, keepMargins = TRUE)


plot(SOM3.teste1, main = "Consumo Agua Anual")


#---- novo teste professor 
R> data(yeast) 
R> set.seed(7) 
R> yeast.supersom <- supersom(yeast, somgrid(8, 8, "hexagonal"), + whatmap = 3:6) 
R> classes <- levels(yeast$class) 
R> colors <- c("yellow", "green", "blue", "red", "orange") 
R> par(mfrow = c(3, 2)) 
R> plot(yeast.supersom, type = "mapping", + pch = 1, main = "All", keepMargins = TRUE)
R> for (i in seq(along = classes)) {
     X.class <- lapply(yeast,
     function(x) subset(x, yeast$class == classes[i])) 
     X.map <- map(yeast.supersom, X.class) 
     plot(yeast.supersom, type = "mapping", classif = X.map, 
     col = colors[i], pch = 1, main = classes[i], keepMargins = TRUE, 
     bgcol = gray(0.85))
 }



#--- K-MEANS - media residencia anual X setor  
KM_MRAS =  data.frame(SETOR = resid$SETOR, MEDIANURES  = resid$MEDRANU)

KM_MRAS = scale(KM_MRAS)

set.seed(1)
wcss = vector()
for (i in 1:10) {
  kmeans = kmeans(x = KM_MRAS, centers = i)
  wcss[i] = sum(kmeans$withinss)
}

plot(1:10, wcss, type = 'b', xlab = 'Clusters', ylab = 'WCSS')


set.seed(1)
kmeans = kmeans(x = KM_MRAS, centers = 5)
previsoes = kmeans$cluster

plot(KM_MRAS, col = previsoes, xlab = 'Setor', ylab = 'Media Residencia Anual')

library(cluster)
clusplot(KM_MRAS, previsoes, xlab = 'Setor', 
         ylab = 'Media Residencia Anual', main = 'Agrupamento Setor X Media Residencia Anual',
         lines = 0, shade = TRUE, color = TRUE, labels = 2)

#--- kmedias media residencia anual e desvio padrao 

KM_MRADP =  data.frame( MEDIANURES  = resid$MEDRANU, DESVPAD = resid$DPRANU)

KM_MRADP = scale(KM_MRADP)

set.seed(1)
wcss = vector()
for (i in 1:10) {
  kmeans = kmeans(x = KM_MRADP, centers = i)
  wcss[i] = sum(kmeans$withinss)
}

plot(1:10, wcss, type = 'b', xlab = 'Clusters', ylab = 'WCSS')


set.seed(1)
kmeans = kmeans(x = KM_MRADP, centers = 5)
previsoes = kmeans$cluster


plot(KM_MRADP, col = previsoes,xlab = 'Media Residencia Anual',ylab = 'Desvio Padrao' )

library(cluster)
clusplot(KM_MRADP, previsoes, xlab = 'Media Residencia Anual', 
         ylab = 'Desvio Padrao', main = 'Agrupamento Media Consumo X Desvio Padrao',
         lines = 0, shade = TRUE, color = TRUE, labels = 2)

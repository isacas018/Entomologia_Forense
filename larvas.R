#### LIBRERIAS A UTILIZAR ####

library(readxl)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(reshape)
library(gridExtra)

#### FUNCIONES ####

# Función para guardar todas las hojas de un archivo excel 

readExcelSheets <- function(filename){
  
  # lee todas las hojas del excel
  all <- readxl::excel_sheets(filename)
  
  # importa cada hoja a una lista 
  list <- lapply(all, function(x) readxl::read_excel(filename, sheet = x))
  
  # guarda el nombre de las hojas
  names(list) <- all
  
  # crea un dataframe para cada hoja identificandolas por el nombre de hoja 
  list2env(list, envir = .GlobalEnv)
}


# Función para organizar los datos de las trampas en una matriz con la forma de la cuadrícula,
# observar "Zonas.png"

posicionar_trampas <- function(lista){
  
  matriz <- matrix(nrow = 11, ncol = 10)
  
  # Sólo posicionará correctamente si la lista que recibe tiene 85 obs
  
  matriz[1, 1:7] <- lista[49:55]
  matriz[2, ] <- lista[56:65]
  matriz[3, c(1, 2, 10)] <- lista[66:68]
  matriz[4, c(1, 2, 10)] <- lista[69:71]
  matriz[5, c(1, 2, 10)] <- lista[72:74]
  matriz[6, c(1, 2, 10)] <- lista[75:77]
  matriz[7:10, 2] <- lista[78:81]
  matriz[11,3:6] <- lista[82:85]
  matriz[3,3:9] <- lista[1:7]
  matriz[4,3:9] <- lista[8:14]
  matriz[5,3:9] <- lista[15:21]
  matriz[6,c(3:5, 7:9)] <- lista[22:27]
  matriz[7,c(3:5, 7:9)] <- lista[28:33]
  matriz[8,3:8] <- lista[34:39]
  matriz[9,3:7] <- lista[40:44]
  matriz[10,3:6] <- lista[45:48]
  colnames(matriz) <- paste("Col.", 1:10)
  rownames(matriz) <- paste("Fila", 1:11)
  
  #matriz <- melt(matriz)
  #colnames(matriz) <- c("x", "y", "valor")
  
  return(matriz)
}

#### LIMPIEZA DE LOS DATOS ####

# Work directory
setwd("~/SS")

# Leemos datos y nos quedamos con los importantes

df <- data.frame(read_xlsx("Trampas pitfall.xlsx"))    # Fecha del archivo: 09/02/23
df <- df[c(1, 4, 7, 34)]
colnames(df) <- c("Trampa", "L_Diptera", "L_Coleoptera", "L_Lepidoptera")

# Sustituimos datos y casteamos a tipo int

df <- mutate_at(df, c("L_Diptera", "L_Coleoptera", "L_Lepidoptera"), 
                ~replace(., is.na(.), 0))

df$L_Diptera <- revalue(df$L_Diptera, c("1(?)" = 1, "1?" = 1))
df$L_Coleoptera <- revalue(df$L_Coleoptera, c("1(?)" = 1, "1?" = 1))

df$L_Diptera <- as.integer(df$L_Diptera)
df$L_Coleoptera <- as.integer(df$L_Coleoptera)
df$L_Lepidoptera <- as.integer(df$L_Lepidoptera)

# Creamos auxiliares

aux <- df[1:874%%2==1, ]
aux2 <- df[1:874%%2==0, ]
rownames(aux) = 1:437
rownames(aux2) = 1:437

# Creamos nuevo df con la suma de ambas revisiones en cada trampa y cada semana

larvas_especie <- data.frame(aux[1], aux[2:4] + aux2[2:4])


data.frame(aux[1], aux2[1])   # Mostrar las revisiones a la par  
# Se pierde la secuencia en algunos renglones como D3 o D11, lo que ocaciona que 
# a veces se sumen los resultados de la revisión de 2 trampas diferentes




# Leemos datos y guardamos en lista nombres de columnas recurrentes que usaremos 

readExcelSheets("Muestreos trampas pitfall, entomología forense.xlsx")  # Fecha del archivo: 09/02/23
category <- c("num_trampa", "larva", "pupa", "pupario_vacio", "adulto")

# Simplificamos muestra A 

muestra_A <- `Muestreo A - Muestreo A 14_04_2`; remove(`Muestreo A - Muestreo A 14_04_2`)
muestra_A <- muestra_A[1:4]
colnames(muestra_A) <- category
muestra_A <- muestra_A[2:49, ]

# Simplificamos muestra B

muestra_B <- `Muestreo B - Muestreo B 21_04_2`; remove(`Muestreo B - Muestreo B 21_04_2`)
muestra_B <- muestra_B[1:4]
colnames(muestra_B) <- category
muestra_B <- muestra_B[2:86, ]

# Simplificamos muestra C

muestra_C <- `Muestreo C - Muestreo C 28_04_2`; remove(`Muestreo C - Muestreo C 28_04_2`)
muestra_C <- muestra_C[1:4]
colnames(muestra_C) <- category
muestra_C <- muestra_C[2:86, ]

#Simplificamos muestra D

muestra_D <- `Muestreo D - Muestreo D 05_05_2`; remove(`Muestreo D - Muestreo D 05_05_2`)
muestra_D <- muestra_D[1:4]
colnames(muestra_D) <- category
muestra_D <- muestra_D[2:86, ]

# Simplificamos muestra E

muestra_E <- `Muestreo E - Muestreo E 11_05_2`; remove(`Muestreo E - Muestreo E 11_05_2`)
muestra_E <- muestra_E[1:4]
colnames(muestra_E) <- category
muestra_E <- muestra_E[2:86, ]

# Simplificamos muestra F

muestra_F <- `Muestreo F - Muestreo F 24_05_2`; remove(`Muestreo F - Muestreo F 24_05_2`)
muestra_F <- muestra_F[1:4]
colnames(muestra_F) <- category
muestra_F <- muestra_F[2:86, ]

# Simplificamos muestra G

muestra_G <- `Muestreo G - Muestreo G 01_06_2`; remove(`Muestreo G - Muestreo G 01_06_2`)
muestra_G <- muestra_G[1:4]
colnames(muestra_G) <- category
muestra_G <- muestra_G[2:86, ]

# Simplificamos muestra H

muestra_H <- `Muestreo H - Muestreo H 26_06_2`; remove(`Muestreo H - Muestreo H 26_06_2`)
muestra_H <- muestra_H[1:5]
colnames(muestra_H) <- category 
muestra_H <- muestra_H[2:86, ]

#Guardamos en lista nombres de renglones recurrentes
trampas <- muestra_B$num_trampa


# Simplificamos ubicación de trampas

ubicacion_trampas <- `Ubicación trampas pitfall - Ubi`; remove(`Ubicación trampas pitfall - Ubi`)
colnames(ubicacion_trampas) <- c("num_trampa", "dist_centro", "dist_trampa_este", 
                                 "dist_trampa_sur")
ubicacion_trampas <- ubicacion_trampas[2:86, 2:4]
ubicacion_trampas <- apply(ubicacion_trampas, 2, as.numeric)
ubicacion_trampas <- data.frame(ubicacion_trampas, row.names = trampas)


# Simplificamos inclinación del terreno

inclinacion <- `Carcterísticas del lugar  - Car`; remove(`Carcterísticas del lugar  - Car`)
colnames(inclinacion) <- c("num_trampa", "temperatura", "pendiente", "dir_pendiente")
inclinacion <- inclinacion[2:86, 2:4]
inclinacion <- data.frame(inclinacion, row.names = trampas)

aux <- inclinacion$pendiente
aux <- gsub("º", " ", aux)
aux <- as.integer(aux)
inclinacion$pendiente_num <- aux
inclinacion
# pendiente numerica y mantener temperatura


# Eliminamos todos los datos que no utilizaremos
remove(`Población por muestreo - Poblac`, `Temperatura ambiental comparaci`, `Temperatura ambiental PEMBU CCA`)

#Creamos auxiliar que tenga datos de S1 pero con 85 obs para parear con el resto 
aux = muestra_A[2:4]
aux[49:85, ] = "0"


# Creamos df con todas las observaciones de larvas a traves de las semanas

df_larvas <- data.frame(aux$larva, muestra_B$larva, muestra_C$larva,muestra_D$larva, 
                        muestra_E$larva, muestra_F$larva, muestra_G$larva, 
                        muestra_H$larva)

colnames(df_larvas) <- paste0("S", 1:8)
df_larvas$S7 <- revalue(df_larvas$S7, c("2 fannias" = 2, "1 fannia" = 1))
df_larvas <- apply(df_larvas, 2, as.integer)
df_larvas <- data.frame(df_larvas, row.names = trampas)


# Creamos df con todas las observaciones de pupas a traves de las semanas

df_pupas <- data.frame(aux$pupa, muestra_B$pupa, muestra_C$pupa, muestra_D$pupa, 
                       muestra_E$pupa, muestra_F$pupa, muestra_G$pupa, 
                       muestra_H$pupa)

df_pupas[is.na(df_pupas)] <- 0
colnames(df_pupas) <- paste0("S", 1:8)
df_pupas$S7 <- revalue(df_pupas$S7, c("1 fannia" = 1, "3 fannias" = 3))
df_pupas <- apply(df_pupas, 2, as.integer)
df_pupas <- data.frame(df_pupas, row.names = trampas)


# Creamos df con todas las observaciones de puparios vacios a traves de las semanas

df_vacios <- data.frame(aux$pupario_vacio, muestra_B$pupario_vacio, 
                        muestra_C$pupario_vacio, muestra_D$pupario_vacio, 
                        muestra_E$pupario_vacio, muestra_F$pupario_vacio, 
                        muestra_G$pupario_vacio, muestra_H$pupario_vacio)

df_vacios[is.na(df_vacios)] <- 0
colnames(df_vacios) <- paste0("S", 1:8)
df_vacios$S4 <- revalue(df_vacios$S4, c("1 fannia" = 1))
df_vacios$S7 <- revalue(df_vacios$S7, c("2 fannias" = 2))
df_vacios <- apply(df_vacios, 2, as.integer)
df_vacios <- data.frame(df_vacios, row.names = trampas)


#### ANÁLISIS ####

apply(df_larvas, 2, sum)
# Si comparas lo obtenido al sumar df_larvas no es lo mismo que "Sumatoria de ..."
# ¿Por qué?


# Total de larvas recolectadas
sum(df_larvas) # el total es 1865


# Trampa que más recolectó a lo largo del experimento
max(apply(df_larvas, 1, sum)) # el max es 302
grep(302, apply(df_larvas, 1, sum)) # el max se encuentra en el renglón 38 (trampa 37)


# Encontrar dónde esta el valor máximo 

max(df_larvas) # el max es 127
grep(127, df_larvas) # el max se encuentra en la columna 3 (Semana 3)
grep(127, df_larvas$S3) # el max se encuentra en el renglon 31 (trampa 30) 


# Dividimos las trampas por zonas y etiquetamos cada zona como factor

areas <- as.data.frame(apply(df_larvas, 1, sum)); colnames(areas) <- "total_larvas"
areas$zone <- 2; areas$zone[49:85] <- 3 
areas$zone[c(17, 18, 19, 24, 25, 30, 31, 36, 37, 38)] <- 1

areas$zone <- factor(areas$zone, levels = c(1, 2, 3), 
                     labels = c("Zona 1", "Zona 2", "Zona 3"))


# Resumen de lo recolectado por zona a lo largo del experiemnto

summarise(group_by(areas, zone), n = n(), total = sum(total_larvas), 
          minimo = min(total_larvas), maximo = max(total_larvas))


# Dividimos las trampas por cuadrantes y etiquetamos cada cuadrante como factor

areas$cuadrante <- 1
areas$cuadrante[c(4:7, 11:14, 18:21, 54, 55, 61:65, 68, 71, 74)] <- 2
areas$cuadrante[c(22:24, 28:30, 34:36, 40:42, 45:47, 75, 76, 78:84)] <- 3
areas$cuadrante[c(25:27, 31:33, 37:39, 43, 44, 48, 77, 85)] <- 4

areas$cuadrante <- factor(areas$cuadrante, levels = c(1, 2, 3, 4), 
                          labels = c("Q1", "Q2", "Q3", "Q4"))

# Resumen de lo recolectado por cuadrante a lo largo del experimento

summarise(group_by(areas, cuadrante), n = n(), total = sum(total_larvas), 
          minimo = min(total_larvas), maximo = max(total_larvas))


# Obtenemos una matriz con la cantidad de larvas que obtuvo cada trampa 

matriz_trampas <- posicionar_trampas(areas$total_larvas)



t(apply(df_larvas, 1, cumsum)) # Suma acumulada por trampa, util?


#### GRÁFICAS ####

# Gráfica de barras

barplot(apply(df_larvas, 2, sum), main = "Larvas encontradas por semana")


# Serie de Tiempo

plot(1:8, df_larvas[37, ], type = "l", ylab = "Cantidad de larvas", xlab = "Semana", 
     main = "Larvas por semana halladas en la trampa 38")


# Mapa de calor

heatmap(matriz_trampas, Rowv = NA, Colv = NA)
heatmap(matriz_trampas, Rowv = NA, Colv = NA, 
        col= colorRampPalette(brewer.pal(8, "Blues"))(265))

# Mapa de calor por semana
heatmap(posicionar_trampas(df_larvas$S1),  Rowv = NA, Colv = NA)
heatmap(posicionar_trampas(df_larvas$S2),  Rowv = NA, Colv = NA)
heatmap(posicionar_trampas(df_larvas$S3),  Rowv = NA, Colv = NA)
heatmap(posicionar_trampas(df_larvas$S4),  Rowv = NA, Colv = NA)
heatmap(posicionar_trampas(df_larvas$S5),  Rowv = NA, Colv = NA)
heatmap(posicionar_trampas(df_larvas$S6),  Rowv = NA, Colv = NA)
heatmap(posicionar_trampas(df_larvas$S7),  Rowv = NA, Colv = NA)
heatmap(posicionar_trampas(df_larvas$S8),  Rowv = NA, Colv = NA)

# Mapa de calor estilizado con ggplot2 
# Requiere procesamiento de datos (función melt)

aux <- melt(matriz_trampas)
colnames(aux) <- c("Fila", "Columna", "Larvas")

# ACLARACIÓN 
# Las filas se refieren a la posición en y, las columnas a la posición en x (Se puede borrar si se desea)

ggplot(aux, aes(x = Columna, y = Fila, fill = Larvas)) +
  geom_tile(color = "white", lwd = 1, linetype = 1) + 
  scale_fill_gradient(low = "#DCC876", high = "brown") +
  geom_text(aes(label = Larvas), color = "white", size = 4) +
  coord_fixed() +
  scale_y_continuous(breaks = seq(1, 11, 1), trans = "reverse") +
  scale_x_continuous(breaks = seq(1, 10, 1)) 


# El problema está en el midpoint
ggplot(aux, aes(x = Columna, y = Fila, fill = Larvas)) +
  geom_tile(color = "white", lwd = 1, linetype = 1) + 
  scale_fill_gradient2(low = "white", mid = "#DCC876", high = "brown", midpoint = 9) +
  geom_text(aes(label = Larvas), color = "white", size = 4) +
  coord_fixed() +
  scale_y_continuous(breaks = seq(1, 11, 1), trans = "reverse") +
  scale_x_continuous(breaks = seq(1, 10, 1)) 




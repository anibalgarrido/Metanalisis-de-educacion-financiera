### Ahorros ###

library(readxl)
Meta <- read_excel("Meta.xlsx", sheet = "Ahorro")

Ahorro <-Meta

mean(Ahorro$M.Tratamiento)


## Load meta package
install.packages("meta")
library(meta)


# CLUSTER DE AHORRO #


Zdatos <- scale(ahorro_meta$RR)
dat<- scale(ahorro_meta$RR)

library(scales)
par(mfrow = c(1, 1))
plot(Zdatos, col = alpha("steelblue", 0.4), pch = 19, las = 1)
text(Zdatos, rownames(Zdatos), pos = 3, cex = .6)
Numgrupos <- 2

grupos <- cutree(fit, k = Numgrupos)
grupos

library(RColorBrewer)
plot(fit, cex = .6, xlab = "", ylab = "Distancia entre grupos", sub = "Cluster jerárquico método de Ward para estudios sobre educación financiera-Ahorros en jóvenes") 
rect.hclust(fit, k = Numgrupos, border = brewer.pal(Numgrupos, "Dark2"))

library(metafor)

study.subgroup<-update.meta(ahorro_meta, 
                            byvar=Cluster, 
                            comb.random = TRUE, 
                            comb.fixed = FALSE)
study.subgroup

forest.meta(study.subgroup)


funnel.meta(ahorro_meta)
title(main="Educación Financiera para jóvenes en ahorros")


## Meta-analysis using metabin()
ahorro_meta <-
            metabin(data        = Ahorro,  # Data
                    event.e     = M.Tratamiento,  # Events in exposed
                    n.e         = Tratamiento,     # Number in exposed
                    event.c     = M.Control,  # Events in control
                    n.c         = Control,     # Number in control
                    sm          = "RR",     # Summary measure
                    studlab     = Date, # Study labels
                    comb.fixed  = TRUE,     # Fixed effects yes
                    comb.random = FALSE     # Random effects no
            )
          
## Show results
summary(ahorro_meta)

forest.meta(ahorro_meta)

## Funnel plot using funnel()
funnel(ahorro_meta)


metabias(ahorro_meta)

## Cumulative meta-analysis using metacum()
strepto_ahorro.cuml <- metacum(ahorro_meta)
strepto_ahorro.cuml

forest(strepto_ahorro.cuml, xlim = c(0.9, 1.5))

## CÁLCULO POR CLUSTERES ##

Ahorroc1<-subset(Ahorro,Tipo=="Cluster 1")
Ahorroc2<-subset(Ahorro,Tipo=="Cluster 2")

# CLUSTER 1 #

## Meta-analysis using metabin()
ahorro_meta <-
  metabin(data        = Ahorroc1,  # Data
          event.e     = M.Tratamiento,  # Events in exposed
          n.e         = Tratamiento,     # Number in exposed
          event.c     = M.Control,  # Events in control
          n.c         = Control,     # Number in control
          sm          = "RR",     # Summary measure
          studlab     = Date, # Study labels
          comb.fixed  = TRUE,     # Fixed effects yes
          comb.random = FALSE     # Random effects no
  )

## Show results
summary(ahorro_meta)

forest.meta(ahorro_meta)

## Funnel plot using funnel()
funnel(ahorro_meta)
title(main="Educación Financiera para jóvenes en ahorros Cluster 2")

metabias(ahorro_meta)

## Cumulative meta-analysis using metacum()
strepto_ahorro.cuml <- metacum(ahorro_meta)
strepto_ahorro.cuml

forest(strepto_ahorro.cuml, xlim = c(0.95, 2.5))

# CLUSTER 2 #

## Meta-analysis using metabin()
ahorro_meta <-
  metabin(data        = Ahorroc2,  # Data
          event.e     = M.Tratamiento,  # Events in exposed
          n.e         = Tratamiento,     # Number in exposed
          event.c     = M.Control,  # Events in control
          n.c         = Control,     # Number in control
          sm          = "RR",     # Summary measure
          studlab     = Date, # Study labels
          comb.fixed  = TRUE,     # Fixed effects yes
          comb.random = FALSE     # Random effects no
  )

## Show results
summary(ahorro_meta)

forest.meta(ahorro_meta)

## Funnel plot using funnel()
funnel(ahorro_meta)


metabias(ahorro_meta)

## Cumulative meta-analysis using metacum()
strepto_ahorro.cuml <- metacum(ahorro_meta)
strepto_ahorro.cuml

forest(strepto_ahorro.cuml, xlim = c(0.95, 1.7))


### Mujeres ###

library(readxl)
Meta1 <- read_excel("Meta.xlsx", sheet = "Mujeres")

Mujeres <-Meta1


## Load meta package
library(meta)


## Meta-analysis using metabin()
mujeres_meta <-
  metabin(data        = Mujeres,  # Data
          event.e     = M.Tratamiento,  # Events in exposed
          n.e         = Tratamiento,     # Number in exposed
          event.c     = M.Control,  # Events in control
          n.c         = Control,     # Number in control
          sm          = "RR",     # Summary measure
          studlab     = Date, # Study labels
          comb.fixed  = TRUE,     # Fixed effects yes
          comb.random = FALSE     # Random effects no
  )

## Show results
mujeres_meta

forest.meta(mujeres_meta)

## Funnel plot using funnel()
funnel(mujeres_meta)
title(main="Educación Financiera para mujeres")

metabias(mujeres_meta)

## Cumulative meta-analysis using metacum()
strepto_mujeres.cuml <- metacum(mujeres_meta)
strepto_mujeres.cuml

forest(strepto_mujeres.cuml, xlim = c(1, 1.5))


### Conocimiento ###

library(readxl)
Meta <- read_excel("Meta.xlsx", sheet = "Conocimiento")

Conocimiento <-Meta


## Load meta package
library(meta)


## Meta-analysis using metabin()
conocimiento_meta <-
  metabin(data        = Conocimiento,  # Data
          event.e     = M.Tratamiento,  # Events in exposed
          n.e         = Tratamiento,     # Number in exposed
          event.c     = M.Control,  # Events in control
          n.c         = Control,     # Number in control
          sm          = "RR",     # Summary measure
          studlab     = Date, # Study labels
          comb.fixed  = TRUE,     # Fixed effects yes
          comb.random = FALSE     # Random effects no
  )

## Show results
conocimiento_meta

forest.meta(conocimiento_meta)

## Funnel plot using funnel()
funnel(conocimiento_meta)
title(main="Educación Financiera para jóvenes en conocimiento")

metabias(conocimiento_meta)

## Cumulative meta-analysis using metacum()
strepto_conocimiento.cuml <- metacum(conocimiento_meta)
strepto_conocimiento.cuml

forest(strepto_conocimiento.cuml, xlim = c(1, 2))


### Mapa de estudios ##

library(mapdata)
library(ggplot2)
library(maps)
library(ggrepel)

# Guardamos la información en un nuevo dataframe llamado mapa_mundo

mapa_mundo <- map_data("world")


# Para visualizar el mapa utilizamos geom_polygon() 

library(tidyverse)

mapa_mundo %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "black",
               color = "white")


mapa_mundo %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "grey80",
               color = "white") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1)) +
  ggtitle( "Países analizados en el estudio de educación financiera") +
  coord_fixed (xlim= c(-140,-25),
               ylim= c(-50,60),
               ratio = 1.2)


ciudades <- c("Perú", "Brasil", "Colombia","México","Ecuador")
coordenadas <- data.frame( long = c(-77.0282364, -46.6388, -74.081749, -102.63894, -80.38477 ), 
                           lat= c(-12.0431805,-23.5489, 4.6097102, 22.63894, -0.583984375 ),
                           stringsAsFactors = F) 

coordenadas$ciudades <- ciudades    

mapa_mundo %>%
  ggplot() +
  geom_polygon(aes( x= long, y = lat, group = group),
               fill = "grey80",
               color = "white") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour= "black", size= 1)) +
  ggtitle( "Países analizados en el meta análisis de educación financiera") +
  coord_fixed (xlim= c(-130,-25),
               ylim= c(-50,35),
               ratio = 1.2)+
  geom_point(data=coordenadas, aes(long, lat),
             color= "red", size=1) +
  geom_text_repel(data = coordenadas, 
                  aes(long, lat, label =ciudades))








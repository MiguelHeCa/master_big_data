
# Preparación de ambiente de trabajo --------------------------------------

paquetes = c("countrycode", "dplyr", "KTensorGraphs", "ade4")
no_insta = paquetes[!paquetes %in% installed.packages()[,1]]
if(length(no_insta) != 0) install.packages(paquetes)

library(readxl)
library(countrycode)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Carga de datos ----------------------------------------------------------

# Descripciones: http://www.ssfindex.com/ssi/indicator-description/
nom_ssi = c(
  # EN | ES
  # Human Wellbeing | Bienestar humano
  ## I. Basic Needs | Necesidades básicas
  "SF", # 1. Sufficient Food | Comida suficiente
  "SD", # 2. Sufficient to Drink | Bebida suficiente
  "SS", # 3. Safe Sanitation | Saneamiento seguro
  ## II. Personal Development & Health
  "Ed", # 4. Education | Educación
  "HL", # 5. Healthy Life | Vida saludable
  "GE", # 6. Gender Equality | Equidad de género
  ## III. Well-balanced Society | Sociedad balanceada
  "ID", # 7. Income Distribution | Distribución de ingreso
  "PG", # 8. Population Growth | Crecimiento poblacional
  "GG", # 9. Good Governance | Gobernanza adecuada
  # Environmental Wellbeing | Bienestar medioambiental
  ## IV. Natural Resources | Recursos materiales
  "Bd", # 10. Biodiversity | Biodiversidad
  "RW", # 11. Renewable Water Resources | Recursos acuíferos renovables
  "Cs", # 12. Consumption | Consumo
  ## V. Climate & Energy | Clima y energía
  "EU", # 13. Energy Use | Uso de energía
  "ES", # 14. Energy Savings | Ahorro de energía
  "Gr", # 15. Greenhouse Gases | Gases de invernadero
  "RE", # 16. Renewable Energy | Energía renovable
  # Economic Wellbeing | Bienestar económico
  ## VI. Transition | Transición
  "OF", # 17. Organic Farming | Agricultura orgánica
  "GS", # 18. Genuine Savings | Ahorro real
  ## VII. Economy | Economía
  "GP", # 19. Gross Domestic Product | Producto Interno Bruto
  "Em", # 20. Employment | Empleo
  "PD"  # 21. Public Debt | Deuda pública
)

ingreso = read_xls(
  "OGHIST.xls",
  range = "A12:AF229",
  sheet = "Country Analytical History",
  col_names = F
) %>% 
  mutate(Ingreso = case_when(
    ...32 == "H"  ~ "A",  # Altos
    ...32 == "UM" ~ "MA", # Medio-Altos
    ...32 == "LM" ~ "MB", # Medio-Bajos,
    ...32 == "L"  ~ "B"   # Bajos
  )) %>% 
  select(BM = ...1, Ingreso)

ssi = lapply(seq(2006, 2016, 2), function(x) {
  d = read_excel("Data_countries_2006-2016.xlsx",
                 sheet = paste("Scores", x),
                 range = "A5:V158", col_names = c("pais", nom_ssi))
  d %>% 
    mutate(BM = countrycode(pais, "country.name", "iso3c"),
           a_o = factor(x),
           nomf = paste(x, BM, sep = "_")) %>% 
    select(nomf, a_o, BM, everything()) %>% 
    left_join(ingreso, by = "BM")
})
names(ssi) = paste0("i", sprintf("%02d", seq(6, 16, 2)))

saveRDS(ssi, "ssi.RDS")

# ade4 --------------------------------------------------------------------
# Carga de datos
library(ade4)
library(adegraphics)

# Convertir matrices de los diferentes años en una matriz larga
ssi.long = do.call(rbind, ssi) %>% 
  arrange(a_o, pais, nomf) %>% 
  tibble::column_to_rownames("nomf")

ssi.var = select(ssi.long, SF:PD) # Mantiene variables
ssi.Hu = ssi.var[, 1:9]           # Bienestar humano
ssi.En = ssi.var[, 10:16]         # Bienestar ambiental
ssi.Ec = ssi.var[, 17:21]         # Bienestar económico
ssi.BM = rep(ssi$i16$BM, 6)       # Nombres de países
ssi.ao = ssi.long$a_o             # Vector de años de los datos

# Convierte la matriz en clases propias de ade4
wit1 = withinpca(ssi.Hu, ssi.ao, scannf = F, scaling = "total")
Enpca = dudi.pca(ssi.En, scale = F, scannf = F, nf = 2)
wit2 = wca(Enpca, ssi.ao, scannf = F, nf = 2)
kta1 = ktab.within(wit1, colnames = ssi.BM)
kta2 = ktab.within(wit2, colnames = ssi.BM)
ssi.HuEn.costatis = costatis(kta1, kta2, scannf = F)

# Cálculo de costatis
costatis1 <- costatis(kta1, kta2, scannf = FALSE)

sa1 <- s.arrow(costatis1$c1 * 4, xlim = c(-3, 3), ylim = c(-3, 3), 
               plot = FALSE)
sc1 <- s.class(costatis1$supIX, factor(ssi.BM), ellipseSize = 0, 
               xlim = c(-3, 3), ylim = c(-3, 3), plabel.col = "red", 
               plot = FALSE)
s1 <- superpose(sa1, sc1)
s1
sa2 <- s.arrow(costatis1$l1 * 3, xlim = c(-3, 3), ylim = c(-3, 3), 
               plot = FALSE)
sc2 <- s.class(costatis1$supIY, factor(ssi.BM), ellipseSize = 0, 
               xlim = c(-3, 3), ylim = c(-3, 3), plabel.col = "blue", 
               plot = FALSE)
s2 <- superpose(sa2, sc2)
s2
# ADEgS(list(s1, s2))


# KTensorGraphs -----------------------------------------------------------
library(KTensorGraphs)

nom_col = as.character(mnem.ssi$Variable)
nom_fil = ssi$i16$BM
nom_alt = as.character(seq(2006, 2016, 2))

ssi.var_ = lapply(ssi, function(x) {
  d = select(x, SF:PD)
})
names(ssi.var_) = paste0("i", sprintf("%02d", seq(6, 16, 2)))

SSI = array(as.matrix(do.call(cbind, ssi.var_)),
            dim = c(nrow(ssi$i16), ncol(ssi.var_$i16), length(nom_alt)),
            dimnames = list( nom_fil, nom_col, nom_alt))

Hu = SSI[, 1:9, ]
En = SSI[, 10:16, ]
Ec = SSI[, 17:21, ]

clr_Hu = rep("#4f5b66", 9) # Human Wellbeing
clr_En = rep("#854442", 7) # Environmental Wellbeing
clr_Ec = rep("#8caba8", 5) # Economical Wellbeing
clr_col = c(clr_Hu, clr_En, clr_Ec)

clr_r = case_when(
  ssi$i16$Ingreso == "A"  ~ "#0084ff",
  ssi$i16$Ingreso == "MA" ~ "#44bec7",
  ssi$i16$Ingreso == "MB" ~ "#d696bb",
  ssi$i16$Ingreso == "B"  ~ "#fa3c4c"
)

COSTATIS(
  Hu,
  En
)
# 
# COSTATIS(
#   Hu,
#   En,
#   dimX = 1,
#   dimY = 2
# )

COSTATIS(
  Hu,
  En,
  dimX = 1,
  dimY = 2,
  coloresf = clr_r,
  coloresc1 = clr_Hu,
  coloresc2 = clr_En,
  contr = T
)

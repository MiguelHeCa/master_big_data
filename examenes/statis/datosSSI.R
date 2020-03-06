
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

# ade4 --------------------------------------------------------------------
library(ade4)
library(adegraphics)

ssi.long = do.call(rbind, ssi) %>% 
  arrange(a_o, pais, nomf) %>% 
  tibble::column_to_rownames("nomf")

ssi.var = select(ssi.long, SF:PD)
ssi.Hu = ssi.var[, 1:9]
ssi.En = ssi.var[, 10:16]
ssi.Ec = ssi.var[, 17:21]
ssi.BM = rep(ssi$i16$BM, 6)
ssi.ao = ssi.long$a_o

wit1 = withinpca(ssi.Hu, ssi.ao, scannf = F, scaling = "total")
Enpca = dudi.pca(ssi.En, scale = F, scannf = F, nf = 2)
wit2 = wca(Enpca, ssi.ao, scannf = F, nf = 2)
kta1 = ktab.within(wit1, colnames = ssi.BM)
kta2 = ktab.within(wit2, colnames = ssi.BM)
ssi.HuEn.costatis = costatis(kta1, kta2)
costatis1 <- costatis(kta1, kta2, scannf = FALSE)

sa1 <- s.arrow(costatis1$c1 * 4, xlim = c(-3, 3), ylim = c(-3, 3), 
               plot = FALSE)
sc1 <- s.class(costatis1$supIX, factor(ssi.BM), ellipseSize = 0, 
               xlim = c(-3, 3), ylim = c(-3, 3), plabel.col = "red", 
               plot = FALSE)
s1 <- superpose(sa1, sc1)
sa2 <- s.arrow(costatis1$l1 * 3, xlim = c(-3, 3), ylim = c(-3, 3), 
               plot = FALSE)
sc2 <- s.class(costatis1$supIY, factor(ssi.BM), ellipseSize = 0, 
               xlim = c(-3, 3), ylim = c(-3, 3), plabel.col = "blue", 
               plot = FALSE)
s2 <- superpose(sa2, sc2)
ADEgS(list(s1, s2))


# KTensorGraphs -----------------------------------------------------------
library(KTensorGraphs)

nom_fil = ssi$i16$BM
nom_alt = as.character(seq(2006, 2016, 2))

ssi.var = lapply(ssi, function(x) {
  d = select(x, SF:PD)
})
names(ssi.var) = paste0("i", sprintf("%02d", seq(6, 16, 2)))

SSI = array(as.matrix(do.call(cbind, ssi.var)),
            dim = c(nrow(ssi$i16), ncol(ssi.var$i16), length(nom_alt)),
            dimnames = list( nom_fil, nom_ssi, nom_alt))

clr_Hu = rep("#ffc300", 9) # Human Wellbeing
clr_En = rep("#bcff00", 7) # Environmental Wellbeing
clr_Ec = rep("#ff4400", 5) # Economical Wellbeing
clr_col = c(clr_Hu, clr_En, clr_Ec)

clr_r = case_when(
  ssi$i16$Ingreso == "A"  ~ "#0084ff",
  ssi$i16$Ingreso == "MA" ~ "#44bec7",
  ssi$i16$Ingreso == "MB" ~ "#d696bb",
  ssi$i16$Ingreso == "B"  ~ "#fa3c4c"
)

Hu = SSI[, 1:9, ]
En = SSI[, 10:16, ]
Ec = SSI[, 17:21, ]

costatis.ssi = KTensorGraphs::COSTATIS(
  Hu,
  En
)

COSTATIS(
  Hu,
  En,
  dimX = 1,
  dimY = 2,
  coloresf = clr_r,
  coloresc1 = clr_Hu,
  coloresc2 = clr_En
)

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

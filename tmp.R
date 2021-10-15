library(sf)
library(rnaturalearth)

library("eurostat")

# Fond de carte

# Creation d'un fond de carte hybride compatible avec un maillage homogène nuts2/3.
# Tous les les pays sont au niveau nuts3 version 2016 sauf l'Autriche, la Belgique, la Suisse, l'Allemagne, la Grèce, les Pays-Bas, la Turquie, l'Irlande, l'Islande et la Norvège.
# Pour des raisons de disponibilité des données post Brexit, Le Royaume-Uni est au niveau nuts2 vesrion 2013. 

nuts2016 <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "20",
  nuts_level = "all",
  year = "2016"
)

nuts2016_3 <- nuts2016[nuts2016$LEVL_CODE == 3, ]
nuts2016_2 <- nuts2016[nuts2016$LEVL_CODE == 2, ]

N2 <-
  c("AT", "BE", "CH", "DE", "EL", "NL", "UK", "TR", "IE", "IS", "NO")
nuts <- rbind(nuts2016_2[nuts2016_2$CNTR_CODE %in% N2, ],
              nuts2016_3[!nuts2016_3$CNTR_CODE %in% N2, ])

nuts <- nuts[nuts$CNTR_CODE != "UK", ]

nuts <- nuts[,c("id","NUTS_NAME","geometry")]
colnames(nuts) <- c("id","name","geometry")

nuts2013 <- get_eurostat_geospatial(
  output_class = "sf",
  resolution = "20",
  nuts_level = "2",
  year = "2013"
)

uk = nuts2013[nuts2013$CNTR_CODE == "UK",c("id","NUTS_NAME","geometry")]
colnames(uk) <- c("id","name","geometry")

nuts <- rbind(nuts, uk)


nuts <-
  nuts[!nuts$id %in% c("FRY10", "FRY20", "FRY30", "FRY40", "FRY50"), ]
nuts <- nuts[nuts$id != "RS", ]

plot(st_geometry(nuts))

# Données

# Import des données statistiques (PIB par habitant en euros zn 2016)

var <- "nama_10r_3gdp"
gdpinh <- get_eurostat(var, time_format = "num")
gdpinh <- subset(gdpinh, gdpinh$unit == "EUR_HAB")
gdpinh <- reshape2::dcast(gdpinh, geo ~ time, value.var = "values")
fields <- c("geo", "2016")
gdpinh <- gdpinh[, fields]
colnames(gdpinh) <- c("id","GDPINH_2016")

# Les données manquantes sont issues de la base de données ESPON et d'estimations

missing <- read.csv("data/missing.csv")
gdpinh = rbind(gdpinh, missing)

# Pour des questions de reproductibilité, nous sauvegardons les données dans le répertoire data.

write.csv(gdpinh, "data/gdpinh.csv")

# Jointure

nuts <- merge(
  x = nuts,
  y = gdpinh,
  by = "id",
  all.x = TRUE
)

View(nuts)

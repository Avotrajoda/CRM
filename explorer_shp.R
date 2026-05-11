library(sf)

# Charger le shapefile
shp <- st_read("input/district/Mada_district.shp", quiet = TRUE)

# Afficher les colonnes
cat("=== COLONNES DU SHAPEFILE ===\n")
print(colnames(shp))

cat("\n=== PREMIERE LIGNE ===\n")
print(st_drop_geometry(shp[1, ]))

cat("\n=== TOUS LES NOMS UNIQUES ===\n")
for (col in colnames(shp)) {
  if (col != "geometry") {
    cat(sprintf("\n%s (unique values: %d)\n", col, n_distinct(shp[[col]])))
    print(head(unique(shp[[col]]), 10))
  }
}

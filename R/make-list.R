library(dplyr)
library(WorldFlora)
library(readr)
library(here)
library(stringr)
library(glue)

# Create output directory
out_dir <- "outputs"
dir.create(here(out_dir), showWarnings=FALSE)

# Load GlobalTreeSearch NC
gts <- read_csv(
  here("data", "globaltreesearch_results_nc.csv")) |>
  mutate(genus=sapply(strsplit(taxon, " "), "[", 1))

## # Load WFO database
## load("/home/ghislain/Botanique/WorldFlora/WFO_data.rda")

## # Check names
## df_wfo_match <- WFO.match(
##   spec.data=gts,
##   spec.name="taxon", 
##   no.dates=TRUE,
##   WFO.data=WFO.data,
##   verbose=TRUE)

## # Write results
## readr::write_csv(
##   df_wfo_match,
##   file=here(out_dir, "wfo-match.csv"))

# Number of genus per family
n_genus_family <- gts |>
  group_by(family) |>
  count(genus) |>
  summarize(n_genus=n())

# Number of species per family
n_species_family <- gts |>
  group_by(family) |>
  summarize(n_species=n())

# Number of genus and species per family
n_genus_species_family <- n_genus_family |>
  left_join(n_species_family, by="family")

# Number of species per genus
n_species_genus <- gts |>
  group_by(family) |>
  count(genus) |>
  rename(n_species=n) |>
  ungroup()
n_species <- sum(n_species_genus$n_species)

# Sorted list of genus (vector)
genus_list <- n_species_genus |>
  select(genus) |> arrange(genus) |>
  pull()
  
# List of genus per family
genus_family <- gts |>
  group_by(family) |>
  summarize(genus_list=paste(sort(unique(genus)), collapse=",")) |>
  select(genus_list) |> pull() |>
  str_split(",")

# List of species per genus
species_genus <- gts |>
  group_by(genus) |>
  summarize(species_list=paste(sort(unique(taxon)), collapse=",")) |>
  select(species_list) |> pull() |>
  str_split(",")

# ==============================
# Make the org file with headers
# ==============================

# Header
header <- "#+include: intro.org\n"

# Make text
n_families <- nrow(n_genus_species_family)
text <- c(header)
for (i in 1:n_families) {
  family <- n_genus_species_family["family"] |> pull() |> getElement(i)
  ng <- n_genus_species_family["n_genus"] |> pull() |> getElement(i)
  ns <- n_genus_species_family["n_species"] |> pull() |> getElement(i)
  line <- glue("* {family} ({ng} genus, {ns} species)")
  text <- c(text, line)
  for (j in 1:ng) {
    gen <- genus_family[[i]][j]
    ns <- n_species_genus |> filter(genus==gen) |>
      select(n_species) |> pull()
    line <- glue("** {gen} ({ns} species)")
    text <- c(text, line)
    for (k in 1:ns) {
      w <- which(genus_list==gen)
      species <- species_genus[[w]][k]
      line <- glue("*** /{species}/")
      text <- c(text, line)
    }
  }
}

# Write the text to the org file
fileConn <- file(here("org", "tree-list.org"), open="w")
writeLines(text, fileConn)
close(fileConn)

# End of file

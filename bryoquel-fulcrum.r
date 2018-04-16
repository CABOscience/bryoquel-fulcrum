# Convert Bryoquel database into a format to import into Fulcrum
# Etienne Laliberte
# First created April 16 2018
# Bryoquel version 3.3

# load libraries
library(dplyr)


# read bryoquel data
taxon <- read.delim('taxon.txt')
vernacularname <- read.delim('vernacularname.txt')
distribution <- read.delim('distribution.txt')


# only keep a subset of taxon
taxon.sub <- taxon %>%
  filter(taxonomicStatus == 'Accepté')

# check that id = taxonID across all records
all(taxon.sub$id == taxon.sub$taxonID) # TRUE

# check number of rows of vernacular names
nrow(vernacularname)

# extract french vernacular names
verna.sub.fr <- vernacularname %>%
  filter(isPreferredName == 'True',
         language == 'Français') %>%
  semi_join(taxon.sub, by = 'id') %>%
  rename(taxonID = id,
         vernaculaNameFR = vernacularName) %>%
  select(taxonID, vernaculaNameFR)
nrow(verna.sub.fr) # 850

# extract english vernacular names
verna.sub.en <- vernacularname %>%
  filter(isPreferredName == 'True',
         language == 'Anglais') %>%
  semi_join(taxon.sub, by = 'id') %>%
  rename(taxonID = id,
         vernaculaNameEN = vernacularName) %>%
  select(taxonID, vernaculaNameEN)
nrow(verna.sub.en) # 850

# get subset of distribution
distribution.sub <- distribution %>%
  semi_join(taxon.sub, by = 'id') %>%
  rename(taxonID = id) %>%
  mutate(Bryoquel_version = 3.3)


# collapse distribution by taxon ID
distribution.collapsed <- distribution.sub %>%
  group_by(taxonID) %>%
  summarise(localities = toString(unique(locality)))

# add French and English vernacular names, and description
taxon.sub2 <- taxon.sub %>%
  left_join(verna.sub.en, by = 'taxonID') %>%
  left_join(verna.sub.fr, by = 'taxonID') %>%
  left_join(distribution.collapsed, by = 'taxonID') %>%
  mutate(Bryoquel_version = 3.3)

# create csv for import into Fulcrum
write.csv(taxon.sub2, file = 'bryoquel_taxon_fulcrum_v3_3.csv', row.names = F,
          na = '', quote = F)

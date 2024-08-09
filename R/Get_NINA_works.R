library(openalex)
library(openalexR)
library(tidyverse)
Norway_insts <- list(
  entity = "institutions",
  country_code = "no",
  verbose = TRUE
)

Nor_insts<-do.call(oa_fetch, c(Norway_insts))
Nor_insts

Nor_insts |> 
  filter(grepl("Nature Research", display_name)) |> 
  select(id)


NINA <- list(
  entity = "works",
  authorships.institutions.id="I4210153706",
  verbose = TRUE
)
NINA_works<-do.call(oa_fetch, c(NINA))


saveRDS(NINA_works, "data/NINA_works.RDS")
#NINA_works |> view()

str(NINA_works$author[1])

extract_country_count <- function(authors_df) {
  # Remove rows where the institution is "Norwegian Institute for Nature Research"
  filtered_df <- authors_df[authors_df$institution_display_name != "Norwegian Institute for Nature Research", ]
  
  # Count occurrences of each country
  country_count <- filtered_df %>%
    group_by(institution_country_code) %>%
    summarise(count = n()) %>%
    ungroup()
  
  return(country_count)
}



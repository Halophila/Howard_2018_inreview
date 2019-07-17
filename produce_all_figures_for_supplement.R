library(here)
list_of_fig_files <- list.files(here("Supplemental_in_Latex")) %>% 
  str_subset(pattern = ".R")

for(i in seq_along(list_of_fig_files)){
  source(here("Supplemental_in_Latex", list_of_fig_files[i]))
}


list_of_figs <- list.files(here()) %>% 
  str_subset(pattern = "png")

for(i in seq_along(list_of_figs)){
  file.rename(from = here(list_of_figs[i]), 
              to = here("Supplemental_in_Latex", list_of_figs[i]))
}

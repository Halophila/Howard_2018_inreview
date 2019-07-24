# Manuscript figures, data, and more

1. Run `produce_all_figures.R` in R. This will run all scripts in the `figures` directory and produce pdf's of each figure for the manuscript body.

2. Run `produce_all_figures_for_supplement.R` in R. This will run all scripts in the `Supplemental_in_Latex` directory and produce pdf's of each figure for the online supplement.

3. Run `supplemental_material.tex` in Latex. This will typeset text and figures into a very pretty pdf.

**Figures directory** - Contains R scripts for all figures for manuscript with stats mixed in

**Supplemental_in_Latex** - Contains R scripts for all figures in the supplemental material as well as a LaTeX file for typesetting a journal-ready online supplement 

**Supplemental_material_in_md.Rmd* - an R markdown alternative version of the online supplement. 

All data are contained in CSV files and their description is in the *metadata.md* file

This repository is to reproduce Holz et al. 2022 - Assessing extinction risk across the geographic ranges of plant species in Europe.

The code was run with the following version of R: 4.1.1.

There are three folders in the repository: data, R, and figures.

In the data folder you will find all national red lists and our data outputs. The main outputs are as follows:
1. "iucn_rl.csv" contains species that occur only in Europe and are threatened in at least one country.
We give the number of countries where they occur (no_countries), the number of countries where they are listed as threatened (no_countries_threatened), 
the resulting percentage of countries where they are threatened (percentage_threatened), and whether they have an IUCN Global Red List category (redlistCategory).
2. "rlspecies_distribution.csv" contains for each species the list of countries where it occurs (botanical_countries_l3), 
whether it is threatened in a particular country (Threatened), and whether it is endemic to Europe (europe_endemic). 
Also, the plant identification key for POWO (plant_name_id) is given.

In the R folder you will find our complete analysis. The R files have following rational:
1. "00_preamble.R" lists all the packages used in our analyis.
2. "01_rldata.R" loads each national Red List, filters threatened species, and combines all lists.
3. "02_rlharmonization.R" harmonizes the different name inputs across red lists and checks for synonyms.
4. "03_rlbotanicalcountries.R" assigns political countries, a botanical country. This is necessary to make use of the species geographic distribution data from POWO.
5. "04_kewdata.R" loads the POWO data, which is not in our data folder due to large file size.
6. "05_redlist_kew_merge.R" creates the data output #2 ("rlspecies_distribution.csv", see above)
7. "06_threatened_range.R" primarily calculates for each European endemic species extinction risk across its geographic range.
8. "07_iucn_compare.R" looks whether species that are threatened in 100% of their range have an IUCN Global Red List category.
9. "08_figures.R" produces some of the figures. Maps were produced in QGis. Inkscape was used to combine figures.

In case you have any questions, don't hesitate to contact us (ingmar.staude@idiv.de)

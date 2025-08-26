## ----setup---------------------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| include: false
#| echo: false

#install.packages("easypackages")
pacman::p_load(
  "animation",
  "BIOMASS",
  "dataMaid",
  "dplyr",
  "extrafont",
  "htmltools",
  "janitor",
  "kableExtra",
  "knitr",
  "openxlsx",
  "tinytex")

knitr::opts_chunk$set(
            echo = TRUE, 
            message = FALSE, 
            warning = FALSE,
            error = TRUE, 
            comment = NA, 
            tidy.opts = list(width.cutoff = 60))

options(htmltools.dir.version = FALSE, htmltools.preserve.raw = FALSE)
sf::sf_use_s2(use_s2 = FALSE)


div.column {
    display: inline-block;
    vertical-align: top;
    width: 50%;
}

#TOC::before {
  content: "";
  display: block;
  height: 50px;
  width: 200px;
  background-image: url(https://verra.org/wp-content/uploads/2024/11/VCS-overview-300x254-1.webp);
  background-size: contain;
  background-position: center;
  background-position: 50% 50%;
  padding-top: 5px !important;
  background-repeat: no-repeat;
}

## ----dummy-import--------------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| echo: true

set.seed(333)
dataset_raw <- openxlsx::read.xlsx("./assets/dataset_raw.xlsx")
write.csv(dataset_raw, "./assets/dataset_tidy.csv", row.names = FALSE)
dataset_tidy <- read.csv("./assets/dataset_tidy.csv")
dataset_tidy |> kbl() |> kable_styling()


## ----dummy-audit---------------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: false
#| echo: true

# str(dataset_tidy)
# dplyr::count(dataset_tidy, Species..j.)
# 
# saveHTML(dataMaid::makeDataReport(
#   dataset_tidy,
#   output = "html",
#   codebook = TRUE,
#   onlyProblematic = TRUE,
#   visuals = setVisuals(all = "basicVisual"),
#   replace = TRUE
#   )
# ) # output "dataMaid_dataset_tidy.html" shown in Appendix A.1


## ----dummy-audit-quiet---------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: true
#| echo: false

str(dataset_tidy)
dplyr::count(dataset_tidy, Species..j.)


## ----dummy-audit-tidy----------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: true
#| echo: true

data.table::setnames(dataset_tidy, old = "Stratum..i.", new = "stratum_i", skip_absent = TRUE)
data.table::setnames(dataset_tidy, old = "Species..j.", new = "species_j", skip_absent = TRUE)
data.table::setnames(dataset_tidy, old = "Plot..sp.", new = "plot_sp", skip_absent = TRUE)
data.table::setnames(dataset_tidy, old = "Tree..l.", new = "tree_l", skip_absent = TRUE)
data.table::setnames(dataset_tidy, old = "Volume..V_.l.j.I.sp..", new = "volume", skip_absent = TRUE)
dataset_tidy$species_j[dataset_tidy$species_j == "sp4"] <- "Sp4"
dataset_tidy$species_j  = as.factor(dataset_tidy$species_j)
dataset_tidy$stratum_i  = as.factor(dataset_tidy$stratum_i)


## ----dummy-audit-save----------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: true
#| echo: true

dataset_tidy$bcef_r     = 0.7
dataset_tidy$cf         = 0.5
dataset_tidy$d          = 0.5
dataset_tidy$a_sp       = 0.1
dataset_tidy$a_sp_m2    = dataset_tidy$a_sp * 10000

dataset_tidy = dataset_tidy %>%
  group_by(stratum_i) %>%
  mutate(a_I_m2 = sum(a_sp_m2), a_I_ha = sum(a_sp)) 

write.csv(dataset_tidy, "./assets/dataset_tidy.csv", row.names = FALSE)
dataset_tidy |> kbl() |> kable_styling()


## ----eq1-----------------------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: true
#| echo: true

dataset_tidy = dataset_tidy |>
  group_by(species_j, stratum_i, plot_sp) |>
  mutate(vji_sp_m3 = sum(volume))

# compute new variable 'vji_sp_m3'
data.table::setDT(dataset_tidy)[, .(
  vji_sp_m3 = sum(volume)
  ),
  by = .(stratum_i, plot_sp, species_j)
] |> kbl() |> kable_styling()


## ----eq2-----------------------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: true
#| echo: true

dataset_tidy = dataset_tidy |>
  group_by(stratum_i, species_j) |>
  mutate(vji_ha_m3 = mean(vji_sp_m3) * 10)

data.table::setDT(dataset_tidy)[, .(
  vji_sp_m3,
  vji_ha_m3
  ),
  by = .(stratum_i, species_j)
] |> kbl() |> kable_styling()


## ----eq3-----------------------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: true
#| echo: true


dataset_tidy <- dataset_tidy %>%
  mutate(chb_ha_tC = vji_ha_m3 * bcef_r * cf)

data.table::setDT(dataset_tidy)[, .(
  vji_sp_m3,
  vji_ha_m3,
  chb_ha_tC
  ),
  by = .(stratum_i, species_j)
] |> kbl() |> kable_styling()


## ----eq4-----------------------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: true
#| echo: true

dataset_tidy <- dataset_tidy %>%
  mutate(cex_ha_tC = vji_ha_m3 * d * cf)

data.table::setDT(dataset_tidy)[, .(
  vji_sp_m3,
  vji_ha_m3,
  chb_ha_tC,
  cex_ha_tC
  ),
  by = .(stratum_i, species_j)
] |> kbl() |> kable_styling()


## ----eq6-----------------------------------------------------------------------------------------
#| warning: false
#| message: false
#| error: false
#| eval: true
#| echo: true

dataset_tidy = dataset_tidy %>%
  mutate(f_rsd = 1.74)

dataset_tidy = dataset_tidy %>%
  mutate(c_rsd = cex_ha_tC * f_rsd)

data.table::setDT(dataset_tidy)[, .(
  vji_sp_m3,
  vji_ha_m3,
  chb_ha_tC,
  c_rsd
  ),
  by = .(stratum_i, species_j)
] |> kbl() |> kable_styling()


## ------------------------------------------------------------------------------------------------
#| eval: false
# htmltools::includeHTML("dataMaid_dataset_tidy.html")


## ------------------------------------------------------------------------------------------------

# convert markdown to script.R 
knitr::purl("VM0010-starter-template.qmd")

# display environment setup
devtools::session_info()




library(haven)
library(dplyr)

#loading data 

data <- read_dta("C:/Users/eleac/Documents/Mageval/M2/Mémoire/Spatial econ/merged2024.dta")

head(data)

############################################################
############################################################
#(a) NETTOYAGE, CREATION SHAPEFILE 

#DROP USELESS VARIABLES

# loc    : 100% missing (1,697,889 NAs) → no information
# ageb   : constant = 0 for all rows → no variation, useless
# _merge : Stata merge artifact (administrative flag) → not analytical

data <- data %>%
  select(-loc, -ageb, -`_merge`)

#DROP EMPTY ROWS (1,374 obs.)

# 1,374 rows have NAs on almost all key variables (state, municipality, urban/rural, sex, age, weights...) --> out-of-scope units or data entry errors.
#they cannot be used in any analysis → dropped.

data <- data %>%
  filter(!is.na(ent) & !is.na(mun) & !is.na(ur))

cat("Rows after dropping empty rows:", nrow(data), "\n")


#RECODE MISSING VALUES

# 99 often means "not specified" / non-response, keeping them as numbers would distort means and regressions → set to NA.

data <- data %>%
  mutate(
    cs_p13_1  = na_if(cs_p13_1, 99),   # education level: 99 = not specified
    anios_esc = na_if(anios_esc, 99),   # years of schooling: 99 = not specified
    mes_cal   = na_if(mes_cal,   99),   # calendar month: 99 = not specified
    e_con     = na_if(e_con,      9),   # marital status: 9 = not specified
    n_hij     = ifelse(n_hij >= 20, NA, n_hij)  # 20+ children = implausible
  )


#FIX NUMERICAL VARIABLES

# hrsocup: hours worked per week. Max possible = 168 (24h × 7 days).
# Values above 168 are physically impossible → NA.
data <- data %>%
  mutate(hrsocup = ifelse(hrsocup > 168, NA, hrsocup))

#CONVERT CATEGORICAL VARIABLES TO FACTORS

#variables stored as dbl+lbl (Stata label) are converted to R factors

vars_factor <- c(
  "ent", "sex", "ur", "zona", "clase1", "clase2", "pos_ocu",
  "rama", "dur9c", "medica5c", "rama_est1", "rama_est2", "dur_est",
  "ambito1", "ambito2", "tue1", "tue2", "tue3", "busqueda",
  "d_ant_lab", "d_cexp_est", "dur_des", "sub_o", "tip_con",
  "dispo", "nodispo", "c_inac5c", "niv_ins", "eda5c", "eda7c",
  "eda12c", "hij5c", "cp_anoc", "imssissste", "ma48me1sm",
  "p14apoyos", "scian", "t_tra", "emp_ppal", "tue_ppal",
  "trans_ppal", "mh_fil2", "mh_col", "sec_ins", "mes_cal", "per"
)

data <- data %>%
  mutate(across(all_of(vars_factor), as_factor))

#RENAME ALL VARIABLES TO ENGLISH

data <- data %>%
  rename(
    # Geography
    municipality         = mun,
    state                = ent,
    sampling_unit        = upm,
    stratum              = est,
    stratum_quarterly    = est_d_tri,
    stratum_monthly      = est_d_men,
    urban_rural          = ur,
    wage_zone            = zona,
    municipality_id      = mun_id,
    # Survey period
    survey_period        = per,
    calendar_month       = mes_cal,
    time_index           = tq,
    year2digit           = year2,
    # Weights
    weight_quarterly     = fac_tri,
    weight_monthly       = fac_men,
    min_wage_monthly     = salario,
    # Demographics
    sex                  = sex,
    age                  = eda,
    age_group5           = eda5c,
    age_group7           = eda7c,
    age_group12          = eda12c,
    education_level      = cs_p13_1,
    years_schooling      = anios_esc,
    education_employed   = niv_ins,
    marital_status       = e_con,
    nb_children          = n_hij,
    nb_children_group    = hij5c,
    # Labor market
    lf_status            = clase1,
    lf_status2           = clase2,
    occupation_pos       = pos_ocu,
    sector               = rama,
    sector_agg           = rama_est1,
    sector_detail        = rama_est2,
    industry_naics       = scian,
    institutional_sec    = sec_ins,
    # Formal / informal employment
    informal_emp         = emp_ppal,     # ← MAIN DEPENDENT VARIABLE
    unit_type_main       = tue_ppal,
    nb_jobs              = t_tra,
    cross_border         = trans_ppal,
    # Hours & earnings
    hours_worked         = hrsocup,
    monthly_income       = ingocup,
    hourly_wage          = ing_x_hrs,
    underemployed        = sub_o,
    long_hours_low_pay   = ma48me1sm,
    # Economic unit
    unit_size1           = ambito1,
    unit_size2           = ambito2,
    unit_type1           = tue1,
    unit_type2           = tue2,
    unit_type3           = tue3,
    self_emp_unskilled   = cp_anoc,
    # Working conditions
    hours_category       = dur9c,
    hours_category2      = dur_est,
    health_benefits      = medica5c,
    social_security      = imssissste,
    contract_type        = tip_con,
    extra_job_search     = busqueda,
    receives_transfer    = p14apoyos,
    # Unemployment
    unemp_experience     = d_ant_lab,
    unemp_condition      = d_cexp_est,
    unemp_duration       = dur_des,
    # Inactivity
    available_inactive   = dispo,
    unavailable_inactive = nodispo,
    inactivity_reason    = c_inac5c,
    # Hussmanns matrix (ILO)
    hussmanns_row        = mh_fil2,
    hussmanns_col        = mh_col,
    # Violence
    homicide_count       = homicide     # ← MAIN EXPLANATORY VARIABLE
  )


#TRANSLATE FACTOR LABELS TO ENGLISH (used IA here)

data <- data %>%
  mutate(
    sex = recode(as.character(sex),
                 "Hombre" = "Male",
                 "Mujer"  = "Female"),
    
    urban_rural = recode(as.character(urban_rural),
                         "Urbano" = "Urban",
                         "Rural"  = "Rural"),
    
    wage_zone = recode(as.character(wage_zone),
                       "Zona A" = "Zone A",
                       "Zona B" = "Zone B"),
    
    lf_status = recode(as.character(lf_status),
                       "No aplica"                          = "Not applicable",
                       "Población económicamente activa"    = "Economically active",
                       "Población no económicamente activa" = "Not economically active"),
    
    lf_status2 = recode(as.character(lf_status2),
                        "No aplica"            = "Not applicable",
                        "Población ocupada"    = "Employed",
                        "Población desocupada" = "Unemployed",
                        "Disponibles"          = "Available (not searching)",
                        "No disponibles"       = "Not available"),
    
    informal_emp = recode(as.character(informal_emp),
                          "No aplica"       = "Not applicable",
                          "Empleo informal" = "Informal",
                          "Empleo formal"   = "Formal"),
    
    marital_status = recode(as.character(marital_status),
                            "Menor de doce años" = "Under 12",
                            "Unión libre"        = "Cohabiting",
                            "Separado(a)"        = "Separated",
                            "Divorciado(a)"      = "Divorced",
                            "Viudo(a)"           = "Widowed",
                            "Casado(a)"          = "Married",
                            "Soltero(a)"         = "Single"),
    
    sector_agg = recode(as.character(sector_agg),
                        "No aplica"        = "Not applicable",
                        "Primario"         = "Primary",
                        "Secundario"       = "Secondary",
                        "Terciario"        = "Tertiary",
                        "No especificado"  = "Unspecified"),
    
    inactivity_reason = recode(as.character(inactivity_reason),
                               "No aplica"             = "Not applicable",
                               "Estudiantes"           = "Student",
                               "Quehaceres domésticos" = "Homemaker",
                               "Pensiones y jubilados" = "Retired/Pensioner",
                               "Limitación física"     = "Physical limitation",
                               "Otra razón"            = "Other")
  )

# CREATE ANALYTICAL VARIABLES


data <- data %>%
  mutate(
    # Binary: 1 = informal, 0 = formal (main dependent variable)
    informal_binary = case_when(
      informal_emp == "Informal" ~ 1,
      informal_emp == "Formal"   ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Binary: currently employed
    employed = ifelse(lf_status2 == "Employed", 1, 0),
    
    # Log income (for regressions) — avoids log(0)
    log_income    = ifelse(monthly_income > 0, log(monthly_income), NA),
    log_hourly    = ifelse(hourly_wage    > 0, log(hourly_wage),    NA),
    
    # Readable period label
    period_label  = paste0(year, "-Q", quarter)
  )

# FINAL CHECK

cat("\n===== FINAL SUMMARY =====\n")
cat("Dimensions:", nrow(data), "x", ncol(data), "\n")

cat("\nTop missing values:\n")
colSums(is.na(data)) %>% sort(decreasing = TRUE) %>% head(10) %>% print()

cat("\ninformal_emp distribution:\n")
table(data$informal_emp, useNA = "always") %>% print()

cat("\nsex distribution:\n")
table(data$sex, useNA = "always") %>% print()

saveRDS(data, "ENOE_2024_clean_EN.rds")

####eventuels pb ?

#213468 NA homicide count ? 
table(data$urban_rural[is.na(data$homicide_count)])

#166130 NA for rural vs 47338 for urbain --> potential selection biais (villes petites = peu de capacité administrative)
# non random missingness (as rural areas with unrecorded violence likely exhibit both higher female informality and higher actual violence rates)

############################################################
####### CREATION OF SHAPEFILE #########

#WORKING AT THE STATE LEVEL (32 states)
install.packages("geodata")
install.packages("terra")  # requis par geodata

library(geodata)
library(terra)

#DOWLOAD OF THE SHAPEFIL OF MEXICO AT THE STATE LEVEL
mex_states <- gadm(country = "MEX", level = 1, path = tempdir())

plot(mex_states)

####

#AGGREGATION AT STATE LEVEL

library(terra)
library(sf)

#FILTER: WOMEN AGED 15+

#mexican labor law (Constitutional reform, 2014): legal working age is 15. We keep all women 15+ (not just employed) to avoid selection bias.

data_women <- data %>%
  filter(sex == "Female", age >= 15)

cat("Women aged 15+ :", nrow(data_women), "\n")

#each state = one observation for the spatial regression.
#Y = informal_rate : share of informal employment among women who are employed (active population)
#main explanatory variable X = average homicide count per states (homicide_avg), measure of local violence exposure
#control variables: education. (more educated women --> more likely to have formal employment), age (experience effect on labor market integration), urban (urban areas have more formal jobs), marital status, children

state_data <- data_women %>%
  group_by(state) %>%
  summarise(
    informal_rate = mean(informal_binary, na.rm = TRUE),
    homicide_avg  = mean(homicide_count, na.rm = TRUE),
    avg_schooling  = mean(years_schooling, na.rm = TRUE),
    avg_age        = mean(age, na.rm = TRUE),
    share_urban    = mean(urban_rural == "Urban", na.rm = TRUE),
    share_married  = mean(marital_status %in%
                            c("Married", "Cohabiting"), na.rm = TRUE),
    avg_children   = mean(nb_children, na.rm = TRUE),
    participation_rate = mean(lf_status2 == "Employed", na.rm = TRUE),
    
#nb of observations per state (quality check)
    n_obs = n()
    
  ) %>%
  ungroup()

cat("Number of states :", nrow(state_data), "\n")
print(state_data)

#LOAD SHAPEFILE

mex_states <- gadm(country = "MEX", level = 1, path = tempdir())
mex_sf     <- st_as_sf(mex_states)

#check state names in shapefile
cat("\nState names in shapefile:\n")
print(mex_sf$NAME_1)

#HARMONIZE STATE NAMES BEFORE MERGE

#the shapefile uses Spanish names with accents. Our "state" factor also uses Spanish names from Stata labels.We manually fix known mismatches.

state_data <- state_data %>%
  mutate(state_name = as.character(state),
         state_name = recode(state_name,
                             "Coahuila de Zaragoza"    = "Coahuila",
                             "Michoacán de Ocampo"     = "Michoacán",
                             "Veracruz de Ignacio de la Llave" = "Veracruz",
                             "México"                  = "México"
         ))

#check names that don't match
cat("\nStates in data not matching shapefile:\n")
setdiff(state_data$state_name, mex_sf$NAME_1)

cat("\nStates in shapefile not matching data:\n")
setdiff(mex_sf$NAME_1, state_data$state_name)

#MERGE SHAPEFILE + STATE DATA

mex_merged <- mex_sf %>%
  left_join(state_data, by = c("NAME_1" = "state_name"))

cat("\nStates with data :", sum(!is.na(mex_merged$informal_rate)), "\n")
cat("States without data :", sum(is.na(mex_merged$informal_rate)), "\n")

#(b) BORDER VARIABLE

# Create a binary variable = 1 if the state is:
#   - On the northern border (USA)
#   - On the southern border (Guatemala / Belize)
#   - Coastal (Pacific, Gulf of Mexico, Caribbean)

# Method: we manually assign based on geography.

border_states <- c(
  # Northern border with USA
  "Baja California",
  "Sonora",
  "Chihuahua",
  "Coahuila",        # will match after recode
  "Nuevo León",
  "Tamaulipas",
  
  # Southern border with Guatemala / Belize
  "Chiapas",
  "Tabasco",
  "Campeche",
  "Quintana Roo",
  
  # Pacific coast
  "Baja California Sur",
  "Sinaloa",
  "Nayarit",
  "Jalisco",
  "Colima",
  "Michoacán",
  "Guerrero",
  "Oaxaca",
  
  # Gulf of Mexico / Caribbean coast
  "Tamaulipas",      # already in north border
  "Veracruz",
  "Yucatán"
)

#remove duplicates
border_states <- unique(border_states)

mex_merged <- mex_merged %>%
  mutate(border = ifelse(NAME_1 %in% border_states, 1, 0))

cat("\nBorder variable distribution:\n")
table(mex_merged$border)

cat("\nBorder states:\n")
mex_merged %>%
  filter(border == 1) %>%
  pull(NAME_1) %>%
  print()

cat("\nNon-border (interior) states:\n")
mex_merged %>%
  filter(border == 0) %>%
  pull(NAME_1) %>%
  print()

# (c) MAP OF THE MAIN VARIABLES

library(dplyr)
library(sf)
library(ggplot2)
library(viridis)


# Map 1 - informal employment
ggplot(mex_merged) +
  geom_sf(aes(fill = informal_rate), color = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "Informal rate") +
  labs(title = "Female informal employment rate by state (2024)",
       caption = "Source: ENOE 2024") +
  theme_void()
ggsave("map_informal.png", width = 8, height = 6)

# Map 2 - homicides
ggplot(mex_merged) +
  geom_sf(aes(fill = homicide_avg), color = "white") +
  scale_fill_viridis_c(option = "inferno", direction = -1,
                       name = "Avg homicides") +
  labs(title = "Average homicide count by state (2024)",
       caption = "Source: ENOE 2024") +
  theme_void()

ggsave("map_homicides.png", width = 8, height = 6)

#Map 3 : Both information
mex_merged <- mex_merged %>%
  mutate(high_homicide = homicide_avg > median(homicide_avg, na.rm = TRUE))
#install.packages("ggpattern")
library(ggpattern)
ggplot() +
  geom_sf(data = mex_merged,
          aes(fill = informal_rate),
          color = "white") +
  geom_sf(data = mex_merged %>%
            filter(homicide_avg > median(homicide_avg, na.rm = TRUE)),
          fill = NA,
          color = "red",
          size = 1) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "Informal rate") +
  labs(title = "Informal employment and high homicide states (2024)",
       caption = "Source: ENOE 2024") +
  theme_void()

#(note: values represent the average monthly homicide count per municipality, aggregated at the state level (2024)

#(d) STATS DESC

mex_merged %>%
  st_drop_geometry() %>%
  select(informal_rate, homicide_avg, avg_schooling, 
         avg_age, share_urban, avg_children) %>%
  summary()

## Informal rate (Y): on average 52% of employed women work in the informal
# sector, ranging from 30% to 74% across states — high dispersion.

# Homicide avg (X): right-skewed distribution, median = 15 but mean = 32,
# a few very violent states pull the average up (max = 154).

# Years of schooling: low variation across states (9 to 11 years),
# may limit explanatory power in the regression.

# Share urban: on average 63% of women live in urban areas (min=49%, max=83%).

# Average children: stable around 2 children across states.

# Note: 1 NA corresponds to a state that did not match during the merge.




data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_schooling = mean(years_schooling, na.rm = TRUE),
    mean_hours = mean(hours_worked, na.rm = TRUE),
    mean_income = mean(monthly_income, na.rm = TRUE),
    mean_hourly = mean(hourly_wage, na.rm = TRUE),
    informal_rate = mean(informal_binary, na.rm = TRUE),
    employment_rate = mean(employed, na.rm = TRUE)
  )
#Mean age in he sample : 35 years old. Mean hours worked (weekly) 18.6. Informal rate of 0.508 on average.


#STATS DESC BY STATE
state_stats <- data %>%
  group_by(state) %>%
  summarise(
    informal_rate = mean(informal_binary, na.rm = TRUE),
    homicide_avg = mean(homicide_count, na.rm = TRUE),
    mean_income = mean(monthly_income, na.rm = TRUE),
    mean_hours = mean(hours_worked, na.rm = TRUE),
    mean_schooling = mean(years_schooling, na.rm = TRUE),
    employment_rate = mean(employed, na.rm = TRUE),
    n = n()
  )

head(state_stats)
#Highest employment rate is found in Baka Califoria Sur, the lowest is found in ChiapaS;
#Baja California has by far the highest homicide rate

#VIOLENCE DISTRIUTION
summary(data$homicide_count)

quantile(data$homicide_count, probs = c(.1,.25,.5,.75,.9), na.rm = TRUE)
#Hakf of the states have a homicide rate of below 11

#CORRELATION VIOLENCE AND INFORMAL
cor(
  state_stats$informal_rate,
  state_stats$homicide_avg,
  use = "complete.obs"
)

#SCATTER PLOT
library(ggplot2)

ggplot(state_stats, aes(x = homicide_avg, y = informal_rate)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average homicides",
    y = "Informal employment rate",
    title = "Violence and informal employment across Mexican states"
  ) +
  theme_minimal()


#INFORMAL WORK BY CHARACTERISTICS 
#BY SEX
data %>%
  group_by(sex) %>%
  summarise(
    informal_rate = mean(informal_binary, na.rm = TRUE),
    mean_income = mean(monthly_income, na.rm = TRUE),
    mean_schooling = mean(years_schooling, na.rm = TRUE)
  )

#BY SECTOR
data %>%
  group_by(sector_agg) %>%
  summarise(
    informal_rate = mean(informal_binary, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(informal_rate))

#DESCRIPTIVE TAB
install.packages("modelsummary")

library(modelsummary)

datasummary_skim(data)

datasummary(
  informal_binary + homicide_count + years_schooling +
    monthly_income + hours_worked ~ Mean + SD + Min + Max,
  data = data
)


###############################################################################
# SPATIAL ECONOMETRICS PROJECT – PARTS (e), (f), (g)
# Topic: Effect of violence (homicide) on female informal employment in Mexico
# Unit of analysis: 32 Mexican states, 2024
###############################################################################

install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')

install.packages("spatialreg")
library(sf)
library(spdep)
library(spatialreg)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)



# ── LOAD DATA ────────────────────────────────────────────────────────────────
# mex_merged is the sf object created at the end of your previous script
# (32 rows = 32 states, geometry + state-level variables)
# If you saved it, load it:
# mex_merged <- readRDS("ENOE_2024_state_level.rds")


###############################################################################
# (e) TESTING FOR SPATIAL AUTOCORRELATION
###############################################################################

# ── 1. BUILD THE SPATIAL WEIGHTS MATRIX ──────────────────────────────────────
# ── SUPPRIMER LES LIGNES AVEC NA AVANT TOUT ──────────────────────────────────
vars_model <- c("informal_rate", "homicide_avg", "avg_schooling", 
                "avg_age", "share_urban", "share_married", 
                "avg_children", "border")

mex_complete <- mex_merged[complete.cases(st_drop_geometry(mex_merged)[, vars_model]), ]

cat("États avec données complètes :", nrow(mex_complete), "/ 32\n")

# ── CONSTRUIRE LA MATRICE DE POIDS SUR mex_complete ──────────────────────────
nb_queen <- poly2nb(mex_complete, queen = TRUE)   # ← mex_complete ici aussi
lw <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
# Queen contiguity: two states are neighbors if they share at least one border
# point. This is the standard choice for administrative units.
nb_queen <- poly2nb(mex_merged, queen = TRUE)

# Row-standardise (W): each row sums to 1.
# Interpretation: the spatial lag of state i = average of its neighbors' values.
lw <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

cat("====================================================\n")
cat("SPATIAL WEIGHTS MATRIX – SUMMARY\n")
cat("====================================================\n")
summary(nb_queen)
cat("\nNote: zero.policy = TRUE handles Baja California Sur (island-like)\n")

# ── 2. OLS BASELINE (needed to compute residuals for Moran's I) ──────────────

ols_base <- lm(
  informal_rate ~ homicide_avg + avg_schooling + avg_age +
    share_urban + share_married + avg_children + border,
  data = mex_merged
)

cat("\n====================================================\n")
cat("OLS BASELINE MODEL\n")
cat("====================================================\n")
summary(ols_base)

# ── 3. MORAN'S I ON OLS RESIDUALS ────────────────────────────────────────────
# 1. Créer mex_complete
vars_model <- c("informal_rate", "homicide_avg", "avg_schooling", 
                "avg_age", "share_urban", "share_married", 
                "avg_children", "border")

mex_complete <- mex_merged[complete.cases(st_drop_geometry(mex_merged)[, vars_model]), ]

# 2. Construire lw SUR mex_complete
nb_queen <- poly2nb(mex_complete, queen = TRUE)
lw <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

# 3. OLS
ols_base <- lm(
  informal_rate ~ homicide_avg + avg_schooling + avg_age +
    share_urban + share_married + avg_children + border,
  data = mex_complete
)

# 4. Vérification
cat("Résidus :", length(residuals(ols_base)), "\n")
cat("Matrice :", length(lw$neighbours), "\n")

# 5. Moran
moran_resid <- moran.test(residuals(ols_base), listw = lw, zero.policy = TRUE)
print(moran_resid)

moran_resid <- moran.test(
  residuals(ols_base),
  listw       = lw,
  zero.policy = TRUE
)

print(moran_resid)

cat("\n====================================================\n")
cat("MORAN'S I TEST ON OLS RESIDUALS\n")
cat("====================================================\n")
print(moran_resid)

# ── INTERPRETATION ────────────────────────────────────────────────────────────
# H0: residuals are randomly distributed in space (no spatial autocorrelation)
# If p < 0.05 → reject H0 → significant spatial autocorrelation in residuals
# → OLS is inefficient/biased; a spatial model is needed.

# ── 4. MORAN'S I ON THE DEPENDENT VARIABLE (informal_rate) ───────────────────

moran_y <- moran.test(
  mex_complete$informal_rate,
  listw       = lw,
  zero.policy = TRUE
)

cat("\n====================================================\n")
cat("MORAN'S I TEST ON informal_rate (raw DV)\n")
cat("====================================================\n")
print(moran_y)

# ── 5. MORAN SCATTER PLOT ─────────────────────────────────────────────────────

moran.plot(
  mex_complete$informal_rate,
  listw       = lw,
  zero.policy = TRUE,
  main        = "Moran Scatter Plot – Female Informal Employment Rate",
  xlab        = "Informal rate (standardised)",
  ylab        = "Spatial lag (standardised)"
)

# ── 6. LAGRANGE MULTIPLIER TESTS (OLS → SAR vs SEM) ──────────────────────────
# These tests help decide whether to use SAR (lag) or SEM (error) model.

lm_tests <- lm.LMtests(
  ols_base,
  listw       = lw,
  test        = c("LMlag", "LMerr", "RLMlag", "RLMerr", "SARMA"),
  zero.policy = TRUE
)

cat("\n====================================================\n")
cat("LAGRANGE MULTIPLIER TESTS\n")
cat("====================================================\n")
print(lm_tests)

# ── DECISION RULE (Anselin 1988 / Florax et al. 2003) ────────────────────────
# 1. If both LMlag and LMerr significant → look at robust versions (RLMlag, RLMerr)
# 2. If RLMlag > RLMerr (more significant) → prefer SAR
# 3. If RLMerr > RLMlag                   → prefer SEM
# 4. If SARMA significant but neither robust → consider SARAR


###############################################################################
# (f) SAR MODEL (Spatial Autoregressive / Spatial Lag Model)
###############################################################################

# SAR: Y = ρ W·Y + X·β + ε
# ρ (rho): spatial autoregressive coefficient
# Interpretation: informal_rate in state i depends on informal_rate of neighbors

sar_model <- lagsarlm(
  informal_rate ~ homicide_avg + avg_schooling + avg_age +
    share_urban + avg_children + border,
  data        = mex_complete,
  listw       = lw,
  zero.policy = TRUE
)

cat("\n====================================================\n")
cat("SAR MODEL RESULTS\n")
cat("====================================================\n")
summary(sar_model)

# ── IMPACTS (SAR): direct, indirect, total effects ────────────────────────────
# In a SAR, a change in X_i affects Y_i (direct) AND Y_j through the spatial
# multiplier (indirect = spillover). OLS coefficients are biased because they
# confound these two channels.

sar_impacts <- impacts(sar_model, listw = lw, R = 500)

cat("\n====================================================\n")
cat("SAR – AVERAGE DIRECT, INDIRECT & TOTAL IMPACTS\n")
cat("====================================================\n")
print(summary(sar_impacts, zstats = TRUE, short = TRUE))

# ── QUICK COMMENT TEMPLATE ────────────────────────────────────────────────────
# ρ > 0 and significant → positive spatial dependence in female informality:
#   states with high informal rate are surrounded by states with high informal rate.
#
# homicide_avg (direct): a higher homicide count in state i → % change in
#   female informal employment in state i itself.
# homicide_avg (indirect): spillover – via neighbors' informal rate.
# homicide_avg (total): sum of both effects.


###############################################################################
# (g) MODEL SELECTION – CHOOSING THE MOST APPROPRIATE SPATIAL MODEL
###############################################################################

# ── 1. SEM MODEL (Spatial Error Model) ───────────────────────────────────────
# SEM: Y = X·β + u,  u = λ·W·u + ε
# λ: spatial autocorrelation in the ERROR TERM (unobserved factors spread spatially)
# β interpretation: same as OLS (no indirect effect on Y via W·Y)

sem_model <- errorsarlm(
  informal_rate ~ homicide_avg + avg_schooling + avg_age +
    share_urban + avg_children + border,
  data        = mex_complete,
  listw       = lw,
  zero.policy = TRUE
)

cat("\n====================================================\n")
cat("SEM MODEL RESULTS\n")
cat("====================================================\n")
summary(sem_model)

# ── 2. SARAR MODEL (SAC – both lag and error) ─────────────────────────────────
# SARAR: Y = ρ·W·Y + X·β + u,  u = λ·W·u + ε
# Most general but uses more degrees of freedom (n=32 → use with caution)

sarar_model <- sacsarlm(
  informal_rate ~ homicide_avg + avg_schooling + avg_age +
    share_urban + avg_children + border,
  data        = mex_complete,
  listw       = lw,
  zero.policy = TRUE
)

cat("\n====================================================\n")
cat("SARAR MODEL RESULTS\n")
cat("====================================================\n")
summary(sarar_model)

# ── 3. SDM MODEL (Spatial Durbin Model) ──────────────────────────────────────
# SDM: Y = ρ·W·Y + X·β + W·X·θ + ε
# Adds spatially lagged regressors → most flexible, nests SAR and SEM.
# LeSage & Pace (2009) recommend SDM as a robust default.

sdm_model <- lagsarlm(
  informal_rate ~ homicide_avg + avg_schooling + avg_age +
    share_urban + avg_children + border,
  data        = mex_complete,
  listw       = lw,
  type        = "mixed",   # "mixed" = SDM in spatialreg
  zero.policy = TRUE
)

cat("\n====================================================\n")
cat("SDM MODEL RESULTS\n")
cat("====================================================\n")
summary(sdm_model)

# ── 4. MODEL COMPARISON – AIC / BIC / Log-Likelihood ─────────────────────────

model_comparison <- data.frame(
  Model   = c("OLS", "SAR", "SEM", "SARAR", "SDM"),
  AIC     = c(AIC(ols_base), AIC(sar_model), AIC(sem_model),
              AIC(sarar_model), AIC(sdm_model)),
  BIC     = c(BIC(ols_base), BIC(sar_model), BIC(sem_model),
              BIC(sarar_model), BIC(sdm_model)),
  LogLik  = c(logLik(ols_base), logLik(sar_model), logLik(sem_model),
              logLik(sarar_model), logLik(sdm_model))
)

cat("\n====================================================\n")
cat("MODEL COMPARISON TABLE\n")
cat("====================================================\n")
print(model_comparison[order(model_comparison$AIC), ])
# → lower AIC/BIC = better fit

# ── 5. LIKELIHOOD RATIO TESTS ─────────────────────────────────────────────────

# SAR vs OLS (is rho significant beyond OLS?)
lr_sar_ols <- anova(ols_base, sar_model)

# SDM vs SAR (do spatially lagged Xs add explanatory power?)
lr_sdm_sar <- anova(sar_model, sdm_model)

# SDM vs SEM (Hausman-type: if theta + rho*beta = 0 → SEM is sufficient)
lr_sdm_sem <- anova(sem_model, sdm_model)

cat("\n====================================================\n")
cat("LR TEST: SAR vs OLS\n")
cat("====================================================\n")
print(lr_sar_ols)

cat("\n====================================================\n")
cat("LR TEST: SDM vs SAR  (H0: SDM reduces to SAR)\n")
cat("====================================================\n")
print(lr_sdm_sar)

cat("\n====================================================\n")
cat("LR TEST: SDM vs SEM  (H0: SDM reduces to SEM)\n")
cat("====================================================\n")
print(lr_sdm_sem)

# ── 6. MORAN'S I ON RESIDUALS OF EACH SPATIAL MODEL ──────────────────────────

moran_sar  <- moran.test(residuals(sar_model),  lw, zero.policy = TRUE)
moran_sem  <- moran.test(residuals(sem_model),  lw, zero.policy = TRUE)
moran_sarar<- moran.test(residuals(sarar_model),lw, zero.policy = TRUE)
moran_sdm  <- moran.test(residuals(sdm_model),  lw, zero.policy = TRUE)

cat("\n====================================================\n")
cat("MORAN'S I ON RESIDUALS – DO MODELS REMOVE AUTOCORRELATION?\n")
cat("====================================================\n")
cat(sprintf("OLS   : I = %.4f, p = %.4f\n",
            moran_resid$statistic, moran_resid$p.value))
cat(sprintf("SAR   : I = %.4f, p = %.4f\n",
            moran_sar$statistic,  moran_sar$p.value))
cat(sprintf("SEM   : I = %.4f, p = %.4f\n",
            moran_sem$statistic,  moran_sem$p.value))
cat(sprintf("SARAR : I = %.4f, p = %.4f\n",
            moran_sarar$statistic,moran_sarar$p.value))
cat(sprintf("SDM   : I = %.4f, p = %.4f\n",
            moran_sdm$statistic,  moran_sdm$p.value))
# → the best model leaves no significant residual spatial autocorrelation

# ── 7. IMPACTS FOR BEST MODEL ─────────────────────────────────────────────────
# Adapt this block to whichever model wins the selection above.
# Example below assumes SDM is selected.

cat("\n====================================================\n")
cat("SDM – AVERAGE DIRECT, INDIRECT & TOTAL IMPACTS\n")
cat("====================================================\n")
sdm_impacts <- impacts(sdm_model, listw = lw, R = 500)
print(summary(sdm_impacts, zstats = TRUE, short = TRUE))

# ── 8. FINAL INTERPRETATION TEMPLATE ─────────────────────────────────────────
cat("\n
=======================================================
INTERPRETATION GUIDE (fill in with your actual numbers)
=======================================================

MODEL SELECTION RATIONALE:
  - OLS Moran's I test:   I = [x], p = [x]  → significant spatial autocorrelation
                          → OLS is inappropriate
  - LM tests:  LMlag p=[x], LMerr p=[x]
               RLMlag p=[x], RLMerr p=[x]
               → [SAR / SEM] preferred by robust LM rule
  - AIC comparison: SDM([x]) < SAR([x]) < SEM([x])
  - LR(SDM vs SAR): p=[x] → [reject/fail to reject] SAR restriction
  - LR(SDM vs SEM): p=[x] → [reject/fail to reject] SEM restriction
  - Residual Moran's I: [best model] eliminates spatial autocorrelation (p=[x])
  → Selected model: [SAR / SEM / SDM] because [...]

EFFECT OF HOMICIDE ON FEMALE INFORMALITY ([selected model]):
  Direct effect   = [x] → A one-unit increase in avg homicide in state i
                           is associated with a [x pp] change in female
                           informality in state i itself, ceteris paribus.
  Indirect effect = [x] → Spillover: through the spatial multiplier /
                           neighbors' informal rate, the total effect spreads.
  Total effect    = [x] → Overall average impact across all states.

SPATIAL PARAMETER (ρ or λ):
  ρ = [x], p = [x]
  → [Positive/Negative] and [significant/not significant]:
     states with high female informality tend to be surrounded by states
     with [high/low] female informality.

OTHER CONTROLS:
  avg_schooling: [sign] → more educated women → [more/less] formal employment
  share_urban  : [sign] → urban areas → [more/less] formal jobs
  border       : [sign] → border states → [higher/lower] informality
                          (maquiladora effect / exposure to drug-trade violence)
\n")

###############################################################################
# END OF SCRIPT
###############################################################################

cat("Lignes dans mex_complete :", nrow(mex_complete), "\n")
cat("Longueur des résidus OLS :", length(residuals(ols_base)), "\n")









#Homicide rate
mexico_municipalities.shp
######libraries-----
library(plm)
library(sandwich)
library(lmtest)
library(dplyr)
library(xtable)
library(jtools)
library(ggstance)
library(coefplot)
library(stargazer)
library(ggpmisc)
library(psych)

###########################################################################
###########################################################################
### ###
### SECTION 1 DATA INPUT AND INITIALIZATION ###
### ###
###########################################################################
###########################################################################

# Data processing for statistical analysis (input 3y data, use 2y lags)---------
firmcomponentyear <-
  read.csv(paste(getwd(), "/data/processed-data.csv", sep = "")) %>%
  
  # cleanup to keep only suppliers, calculate age, convert home country variables to international variables
  filter(key == "supplier") %>%
  dplyr::mutate(size = (as.numeric(size))) %>%
  dplyr::mutate(age = as.numeric(2019 - foundingyear)) %>%
  dplyr::mutate(foundingyear = ifelse(foundingyear == 0, NA, foundingyear)) %>%
  dplyr::mutate(patentcount_priorcum_int = patentcount_priorcum - patentcount_priorcum_home) %>%
  dplyr::mutate(OEM_intsupp_perc_3y = 1 - OEM_homesupp_perc_3y) %>%
  
  # cleanup to keep only countries with OEMs
  filter(country_source %in% c("germany", "denmark", "spain", "usa", "china", "japan", "india")) %>%
  dplyr::mutate (country_source2 = ifelse (
    country_source %in% c("germany", "denmark", "spain"),
    "eusupplier",
    as.character(country_source)
  )) %>%
  dplyr::mutate(intl = OEM_intsupp_perc_3y) %>%
  dplyr::mutate(market.china = dongfang_3y + guodian_3y + goldwind_3y + ming.yang_3y) %>%
  dplyr::mutate(market.europe = enercon_3y + gamesa_3y + nordex_3y + repower_3y + siemens_3y) %>%
  dplyr::mutate(market.usa = general.electric_3y) %>%
  dplyr::mutate(market.india = suzlon_3y) %>%
  
  # add year lags
  dplyr::group_by(component, name_supplier) %>%
  arrange((year)) %>%
  dplyr::mutate(lag4 = dplyr::lag(intl, 4)) %>%
  dplyr::mutate(lag.data4y = intl - lag4) %>%
  dplyr::mutate(lag3 = dplyr::lag(intl, 3)) %>%
  dplyr::mutate(lag.data3y = intl - lag3) %>%
  dplyr::mutate(lag2 = dplyr::lag(intl, 2)) %>%
  dplyr::mutate(lag.data2y = intl - lag2) %>%
  dplyr::mutate(lag1 = dplyr::lag(intl, 1)) %>%
  dplyr::mutate(lag.data1y = intl - lag1) %>%
  dplyr::mutate(firmcomponent = paste(name_supplier, component)) %>%
  ungroup() %>%
  
  # use log for firm size and remove firms where we don't have size information
  dplyr::mutate(size = ifelse(is.infinite(log(size)),
                              NA,
                              log(size))) %>%
  
  # calculate age
  dplyr::mutate(age = ifelse(age == 2019,
                             NA,
                             age)) %>%
  
  # replacing NA in lag column
  # because the first 2 years will not have a lag
  # we assume that the share of international activity is the shift
  dplyr::mutate(lag.data4y = ifelse(is.na(lag.data4y),
                                    OEM_intsupp_perc_3y,
                                    lag.data4y)) %>%
  dplyr::mutate(lag.data3y = ifelse(is.na(lag.data3y),
                                    OEM_intsupp_perc_3y,
                                    lag.data3y)) %>%
  dplyr::mutate(lag.data2y = ifelse(is.na(lag.data2y),
                                    OEM_intsupp_perc_3y,
                                    lag.data2y)) %>%
  dplyr::mutate(lag.data1y = ifelse(is.na(lag.data1y),
                                    OEM_intsupp_perc_3y,
                                    lag.data1y)) %>%
  mutate(SourceComponentYear = paste(name_supplier, component, year,sep=";"))

# Simplified table for regressions -----
statdata <-
  tibble(
    sourcecomponentyear = firmcomponentyear$SourceComponentYear,
    shift4y = firmcomponentyear$lag.data4y,
    shift3y = firmcomponentyear$lag.data3y,
    shift2y = firmcomponentyear$lag.data2y,
    shift1y = firmcomponentyear$lag.data1y,
    comp_diversification = (firmcomponentyear$diverse_wind_3y),
    #component specialization
    wind_specialization = as.factor(firmcomponentyear$wind_specialization),
    #wind specialization
    patentsintl = firmcomponentyear$patentcount_priorcum_int,
    patentshome = firmcomponentyear$patentcount_priorcum_home,
    size = (firmcomponentyear$size),
    age = firmcomponentyear$age,
    outsource = firmcomponentyear$OEM_outsource_perc_3y,
    firmname = firmcomponentyear$name_supplier,
    intl = firmcomponentyear$intl,
    country = firmcomponentyear$country_source,
    country.agg = firmcomponentyear$country_source2,
    component = firmcomponentyear$component,
    year = firmcomponentyear$year,
    market.eu = firmcomponentyear$market.europe,
    market.us = firmcomponentyear$market.usa,
    market.cn = firmcomponentyear$market.china,
    market.in = firmcomponentyear$market.india
  ) %>%
  unique()

# Import complexity calculations-----------
complexity <-
  read.csv(paste(getwd(), "/data/complexity-comparison.csv", sep =
                   ""),
           sep = ",")
statdata <-
  statdata %>% left_join(complexity, by = c("component", "year")) %>%
  unique()


# Creating separate tables for country/countries suppliers----------
statdata.big <-
  statdata %>% filter(country %in% c("germany", "denmark", "spain", "usa", "china"))
statdata.EU <- statdata %>% filter(country.agg == "eusupplier")
statdata.china <- statdata %>% filter(country.agg == "china")
statdata.US <- statdata %>% filter(country.agg == "usa")
statdata.india <- statdata %>% filter(country.agg == "india")

# # saving data-------
# write.table(
#   statdata,
#   paste(getwd(), "/data/statdata_submission.csv", sep = ""),
#   sep = ",",
#   row.names = FALSE
# )


###########################################################################
###########################################################################
### ###
### SECTION 2 MAIN STATISTICAL ANALYSIS ###
### ###
###########################################################################
###########################################################################

# Printing descriptive stats and correlation table of the main variables

###### function for correlations ------
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-
  function(x,
           method = c("pearson", "spearman"),
           removeTriangle = c("upper", "lower"),
           result = c("none", "html", "latex")) {
    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix <- rcorr(x, type = method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value
    
    ## Define notions for significance levels; spacing is important.
    mystars <-
      ifelse(p < .0001, "***", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(
      x
    )), R), 3))[, -1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep = "")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep = "")
    
    ## remove upper triangle of correlation matrix
    if (removeTriangle[1] == "upper") {
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if (removeTriangle[1] == "lower") {
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew) - 1])
    if (result[1] == "none")
      return(Rnew)
    else{
      if (result[1] == "html")
        print(xtable(Rnew), type = "html")
      else
        print(xtable(Rnew), type = "latex")
    }
  }

# summary stats----
statdata.tmp <- statdata %>%
  na.omit() %>%
  select(
    shift2y,
    PCI_hs02,
    wind_specialization,
    comp_diversification,
    patentsintl,
    patentshome,
    size,
    age,
    outsource
  )
correlations <- corstars(statdata.tmp)
descriptions <- psych::describe(statdata.tmp)

summary.stats <- cbind(descriptions, correlations)

# write.table(summary.stats,
#             paste(getwd(), "/data/summarystats_submission.csv", sep = ""),
#             sep = ",",
#             row.names = FALSE
# )


# Statistical analysis: stat models (OLS) with 2 year lag and complexity using hs02---------

# model 1 = all OEM countries / controls
m1 <- lm(shift2y ~ wind_specialization + comp_diversification+ patentsintl  + patentshome + size + age + outsource +
    factor(country) + factor(year) + factor(firmname),
  data = statdata)
m1$rse <-
  sqrt(diag(vcovHC(m1, type = "HC1"))) # for reporting robust SE

# model 2 = all OEM countries / shift
m2 <-
  lm(shift2y ~ PCI_hs02 + wind_specialization + comp_diversification + patentsintl  + patentshome + size + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata)

m2$rse <-
  sqrt(diag(vcovHC(m2, type = "HC1"))) # for reporting robust SE

# models 3 - 5 separating by country of supplier

# model 3 = suppliers in EU [OEM countries aggregated / shift /FE]
m3 <-
  lm(
    shift2y ~  PCI_hs02 + wind_specialization + comp_diversification+patentsintl  + patentshome + size + age + outsource
    + factor(year) + factor(firmname),
    data = statdata.EU
  )
m3$rse <-
  sqrt(diag(vcovHC(m3, type = "HC1"))) # for reporting robust SE

# model 4 = suppliers in US [OEMs / shift /FE]
m4 <-
  lm(
    shift2y ~  PCI_hs02 + wind_specialization + comp_diversification+ patentsintl  + patentshome + size + age + outsource +
      factor(year) + factor(firmname),
    data = statdata.US
  )
m4$rse <-
  sqrt(diag(vcovHC(m4, type = "HC1"))) # for reporting robust SE

# model 5 = suppliers in China [OEMs / shift / FE]
m5 <-
  lm(
    shift2y ~  PCI_hs02 + wind_specialization + comp_diversification +patentsintl  + patentshome + size + age + outsource +
      factor(year) + factor(firmname),
    data = statdata.china
  )
m5$rse <-
  sqrt(diag(vcovHC(m5, type = "HC1"))) # for reporting robust SE


#models 6 - 8 separating by country of OEM

# model 6 = target EU countries / shift /FE
statdata.targetEU <- statdata %>% filter(market.eu != 0)
m6 <-
  lm(
    shift2y ~  PCI_hs02 + wind_specialization + comp_diversification +patentsintl  + patentshome + (size) + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata.targetEU
  )
m6$rse <-
  sqrt(diag(vcovHC(m6, type = "HC1"))) # for reporting robust SE


# model 7 = target US OEMs / shift /FE
statdata.targetUS <- statdata %>% filter(market.us != 0)
m7 <-
  lm(
    shift2y ~  PCI_hs02 + wind_specialization + comp_diversification +patentsintl  + patentshome + (size) + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata.targetUS
  )
m7$rse <-
  sqrt(diag(vcovHC(m7, type = "HC1"))) # for reporting robust SE

# model 8 = target China OEMs / shift / FE
statdata.targetchina <- statdata %>% filter(market.cn != 0)
m8 <-
  lm(
    shift2y ~  PCI_hs02 + wind_specialization + comp_diversification +patentsintl  + patentshome + (size) + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata.targetchina
  )
m8$rse <-
  sqrt(diag(vcovHC(m8, type = "HC1"))) # for reporting robust SE


# export results
stargazer(
  m1,
  m2,
  m3,
  m4,
  m5,
  m6,
  m7,
  m8,
  se = (
    list(m1$rse, m2$rse, m3$rse, m4$rse, m5$rse, m6$rse, m7$rse, m8$rse)
  ),
  object.names = TRUE,
  column.labels = c(
    "Controls",
    "Complexity",
    "European suppliers",
    "US suppliers",
    "Chinese suppliers",
    "Suppliers to European OEMs",
    "Suppliers to US OEMs",
    "Suppliers to Chinese OEMs"
  ),
  omit = c("country", "year", "firmname"),
  add.lines = list(
    c("Country FE", "YES", "YES", "NO", "NO", "NO", "NO", "NO", "NO"),
    c("Year FE", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES"),
    c("Firm FE", "YES", "YES", "YES", "YES", "YES", "YES", "YES", "YES")
  ),
  omit.stat = c("f", "ser"),
  type = "html",
  style = "all",
  out = "models_2ylag_hs02_submission.htm"
) #, supplier only


# Plot FIGURE to show coefficients (Figure 5) ------
customPalette <- rev(c("grey60", "#66C2A5", "#E78AC3", "#FFD92F"))

# plotting coefficients for supplier models
coefficientplot <-
  multiplot(
    m2,
    m3,
    m4,
    m5,
    pointSize = 2,
    sort = "natural",
    decreasing = TRUE,
    lwdInner=1,
    outerCI=0,
    zeroType=1,
    zeroLWD = 0.2,
    zeroColor = "black",
    coefficients = ((
      c(
        "outsource",
        "age",
        "size",
        "patentshome",
        "patentsintl",
        "comp_diversification",
        "wind_specialization",
        "PCI_hs02"
      )
    )),
    names = c("Model z2", "Model y3", "Model x4", "Model w5")
  ) +
  scale_x_continuous(
    limits = c(-0.4, 0.4),
    breaks = seq(-0.4, 0.4, 0.2),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = customPalette, name = "") +
  scale_color_manual(values = customPalette, name = "") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.text = element_text(size = 6),
    axis.text = element_text(size = 6)
  ) +
  xlab("Model coefficients") +
  guides(colour = guide_legend(
    nrow = 2,
    byrow = TRUE,
    reverse = TRUE
  ))

plot(coefficientplot)

ggsave(
  file = paste(getwd(), "/figures/coeff_suppliers_sub.pdf", sep = ""),
  coefficientplot,
  width = 3.45,
  height = 3.9,
  units = "in",
  useDingbats = FALSE
)


#   plotting coefficients for OEM models
coefficientplot <-
  multiplot(
    m2,
    m6,
    m7,
    m8,
    pointSize = 2,
    sort = "natural",
    decreasing = TRUE,
    lwdInner=1,
    outerCI=0,
    zeroType=1,
    zeroLWD = 0.2,
    zeroColor = "black",
    coefficients = ((
      c(
        "outsource",
        "age",
        "size",
        "patentshome",
        "patentsintl",
        "comp_diversification",
        "wind_specialization",
        "PCI_hs02"
      )
    )),
    names = c("Model z2", "Model y6", "Model x7", "Model w8")
  ) +
  scale_x_continuous(
    limits = c(-0.4, 0.4),
    breaks = seq(-0.4, 0.4, 0.2),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = customPalette, name = "") +
  scale_color_manual(values = customPalette, name = "") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.text = element_text(size = 6),
    axis.text = element_text(size = 6)
  ) +
  guides(colour = guide_legend(
    nrow = 2,
    byrow = TRUE,
    reverse = TRUE
  ))

plot(coefficientplot)

ggsave(
  file = paste(getwd(), "/figures/coeff_OEM_sub.pdf", sep = ""),
  coefficientplot,
  width = 3.45,
  height = 3.9,
  units = "in",
  useDingbats = FALSE
)


###########################################################################
###########################################################################
### ###
### SECTION 3 ROBUSTNESS CHECKS ###
### ###
###########################################################################
###########################################################################

# Robustness checks ----

# correlations
complexity.tmp <- statdata %>%
  na.omit() %>%
  select(shift2y,
         PCI_hs02,
         PCI_hs07,
         FlemingSorenson,
         Broeckel)
correlations.comp <- corstars(complexity.tmp)
remove(complexity.tmp)

# Complexity robustness----

# model 2 = all OEM countries / shift 2y with HS02
m2 <-
  lm(
    shift2y ~ PCI_hs02 + wind_specialization + comp_diversification +patentsintl  + patentshome + size + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata
  )
m2$rse <-
  sqrt(diag(vcovHC(m2, type = "HC1"))) # for reporting robust SE


# model 3 = all OEM countries / shift 2y with HS07
m3 <-
  lm(
    shift2y ~ PCI_hs07 + wind_specialization + comp_diversification +patentsintl  + patentshome + size + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata
  )
m3$rse <-
  sqrt(diag(vcovHC(m3, type = "HC1"))) # for reporting robust SE

# model 4 = all OEM countries / shift 2y with Fleming and Sorenson
m4 <-
  lm(
    shift2y ~ FlemingSorenson + wind_specialization + comp_diversification+patentsintl  + patentshome + size + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata
  )
m4$rse <-
  sqrt(diag(vcovHC(m4, type = "HC1"))) # for reporting robust SE

# model 5 = all OEM countries / shift 2y with normalized degree / Broeckel
m5 <-
  lm(
    shift2y ~ Broeckel + wind_specialization + comp_diversification+patentsintl  + patentshome + size + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata
  )
m5$rse <-
  sqrt(diag(vcovHC(m5, type = "HC1"))) # for reporting robust SE

# export results
stargazer(
  m2,
  m3,
  m4,
  m5,
  se = (list(m2$rse, m3$rse, m4$rse, m5$rse)),
  object.names = TRUE,
  column.labels = c(
    "PCI.HS02",
    "PCI.HS07",
    "FlemingSorenson",
    "Broeckel"
  ),
  omit = c("country", "year", "firmname"),
  add.lines = list(
    c("Country FE",  "YES", "YES", "YES", "YES"),
    c("Year FE",  "YES", "YES", "YES", "YES"),
    c("Firm FE", "YES", "YES", "YES", "YES")
  ),
  omit.stat = c("f", "ser"),
  type = "html",
  style = "all",
  out = "models_robust_complexity_submission.htm"
) #, supplier only


# Lags robustness------
# model 2 = all OEM countries / shift 1y with HS02
m2 <-
  lm(
    shift1y ~ PCI_hs02 + wind_specialization + comp_diversification+patentsintl  + patentshome + size + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata
  )
m2$rse <-
  sqrt(diag(vcovHC(m2, type = "HC1"))) # for reporting robust SE

#model 3 = all OEM countries / shift 2y with HS02
m3 <-
  lm(
    shift2y ~ PCI_hs02 + wind_specialization + comp_diversification+patentsintl  + patentshome + size + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata
  )
m3$rse <-
  sqrt(diag(vcovHC(m3, type = "HC1"))) # for reporting robust SE

#model 4 = all OEM countries / shift 2y with complexity.cpc
m4 <-
  lm(
    shift3y ~ PCI_hs02 + wind_specialization + comp_diversification+patentsintl  + patentshome + size + age + outsource +
      factor(country) + factor(year) + factor(firmname),
    data = statdata
  )
m4$rse <-
  sqrt(diag(vcovHC(m4, type = "HC1"))) # for reporting robust SE

# export results
stargazer(
  m2,
  m3,
  m4,
  se = (list(m2$rse, m3$rse, m4$rse)),
  object.names = TRUE,
  column.labels = c("Lag.1y", "Lag.2y", "Lag.3y"),
  omit = c("country", "year", "firmname"),
  add.lines = list(
    c("Country FE",  "YES", "YES", "YES"),
    c("Year FE",  "YES", "YES", "YES"),
    c("Firm FE", "YES", "YES", "YES")
  ),
  omit.stat = c("f", "ser"),
  type = "html",
  style = "all",
  out = "models_robust_lags_submission.htm"
) # supplier only


# Interaction robustness------

statdata <- statdata %>% mutate(year = as.factor(year))

# model 1 = all OEM countries / year X comp_div
m1 <-
  lm(
    shift2y ~ PCI_hs02 + wind_specialization + comp_diversification + patentsintl  + patentshome + size + age + outsource + 
      year * comp_diversification +
      factor(country) + factor(year) + factor(firmname),
    data = statdata
  )
m1$rse <-
  sqrt(diag(vcovHC(m1, type = "HC1"))) # for reporting robust SE


#export results
stargazer(
  m1,
  se = (list(m1$rse)),
  object.names = TRUE,
  column.labels = c("Shift2y", "Year.Comp"),
  omit = c("country", "firmname"),
  add.lines = list(
    c("Country FE",  "YES"),
    c("Year FE",  "YES"),
    c("Firm FE",  "YES")
  ),
  omit.stat = c("f", "ser"),
  type = "html",
  style = "all",
  out = "models_robust_int_submission.htm"
) # supplier only

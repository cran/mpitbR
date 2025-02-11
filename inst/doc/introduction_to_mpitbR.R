## ----eval=FALSE---------------------------------------------------------------
# install.packages("mpitbR")

## ----eval=FALSE---------------------------------------------------------------
# library(devtools)
# 
# install_github("girelaignacio/mpitbR")

## -----------------------------------------------------------------------------
library(mpitbR)

head(ben_dhs06)

## ----tidyverse, message=FALSE-------------------------------------------------
# Load `tidyverse` package
library(tidyverse)

# Count missing values by all the deprivation indicators columns 
  # (all their names start with d_*)
indicators_NAs <- ben_dhs06 %>% 
  summarise(across(grep("^d_",colnames(ben_dhs06)),  ~sum(is.na(.))))

print(indicators_NAs)

# Now compare the total number of missing values in the dataset with the indicators. 
total_NAs <- sum(is.na(ben_dhs06))
print(total_NAs == sum(indicators_NAs))

## ----na.omit------------------------------------------------------------------
ben_dhs06 <- na.omit(ben_dhs06)

## ----survey, message=FALSE----------------------------------------------------
# Load `survey` library
library(survey)

# Define the survey design
svydata <- svydesign(ids = ~psu, weights = ~weight, strata = ~strata, data = ben_dhs06)

## ----indicators---------------------------------------------------------------
# Group indicators by dimension
indicators <-  list(hl = c("d_nutr","d_cm"),
                    ed = c("d_satt","d_educ"),
                    ls = c("d_elct","d_sani","d_wtr","d_hsg","d_ckfl","d_asst"))

## ----set_proj-----------------------------------------------------------------
# Set the multidimensional poverty project
set <- mpitb.set(data = svydata, indicators = indicators,
                 name = "ben_dhs06", desc = "Benin global MPI 2006")

## ----first_globalMPI----------------------------------------------------------
# Estimate the Benin global MPI 2006
estimate.01 <- mpitb.est(set, k = 33, measures = "M0", indmeasures = NULL)

## ----head_first_globalMPI-----------------------------------------------------
# Take a glance at the results
as.data.frame(estimate.01$lframe)

## ----rural-urban--------------------------------------------------------------
# Include living areas in the MPI calculation
estimate.02 <- mpitb.est(set, k = 33, measures = "M0", indmeasures = NULL, 
                         over = "area", verbose = FALSE)

# View results
as.data.frame(estimate.02$lframe)

## ----plot_rural-urban, fig.cap="Multidimensional Poverty by Living Areas in Benin 2006",fig.width=5, fig.height=3, fig.align="center"----
# Save complete subgroups names to be used in the plots
subg_names <- c("National","Rural","Urban")

plt_data.MPI <- as.data.frame(estimate.02$lframe) %>%
  # Replace the subgroup names by their complete names
  mutate(subg = factor(stringi::stri_replace_all_regex(
    subg, pattern = c("nat","rural","urban"), 
    replacement = subg_names, vectorize = F), levels = subg_names))

# Plot!
plt <- ggplot(plt_data.MPI, 
              aes(x = subg, y = b, fill = subg)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  # Add confidence intervals
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0.15) +
  # Axis labels
  labs(x = "Level of Analysis", y = "MPI", fill = "Subgroups") +
  # White background
  theme_bw() +
  # Legend position
  theme(legend.position = "bottom") + 
  # Bars color
  scale_fill_manual(values = c("#8C8C8CFF", "#88BDE6FF", "#FBB258FF"))

# Show plot
plt

## ----rural-urban2-------------------------------------------------------------
# Include incidence H and intesity A in the MPI calculation
estimate.03 <- mpitb.est(set, k = 33, measures = c("M0","H","A"), indmeasures = NULL, 
                         over = c("area"), verbose = FALSE)

# Explore coefficients of H and A
  # Incidence
coef(subset(estimate.03$lframe, measure == "H" & loa == "area"))
 # Intensity
coef(subset(estimate.03$lframe, measure == "A" & loa == "area"))

## ----indicators_measures------------------------------------------------------
# Estimate indicator-specific measures
  # We specify nothing in `indmeasures`
  # since all of them are calculated by default. 
estimate.04 <- mpitb.est(set, k = 33, measures = "M0", over = "area")

# View results
head(estimate.04$lframe)

## ----plt_data.hd--------------------------------------------------------------
# Save complete indicators names to be used in the plots
indicators_names <- c("Nutrition","Child Mortality",
                      "School Attendance","Years of Schooling",
                      "Electricity","Sanitation","Water",
                      "Housing","Cooking Fuel","Assets")

# Rearrange data to create fancier plots :)
plt_data.hd <- as.data.frame(estimate.04$lframe) %>%
  # Filter by the indicators headcount ratios
  filter(measure == "hd" | measure == "hdk") %>%
  # Replace the indicators names by their complete names
  mutate(indicator = factor(stringi::stri_replace_all_regex(
    indicator, pattern = unlist(indicators), 
    replacement = indicators_names, vectorize = F), levels = indicators_names)) %>%
  # Replace the subgroup names by their complete names
  mutate(subg = factor(stringi::stri_replace_all_regex(
    subg, pattern = c("nat","rural","urban"), 
    replacement = subg_names, vectorize = F), levels = subg_names)) %>%
  # Replace the measure abbreviation by their complete names
  mutate(measure = ifelse(measure == 'hd', 'Uncensored',
                          ifelse(measure == 'hdk', 'Censored', measure)))

## ----plt_hd_code,fig.cap="\\label{fig:plt_hd}Indicators headcount ratios in Benin 2006",fig.width=8, fig.height=4, fig.align="center"----
# Plot!
plt <- ggplot(plt_data.hd, 
       aes(x = indicator, y = b, fill = measure)) +
  geom_bar(stat = "identity", width = 0.5,
           position=position_dodge()) +
  # Headcount as percentage
  scale_y_continuous(labels = scales::percent) +
  # Legend position
  theme(legend.position = "right") + 
  # Axis Labels
  labs(y = "Indicators Headcount ratios", fill = "Measure", x = "Indicators") +
  # White background
  theme_bw() + 
  # facet by population subgroups
  facet_grid(rows = vars(subg)) +
  # Fit indicators names by rotating them
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show plot
plt 

## ----plot_ctb_data------------------------------------------------------------
# Filter contributions estimates
  # Absolute contributions
plt_data.actb <- as.data.frame(estimate.04$lframe) %>% 
  filter(measure == "actb") %>%
  # Order indicators by dimensions conveniently to plot
  # we want to avoid alphabetical order in the plot and group by dimension
  mutate(indicator = factor(stringi::stri_replace_all_regex(
    indicator, pattern = unlist(indicators),
    replacement = indicators_names, vectorize = F), levels = indicators_names)) %>%
  # Replace the subgroup names by their complete names
  mutate(subg = factor(stringi::stri_replace_all_regex(
    subg, pattern = c("nat","rural","urban"),
    replacement = subg_names, vectorize = F), levels = subg_names))

  # Percentage contributions
plt_data.pctb <- as.data.frame(estimate.04$lframe) %>% 
  filter(measure == "pctb") %>%
  # Order indicators by dimensions conveniently to plot
  # we want to avoid alphabetical order in the plot and group by dimension
  mutate(indicator = factor(stringi::stri_replace_all_regex(
    indicator, pattern = unlist(indicators), 
    replacement = indicators_names, vectorize = F), levels = indicators_names)) %>%
  # Replace the subgroup names by their complete names
  mutate(subg = factor(stringi::stri_replace_all_regex(
    subg, pattern = c("nat","rural","urban"), 
    replacement = subg_names, vectorize = F), levels = subg_names))

# Define palettes by indicators (different colors for each dimension)
palettes <- c("#A50026FF", "#D73027FF",
              "#FFFFE5FF", "#FFF7BCFF",
              "#B9DDF1FF", "#94C1E0FF","#75A6CBFF",
              "#5889B6FF", "#42779EFF", "#2A5783FF")

## ----plot_ctb_code, fig.cap="\\label{fig:plot_ctb}Indicators contributions to the MPI in Benin 2006",fig.width=8, fig.height=4, fig.align="center"----

# Plot Absolute contribution
plt.actb <- ggplot(plt_data.actb, 
                   aes(x = subg, y = b, fill = indicator)) +
  geom_bar(stat = "identity", width = 0.5) +
  # Axis Labels 
  labs(y = "Contribution to MPI value", fill = "Indicators", x = "Subgroups") +
  # White background
  theme_bw() + 
  # Remove legend
  theme(legend.position = "none") +
  # Colour palettes of each indicator
  scale_fill_manual(values = palettes) 

# Plot Percentage contribution
plt.pctb <- ggplot(plt_data.pctb, 
       aes(x = subg, y = b, fill = indicator)) +
    geom_bar(stat = "identity", width = 0.5) +
    # Contributions as percentage
    scale_y_continuous(labels = scales::percent) +
    # Axis labels
    labs(y = "Percentage Contribution to the MPI", fill = "Indicators", x = "Subgroups") +
    # White background
    theme_bw() +
    # Legend position
    theme(legend.position = "right") + 
    # Colour palettes of each indicator
    scale_fill_manual(values = palettes) 

# Show plot
gridExtra::grid.arrange(plt.actb, plt.pctb, ncol = 2,
                        widths = c(0.40, 0.55), 
                        heights = c(1))


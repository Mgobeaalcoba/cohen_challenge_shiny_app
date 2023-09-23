# Post installation of TidyTuesday by CRAN in my RENV, I download the data set of programming languages in my project.
tuesdata <- tidytuesdayR::tt_load(2023, week = 12)

write.csv(tuesdata$languages, "languages_dataset.csv")

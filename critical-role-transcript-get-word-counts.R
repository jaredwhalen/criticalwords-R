# This code uses transcripts of season 2 of Critical Role to determine the number
# of words spoken by each player episode. Transcriptions come from https://criticalrole.fandom.com.

# set wd to source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readr)
library(dplyr)
library(rvest)

# Import the episode transcription data.
cr_s2_data <- read_csv('./output/critical-role-s2-transcript-data.csv')

#- STEP 3. Fix names

# For my purposes, I only cared about the core cast. This means that I needed
# to standarize castmember names, identify guests, and deal with irregular
# speakers such as multiple names at once. Ultimately, I wanted there to be nine
# speaker classifications: LAURA, MARISHA, ASHLEY, LIAM, SAM, TALIESIN, TRAVIS, MATT
# and then an OTHER categoryfor everything else. Obviously this can be easily
# modified if needed.

# See all unique speaker values
data.frame(speaker = unique(cr_s2_data$speaker))

# Removing unwanted speakers - While most names can and should be standized, there are
# some that I opted for removing outright. These included speakers like "AUDIENCE" and
#"OFF-SCREEN." I also removed incidents where multiple people are speaking at once since
# it happens very rarely but would be difficult to handle for my use. That list is below
# and is used to filter rows out.
name_remove_flags = 'and|,|&|OFF-SCREEN|\\san\\s|ALL|EVERYONE|EVERYBODY|AUDIENCE|CAST'
# In addition to filtering out unwanted speakers, this section cleans names to remove
# any non-alphabetic characters and convert to uppercase.
cr_s2_data <-  cr_s2_data %>% 
  filter(!grepl(name_remove_flags, speaker, ignore.case = TRUE)) %>% 
  mutate(speaker = toupper(trimws(gsub("[^[:alnum:]]", "", speaker))))
#We then makes a list of distinct speakers. This data
# is then exported as csv for manual data cleaning.
messy_names <- data.frame(speaker = unique(cr_s2_data$speaker)) %>% 
  filter(!grepl(name_remove_flags, speaker, ignore.case = TRUE)) %>%
  mutate(speaker = gsub("[^[:alnum:]]", "", speaker)) %>% 
  distinct()
write_csv(messy_names, './output/messy_names.csv')

# Cleaning the names - There are multiple ways to do this. I chose to do this outside of R
# and simply make a two column spreadsheet that contains every unique speaker name and
# the corresponding correct name. Note, I used "OTHER" for anyone who isn't a core cast
# member. This is then imported below and used to clean the data.
messy_names_fix <- read_csv('./input/messy_names_fix.csv')

# Replace with correct names - Join the corrected values on the old name, drop the old
# speaker column, and then rename the new column.
cr_s2_data.fixed <- cr_s2_data %>%  
  left_join(messy_names_fix) %>%
  select(-speaker) %>%
  rename(speaker = fix)

# Determine words by character - This section relies heavily on dplyr and it would be
# tedious to write out comments for each step In summary, this code groups the rows by
# episode number and speaker, gets the number of words for each section, and summarizes
# the data to show the number of words per speaker for each episode.
# For more information about dplyr consult the docs: https://dplyr.tidyverse.org/
words_per_episode_by_character <- cr_s2_data.fixed %>% 
  group_by(episode_number, speaker) %>% 
  mutate(words = sapply(strsplit(text, " "), length)) %>% 
  summarise(words_per_episode = sum(words)) %>% 
  arrange(episode_number, desc(words_per_episode)) %>% 
  ungroup() %>% 
  mutate(speaker = toupper(trimws(speaker)))
  
# Checks to just make sure there aren't any NA or additional speakers in either variable.
unique(cr_s2_data.fixed$speaker)
unique(words_per_episode_by_character$speaker)

# Lastly, export your data.
write_csv(cr_s2_data.fixed, './output/critical-role-s2-transcript-data-clean.csv')

# Clean up workspace.
rm(list = ls())

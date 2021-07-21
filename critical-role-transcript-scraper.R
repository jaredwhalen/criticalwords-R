# This code scrapes the transcripts of season 2 of Critical Role and produces
# a dataframe where every line  is a new speaker talking. Transcriptions
# come from https://criticalrole.fandom.com.

# set wd to source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readr)
library(dplyr)
library(rvest)

#- STEP 1. Get links

# This is a local path to the CLEANED html of just the season two
# episodes (1-120), removing special episodes. While this could be dynamically
# scraped from https://criticalrole.fandom.com/wiki/Transcripts, for
# now it easiest to just pull off the page and pull locally.
local_path = './input/campaign-2-html'

# Get every link - We want to get the url of every transcription page. In order
# to do that, we first pull every link on the page, including those that lead to
# pages we don't care about.
wiki_a_tags <- read_html(local_path) %>%
  html_nodes('a')
# Get the index of every link containing 'Transcript' - Using some basic pattern
# matching with grepl, we get a logical vector that tells us the index of every
# `a tag` that contains the word "Transcript."
transcript_indices <- wiki_a_tags %>%
  html_text() %>%
  grepl('Transcript', .)
# Use the indices to pull the tags - Using the logical vector created above, we
# subset the list of tags, keeping only those leading to a transcript.
transcript_a_tags <- wiki_a_tags[transcript_indices]
# Pull the hrefs and titles - Not only do we have the link to each episide's
# transcript, we also have its title. We use html_attr to make distinct character
# vectors for each. Then we can make a new dataframe containing the title and link
# for every episode. For safekeeping and later use, save this as a csv.
transcript_hrefs <- transcript_a_tags %>%
  html_attr('href')
transcript_titles <- transcript_a_tags %>%
  html_attr('title')
season_df <- data.frame(title = sapply(transcript_titles, function(x)
  gsub("/Transcript", "", as.character(x)), USE.NAMES = F),
  href = transcript_hrefs)
# save to output folder
write_csv(season_df, 'output/episodes.csv')
# clean up our workspace, only keeping variables which we'll use later
rm(list = setdiff(ls(), c("season_df")))

#- STEP 2. Import and structure data

# Base url to the transcripts
wiki_url = 'https://criticalrole.fandom.com/'
# Make a new list to store episode data
episode_list <- list()

# Scrape every episode - This is by far the most complicated block of code in
# this script. This will loop over every row in the season_df dataframe created
# in the previous step.
for (num in 1:nrow(season_df)) {
  
  # A simple way of figuring out what is going on in a loop is to
  # manually assign the index (in this case `num`) and the go 
  # line-by-line as usual
  #---
  #num <- 1 #ONLY FOR TESTING PURPOSES
  #---
  
  # Since the episodes are in order, we can safely assign the episode number
  # based on the loop iteration. This num value is also used to access the
  # episode title and relative url from the dataframe.
  episode_number <- as.character(num)
  episode_title <- as.character(season_df[num,]$title)
  
  message("Scraping: ", episode_number, ".) ", episode_title)
  
  # Make a new list to store the text data.
  text_list = list()
  
  # Using the base url and the relative url, make the absolute url to scrape.
  transcript_url <- paste(wiki_url, season_df[num,]$href, sep = '')
  
  # Use read_html to scrape the entire page, and then access the div containing
  # the actual transcipt. To figure this information out, it is best to poke
  # around the html of a webpage using Inspect Element in the browser.
  body <- read_html(transcript_url) %>%
    html_node('#content') %>%
    html_node('.mw-parser-output') %>%
    html_nodes('*')
  
  # For my purposes, I wanted to skip the pre-show. Since the beginning of each
  # section has an element with the relative ID, it is easy to find that start
  # and end of each section. Here we loop through every node and get those
  # starting points that are then used to subset the body nodeset.
  for (i in 1:length(body)) {
    if (length(html_nodes(body[i], '#Part_I'))) {
      part_I_start <- i
    }
    if (length(html_nodes(body[i], '#Break'))) {
      break_start <- i
    }
    if (length(html_nodes(body[i], '#Part_II'))) {
      part_II_start <- i
    }
  }
  
  part_I <- body[(part_I_start + 3):(break_start - 1)]
  part_II <- body[(part_II_start + 3):length(body)]
  
  # Each node of these subsets is a section of text from a speaker. Every new
  # line indicates a new speaker. Every line starts with the speaker's name,
  # followed by a colon. Using this format, we can loop over each section and
  # split the speaker from the text itself.
  for (i in 1:length(part_I)) {
    #---
    # i <- 1 #ONLY FOR TESTING PURPOSES
    #---
    # Similiar to the method in the main loop, using the index to subset.
    line <- html_text(part_I[i])
    # Split the line into speaker and text.
    parts <- regmatches(line, regexpr(": ", line), invert = TRUE)[[1]]
    # To avoic erros, we check to make sure parts is only two parts.
    if (length(parts) == 2) {
      speaker <- parts[1]
      text <- parts[2]
      # Build a new dataframe with the speaker and text variables and insert
      # into the list created earlier. We can insert the new dataframe at the
      # end of the list using its length plus one
      text_list[[length(text_list) + 1]] <- data.frame(speaker, text)
    }
  }
  # Repeat for Part II
  for (i in 1:length(part_II)) {
    line <- html_text(part_II[i])
    parts <- regmatches(line, regexpr(": ", line), invert = TRUE)[[1]]
    if (length(parts) == 2) {
      speaker <- parts[1]
      text <- parts[2]
      text_list[[length(text_list) + 1]] <- data.frame(speaker, text)
    }
  }
  # Make a new dataframe for the entire episode by binding the rows in the
  # text list. We also go ahead and add episode number and title columns.
  episode_df <- bind_rows(text_list) %>%
    mutate(episode_number = episode_number,
           episode_title = episode_title)
  
  # Now we can add the episode to the list of all the episodes
  episode_list[[length(episode_list) + 1]] <- episode_df
}

# Once we are finished looping, we can bind the bind the rows in the
# episode list to make a single dataframe containing every episode.
season <- bind_rows(episode_list)
# For safekeeping and later use, save this as a csv. This also lets you
# import this data directly instead of running the scraper again.
write_csv(season, './output/critical-role-s2-transcript-data.csv')
# Clean up workspace.
rm(list = ls())

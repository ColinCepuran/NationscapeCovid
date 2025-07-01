rm(list=ls())

dropbox_project_sync_off <- function() { # miscellaneous code for handling all the system files dropbox generates
  require(usethis)
  this_project <- usethis::proj_get()
  
  if (grep("Dropbox", this_project) == 0) {warning("This project is not in a Dropbox folder")}
  
  dir_to_block <- paste0(this_project,"/.Rproj.user")
  file_to_block <- paste0(this_project,".Rproj")
  
  dir_to_block <- gsub("/", "\\\\", dir_to_block)
  file_to_block <- gsub("/", "\\\\", file_to_block)
  
  # Powershell command examples:
  # These set flags to prevent syncing
  # Set-Content -Path C:\Users\myname\Dropbox\mywork\test\test.Rproj -Stream com.dropbox.ignored -Value 1
  # Set-Content -Path C:\Users\myname\Dropbox\mywork\test\.Rproj.user -Stream com.dropbox.ignored -Value 1
  
  s1 <- paste0('powershell -Command \"& {Set-Content -Path ', file_to_block, ' -Stream com.dropbox.ignored -Value 1}\"')
  s2 <- paste0('powershell -Command \"& {Set-Content -Path ', dir_to_block, ' -Stream com.dropbox.ignored -Value 1}\"')
  
  shell(s1)
  shell(s2)
}

dropbox_project_sync_off()

pt <- function(...){ # make figures prettier. You'll need to download the fira code font
  theme(text = element_text(colour = "#16161d", family = "Fira Code", size = 12),
        title = element_text(color = "#16161d", family = "Fira Code", size = 12),
        plot.title = element_text(color = "#16161d", family = "Fira Code", size = 9, hjust = .5),
        line = element_line(color = "#16161d"),
        rect = element_rect(fill = "white",
                            color = "white"),
        axis.ticks = element_line(color = "#16161d"),
        axis.line = element_line(color = "#16161d",
                                 linetype = 1),
        axis.text.x = element_text(color = "#16161d", family = "Fira Code", size = 12),
        axis.text.y = element_text(color = "#16161d", family = "Fira Code", size = 12),
        strip.text = element_text(color = "#16161d", family = "Fira Code", size = 12),
        legend.background = element_rect(fill = NULL, color = NA),
        legend.key = element_rect(fill = "white",
                                  colour = NULL, linetype = 0),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        panel.border = element_blank(),
        panel.grid = element_line(color = "#16161d"),
        panel.grid.major = element_line(color = "#16161d", size = .05),
        panel.grid.minor = element_line(color = "#16161d", size = .05),
        plot.background = element_rect(fill = NULL, colour = NA,
                                       linetype = 0))
}

library(tidyverse) # for data management
library(magrittr) # for expanded pipes
library(haven) # for stata files
library(extrafont) # for expanded fonts

extrafont::loadfonts(quiet = TRUE) # initialize system fonts

load("E:\\Dropbox\\My Functions.RData") # load personal functions--here, just exporting figures.

files <- list.files(path = "E:\\Dropbox\\Surveys\\Nationscape\\phase_2", # here's where I stored the nationscape data
                    pattern = "\\.dta$", # get stata files
                    recursive = T) # look within subfolders--just a precaution.

df <- matrix(NA, nrow = 26, ncol =2 ) %>% as.data.frame() # create the data frame we'll use to generate visualizations

names(df) <- c("End Date", "Support") # assign column names


for (i in 1:length(files)) { # for each of the surveys we've identified...
  prelim <- haven::read_dta(paste0("E:\\Dropbox\\Surveys\\Nationscape\\phase_2\\",files[i])) # read the survey
  
  df[i,1] <- prelim %>% pull(start_date) %>% max() %>% as.Date(substr(.,1,10)) # extract the last response start date
  
  if("right_track" %in% names(prelim)){ # extract the question capturing right track/wrong track
    df[i,2] <- prelim %>% count(right_track, wt = weight) %>% # weighted counts
      mutate(pct = n/sum(n)) %>% mutate(pct = pct*100) %>% # calculate percentages
      filter(is.na(right_track)==F) %>% # drop missing responses
      summarize(out = pct[right_track==1] - pct[right_track==2]) %>% # summarize right/wrong margins 
      pull(out) # extract only the margin
  }
}

p <- df %>% # plot this figure
  filter(is.na(Support)==F) %>%
  mutate(`Survey Start Date` = as.Date(`End Date`)) %>%
  arrange(desc(`Survey Start Date`)) %>%
  ggplot(aes(x = `Survey Start Date`,
             y = Support)) + geom_point() + geom_path() + ylim(c(-55,50)) + # sensible y-axis margins.
  geom_hline(aes(yintercept = 0), linetype = 2) +
  labs(y = "Right-Wrong Margin") + pt() # apply our plot defaults

mygg(p,"E:\\Dropbox\\Blogging\\covid\\rightwrong",5,3) # here's where I export the figure...


# repeat the same process for figure capturing support for mandates

df <- matrix(NA, nrow = length(files), ncol =2 ) %>% as.data.frame()

names(df) <- c("End Date", "Support")

for (i in 1:length(files)) {
  prelim <- haven::read_dta(files[i])
  
  df[i,1] <- prelim %>% pull(start_date) %>% max() %>% as.Date(substr(.,1,10))
  
  if("extra_covid_require_mask" %in% names(prelim)){
    df[i,2] <- prelim %>% count(extra_covid_require_mask, wt = weight) %>%
      mutate(pct = n/sum(n)) %>% mutate(pct = pct*100) %>%
      filter(is.na(extra_covid_require_mask)==F) %>%
      summarize(out = sum(pct[extra_covid_require_mask<3])) %>% # quote is "somewhat" or "strongly" support mask mandates.
      pull(out)
  }
}

p <- df %>%
  filter(is.na(Support)==F) %>%
  mutate(`Survey Start Date` = as.Date(`End Date`)) %>%
  arrange(desc(`Survey Start Date`)) %>%
  ggplot(aes(x = `Survey Start Date`,
             y = Support)) + geom_point() + geom_path() + ylim(c(30,100)) +
  # geom_hline(aes(yintercept = 0), linetype = 2) +
  labs(y = "% Supporting") + pt()

mygg(p,"E:\\Dropbox\\Blogging\\covid\\masks",3,3)

  

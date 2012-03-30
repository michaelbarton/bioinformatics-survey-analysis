#!/usr/bin/env Rscript

library(ggplot2)

source('lib/generate_plot_file.r')
source('lib/survey.r')

lang.data.frame <- function(data,year){
  data.frame(
    lang = unlist(sapply(as.character(data),strsplit,", ")),
    year = as.character(year))
}

raw.language <- list(
  '2008' = read.csv('data/2008/data/survey.csv')$languages,
  '2012' = survey.2012()$languages)

languages <- rbind(
  lang.data.frame(raw.language[['2012']],2012),
  lang.data.frame(raw.language[['2008']],2008))

languages$lang <- gsub(".*[Ss]hell.*|.*[Bb][Aa][Ss][Hh].*|sh|zsh.|unix.*", "Shell",
                  gsub("^C\\+?\\+?$",                                      "C/C++",
                  gsub("(.*[cC]#.*)|.*\\.Net",                             "C#/.Net",
                  gsub("[.*[Ff]ortran.*",                                  "FORTRAN",
                  gsub(".*(awk|AWK|Awk).*",                                "Awk",
                  gsub(".*[Ss][Qq][Ll]",                                   "SQL",
                  gsub("[Jj]ava[Ss]cript",                                 "Javascript",
                  gsub("[Pp][Hh][Pp].?",                                   "PHP",
                  gsub("scilab.*|Matlab",                                  "Matlab / Scilab",
                  gsub("ruby",                                             "Ruby",
                  gsub("prolog",                                           "Prolog",
                  gsub("Scheme|Lisp",                                      "Lisp / Scheme",
                  gsub("(.*not (a )?prog.*)|nil|[Nn]one.?|\\.\\.\\)",      NA,
                       languages$lang)))))))))))))

frequencies <- ddply(languages,.(year),function(df){
  within(count(df),{
    freq <- freq / length(raw.language[[as.character(year[1])]]) * 100
  })
})

changes <- subset(
  within(
    cast(
      melt(frequencies),
      lang ~ year + ., fill = 0),
    change <- `2012` - `2008`),
  abs(change) > 2)

changes <- within(changes,lang <- factor(lang,levels=lang[order(change)]))

p <- ggplot(changes, aes(x=lang,y=change))
p <- p + geom_bar()
p <- p + coord_flip()
p <- p + theme_bw()
p <- p + scale_x_discrete("")
p <- p + scale_y_continuous("Programming Usage Change (% Point)")

generate_plot_file(p, 'programming_change.png')

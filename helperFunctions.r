
# https://stackoverflow.com/questions/34533472/insert-blanks-into-a-vector-for-e-g-minor-tick-labels-in-r

# df <- data.frame(months = seq(as.Date("2009-04-01") ,as.Date("2010-07-01"), by = "1 month"),
#                  numbers <- sample(c(0,90), 16, replace = TRUE))

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE)
  
{ x <- as.character(x)

if (!inverse) {
  if(empty) {
    x[1:nth == 1] <- ""
    x
  } else {
    x[1:nth != 1]
  }
} else {
  
  if(empty) {
    x[1:nth != 1] <- ""
    x
  } else {
    x[1:nth == 1]
  }
}
}

# https://stackoverflow.com/questions/14490071/adding-minor-tick-marks-to-the-x-axis-in-ggplot2-with-no-labels#ggplot2-with-no-labels/14490652#14490652

pretty_date_breaks <- function(x = empty, smbl ="", format1 = "%Y %b", format2 = "%Y"){
  
  # smbl = "Time"; format1 = "%Y %b"; format2 = "%Y"; y <- date_Vec%>% format(., "%Y %b")
  humanity <- function(y){
    
    if (!is.na(y)){
      
      d <- grepl("Jan", y %>% format(., "%Y %b"))
      
      # if ( y >= 0 ){
      #   y_is_positive <- ""
      # } else {
      #   y_is_positive <- "-"
      # }
      
      if (d ) {
        paste0(y%>% format(., format2 )  %>%
                 sub("(.*) (.*)", "\\1\n\\2", .))
        # }else if( between(m1, 1, 10)){
        #   paste0( y_is_positive, smbl
        #           ,fractional::fractional(y, eps = 1e-06, maxConv = 20, sync = TRUE))
      } else {
        ""
      }
      # } else if (is.na(y) | is.null(y)){
      #   "-"
    }
  }
  
  firstEntry <- paste0( x[1] %>% 
                          format(., format1) %>%
                          sub("(.*) (.*)", "\\1\n\\2", .)
                        ," "
                        ,smbl
  )
  
  c( firstEntry
     ,sapply(x, humanity)[-1]
  )
  
}

# pretty_date_breaks(  date_Vec )
# 
# 
# df %>% ggplot()+
#   geom_line(aes(x = months, y = numbers)) +
#   scale_x_date(breaks = seq(min(df$months), max(df$months), by = "1 month"),
#                labels = pretty_date_breaks( seq(min(df$months), max(df$months), by = "1 month")
#                                             ,format1 = "%Y %b", format2 = "%Y")
#                #,limits = c(1900,2000), expand = c(0,0)
#   )

prettyFractions <- function(x = NULL, smbl ="", signif = 3){
  humanity <- function(y){
    # y <- t$estPcnt
    if (!is.na(y)){
      d <- signif(y, digits = 3) %/% 1e-1
      
      c <- signif(y, digits = 3) %/% 1e-2
      m <- signif(y, digits = 3) %/% 1e-3
      m1 <- signif(y, digits = 3) %/% 1e-4
      m2 <- signif(y, digits = 3) %/% 1e-5
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( between(d, 1, 10) ) {
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-02, maxConv = 20, sync = TRUE))
      } else if ( between(c, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-03, maxConv = 20, sync = TRUE))
      } else if ( between(m, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-04, maxConv = 20, sync = TRUE))
      }else if( between(m1, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-06, maxConv = 20, sync = TRUE))
      } else {
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-09, maxConv = 20, sync = TRUE))
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x, humanity)
}


human_numbers <- function(x = NULL, smbl ="", signif = 2){
  # https://stackoverflow.com/questions/11610377/
  humanity <- function(y){
    
    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}

theme_plot <- theme_bw() + # based on ggthemes::theme_wsj
    #theme( # colorhex title_family element_line el, elname)
        theme(line = element_line(linetype = 1, colour = "black"), 
              panel.border = element_blank(), # element_rect(linetype = 1, fill = NA
                                          #,size = .5),
              strip.background = element_blank(),
              panel.spacing.x = unit(.01, "npc"),
              # axis.title.y.right = element_blank(),                # hide right axis title
              # axis.text.y.right = element_blank(),                 # hide right axis labels
              # axis.ticks.y = element_blank(),                      # hide left/right axis ticks
              # axis.text.y = element_text(margin = margin(r = 0)),
              # title = element_text(family = title_family, size = rel(2)), 
              axis.title = element_blank(), axis.text = element_text(face = "bold", 
                                                                     size = rel(1)), axis.text.x = element_text(colour = NULL), 
              # axis.text.y = element_text(colour = NULL), 
              axis.ticks = element_line(colour = NULL), 
              axis.ticks.y = element_blank(),
              axis.ticks.x = element_line(colour = NULL), 
              axis.line = element_line(),
              axis.line.y = element_blank(), 
              legend.background = element_rect(), legend.position = "top", 
              legend.direction = "horizontal", legend.box = "vertical", 
              panel.grid = element_line(colour = NULL, linetype = 3, size =.3 ,lineend = "round"), 
              panel.grid.major = element_line(colour = "gray30"), 
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(), 
              plot.title = element_text(hjust = 0, face = "bold"), 
              plot.margin = unit(c(1, 1, 1, 1), "lines")
              #,strip.background = element_rect()
             )

plot_theme <-  theme_minimal() +
  theme( strip.text.x = element_text(size = 15, 
                                     colour = "black", 
                                     angle = 00,
                                     hjust = 0 )
         ,axis.text.x = element_text(size = 15, 
                                     colour = "black")
         ,axis.text.y = element_text(size = 15, 
                                     colour = "black"))


newtheme <- hrbrthemes::theme_ipsum_rc(plot_title_size = 30, subtitle_size = 30, caption_size = 20) +
  theme(#panel.border = element_blank(),
    panel.grid.major = element_blank() # element_line(color = "gray91"),
    ,panel.grid.minor = element_blank() #element_line(color = "gray94"),
    ,strip.background = element_rect(colour = NA,
                                     fill="blue") # element_blank(),
    ,strip.text.x = element_text(size = 15,
                                 colour = "white",
                                 angle = 00,
                                 hjust = 0 #, lineheight = .5 LegendTitle lcols district
    )
    # ,strip.text.x = element_blank()
    ,panel.spacing.x = unit(.01, "npc"), # if no spacing preferred between bars change to zero
    axis.title.y = element_text(face = "bold", size = 15, angle = 90)
    ,axis.title.x =  element_blank() #element_text(face = "bold", size = 5, angle = 90, hjust = 0),
    ,axis.text.x = element_text(face = "bold", size = 15, angle = 00, vjust = 1)
    ,axis.text.y = element_text(face = "bold", size = 15, angle = 00),
    legend.position = "top"
    ,axis.line.x = element_line(color="black", size = 1)
    ,plot.background = element_rect(fill = "gray90"))

geo_theme <- hrbrthemes::theme_ipsum_rc(plot_title_size = 30, subtitle_size = 30, caption_size = 20) +
  theme(#panel.border = element_blank(),
    # panel.grid.major = element_blank() # element_line(color = "gray91"),
    #  ,panel.grid.minor = element_blank() #element_line(color = "gray94"),
    strip.background = element_rect(colour = NA,
                                    fill="blue") # element_blank(),
    ,strip.text.x = element_text(size = 15,
                                 colour = "white",
                                 angle = 00,
                                 hjust = 0 #, lineheight = .5 LegendTitle lcols district
    )
    # ,axis.line.x = element_line(color="black", size = .5)
    # ,strip.text.x = element_blank()
    ,panel.spacing.x = unit(.01, "npc"), # if no spacing preferred between bars change to zero
    axis.title.y = element_blank()# element_text(face = "bold", size = 15, angle = 90)
    ,axis.title.x = element_blank() #element_text(face = "bold", size = 5, angle = 90, hjust = 0),
    ,axis.text.x = element_blank()# element_text(face = "bold", size = 10, angle = 00, vjust = 1)
    ,axis.text.y = element_blank() # element_text(face = "bold", size = 15, angle = 00),
    ,legend.position = "top"
    ,legend.key.width = unit(2, "cm")
    ,plot.background = element_rect(fill = "gray90"))



navToCensusSite  <- function(){

  library(RSelenium)
  library(rvest)
  library(XML)

  eCaps <- list(
    chromeOptions = list(
      args = c('--no-sandbox', '--disable-gpu', '--start-maximized','--disable-popup-blocking','--disable-extensions')
      #,prefs = list(
      # "profile.default_content_settings.popups" = 0L,
      # "profile.default_content_setting_values.automatic_downloads" = 1L,
      # "download.prompt_for_download" = FALSE,
      # "download.directory_upgrade" = TRUE,
      # "safebrowsing.enabled" = TRUE,
      # "safebrowsing.disable_download_protection" = TRUE,
      # "download.default_directory" = getwd() # paste0("C:/selenium",'/',gsub("-|:| ", "",Sys.Date()))
      # )
    )
  )

  try(rm(rD))
  try(remDr$close())
  try(rm(remDr))
  gc()

  # rD <- rsDriver(browser = "chrome"
  #                ,verbose = TRUE
  #                ,port=4444L
  #                #,chromever = "73.0.3683.68"
  #                #,chromever = "78.0.3904.70"
  #                ,chromever = "80.0.3987.106"
  #                , extraCapabilities = eCaps)

  chromeVers <- binman::list_versions("chromedriver") %>%
    unlist(., use.names = FALSE) %>%
    rev

  url <- "https://www.yahoo.com/"

  for (i in seq_along(chromeVers)) {

    print(paste(i,"of", length(chromeVers)
                , "--", chromeVers[i]))

    system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)

    try(rm(rD))
    try(remDr$close())
    try(rm(remDr))
    gc()

    # error handling - skips to next URL if it gets an error
    rD <- try(rsDriver(browser = "chrome"
                       ,verbose = TRUE
                       ,chromever = chromeVers[i] # "73.0.3683.68"
                       ,extraCapabilities = eCaps))

    # if (class(rD)[1] == "try-error")
    #   next
    # Sys.sleep(1)

    remDr <- try(rD[["client"]])

    Sys.sleep(3) # give time for browser to get ready

    try(remDr$navigate(url))

    out <-  tryCatch(
      remDr$getCurrentUrl() %>%
        as.character(),
      error = function(cond) {
        message(paste("URL does not seem to exist:", url))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return("not it")
      }
    )

    if (out == url)

      break

  }

  Sys.sleep(2)

  assign('rD', rD, 1)
  assign('remDr', remDr, 1)
  assign('eCaps', eCaps, 1)
}



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
                ,fractional::fractional(y, eps = 1e-02, maxConv = 20, sync = TRUE) %>%
                  paste0(" ",sub("(.*)/(.*)", "\\1", .),"/", sub("(.*)/(.*)", "\\2", .) %>%
                           as.numeric(.) %>%
                           scales::comma(.)) %>%  sub("(.*) (.*)", "\\2", .) )
      } else if ( between(c, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-03, maxConv = 20, sync = TRUE) %>%
                  paste0(" ",sub("(.*)/(.*)", "\\1", .),"/", sub("(.*)/(.*)", "\\2", .) %>%
                           as.numeric(.) %>%
                           scales::comma(.)) %>%  sub("(.*) (.*)", "\\2", .) )
      } else if ( between(m, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-04, maxConv = 20, sync = TRUE) %>%
                  paste0(" ",sub("(.*)/(.*)", "\\1", .),"/", sub("(.*)/(.*)", "\\2", .) %>%
                           as.numeric(.) %>%
                           scales::comma(.)) %>%  sub("(.*) (.*)", "\\2", .) )
      } else if( between(m1, 1, 10)){
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-06, maxConv = 20, sync = TRUE) %>%
                  paste0(" ",sub("(.*)/(.*)", "\\1", .),"/", sub("(.*)/(.*)", "\\2", .) %>%
                           as.numeric(.) %>%
                           scales::comma(.)) %>%  sub("(.*) (.*)", "\\2", .) )
      } else {
        paste0( y_is_positive, smbl
                ,fractional::fractional(y, eps = 1e-09, maxConv = 20, sync = TRUE) %>%
                  paste0(" ",sub("(.*)/(.*)", "\\1", .),"/", sub("(.*)/(.*)", "\\2", .) %>%
                           as.numeric(.) %>%
                           scales::comma(.)) %>%  sub("(.*) (.*)", "\\2", .) )
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }

  sapply(x, humanity)
}

if(FALSE) fractional::fractional(y, eps = 1e-09, maxConv = 20, sync = TRUE) %>%
  sub("(.*)/(.*)", "\\2", .) %>%
  as.numeric(.) %>%
  scales::comma(.)

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
              #strip.background = element_blank(),
              strip.background = element_rect(colour = NA,
                                              fill = "blue"), # element_blank(),
              strip.text.x = element_text(size = 10,
                                          colour = "white",
                                          angle = 00,
                                          hjust = 0 #, lineheight = .5 LegendTitle lcols district
              ),
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




# calendar heat map based on this
# https://dominikkoch.github.io/Calendar-Heatmap/

#' Calendar Heatmap
#'
#' Creates a colour coded calendar visualising time series data
#'
#' @param dates A vector containing the dates in `Date` format.
#' @param values A vector containing the corresponding values as numeric.
#' @param title Main plot title (optional).
#' @param subtitle Main plot subtitle (optional).
#' @param legendtitle Legend title (optional).
#'
#' @return ggplot object
#'
calendarHeatmap <- function(dates, values, title = "", subtitle = "", legendtitle = "", guideName = ""){

  if (!require("lubridate")) install.packages("lubridate"); library(lubridate)

  # Parameter checks
  if(missing(dates)){
    stop("Need to specify a dates vector.")
  }
  if(missing(values)){
    stop("Need to specify a values vector.")
  }
  if(!is.Date(dates)){
    stop("dates vector need to be in Date format.")
  }
  if(length(dates) != length(values)){
    stop("dates and values need to have the same length.")
  }


  # load required packages
  require(ggplot2)

  my_theme <- function() {

    # Colors
    color.background = "white"
    color.text = "#22211d"

    # Begin construction of chart
    theme_bw(base_size=15) +

      # Format background colors
      theme(panel.background = element_rect(fill=color.background, color=color.background)) +
      theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
      theme(panel.border     = element_rect(color=color.background)) +
      theme(strip.background = element_rect(fill=color.background, color=color.background)) +

      # Format the grid
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(axis.ticks       = element_blank()) +

      # Format the legend
      theme(legend.position = "bottom") +
      theme(legend.text = element_text(size = 8, color = color.text)) +
      theme(legend.title = element_text(size = 10, face = "bold", color = color.text)) +

      # Format title and axis labels
      theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
      theme(axis.text.x      = element_text(size=12, color="black")) +
      theme(axis.text.y      = element_text(size=12, color="black")) +
      theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
      theme(axis.title.y     = element_text(size=14, color="black", vjust=1.25)) +
      theme(axis.text.x      = element_text(size=10, hjust = 0, color = color.text)) +
      theme(axis.text.y      = element_text(size=10, color = color.text)) +
      theme(strip.text       = element_text(face = "bold")) +

      # Plot margins
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
  }

  # create empty calendar
  min.date <- as.Date(paste(format(min(dates), "%Y"),"-1-1",sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"),"-12-31", sep = ""))
  df <- data.frame(date = seq(min.date, max.date, by="days"), value = NA)

  # fill in values
  df$value[match(dates, df$date)] <- values

  df$year  <-  as.factor(format(df$date, "%Y"))
  df$month <- as.numeric(format(df$date, "%m"))
  df$doy   <- as.numeric(format(df$date, "%j"))
  #df$dow  <- as.numeric(format(df$date, "%u"))
  #df$woy  <- as.numeric(format(df$date, "%W"))
  df$dow <- as.numeric(format(df$date, "%w"))
  df$woy <- as.numeric(format(df$date, "%U")) + 1

  df$dowmapped <- ordered(df$dow, levels = 6:0)
  levels(df$dowmapped) <- rev(c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

  fillBreaks <- values %>%
    pretty(., 5)

  if(guideName == "") {
    guideName = "Value"
  }

  g <- ggplot(df, aes(woy, dowmapped, fill = value)) +
    geom_tile(colour = "darkgrey") +
    facet_wrap(~year, ncol = 1) + # Facet for years
    coord_equal(xlim = c(2.5,54)) + # square tiles
    scale_x_continuous(breaks = 53/12*(1:12)-1.5, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
    my_theme() +
    #scale_fill_distiller(palette = "Paired") +
    # +
    scale_fill_gradient2(
      #trans = "sqrt"
      #alpha = .7
      oob = scales::squish,
      name = guideName,
      low = "#9DBF9E",# "#B5E384", # scales::muted("red"),
      mid = "#FCB97D", # "yellow",
     high = "#A84268", #"#D61818", # scales::muted("blue"),
      ,limits = c(min(fillBreaks), max(fillBreaks))
      ,midpoint = median(fillBreaks)
     ,na.value = "white"
      ,breaks = fillBreaks,
      guide = guide_colorbar(
        direction = "horizontal",
        barheight = unit(2, units = "mm"),
        barwidth = unit(75, units = "mm"),
        title.position = 'top',
        title.hjust = 0.5
      ),
  ,labels = fillBreaks %>% human_numbers()) + #scales::comma(.)) +    #
    # scale_fill_gradientn(colours = c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384"),
    #                      na.value = "white",
    #                      name = legendtitle,
    #                      trans = "reverse",
    #                      guide = guide_colorbar(
    #                        direction = "horizontal",
    #                        barheight = unit(2, units = "mm"),
    #                        barwidth = unit(75, units = "mm"),
    #                        title.position = 'top',
    #                        title.hjust = 0.5
    #                      )) +
    labs(x = NULL,
         y = NULL,
         title = title,
         subtitle = subtitle)

  my.lines<-data.frame(x=numeric(),
                       y=numeric(),
                       xend=numeric(),
                       yend=numeric(),
                       year=character())

  for(years in levels(df$year)){
    df.subset <- df[df$year == years,]

    y.start <- df.subset$dow[1]
    x.start <- df.subset$woy[1]

    x.top.left <- ifelse(y.start == 0, x.start - 0.5, x.start + 0.5)
    y.top.left <- 7.5
    x.top.right <- df.subset$woy[nrow(df.subset)] + 0.5
    y.top.right <- 7.5

    x.mid.left01 <- x.start - 0.5
    y.mid.left01 <- 7.5 - y.start
    x.mid.left02 <- x.start + 0.5
    y.mid.left02 <- 7.5 - y.start

    x.bottom.left <- x.start - 0.5
    y.bottom.left <- 0.5
    x.bottom.right <- ifelse(y.start == 6, df.subset$woy[nrow(df.subset)] + 0.5, df.subset$woy[nrow(df.subset)] - 0.5)
    y.bottom.right <- 0.5

    my.lines<-rbind(my.lines,
                    data.frame(x    = c(x.top.left, x.bottom.left, x.mid.left01, x.top.left, x.bottom.left),
                               y    = c(y.top.left, y.bottom.left, y.mid.left01, y.top.left, y.bottom.left),
                               xend = c(x.top.right, x.bottom.right, x.mid.left02, x.mid.left02, x.mid.left01),
                               yend = c(y.top.right, y.bottom.right, y.mid.left02, y.mid.left02, y.mid.left01),
                               year = years))

    # lines to separate months
    for (j in 1:12)  {
      df.subset.month <- max(df.subset$doy[df.subset$month == j])
      x.month <- df.subset$woy[df.subset.month]
      y.month <- df.subset$dow[df.subset.month]

      x.top.mid <- x.month + 0.5
      y.top.mid <- 7.5

      x.mid.mid01 <- x.month - 0.5
      y.mid.mid01 <- 7.5 - y.month - 1
      x.mid.mid02 <- x.month + 0.5
      y.mid.mid02 <- 7.5 - y.month - 1

      x.bottom.mid <- ifelse(y.month == 6, x.month + 0.5, x.month - 0.5)
      y.bottom.mid <- 0.5

      my.lines<-rbind(my.lines,
                      data.frame(x    = c(x.top.mid, x.mid.mid01, x.mid.mid01),
                                 y    = c(y.top.mid, y.mid.mid01, y.mid.mid01),
                                 xend = c(x.mid.mid02, x.mid.mid02, x.bottom.mid),
                                 yend = c(y.mid.mid02, y.mid.mid02, y.bottom.mid),
                                 year = years))

    }

  }

  # add lines
  g <- g + geom_segment(data=my.lines, aes(x,y,xend=xend, yend=yend), lineend = "square", color = "black", inherit.aes=FALSE)

  return(g)
}

intr_Dat <- function(x) {
  # this fuction cleans up the data estimages from the cdc

  cdcInterpolated <- x %>%
    mutate(EndMonth = sub(".*April_(.*_\\d{2}).*", "\\1", link) %>%
             sub("_", " ",.) %>%
             sapply(., function(x)
               case_when(
                 grepl("October|November|December", x
                       , ignore.case = TRUE) ~ paste("2009", x),
                 TRUE ~ paste("2010", x)
               )) %>%
             as.Date(., format = "%Y %B %d")) %>%
    bind_rows( x %>% distinct(X2009H1N1 ,type, metric) %>%
                 mutate(Valuenum = 0,
                        EndMonth = as.Date("2009-04-12") )) %>%
    unite("Pop_type", X2009H1N1, type, remove = FALSE) %>%
    filter(X2009H1N1 != type) %>%
    select(-value) %>%
    pivot_wider(names_from = metric, values_from = Valuenum)

}

est_intr_Dat <- function(x) {
  # this fuction gets x and intrapolates montly counts based on
  # data point given

  cols_estimate <- names(x)[6:8]
  rows_estimate <- x$Pop_type %>% unique # %>% .[!grepl("Total", .)]

  estData_ <- list()
  est_ <- list()

  for (i in seq_along(rows_estimate)) {

    tmp <- x %>%
      filter(Pop_type == rows_estimate[i]) %>%
      right_join( data.frame(EndMonth = seq(x$EndMonth %>% min()
                                            ,x$EndMonth %>% max(),
                                            by = "day"))
      )

    for (j in seq_along(cols_estimate)) {

      tmp_ <- tmp %>%
        select(EndMonth, cols_estimate[j]) %>%
        arrange(EndMonth)

      x. <- tmp_ %>% select(1) %>% unlist
      y. <- tmp_ %>% select(2) %>% unlist

      est_[[j]] <- splinefun(x., y., method = "hyman") (x.) %>%
        as.tibble() %>%
        mutate(colEst = paste0("imp_", cols_estimate[j]),
               SubIteration = j) %>%
        bind_cols(EndMonth = x.) %>%
        mutate(EndMonth = as.Date(EndMonth, origin = "1970-01-01"))

    }
    estData_[[i]] <- est_ %>%
      rbindlist(.) %>%
      mutate(Pop_type = rows_estimate[i],
             Iteration = i)
  }

  t_intr_estm <- estData_ %>%
    rbindlist(., idcol = "id") %>%
    select(-c(Iteration, SubIteration)) %>%
    pivot_wider(names_from = colEst, values_from = value)

  return(t_intr_estm)
}


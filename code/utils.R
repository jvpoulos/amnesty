# Create function for plot theme
ThemeBw1 <- function(base_size = 16, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "top"
    )
}

ThemeBw2 <- function(base_size = 16, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      axis.title.y =      element_text(margin = unit(c(0, 3, 0, 0), "mm"), size = base_size,angle=90,vjust=.01,hjust=0.5),
      legend.position = "top",
      plot.title = element_text(margin = unit(c(0, 0, 3, 0), "mm"),hjust = 0.5)
    )
}

# Summary figure for estimates
ForestPlot <- function(d, xlab, ylab){
  # Forest plot for summary figure
  p <- ggplot(d, aes(x=x, y=y, ymin=y.lo, ymax=y.hi,colour=Analysis)) + 
    geom_pointrange(size=1, alpha=0.4) + 
    coord_flip() +
    geom_hline(aes(yintercept=0), lty=2) +
    ylab(xlab) +
    xlab(ylab) #switch because of the coord_flip() above
  return(p)
}

Capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

FreqFunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), ", ")))), n)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

NormalizeIt <- function(x){
  x[!is.na(x)] <- (x[!is.na(x)]-min(x[!is.na(x)]))/(max(x[!is.na(x)])-min(x[!is.na(x)]))
  return(x)
}

StFirst <- function(first) {
  first  <-gsub("PAT$","PATRICK", first)
  first  <-gsub("DANL$","DANIEL", first)
  first  <-gsub("RICHD$","RICHARD", first)
  first  <-gsub("CHAS$","CHARLES", first)
  first  <-gsub("JOS$","JOSEPH", first)
  first  <-gsub("BENJ$","BENJAMIN", first)
  first  <-gsub("SAML$","SAMUEL", first)
  first  <-gsub("SAM$","SAMUEL", first)
  first  <-gsub("ROBT$","ROBERT", first)
  first  <-gsub("GEO$","GEORGE", first)
  first  <-gsub("JNO$","JOHN", first)
  first  <-gsub("JAS$","JAMES", first)
  first  <-gsub("THOS$","THOMAS", first)
  first  <-gsub("WM$","WILLIAM", first)
  first  <-gsub("ARTHER$","ARTHUR", first)
  first  <-gsub("WESLY$","WESLEY", first)
  first  <-gsub("CHRIS$","CHRISTOPHER", first)
  first  <-gsub("CHARLEY$","CHARLES", first)
  first  <-gsub("ANT$","ANTHONY", first)
  first  <-gsub("SANDY$","WILLIAM", first)
  first  <-gsub("ELIGAH$","ELIJAH", first)
  first  <-gsub("BENJM$","BENJAMIN", first)
  first  <-gsub("ZACH$","ZACHARY", first)
  first  <-gsub("ELIJA$","ELIJAH", first)
  first  <-gsub("SAMUL$","SAMUEL", first)
  first  <-gsub("ISAMUEL$","SAMUEL", first)
  first  <-gsub("JN$","JOHN", first)
  first  <-gsub("WILIE$","WILLIAM", first)
  first  <-gsub("SAME$","SAMUEL", first)
  first  <-gsub("ARON$","AARON", first)
  first  <-gsub("FREDK$","FREDERICK", first)
  first  <-gsub("JIM$","JAMES", first)
  first  <-gsub("HARVY$","HARVEY", first)
  first  <-gsub("MICH$","MITCHELL", first)
  first  <-gsub("MITCHELL$","MITCHELL", first)
  first  <-gsub("MAT$","MATTHEW", first)
  first  <-gsub("ISAC$","ISAAC", first)
  first  <-gsub("NATHL$","NATHANIEL", first)
  first  <-gsub("TIM$","TIMOTHY", first)
  first  <-gsub("CH$","CHARLES", first)
  first  <-gsub("THEO$","THEODORE", first)
  first  <-gsub("MICHL$","MICHAEL", first)
  first  <-gsub("RUSSEL$","RUSSELL", first)
  first  <-gsub("NIELL$","NIEL", first)
  first  <-gsub("RICH$","RICHARD", first)
  first  <-gsub("EDW$","EDWIN", first)
  first  <-gsub("ALX$","ALEX", first)
  first  <-gsub("FREDERIC$","FREDERICK", first)
  first  <-gsub("ROB$","ROBERT", first)
  first  <-gsub("JEFF$","JEFFERY", first)
  first  <-gsub("AUG$","AUGUSTUS", first)
  first  <-gsub("SOL$","SOLOMON", first)
  first  <-gsub("NAT$","NATHANIEL", first)
  first  <-gsub("WASH$","WASHINGTON", first)
  first  <-gsub("WILLIAN$","WILLIAM", first)
  first  <-gsub("JAME$","JAMES", first)
  first  <-gsub("ARCHD$","ARCHIBALD", first)
  first  <-gsub("GEORG$","GEORGE", first)
  first  <-gsub("ARCH$","ARCHIBALD", first)
  first  <-gsub("DAN$","DANIEL", first)
  first  <-gsub("WILL$","WILLIAM", first)
  first  <-gsub("BENJA$","BENJAMIN", first)
  first  <-gsub("MICHEL$","MICHAEL", first)
  first  <-gsub("JOE$","JOSEPH", first)
  first  <-gsub("THO$","THOMAS", first)
  first  <-gsub("ALEXR$","ALEXANDER", first)
  first  <-gsub("JO$","JOHN", first)
  first  <-gsub("JANES$","JAMES", first)
  first  <-gsub("MIKE$","MICHAEL", first)
  first  <-gsub("BENJN$","BENJAMIN", first)
  first  <-gsub("CHS$","CHRIS", first)
  first  <-gsub("JOHNATHAN$","JONATHAN", first)
  first  <-gsub("JACK$","JOHN", first)
  first  <-gsub("EDWD$","EDWARD", first)
  first  <-gsub("ED$","EDWARD", first)
  first  <-gsub("BEN$","BENJAMIN", first)
  first  <-gsub("REUBENJAMIN$","REUBEN", first)
  first  <-gsub("ALFREDWARD$","ALFRED", first)
}

StCounty <- function(county) {
  county <- gsub("[[:punct:]]", " ", county)
  county <- gsub("Petersburg  Independent City", "Petersburg", county)
  county <- gsub("Portsmouth  Independent City", "Portsmouth", county)
  county <- gsub("Alexandria  Independent City", "Alexandria", county)
  
  county <- Capwords(county)
}

CleanIpums <- function(ipums,one.perc=FALSE,complete=TRUE) {
  if(one.perc){
    # Subset to individuals with nonzero and nonmissing real property 
    ipums <- subset(ipums, realprop>0)
    
    # Remove non-alphabetic characters from name and make all uppercase
    ipums$surname<- trimws(toupper(gsub("[^[:alpha:] ]", "",ipums$namelast))) 
    ipums$first <- trimws(toupper(gsub("[^[:alpha:] ]", "",ipums$namefrst))) 
  }
  
  # Trim spaces in surname
  ipums$surname <- gsub(" ","",ipums$surname)
  ipums$first <- gsub("  ", " ",ipums$first)
  
  # Split first and middle name
  ipums$first <- trimws(unlist(lapply(strsplit(ipums$first," "), function(x) x[1])))
  ipums$middle.name <- trimws(unlist(lapply(strsplit(ipums$first," "), function(x) x[2])))
  
  # Standardize first name
  ipums$first <- StFirst(ipums$first)
  
  # Drop obs with missing names
  ipums$surname.length <- nchar(ipums$surname)
  ipums$first.length <- nchar(ipums$first)
  ipums <- subset(ipums, surname.length>2 & first.length>0)
  
  if(complete){

  # Standardize county
  ipums$county <- StCounty(ipums$county)
  }
  
  # Create soundex of first and surnames
  ipums$sound.surname <- soundex(ipums$surname)
  ipums$sound.first <- soundex(ipums$first)
  
  # Create first name initial
  ipums$first.initial <- substring(ipums$first, 1, 1) 
  
  return(ipums)
}

SimRD <- function(r.prob,delta, s.size, rv, cutoff){
  # Simulate data
  design <-data.frame("rv"=sample(rv, s.size, replace=TRUE),
                      "response"=NA)
  if(!is.null(r.prob)){
    design$response[design$rv >= cutoff] <- rbinom(nrow(design[design$rv >= cutoff,]), 1, 0.1 + r.prob)
    design$response[design$rv < cutoff] <- rbinom(nrow(design[design$rv < cutoff,]), 1, 0.1)
  }
  if(!is.null(delta)){
    design$response[design$rv >= cutoff] <- rnorm(nrow(design[design$rv >= cutoff,]), 3000+delta, sd=25000)
    design$response[design$rv < cutoff] <- rnorm(nrow(design[design$rv < cutoff,]), 3000, sd=25000)
  } 
  # Fit the model
  fit <- rdrobust(design$response, 
                  design$rv,
                  c=cutoff)
  # Return p value
  return(fit$pv[3,]) 
}
library(XML)
library(ggplot2)
library(stringr)
library(httr)

theurl <- "http://en.wikipedia.org/wiki/Rape_statistics#Rape_statistics_by_country"
tables <- GET(theurl)
tables <- readHTMLTable(rawToChar(tables$content), stringsAsFactors=F)

rapes <- tables[[5]]
rapes <- rapes[-1, ]
colnames(rapes) <- c("Countries", paste0("tot200", 3:9), "tot2010", paste0("rate200", 3:9), "rate2010")
attr(rapes, "tot") <- "The total count of rapes committed"
attr(rapes, "rate") <- "Rate per 100,000 population"

rapes2010 <- subset(rapes, select=c("Countries", "rate2010"))
rapes2010$rate2010 <- as.numeric(rapes2010$rate2010)
rapes2010 <- na.omit(rapes2010)

rapes2010.top10 <- rapes2010[ order(-rapes2010$rate2010), ]
india <- subset(rapes2010, str_detect(Countries, "India"))
rapes2010.top10 <- rapes2010.top10[1:10, ]
rapes2010.top10.ind <- rbind(rapes2010.top10, india)

rapeplot <- ggplot(data=rapes2010.top10.ind, aes(y=rate2010, x=Countries, fill=Countries)) + 
  geom_bar(colour="black", stat="identity") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  ggtitle("Rate of Rape in 2010") +
  ylab("Rapes per 100,000 population") +
  xlab("Top 10 Countries + India") +
  geom_text(aes(label=rate2010), vjust=-0.2)

png(filename="Std_PNG_cairo.png", type="cairo", units="in", 
    width=10, height=6, pointsize=12, res=300)
print(rapeplot)
dev.off()

summary(rapes2010.top10$rate2010)


further <- subset(rapes, select=c("Countries", paste0("rate200", 3:9), "rate2010"))
further <- data.frame(lapply(further,
                             function(colmn) {
                               no.of.ints <- sum(!is.na(as.integer(colmn)))
                               if(no.of.ints==0)
                                 return(colmn) else
                                   return(as.integer(colmn))
                               }
                             )
                      )


further <- na.omit(further)


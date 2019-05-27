install.packages("RDCOMClient", repos = "http://www.omegahat.org/R")
install.packages("R2PPT")

library(R2PPT)
library(RDCOMClient)

library(ggplot2)
p <- qplot(x=1,y=1)
print(p)
ggsave(my_temp_file<-paste(tempfile(),".wmf",sep=""), plot=p)
mypres <- PPT.Init(method="RDCOMClient")

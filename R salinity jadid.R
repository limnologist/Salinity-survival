install.packages("OIsurv")
 library(OIsurv)
 library(xlsx)
 d <- read.xlsx("x:\\salinitygammarids.xlsx",1)
 str(d)
 SO1 <- Surv(d$time,d$Death)
 head(SO1)
 m1 <-survfit(SO1~1)
 m1
 plot(m1)
 install.packages("ggplot2")
 library(GGally)
 ggsurv(m1)+ggplot2::theme_bw()
 m2 <- survfit(SO1~salinity,data=d)
 ggsurv(m2,CI=T)+ggplot2::theme_bw()+ ggplot2::scale_x_continuous(expand=c(0,0),limits=c(0,75),breaks=seq(0,72,12))
 m3 <- survfit(SO1~infection,data=d)
 ggsurv(m3,CI=T)+ggplot2::theme_bw()+ ggplot2::scale_x_continuous(expand=c(0,0),limits=c(0,75),breaks=seq(0,72,12))
 m4 <- survfit(SO1~species.,data=d)
 ggsurv(m4,CI=T)+ggplot2::theme_bw()+ ggplot2::scale_x_continuous(expand=c(0,0),limits=c(0,75),breaks=seq(0,72,12))
 m5 <- coxph(SO1~as.factor(salinity)+infection+species.,data=d)

summary(m5)
ggsurv(
  survfit(
    m5 <- coxph(
      SO1~salinity +
        infection
      + species.
      + species.:infection
      + salinity:infection
      + salinity:infection:species.
      + salinity:species.
      ,
      data = d
    )
  ),
  xlab = "\nHours",
  ylab = "Survival\n"
) +


ggplot2::theme_bw() +
     ggplot2::scale_x_continuous(
         expand = c(0,0),
         limits = c(0,75),
         breaks = seq(0,72,12)
       )

m6 <- step(m5)
str(m6)
layout(rbind(1:4))
plot(cox.zph(m6),df=2)
summary(m6)

or 
ggsurv(
  survfit(
    m5 <- coxph(
      SO1~salinity +
        infection
      + species.
      + salinity:infection:species.
     ,
      data = d
    )
  ),
  xlab = "\nHours",
  ylab = "Survival\n"
) +
  ggplot2::theme_bw() +
  ggplot2::scale_x_continuous(
    expand = c(0,0),
    limits = c(0,75),
   breaks = seq(0,72,12)
  )
m6 <- step(m5)
names(d)
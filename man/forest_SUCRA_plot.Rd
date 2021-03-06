% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest_SUCRA_plot.R
\name{forest_SUCRA_plot}
\alias{forest_SUCRA_plot}
\title{Forest-SUCRA plot of net-meta model for MD, HR, and RR}
\usage{
forest_SUCRA_plot(
  bmt,
  sucra,
  x.lab,
  x1,
  x2,
  digits,
  point.size = 4,
  point.shape = 23,
  title.size,
  table.text.size,
  sucra.text.size,
  labels = NULL,
  font.family = "Helvetica",
  plot.scale = 0.9
)
}
\arguments{
\item{bmt}{bayesian net-meta result from model_gemtc}

\item{sucra}{SUCRA values}

\item{x.lab}{label in the x-axis}

\item{x1}{the left xlim}

\item{x2}{the right xlim}

\item{digits}{digits of the results}

\item{point.size}{point size}

\item{point.shape}{point shape see: \url{http://www.sthda.com/english/wiki/ggplot2-point-shapes}}

\item{title.size}{title font size}

\item{table.text.size}{table text size}

\item{sucra.text.size}{sucra text size}

\item{labels}{label od the whole plot}

\item{font.family}{"Helvetica" or "Times New Roman"}

\item{plot.scale}{the scaled size of the whole plot}
}
\value{
ggplot object
}
\description{
To give the forest plot with SUCRA of bayesian results from model_gemtc
}
\examples{
LDT1 <- read.csv(system.file("extdata", "HR_SH_D.csv", package = "net.meta"))
trt1 <- read.table(system.file("extdata", "HR_SH_D.txt", package = "net.meta"),
    header=TRUE,quote = '"', stringsAsFactors=FALSE)

trt1$description <- factor(trt1$description, trt1$description)
LDT1$study <- factor(LDT1$study, unique(LDT1$study))

bmt1 <- model_gemtc(
long.data=LDT1,
id.treatments=trt1,
reference="A",
outcome="HR",
mtc.n.adapt = 500, mtc.n.iter = 1000, mtc.thin = 20)

sucra<-SUCRA(bmt1)

forest_SUCRA_plot(
bmt=bmt1,
sucra=sucra,
x.lab="X caption",
x1=0,
x2=40,
digits = 2,
title.size =12,
table.text.size=4,
sucra.text.size=4,
labels="Title",
font.family = "Helvetica",
plot.scale=0.9
)

}

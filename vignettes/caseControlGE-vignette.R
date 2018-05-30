## ----setup, cache=FALSE, include=FALSE-----------------------------------
knitr::opts_chunk$set(
  collapse=TRUE, cache=TRUE, autodep=TRUE,
  # echo=TRUE,
  comment = "#>"
)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))


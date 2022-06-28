use "GLES_tobit.dta", clear

* raw:
eststo: quietly tobit  dv pre##post, ll(-3) ul(3) vce(cluster Individual)

* individual FEs:
eststo: quietly tobit  dv pre##post i.Individual, ll(-3) ul(3) vce(cluster Individual)

esttab using "output.csv", scalars(se)
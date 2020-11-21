## This is a bug fix version, related github issue:
* Fix bug for tex annotation with facet, git issue https://github.com/s6juncheng/ggpval/issues/9
* Add support for ggplotly, git issue https://github.com/s6juncheng/ggpval/issues/8


## R CMD check results
There were no ERRORs or WARNINGs in other platforms except Windows. Details bellow:
The Windows error is due to the error of package 'utf8'
https://www.r-project.org/nosvn/R.check/r-devel-windows-ix86+x86_64/utf8-00check.html

ERROR: dependency 'utf8' is not available for package 'pillar'


## Downstream dependencies
I have also run R CMD check on downstream dependencies for ggpval, there were no ERRORs or WARNINGs.

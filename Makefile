## This is park/serial

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += serial.tex
serial.pdf: serial.tex forward.pdf

forward.tex: forward.R
	R CMD BATCH --vanilla forward.R

forward.pdf: forward.tex
	pdflatex forward.tex

######################################################################

### Makestuff

Sources += Makefile

## Sources += content.mk
## include content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff
Makefile: makestuff/Makefile
## makestuff: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls $@

localstuff: 
	ln -s ../makestuff .
	ls makestuff/Makefile

-include makestuff/os.mk

## -include makestuff/wrapR.mk
-include makestuff/texdeps.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk

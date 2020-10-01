## This is park/serial

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Ignore += forward.tex
forward.tex: forward.Rout ;

Ignore += rR.tex
rR.tex: rR.Rout ;

Sources += serial.tex
serial.pdf: serial.tex forward.pdf rR.pdf

Sources += response.tex
## response.pdf: response.tex

## serial.ld.pdf: serial.tex
serial.ld.tex: serial.tex.1e83426.oldfile

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

-include makestuff/wrapR.mk
-include makestuff/texdeps.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk

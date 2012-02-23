################################################################
# Install the SWI-Prolog utf8proc package for MS-Windows
#
# Author: Jan Wielemaker
#
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk

LIBDIR=		$(PLBASE)\library
LIBPL=		unicode.pl

OBJ=		utf8proc.obj unicode4pl.obj

all:		unicode4pl.dll

unicode4pl.dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

install::
		@echo Copying $(LIBPL)
		@for %f in ($(LIBPL)) do @copy %f "$(LIBDIR)"
		copy unicode4pl.dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy unicode4pl.pdb "$(BINDIR)"
!ENDIF
		$(MAKEINDEX)

html-install::
		copy utf8proc.html "$(PKGDOC)"

pdf-install::
		copy utf8proc.pdf "$(PKGDOC)"

xpce-install::

uninstall::
		cd $(LIBDIR) & del $(LIBPL) README.TXT
		del "$(BINDIR)\unicode4pl.dll"
		$(MAKEINDEX)

clean::
		if exist *~ del *~
		if exist *.obj del *.obj

distclean:	clean
		if exist *.dll del *.dll
		if exist *.pdb del *.pdb



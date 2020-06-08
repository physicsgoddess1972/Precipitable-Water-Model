#
# ${R_HOME}/src/modules/X11/Makefile


srcdir = .
top_srcdir = ../../..

top_builddir = ../../..
subdir = src/modules/X11
R_HOME = $(top_builddir)

include $(top_builddir)/Makeconf

## When compiling rbitmap.c it is essential that we find the
## png.h matching BITMAP_LIBS.  Since /opt/X11 on macOS contains libpng
## and we might want our own version, we give priority to BITMAP_*
ALL_CPPFLAGS =  -I/usr/include/libpng16 -I/usr/include/x86_64-linux-gnu $(R_XTRA_CPPFLAGS) $(X_CFLAGS) -I/usr/include/cairo -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/pixman-1 -I/usr/include/uuid -I/usr/include/freetype2 -I/usr/include/libpng16 \
  -I$(top_srcdir)/src/library/grDevices/src/cairo $(CPPFLAGS) $(DEFS)
ALL_CFLAGS = $(ALL_CFLAGS_LO)
ALL_X_LIBS = $(X_PRE_LIBS) -lpng16 -lz -lfreetype -lz -lcairo -lX11 -lXext $(X_LIBS) $(X_EXTRA_LIBS)

SOURCES = devX11.c rotated.c rbitmap.c 
DEPENDS = $(SOURCES:.c=.d)
OBJECTS = $(SOURCES:.c=.o)
DE_SOURCES = dataentry.c
DE_OBJECTS = $(DE_SOURCES:.c=.o)
HEADERS = bitmap.h devX11.h rotated.h rlogo_icon.h

distdir = $(top_builddir)/$(PACKAGE)-$(VERSION)/$(subdir)
DISTFILES = \
	Makefile.in $(SOURCES) $(HEADERS) $(DE_SOURCES)

R_X11_la = R_X11$(SHLIB_EXT)
## Note that this is used after LDFLAGS, which usually contains -L flags.
## AIX needs LIBM
R_X11_la_LIBADD = -ltiff -ljpeg -lpng16 -lz $(ALL_X_LIBS) $(LIBR1) $(LIBM) # $(LIBINTL)

R_de_la = R_de$(SHLIB_EXT)
R_de_la_LIBADD = $(ALL_X_LIBS) $(LIBR1) $(LIBM) # $(LIBINTL)

all: R

Makefile: $(srcdir)/Makefile.in \
  $(top_builddir)/config.status \
  $(SOURCES)
	@cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

Makedeps: Makefile $(DEPENDS)
	@cat $(DEPENDS) >> Makefile
	@touch $@

R: Makedeps
	@$(MAKE) $(R_X11_la) $(R_de_la)
	@$(MAKE) rhome="$(abs_top_builddir)" install

$(R_X11_la): $(OBJECTS)
	$(SHLIB_LINK) -o $@ $(OBJECTS) $(R_X11_la_LIBADD)

$(R_de_la): $(DE_OBJECTS)
	$(SHLIB_LINK) -o $@ $(DE_OBJECTS) $(R_de_la_LIBADD)

install: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(R_X11_la) "$(DESTDIR)$(Rexecmodulesdir)/$(R_X11_la)"
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(R_de_la) "$(DESTDIR)$(Rexecmodulesdir)/$(R_de_la)"
installdirs:
	@$(MKINSTALLDIRS) "$(DESTDIR)$(Rexecmodulesdir)"
install-strip: installdirs
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(R_X11_la) "$(DESTDIR)$(Rexecmodulesdir)/$(R_X11_la)"
	@$(SHELL) $(top_srcdir)/tools/copy-if-change $(R_X11_la) "$(DESTDIR)$(Rexecmodulesdir)/$(R_de_la)"
	@if test -n "$(STRIP_SHARED_LIB)"; then \
	  $(STRIP_SHARED_LIB) "$(DESTDIR)$(Rexecmodulesdir)/$(R_X11_la)" ;\
	  $(STRIP_SHARED_LIB) "$(DESTDIR)$(Rexecmodulesdir)/$(R_de_la)" ;\
	fi
uninstall:
	@rm -f "$(Rexecmodulesdir)/R_X11$(SHLIB_EXT)"\
	  "$(Rexecmodulesdir)/R_de(SHLIB_EXT)"

mostlyclean: clean
clean:
	@-rm -Rf .libs _libs
	@-rm -f Makedeps *.d *.o *.a *.lo *.la *$(SHLIB_EXT)
distclean: clean
	@-rm -f Makefile
maintainer-clean: distclean

TAGS info dvi check:

distdir: $(DISTFILES)
	@for f in $(DISTFILES); do \
	  test -f $(distdir)/$${f} \
	    || ln $(srcdir)/$${f} $(distdir)/$${f} 2>/dev/null \
	    || cp -p $(srcdir)/$${f} $(distdir)/$${f}; \
	done

## Automagically generated dependencies:


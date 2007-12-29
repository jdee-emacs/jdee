# Update the html directory from the src directory.

DOCDIR = ../../..
HTMLDIR = $(DOCDIR)/html
CP = cp

IMAGES =  debug1.gif \
	  debug2.gif \
	  breakpoint.gif \
	  debug_cursor.gif \
	  display_var2.gif \
	  display_locals.gif \
	  display_obj.gif \
	  display_var1.gif \
          set_var1.gif \
          set_var2.gif \
          set_var3.gif \
          set_var4.gif
         

all: 
	$(MAKE) imagedir \
		-C $(JDEDEV)/jde/doc/html/jdb-ug/images \
		-f $(JDEDEV)/jde/doc/src/jdb-ug/images/html.makefile

imagedir: $(IMAGES)

%.gif : /cygdrive/c/applications/cygwin/home/jde-dev/jde/doc/src/jdb-ug/images/%.gif
	$(CP) $< .
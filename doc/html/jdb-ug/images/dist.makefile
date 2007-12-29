# Build the secton of the JDE distribution containing the
# images for the HTML version of the JDB User's Guide.


IMAGES =  breakpoint.gif \
	  debug1.gif \
	  debug2.gif \
	  debug_cursor.gif \
	  display_var2.gif \
	  display_locals.gif \
	  display_obj.gif \
	  display_var1.gif \
          set_var1.gif \
          set_var2.gif \
          set_var3.gif \
          set_var4.gif

all: $(IMAGES)

%.gif : $(JDEDIR)/doc/html/jdb-ug/images/%.gif
	$(CP) $< .
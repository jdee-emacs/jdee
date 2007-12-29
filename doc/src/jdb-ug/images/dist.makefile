IMAGES =  debug1.gif \
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

%.gif : $(JDEDIR)/doc/src/jdb-ug/images/%.gif
	$(CP) $< .
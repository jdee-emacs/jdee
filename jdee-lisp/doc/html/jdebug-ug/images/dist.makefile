# Build the secton of the JDE distribution containing the
# images for the HTML version of the JDEbug User's Guide.


IMAGES =  enable_jdebug.gif \
          jdebug_menu.gif \
          window_config.gif \
          source_path.gif

all: $(IMAGES)

%.gif : $(JDEDIR)/doc/html/jdebug-ug/images/%.gif
	$(CP) $< .
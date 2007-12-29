IMAGES =  jdemenu.gif \
	  regjdk1.gif \
	  regjdk2.gif \
	  regjdk3.gif \
	  seljdk1.gif \
	  compile_buffer.gif\
          keybindings.gif \
          OverrideMeth1.gif \
          OverrideMeth2.gif \
          OverrideMeth3.gif \
          OverrideMeth4.gif \
          OverrideMeth5.gif \
          RegisterTemplate.gif \
          TemplateList.gif \
          WorkingDirectory.gif \
          speedbar1.gif \
          speedbar2.gif \
          speedbar3.gif \
          speedbar4.gif \
          speedbar5.gif \
          classes_menu1.gif \
	  completion_sel_method.gif \
	  find_minibuf_prompt_regex.gif \
	  find_minibuf_prompt_dirs.gif \
	  find_minibuf_result.gif \
	  find_options_buffer.gif

all: $(IMAGES)

%.gif : $(JDEDIR)/doc/src/jde-ug/images/%.gif
	$(CP) $< .
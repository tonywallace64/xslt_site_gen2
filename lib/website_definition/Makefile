# Website content makefile
#
# The compile dependencies are stored in temp/xmllist.txt
# These dependencies are calculated from the pagedata.xml
# xinclude statements.  These dependencies are extracted
# from the pagedata.xml file by the xslt template gen_includes.xsl.
#
# The script ebin/make_recursion.beam (which is compiled
# from src/make_recursion.erl calculates a target for each
# source file, and calls make recursively to generate those
# targets.
#
# The navigation bars are generated from both the pagedata.xml
# and the content xml files.  If make detects that an xhtml
# target file needs to be regenerated, then the navigation bars
# are regenerated also.  This is done by touching the file
# temp/make_nav which makes the navigation bars out of date.
#
# A recursive call to make make_nav from the compile rule
# ensures that if these navigation bars are out of date
# that they are regenerated.
#
# XML validation is done in the xhtml generation rule
# with xmllint with the schema in temp/site.rng.  This
# schema is generated from the source schema, src/site.rnc
# with trang, allowing the same schema to be used with
# emacs and site validation.

.PHONY: clean deploy html/pagedata.xhtml temp/make_nav

compile: temp/xmllist.txt ebin/make_recursion.beam
	escript ebin/make_recursion.beam temp/xmllist.txt html/%.xhtml
	make make_nav

temp/xmllist.txt: site_data/pagedata.xml src/gen_includes.xsl
	xsltproc src/gen_includes.xsl site_data/pagedata.xml > temp/xmllist.txt
 
temp/site.rng: src/site.rnc
	trang $< $@

temp/make_nav:
	touch temp/make_nav

make_nav: site_data/pagedata.xml src/make_nav.xsl temp/make_nav
	xsltproc src/make_nav.xsl $< > temp/allnav.xml
	./xslt_extn +finaloutput < temp/allnav.xml
	mv *_nav.xml html

ebin/%.beam: src/%.erl
	erlc -o ebin $<

html/pagedata.xhtml: 
	echo "don't make pagedata.xhtml"
html/%.xhtml: site_data/%.xml site_data/pagedata.xml src/gen_page.xsl temp/site.rng
	touch temp/make_nav
	xmllint --noout --relaxng temp/site.rng $<
	xsltproc  src/gen_page.xsl $< >$@

deploy: 
	cp -r html/* ../../priv/static

clean:
	-rm -f html/*.xhtml
	-rm -f temp/*

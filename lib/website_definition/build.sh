cd site_data
# build site by file inclusion
escript ../xslt_extn < pagedata.xml > fullsite.xml 2> errors.txt
# transform by xslt
xsltproc ../src/make_web.xsl fullsite.xml > allpages.xml
# split output files
escript ../xslt_extn +finaloutput < allpages.xml 
# put results
mv *.xhtml ../html

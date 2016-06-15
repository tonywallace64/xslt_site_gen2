cd site_data
# build site by file inclusion
rm -v fullsite.xml errors.txt allpages.xml logfile *.html *.xhtml
echo  'escript ../xslt_extn < pagedata1.xml > fullsite.xml 2> errors.txt'
escript ../xslt_extn < pagedata1.xml > fullsite.xml 2> errors.txt
# transform by xslt
echo 'xsltproc ../src/make_web.xsl fullsite.xml > allpages.xml'
xsltproc ../src/make_web.xsl fullsite.xml > allpages.xml
# split output files
echo 'escript ../xslt_extn +finaloutput < allpages.xml 2> logfile'
escript ../xslt_extn +finaloutput < allpages.xml 2> logfile
# put results
# mv *.xhtml ../html

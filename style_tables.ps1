7z.exe x secondary_report_template.docx /word/document.xml
sed 's/<w:tblPr>/<w:tblPr><w:tblStyle w:val=\"TableGrid\"\/>/g' word\document.xml > word\document2.xml
copy word\document2.xml word\document.xml /y
7z.exe u secondary_report_template.docx word\document.xml
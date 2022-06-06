zip::unzip("secondary_report_template.docx", "word/document.xml")

docu_xml <- readLines("word/document.xml")
docu_xml_edited <- gsub("<w:tblPr>", "<w:tblPr><w:tblStyle w:val=\"Table Grid\"/>", docu_xml)
writeLines(docu_xml_edited, "word/document.xml")
zip("secondary_report_template.docx", "word/document.xml")



# system(paste("unzip", "school_report_template.docx", "-d temp_dir"))


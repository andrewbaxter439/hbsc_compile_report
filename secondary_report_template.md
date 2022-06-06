---
# title: "HEALTH BEHAVIOUR IN SCHOOL-AGED CHILDREN STUDY"
output:
  officedown::rdocx_document:
      # toc: yes
      # toc_depth: 2
    reference_docx: templates/pilot_secondary_format.docx
    tables:
      style: Table Grid
      layout: autofit
      width: 1.0
      conditional:
       first_row: true
       first_column: false
       last_row: false
       last_column: false
       no_hband: false
       no_vband: true
    lists:
      ol.style: 'Number alignment: Left'
      ul.style: List Paragraph
    page_margins:
      bottom: 1
      top: 1
      right: 0.8
      left: 0.8
      header: 0
      footer: 0
      gutter: 0
params:
  censor:
    label: 'Censor small numbers? Unselect only for development!'
    input: checkbox
    value: FALSE
  school: 
    label: 'School'
    input: select
    value: "022"
    choices: ["003", "070", "075", "007", "008", "010", "013", 
"014", "017", "019", "021", "022", "024", "026", "028", "029", 
"033", "034", "035", "040", "041", "045", "046", "047", "049", 
"051", "054", "055", "058", "059", "060", "062", "064", "069", 
"072", "074", "076", "078", "083", "084", "056", "061", "063"]
---




































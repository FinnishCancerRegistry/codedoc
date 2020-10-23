Example of manually created sections
====================================

Age
---

current\_age values are formed in function birth\_date\_to\_current\_age
by countingthe number of days between today and the birth day and
dividing it by365.242199

Age group
---------

age\_group values are formed in function age\_to\_age\_group. The
functioncreates 5 age groups from age values by splittingthe age values
at breaks {0, 20, 40, 60, 80, Inf}

Example of generated sections
=============================

    invisible(lapply(key_df[["key"]], function(key) {
      
      cat(c(
        "",
        paste0("## ", key),
        "",
        codedoc_lines("age_group"),
        ""
      ), sep = "\n")
      
    }))

current\_age
------------

age\_group values are formed in function age\_to\_age\_group. The
function creates 5 age groups from age values by splitting the age
values at breaks {0, 20, 40, 60, 80, Inf}

age\_group
----------

age\_group values are formed in function age\_to\_age\_group. The
function creates 5 age groups from age values by splitting the age
values at breaks {0, 20, 40, 60, 80, Inf}

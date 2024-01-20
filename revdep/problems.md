# ctrdata

<details>

* Version: 1.16.0
* GitHub: https://github.com/rfhb/ctrdata
* Source code: https://github.com/cran/ctrdata
* Date/Publication: 2023-11-24 15:40:02 UTC
* Number of recursive dependencies: 126

Run `revdepcheck::revdep_details(, "ctrdata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ctrdata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ctrOpenSearchPagesInBrowser
    > ### Title: Open reigster to show query results or search page
    > ### Aliases: ctrOpenSearchPagesInBrowser
    > 
    > ### ** Examples
    > 
    > 
    ...
    > # Open all and check copyrights before using registers
    > ctrOpenSearchPagesInBrowser(copyright = TRUE)
    > 
    > # Open specific register advanced search page
    > ctrOpenSearchPagesInBrowser(register = "CTGOV")
    > ctrOpenSearchPagesInBrowser(register = "CTGOV2")
    > ctrOpenSearchPagesInBrowser(register = "CTIS")
    > ctrOpenSearchPagesInBrowser(register = "EUCTR")
    > ctrOpenSearchPagesInBrowser(register = "ISRCTN")
    > ctrOpenSearchPagesInBrowser(url = "status=Ended", register = "CTIS")
    ```


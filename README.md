# r_selfenc

Create a self-decrypting file for R using AES symmetric encryption. This one single R source file generated by _r_selfenc_ stores encrypted data and the function to decrypt it's data.  

## Quick start

### Encrypt

Source `main.R` and call `encrypt_to_source(...)` with some objects to produce a self-encrypted R file.

Example
```R
  test_1 <- data.frame(a=c(1,2),b=c("a","b"))
  test_2 <- function(x) {x*x / 10}
  
  encrypt_to_source(
    c("test_1","test_2"), 
    password="lcbc", 
    fn_out="/tmp/my_encrypted_sensitive_data.R"
  )
```

### Decrypt

Decryption is done by typing a single command in R. Just source the self-encrypted R file from an interactive R session (will prompt for the password).

Example
```R
  souce("/tmp/my_encrypted_sensitive_data.R")
```

## Requirements

 * digest package

## TODO

 * change ECB to CBC
 * check if decryption was successful

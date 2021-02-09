# r_selfenc

Create self-encrypted files for R using AES symmetric encryption.

## Quick start

### Encrypt

Source `main.R` and call `encrypt_to_source(...)` with some objects to produce a self-encrypted R file.

### Decrypt

Source the self-encrypted R file from an interactive R session (will prompt for the password).

## Requirements

 * digest package

## TODO

 * change ECB to CBC
 * check if decryption was successful

stopifnot("digest" %in% installed.packages())

#-------------------------------------------------------------------------------

decrypt_from_raw <- function(r_enc) {
  stopifnot("digest" %in% installed.packages())
  key32 <- digest::digest(readline("password:"), algo="sha256", raw=TRUE)
  # decrypt
  aes <- digest::AES(key32, mode="ECB")
  r_dec <- aes$decrypt(r_enc, raw=TRUE)
  # unpad data
  s_dump <- rawToChar(r_dec[r_dec>0])
  # source decrypted dump
  zz <- textConnection(s_dump)
  source(zz)
  close(zz)
}

#-------------------------------------------------------------------------------

generate_pw <- function() {
  paste(
    sample(
      unlist(
        mapply(
          sample
          ,list(
            a=letters
            ,b=LETTERS
            ,c=0:9
            ,d=c("-","/","?","!","+","*")
          ), c(3,3,2,2)
        )
      )
    )
    ,collapse=""
  )
}

#-------------------------------------------------------------------------------

encrypt_to_source <- function(objects, password=NULL, key32=NULL, 
    fn_out, on_decrypt=function(){cat("decryption successful\n")}, envir=parent.frame()) {
  if (is.null(password) && is.null(key32)) {
    # no key or pw ? -> generate pw
    password <- generate_pw();
    cat(sprintf("The password is \"%s\"\n", password))
  }
  if (!is.null(password)) {
    key32 <- digest::digest(password, algo="sha256", raw=T)
  }
  zz <- textConnection("s_dump", "w")
  dump(objects, file=zz, envir=envir)
  close(zz)
  r_dec <- charToRaw(paste(s_dump,collapse="\n"))
  # pad data
  r_dec <- c(r_dec,as.raw(rep(0,16-length(r_dec)%%16)))
  aes <- digest::AES(key32, mode="ECB")
  r_enc <- aes$encrypt(r_dec)
  dump("r_enc", file=fn_out)
  dump("decrypt_from_raw", file=fn_out, append=TRUE)
  dump("on_decrypt", file=fn_out, append=TRUE)
  # decrypt
  write(sprintf("decrypt_from_raw(r_enc)"), file=fn_out, append=TRUE)
  if (!is.null(on_decrypt)) {
    # callback
    write(sprintf("on_decrypt()"), file=fn_out, append=TRUE)
  }
  # cleanup
  write(sprintf('rm("r_enc","decrypt_from_raw","on_decrypt")'), file=fn_out, append=TRUE)
  invisible(password)
}

#-------------------------------------------------------------------------------

test <- function() {
  # create test data
  private_test_1 <- data.frame(a=c(1,2),b=c("a","b"))
  private_test_2 <- function(x) {x*x / 10}
  # setup pw
  password <- "lcbc"
  key32 <- digest::digest(password, algo="sha256", raw=T)
  # write encryted file
  encrypt_to_source(
    objects=c("private_test_1","private_test_2"),
    key=key32,
    fn_out="/tmp/my_encrypted_sensitive_data.R",
    # run some function on the data at the end
    on_decrypt=function(){
      cat("decryption successful!\n")
      cat(capture.output(summary(private_test_1)), sep="\n")
    }
  )
}

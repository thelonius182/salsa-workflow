pacman::p_load(gmailr, tidyr)

# path_old <- "/mnt/muw/client_secret_379932868231-nrpm19tnv90idmkfl0congtbr9390erg.apps.googleusercontent.com.json"
# d <- fs::dir_create(rappdirs::user_data_dir("gmailr"), recurse = TRUE)
# fs::file_move(path_old, d)

# gm_oauth_client()
options(gargle_oauth_email = "cz.teamservice@gmail.com")
gm_auth_configure()
text_msg <- gm_mime() |>
  gm_to("lcavdakker@gmail.com") |>
  gm_from("cz.teamservice@gmail.com") |>
  gm_subject("NipperStudio: job ended abnormally") |> 
  gm_text_body("Hello Mailword! #7!")

gm_send_message(text_msg)

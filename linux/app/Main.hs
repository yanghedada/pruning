import Options.Applicative.Simple

import OptParse

main :: IO ()
main = do
    (opts,runCmd) <-
        simpleOptions "Version 0.1.0.0"
                      "cliperc - Cliper Tool client executable"
                      "Run init command when using for first time"
                      (pure ()) $
        do addCommand "init"
                      "Init everything, and generate a config file (default under $HOME directory)"
                      initCmd
                      initParser
           addCommand "register"
                      "Register an account at the server"
                      registerCmd
                      registerParser
           addCommand "login"
                      "Login using registered account"
                      loginCmd
                      loginParser
           addCommand "post"
                      "Post data to the server"
                      (const undefined)
                      (pure ())
           addCommand "logout"
                      "Logout (erase token)"
                      logoutCmd
                      logoutParser
    runCmd

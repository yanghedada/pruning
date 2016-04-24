import Options.Applicative.Simple
import System.IO

import OptParse

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    (opts,runCmd) <-
        simpleOptions "Version 0.1.0.0"
                      "cliper - Cliper Tool client executable"
                      "Run init command when using for the first time"
                      (pure ()) $
        do addCommand "init"
                      "Init everything, and generate a config file \
                      \(default under $HOME directory)"
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
                      postCmd
                      postParser
           addCommand "logout"
                      "Logout (erase token)"
                      logoutCmd
                      logoutParser
           addCommand "status"
                      "Check if you have logged in"
                      statusCmd
                      statusParser
           addCommand "daemon"
                      "The Ultimate Power of Evil Dark"
                      daemonCmd
                      daemonParser
    runCmd

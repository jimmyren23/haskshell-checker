# HaskShell Checker

Jim Ren (`jimmyren`) & Hyeon Jeong Choi (`choihye`)

This project parses bash shell script to check for any syntax errors and pseudo-interpret each command to make stylistic and logical suggestions.

## Module organization

**app**

This folder contains `Main.hs` file, which has the entry point for our executable. To run, type `stack run`, which will prompt the user to enter in the script that they would like to check over. Given that it's an infinite loop, press `q` to exit out of the prompt. 

**src**

This folder contains different reusable modules we have built for the project. 

- `ShellSyntax.hs` : This module contains different data and types defined for different types of commands and structures available in bash script
- `Parsing.sh` : This module contains basic building blocks for more complex parsers that were built in `ShellParsing.sh`. We referenced `Parser.hs` from hw-05 for this portion.
- `ShellParsing.sh` : This module contains all of the parsers and helper functions used to parse the script into a the syntax that we have defined in `ShellSyntax.hs`
- `Checker.hs` : This module contains all of the logic for checking the syntax of parsed commands and also pseudo-intepret them to make suggestions
- `Suggestion` : This module includes logic from end-to-end - from reading in a script to parsing each command in the script to raising errors or suggestions
- `PrettyPrint.hs` : This module contains logic for converting parsed commands defined by our syntax back to simple strings, mainly for testing purposes. We also referenced `LuSyntax.hs` from hw-05 to better understand.

**test**

This folder contains all of the tests written for the project

- `CheckerHUnitTests.hs` : This module contains all of the unit tests written for each of the checkers
- `sample-{n}.txt` : Sample bash scripts to run on our checker


## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries

- pretty

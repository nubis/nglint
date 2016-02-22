module NgLint.Output.Common where

import NgLint.Messages
import NgLint.Common

type Formatter = String -> [LintMessage] -> IO ()

module NgLint.Common where

import Text.Parsec.Pos

data ErrorCode = NG001 | NG002 | NG003 | NG004 | NG005 deriving (Eq)

data LintMessage = LintMessage SourcePos ErrorCode deriving (Eq)

module NgLint.Messages where

import Control.Arrow ((>>>))
import NgLint.Parser
import NgLint.Position
import Text.Parsec.Pos (SourcePos)
import NgLint.Common

instance Show LintMessage where
    show (LintMessage pos code) = show pos ++ ": " ++ show code

instance Ord LintMessage where
    compare (LintMessage p1 _) (LintMessage p2 _) = compare p1 p2

instance Position LintMessage where
    getPos (LintMessage pos _) = pos

instance Show ErrorCode where
    show NG001 = "NG001: root directive inside location block"
    show NG002 = "NG002: if can be replaced with something else"
    show NG003 = "NG003: enabling SSLv3 leaves you vulnerable to POODLE attack"
    show NG004 = "NG004: enabling server_tokens leaks your web server version number"
    show NG005 = "NG005: enabling TLSv1 leaves you vulnerable to CRIME attack"

label :: ErrorCode -> [ASTNode] -> [LintMessage]
label code = map buildMessage
    where buildMessage node = LintMessage (getPos node) code

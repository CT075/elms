package elms.runtime

import elms.util.Logger

var Log: Logger = Logger.default

case class LMSRuntimeException(msg: String)
    extends RuntimeException(s"[ELMS internal error] $msg")

case class LMSUnsupportedException(msg: String)
    extends RuntimeException(s"Unsupported operation: $msg")

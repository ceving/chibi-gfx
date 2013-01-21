(load "./finalizer.so")

(import (chibi ast))
(gc)
(system "find /proc/$PPID/fd -ls|sed s%.*/proc%%")
(XOpenDisplay #f)
(gc)
(system "find /proc/$PPID/fd -ls|sed s%.*/proc%%")

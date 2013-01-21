(import (chibi ast))

(load "./finalizer.so")

(system "find /proc/$PPID/fd -ls|sed s%.*/proc%%")
(XOpenDisplay #f)
(gc)
(system "find /proc/$PPID/fd -ls|sed s%.*/proc%%")

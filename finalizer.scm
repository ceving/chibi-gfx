(import (chibi ast))

(load "./finalizer.so")

(XOpenDisplay #f)
(system "find /proc/$PPID/fd -ls|sed s%.*/proc%%")
(gc)
(system "find /proc/$PPID/fd -ls|sed s%.*/proc%%")

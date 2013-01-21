(load "./nofinalizer.so")

(let ((d (XOpenDisplay #f)))
  (system "find /proc/$PPID/fd -ls|sed s%.*/proc%%")
  (XCloseDisplay d)
  (system "find /proc/$PPID/fd -ls|sed s%.*/proc%%"))

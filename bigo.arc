(= gc-msec      current-gc-milliseconds
   process-msec current-process-milliseconds)

(mac utime body
  (w/uniq (gtime ggc gmem)
    `(with (,gtime (msec) ,ggc (gc-msec) ,gmem (memory))
       (do1 (do ,@body)
            (prn "time: " (- (msec) ,gtime)
                 " gc: " (- (gc-msec) ,ggc)
                 " mem: " (- (memory) ,gmem))))))

; TODO write a big o func/macro 
; print bigo of time and memory
; suppress print of body statements

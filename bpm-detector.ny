;nyquist plug-in
;version 4
;type analyze
;release 0.3
;author ("Vasili Bulkin")
$name (_ "Tempo detector (BPM)")
$action (_ "Detect tempo (BPM)...")
$copyright (_ "Released under terms of the GNU General Public License version 2")

$control @bpm-low (_ "Minimum BPM") real "BPM" 100 40 180
$control @bpm-high (_ "Maximum BPM") real "BPM" 200 60 240
$control text (_  "Advanced options")
$control @rms-window (_ "RMS window size") int "" 100 10 5000
$control text (_ "The following options control per-frame BPM estimation.")
$control text (_ "It is enabled when the size of frame is not 0.")
$control @frame-size (_ "Size of frame") real "s" 0 0 60
$control @frame-overlap (_ "Overlap between frames") real "s" 0 0 30

(defun arange (from to &key (step 1))
  (let ((res nil))
    (dotimes (i (truncate (/ (- to from) step)) (reverse res))
      (push (+ from (* step i)) res))))

(defun float-comp (f1 f2 &key (scale 10000))
  (= (truncate (* f1 scale))
     (truncate (* f2 scale))))

(defun try-bpm (snd bpm)
  (let* ((sr (snd-srate snd))
         (time-step (/ 60.0 bpm))
         (l (snd-length snd 1000000000))
         (scale (/ 1.0 (/ l sr time-step)))
         (init (snd-xform snd sr 0 0 time-step scale)))
    (dolist (offset
              (arange time-step (/ l sr) :step time-step)
             `(,(peak init l) ,init))
      (setq init (sum init (snd-xform snd sr 0 offset (+ offset time-step) scale))))))

(defun try-bpm-range (snd bpm-range)
  (sort (mapcar (lambda (bpm) (append `(,bpm) (try-bpm snd bpm))) bpm-range)
        (lambda (x y) (> (cadr x) (cadr y)))))

(defun find-peak-offset (snd peak)
  (dotimes (i (snd-length snd 10000000))
    (when (float-comp peak (abs (snd-fetch snd)))
      (return i))))

(defun calculate-min-step (bpm sr)
  (- (/ 60 (- (/ 60.0 bpm)
              (/ 1.0 sr)))
     bpm))

(defun get-bpm (sound sr bpm-low bpm-high &optional t0)
  (let* ((found-bpm (car (try-bpm-range sound (arange bpm-low bpm-high))))
         (step (* 0.01 (truncate (* 200 (calculate-min-step (car found-bpm) sr)))))
         (second-iter (first (car (try-bpm-range sound (arange (- (car found-bpm) 0.6)
                                                               (+ (car found-bpm) 0.6)
                                                               :step step)))))
         (peak-offset (or (find-peak-offset (third found-bpm) (second found-bpm)) 0)))
    `(,(+ t0 (/ peak-offset sr)) ,(format nil (_ "BPM = ~a") second-iter))))

(defun get-target-sample-rate (sr)
  (cond
    ((= 0 (rem (truncate sr) 11025)) 11025.0)
    ((= 0 (rem (truncate sr) 12000)) 12000.0)
    (t sr)))

(cond
  ((and (> @frame-size 0) (<= @frame-size @frame-overlap))
   (_ "Frame size must be bigger than frame overlap"))
  (t (let* ((sr (get-target-sample-rate *sound-srate*))
            (src-sound (if (arrayp *track*)
                           (sum (aref *track* 0)
                                (aref *track* 1))
                           *track*))
            (sound (rms src-sound sr @rms-window))
            (end-time (/ (snd-length sound 1000000000) sr))
            (frames (if (= 0 @frame-size)
                        '(0.0)
                        (arange 0 end-time
                                :step (- @frame-size @frame-overlap)))))
       (mapcar (lambda (frame)
                 (get-bpm (if (= @frame-size 0)
                              sound
                              (snd-xform sound sr 0 frame (+ frame @frame-size) 1))
                          sr
                          @bpm-low @bpm-high frame))
               (print frames)))))
